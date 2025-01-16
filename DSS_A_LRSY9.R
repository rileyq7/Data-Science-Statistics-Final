library(haven)
library(ggplot2)
library(car)
library(tidyverse)
library(geepack)
library(lmtest)
library(pROC)
library(ResourceSelection)

# Load Data
exam_dtax <- read_dta("exam_data_2024A.dta")

summary(exam_dtax)

# descriptive statistics for those without diabetes
summary(exam_dta[which(exam_dta$diabetes == 0),]$age)
summary(exam_dta[which(exam_dta$diabetes == 0),]$bmi)
summary(exam_dta[which(exam_dta$diabetes == 0),]$skin_thickness)
summary(exam_dta[which(exam_dta$diabetes == 0),]$glucose)
summary(exam_dta[which(exam_dta$diabetes == 0),]$blood_pressure)
summary(exam_dta[which(exam_dta$diabetes == 0),]$insulin)
summary(exam_dta[which(exam_dta$diabetes == 0),]$pregnancies)
summary(exam_dta[which(exam_dta$diabetes == 0),]$diabetes_genetic_score)

# Descriptive statistics for those with diabetes
summary(exam_dta[which(exam_dta$diabetes == 1),]$age)
summary(exam_dta[which(exam_dta$diabetes == 1),]$bmi)
summary(exam_dta[which(exam_dta$diabetes == 1),]$skin_thickness)
summary(exam_dta[which(exam_dta$diabetes == 1),]$glucose)
summary(exam_dta[which(exam_dta$diabetes == 1),]$blood_pressure)
summary(exam_dta[which(exam_dta$diabetes == 1),]$insulin)
summary(exam_dta[which(exam_dta$diabetes == 1),]$pregnancies)
summary(exam_dta[which(exam_dta$diabetes == 1),]$diabetes_genetic_score)

# Distribution plots 
# Age
boxplot(age ~ as_factor(diabetes), data = exam_dtax,
        xlab = "Diabetes", ylab = "Age",
        main = "Boxplot of Age by Diabetes Status")
# BMI
boxplot(bmi ~ as_factor(diabetes), data = exam_dtax,
        xlab = "Diabetes", ylab = "BMI",
        main = "Boxplot of BMI by Diabetes Status")
# Skin Thickness
boxplot(skin_thickness ~ as_factor(diabetes), data = exam_dtax,
        xlab = "Diabetes", ylab = "Skin Thickness",
        main = "Boxplot of Skin Thickness by Diabetes Status")
# Glucose
boxplot(glucose ~ as_factor(diabetes), data = exam_dtax,
        xlab = "Diabetes", ylab = "Glucose",
        main = "Boxplot of Glucose by Diabetes Status")
# Blood Pressure
boxplot(blood_pressure ~ as_factor(diabetes), data = exam_dtax,
        xlab = "Diabetes", ylab = "Blood Pressure",
        main = "Boxplot of Blood Pressure by Diabetes Status")
# Insulin
boxplot(insulin ~ as_factor(diabetes), data = exam_dtax,
        xlab = "Diabetes", ylab = "Insulin",
        main = "Boxplot of Insulin by Diabetes Status")
# Pregnancies
boxplot(pregnancies ~ as_factor(diabetes), data = exam_dtax,
        xlab = "Diabetes", ylab = "Pregnancies",
        main = "Boxplot of Pregnancies by Diabetes Status")
# Diabetes Genetic Score
boxplot(diabetes_genetic_score ~ as_factor(diabetes), data = exam_dtax,
        xlab = "Diabetes", ylab = "Diabetes Genetic Score",
        main = "Boxplot of Diabetes Genetic Score by Diabetes Status")

# Split data with randomization
set.seed(12345)

exam_dtax <- exam_dtax %>%
  mutate(rvar = runif(n())) %>% 
  arrange(rvar) %>%
  mutate(S = if_else(row_number() > 1000, 1, 0))

### Fit a model to the training data

# Logistic regression to model test data
model <- glm(diabetes ~ age + bmi + skin_thickness + glucose + blood_pressure + 
               insulin + pregnancies + diabetes_genetic_score,
             data = exam_dtax %>% filter(S == 0), family = binomial())
tidy_model <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
print(tidy_model)

# predict linear predictor
exam_dtax$m2lp <- predict(model, newdata = exam_dtax, type = "link")

# predict probabilties for all individuals
exam_dtax$m2pr <- predict(model, newdata = exam_dtax, type = "response")

# summarize precited probabilities of all individuals against diabetes
exam_dtax %>% 
  group_by(diabetes) %>% 
  summarise(mean_prob = mean(m2pr, na.rm = TRUE),
            sd_prob   = sd(m2pr, na.rm = TRUE),
            min_prob  = min(m2pr, na.rm = TRUE),
            max_prob  = max(m2pr, na.rm = TRUE))

# Boxplot of predicted prbabilities against Diabetes
boxplot(m2pr ~ as_factor(diabetes), data = exam_dtax,
        xlab = "Diabetes Status", ylab = "Predicted Probabilities",
        main = "Boxplot of Predicted Probabilities by Diabetes Status")


### Model Validation

## ROC Curves

# ROC curve for S==0
roc_0 <- roc(exam_dtax$diabetes[exam_dtax$S == 0], exam_dtax$m2pr[exam_dtax$S == 0])
#Plot Roc with x-axis specificity
plot(roc_0, main = "ROC Curve for S=0", print.thres = TRUE, print.auc = TRUE, 
     xlim = c(0, 1), ylim = c(0, 1), )

# ROC curve for S==1
roc_1 <- roc(exam_dtax$diabetes[exam_dtax$S == 1], exam_dtax$m2pr[exam_dtax$S == 1])
plot(roc_1, main = "ROC Curve for S=1", print.thres = TRUE, print.auc = TRUE,
     xlim = c(0, 1), ylim = c(0, 1), )

# Plot ROC curve for S == 0
plot(roc_0, main = "ROC Curves for S=0 and S=1", 
     xlab = "1 - Specificity", ylab = "Sensitivity", 
     col = "blue", print.auc = TRUE, 
     xlim = c(0, 1), ylim = c(0, 1), print.auc.y = 0.4)
# Add the ROC curve for S == 1 to the same plot
plot(roc_1, col = "red", add = TRUE, print.auc = TRUE,
     print.auc.y = 0.3)
# Add a legend
legend("bottomright", legend = c("S = 0", "S = 1"), 
       col = c("blue", "red"), lwd = 2)

## Goodness of fit test - Hosemer Lemeshow

# For S==0
hl_data_0 <- na.omit(data.frame(diabetes = exam_dtax$diabetes[exam_dtax$S == 0], fitted_values = exam_dtax$m2pr[exam_dtax$S == 0]))
hl_test_0 <- hoslem.test(hl_data_0$diabetes, hl_data_0$fitted_values, g = 10)
print(hl_test_0)

#tabulate observed vs. expected with deciles
hl_table_S0 <- exam_dtax %>%
  filter(S == 0) %>%
  mutate(decile = ntile(m2pr, 10)) %>%     # Create decile groups
  group_by(decile) %>%
  summarise(
    observed_0 = sum(diabetes == 0),
    observed_1 = sum(diabetes == 1),
    expected_0 = sum(1 - m2pr),
    expected_1 = sum(m2pr),
    total_observed = observed_0 + observed_1,
    total_expected = expected_0 + expected_1
  )
print("Hosmer-Lemeshow Table for S = 0")
print(hl_table_S0)


# Perform test for S == 1
hl_data_1 <- na.omit(data.frame(diabetes = exam_dtax$diabetes[exam_dtax$S == 1], fitted_values = exam_dtax$m2pr[exam_dtax$S == 1]))
hl_test_1 <- hoslem.test(hl_data_1$diabetes, hl_data_1$fitted_values, g = 10)
print(hl_test_1)

# Tabulate observed vs expected counts by deciles for S == 1
hl_table_S1 <- exam_dtax %>%
  filter(S == 1) %>%
  mutate(decile = ntile(m2pr, 10)) %>%     # Create decile groups
  group_by(decile) %>%
  summarise(
    observed_0 = sum(diabetes == 0),
    observed_1 = sum(diabetes == 1),
    expected_0 = sum(1 - m2pr),
    expected_1 = sum(m2pr),
    total_observed = observed_0 + observed_1,
    total_expected = expected_0 + expected_1
  )
print("Hosmer-Lemeshow Table for S = 1")
print(hl_table_S1)


## Create cut variables for S==0 and S==1 seperately
exam_dtax <- exam_dtax %>%
  mutate(
    m2prg0 = if_else(S == 0, 
                     as.integer(cut(m2pr, breaks = quantile(m2pr[S == 0], probs = seq(0, 1, by = 0.1)), 
                                    include.lowest = TRUE, labels = FALSE)) - 1, 
                     NA_integer_),
    m2prg1 = if_else(S == 1, 
                     as.integer(cut(m2pr, breaks = quantile(m2pr[S == 1], probs = seq(0, 1, by = 0.1)), 
                                    include.lowest = TRUE, labels = FALSE)) - 1, 
                     NA_integer_)
  )

## Graph sums of predicted probabilities and actual outcomes

# Summarize and Reshape data S==0
data_s0 <- exam_dtax %>%
  filter(S == 0) %>%
  group_by(m2prg0) %>%
  summarise(
    sum_m2pr = sum(m2pr),
    sum_diabetes = sum(diabetes)
  ) %>%
  pivot_longer(cols = c(sum_m2pr, sum_diabetes), names_to = "variable", values_to = "value")

# Bar plot S==0
ggplot(data_s0, aes(x = factor(m2prg0), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(
    values = c("sum_m2pr" = "blue", "sum_diabetes" = "red")) +
  labs(x = "Predicted Probability Groups (S=0)", y = "Sum",
       title = "Sum of Predicted Probabilities and Diabetes (S=0)", fill = "") +
  theme_minimal()

# Summarize and reshape data for S==1
data_s1 <- exam_dtax %>%
  filter(S == 1) %>%
  group_by(m2prg1) %>%
  summarise(
    sum_m2pr = sum(m2pr),
    sum_diabetes = sum(diabetes)
  ) %>%
  pivot_longer(cols = c(sum_m2pr, sum_diabetes), names_to = "variable", values_to = "value")

# Bar plot for S == 1
ggplot(data_s1, aes(x = factor(m2prg1), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(
    values = c("sum_m2pr" = "blue", "sum_diabetes" = "red")) +
  labs(x = "Predicted Probability Groups (S=0)", y = "Sum",
       title = "Sum of Predicted Probabilities and Diabetes (S=1)", fill = "") +
  theme_minimal()

# Initialize a data frame to store the results
auc_data <- data.frame(run = integer(), auc0 = numeric(), auc1 = numeric())

# Set the number of iterations
n_iterations <- 10

for (i in 1:n_iterations) {
  
  # Randomly split the data (replicate the steps from before)
  exam_dtax$rvar <- runif(nrow(exam_dtax))
  exam_dtax <- exam_dtax[order(exam_dtax$rvar), ]
  exam_dtax$S <- 0
  exam_dtax$S[exam_dtax$rvar > exam_dtax$rvar[1000]] <- 1
  
  # Fit the model in the training data (S == 0)
  model <- glm(diabetes ~ age + glucose + blood_pressure + bmi + skin_thickness +
                 insulin + pregnancies + diabetes_genetic_score, data = exam_dtax %>% filter(S == 0), family = binomial())
  
  # Predict the probabilities for all individuals
  exam_dtax$m2pr <- predict(model, newdata = exam_dtax, type = "response")
  
  # Calculate AUC for S == 0
  roc_0 <- roc(exam_dtax$diabetes[exam_dtax$S == 0], exam_dtax$m2pr[exam_dtax$S == 0])
  auc0 <- auc(roc_0)
  
  # Calculate AUC for S == 1
  roc_1 <- roc(exam_dtax$diabetes[exam_dtax$S == 1], exam_dtax$m2pr[exam_dtax$S == 1])
  auc1 <- auc(roc_1)
  
  # Store the results in the data frame
  auc_data <- rbind(auc_data, data.frame(run = i, auc0 = auc0, auc1 = auc1))
}
# Save the AUC data to a CSV file
write.csv(auc_data, "auc_data.csv", row.names = FALSE)
