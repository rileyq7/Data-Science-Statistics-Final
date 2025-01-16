# Load all necessary packages for computation
library(readxl)
library(haven)
library(boot)
library(ggplot2)
install.packages("survey")
library(survey)
install.packages("gfoRmula")
install.packages("data.table")
library(gfoRmula)
library(data.table)


# Load Data
data <- read_dta("exam_data_2024B.dta")

# Summary of data
summary(data)

# Check for missing values
colSums((is.na(data)))

# Visualize distribution of bmi change with smoking cessation
ggplot(data, aes(x= factor(smoking_cessation), y = bmi_ch_percent, fill = factor(smoking_cessation))) +
  geom_boxplot() +
  labs(x = "Smoking Cessation", y = "BMI Change (%)", fill = "Smoking Cessation") + 
  theme_minimal()


## Model Building
# Linear regression without covariates
model1 <- lm(bmi_ch_percent ~ smoking_cessation, data = data)
summary(model1)

# Linear regression with covariates
model2 <- lm(bmi_ch_percent ~ smoking_cessation + age + sex + education + n_cigarettes + 
               CVD + dementia + diuretics + bmi, data = data)
summary(model2)


## Inverse probability weighting
# Calculate propensity scores
ps_model <- glm(smoking_cessation ~ age + sex + education + n_cigarettes + CVD + 
                  dementia + diuretics + bmi, data = data, family = binomial)

# Predicted probabilities
data$ps <- predict(ps_model, type = "response")

# Create weights
data$weight <- ifelse(data$smoking_cessation == 1, 1 / data$ps, 1 / (1 - data$ps))

# Weighted linear regression
design <- svydesign(ids = ~1, weights = ~weight, data = data)
ipw_model <- svyglm(bmi_ch_percent ~ smoking_cessation, design = design)
summary(ipw_model)


## G-formula
# manipulating data
data$interv <- -1 # 1st copy: equal to original one

interv0 <- data # 2nd copy: treatment set to 0, outcome to missing
interv0$interv <- 0
interv0$smoking_cessation <- 0
interv0$bmi_ch_percent <- NA

interv1 <- data # 3rd copy: treatment set to 1, outcome to missing
interv1$interv <- 1
interv1$smoking_cessation <- 1
interv1$bmi_ch_percent <- NA

onesample <- rbind(data, interv0, interv1) # combining datasets

# Model
std <- glm(
  bmi_ch_percent ~ smoking_cessation + sex + age + I(age ^ 2) +
    as.factor(education) + CVD + diuretics + dementia,
  data = onesample
)
summary(std)

onesample$predicted_meanY <- predict(std, onesample)

# estimate mean outcome in each of the groups interv=0, and interv=1
# this mean outcome is a weighted average of the mean outcomes in each combination
# of values of treatment and confounders, that is, the standardized outcome
mean(onesample[which(onesample$interv == -1), ]$predicted_meanY)
mean(onesample[which(onesample$interv == 0), ]$predicted_meanY)
mean(onesample[which(onesample$interv == 1), ]$predicted_meanY)

# function to calculate difference in means
standardization <- function(data, indices) {
  # create a dataset with 3 copies of each subject
  d <- data[indices, ] # 1st copy: equal to original one`
  d$interv <- -1
  d0 <- d # 2nd copy: treatment set to 0, outcome to missing
  d0$interv <- 0
  d0$smoking_cessation <- 0
  d0$bmi_ch_percent <- NA
  d1 <- d # 3rd copy: treatment set to 1, outcome to missing
  d1$interv <- 1
  d1$smoking_cessation <- 1
  d1$bmi_ch_percent <- NA
  d.onesample <- rbind(d, d0, d1) # combining datasets
  
  # linear model to estimate mean outcome conditional on treatment and confounders
  # parameters are estimated using original observations only (interv= -1)
  # parameter estimates are used to predict mean outcome for observations with set
  # treatment (interv=0 and interv=1)
  fit <- glm(
    bmi_ch_percent ~ smoking_cessation + sex + age + I(age ^ 2) +
      as.factor(education) + CVD + diuretics + dementia,
    data = d.onesample
  )
  
  d.onesample$predicted_meanY <- predict(fit, d.onesample)
  
  # estimate mean outcome in each of the groups interv=-1, interv=0, and interv=1
  return(c(
    mean(d.onesample$predicted_meanY[d.onesample$interv == -1]),
    mean(d.onesample$predicted_meanY[d.onesample$interv == 0]),
    mean(d.onesample$predicted_meanY[d.onesample$interv == 1]),
    mean(d.onesample$predicted_meanY[d.onesample$interv == 1]) -
      mean(d.onesample$predicted_meanY[d.onesample$interv == 0])
  ))
}

# bootstrap
results <- boot(data = data,
                statistic = standardization,
                R = 500)

# generating confidence intervals
se <- c(sd(results$t[, 1]),
        sd(results$t[, 2]),
        sd(results$t[, 3]),
        sd(results$t[, 4]))
mean <- results$t0
ll <- mean - qnorm(0.975) * se
ul <- mean + qnorm(0.975) * se

bootstrap <-
  data.frame(cbind(
    c(
      "Observed",
      "No Treatment",
      "Treatment",
      "Treatment - No Treatment"
    ),
    mean,
    se,
    ll,
    ul
  ))
bootstrap


