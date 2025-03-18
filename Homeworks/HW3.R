getwd()
setwd("/Users/samuel/Desktop/stats/212/Homeworks/Data")

install.packages("leaps")

require(leaps)
library(leaps)

df <- read.csv("Baseball-Salary-Data.csv")
head(df)

m1 <- lm(salary ~ . - player, data = df)
summary(m1)

par(mfrow = c(2,2))
plot(m1)

#b is jus the R square of 0.7014, which translates to 70.14%
#c The coefficient for the predictor is -2.698 which predicts a decrease in salaries holding all the other variables constant.
#This could be due to the variation in salary data is explained by other variables such as home run and stolen bases.
#d Given that the P-value is 2.2e-16, which is smaller than 0.05 thus rejecting the null. This means that the model has good utility, and is a helpful indicative at predicting salary data.

m2 <- lm(salary ~ . - player - batting.average - on.base.percent - hits - doubles - triples, data = df)
summary(m2)
#e
SSE1 = mean(m1$residuals^(2))
SSE2 = mean(m2$residuals^(2))
DF1 = 5
DF2 = nrow(df) - 16 - 1
FStat = ((SSE2-SSE1)/DF1)/(SSE1/DF2)
pValue = pf(FStat, DF1, DF2, lower.tail = F)
print(pValue)

#f This value will be the R-squared which is 0.6981 about 69.81%

#g
par(mfrow = c(1, 3))
m2_summ <- summary(m2)
resids <- m2$residuals
plot(m2$fitted.values, resids, xlab = "Predicted", ylab = "Residuals", main = "(i)")
plot(density(resids), main = "(ii)")
std_resid <- (resids - mean(resids)) / sd(resids)

qqnorm(std_resid, main = "(iii)")
qqline(std_resid)


#Strategy:
# Evalue using: AIC: Balance model fit and complexity
# BIC: Penalize more than AIC
# Mallows' Cp: Identify the models with the low bias and variance trade offs


source("AIC-Leaps.R")
df <- read.csv("Baseball-Salary-Data.csv")
df <- df[, -18]

library(leaps)
leaps_ic <- leaps.AIC(df[,2:17], df[, 1])
# includes all predictors             incliudes the response variable
leaps_output <- leaps(df[, 2:17], y = df[, 1], nbest = 1) # perform AIC\BIC calc

var_names <- colnames(df[, 2:17]) # get predictors name
best_model_index <- which.min(leaps_output$Cp) # Get selected variables
var_mask <- leaps_output$which[best_model_index, ] 
model_vars <- var_names[var_mask] # extract the best predictor names
model_vars


# j 
#k

# i
fits <- lm(salary ~ . , data = df[, c("salary", model_vars)])
resids <- fits$residuals
std_resid <- (resids - mean(resids)) / sd(resids)
par(mfrow = c(1,1))
plot(std_resid)
abline(h = c(-3,3), col = "blue")

extremes <- which(abs(std_resid) > 3)
extremes
df[extremes, ]

# j 
predicted_values <- fitted(fits)

# Plot standardized residuals vs. predicted values
plot(predicted_values, std_residuals, xlab = "Predicted Salary", ylab = "Standardized Residuals",
     main = "Standardized Residuals vs. Predicted Values", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
#k

colMeans(df)

plot(fits$fitted.values, std_resid, xlab = "Predicted", ylab = "Residual")

qqnorm(std_resid)
qqline(std_resid)

par(mfrow = c(1, 1))
par(mar=c(3,3,3,3))
cd <- cooks.distance(fits)
plot(cd, ylab = "Cook's D")


influential_points <- which(cd > (4 / length(cd)))
print(influential_points)

df[influential_points, ]


### QUESTION 2 #####

#b
dta2 <- read.table("SleepRem.txt", header = TRUE, sep = "")
attach(dta2)
fit <- aov(values ~ as.factor(ind), data = dta2)
anova(fit)

# Null should be rejected given that p value is smaller than 0.05

#c
#To test variance
anova(aov(resid(aov(values ~ ind))**2 ~ ind))

# p-value of 0.621. This suggests that the assumption of equal
#variance is approximately valid.

#to test stability
shapiro.test(resid(aov(values ~ ind)))
# p-value = 0.1285 suggests that the normality assumption
#approximately holds.

fit = aov(values ~ ind)
boxplot(resid(fit))

plot(fit, which=2)

# plots also suggest that the normality assumption is approximately satisfied, in agreement with the Shapiro-Wilk test p-value.








