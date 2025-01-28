getwd()
setwd("/Users/samuel/Desktop/stats/212/Homeworks/Data")

par(mfrow = c(1,1))
par(mar = c(4,4,4,4))

data <- read.table("BacteriaDeath.txt", header = TRUE)

t <- data[, 1]
y_t <- data[, 2]

#(t,y) Graph
#plot(x = t, y = y_t, xlab = "t", ylab = "y", main = "(t,y) Graph")

#(t, log(y)) graph
y_log <- log(y_t)
plot(x = t, y = y_log, xlab = "t", ylab = "log(y)", main = "(t,log(y)) Graph")

lm_fit <- lm(y_log~t)
#summary(lm_fit)
#equation: log(y) = 5.9732 - 0.2184t
# solving for y = e^(5.9732 - 0.2184t)

data <- read.table("WindSpeed.txt", header = TRUE)
output <- data[,1]
speed <- data[,2]

plot(x = speed,  y= output, xlab = "speed",  ylab= "output", main = "(x,y)")

speed2 <- 1 / (speed)
plot(x = speed2,  y= output, xlab = "speed",  ylab= "output", main = "(1/x,y)")

lim_fit <-lm(output~speed2)
summary(lim_fit)

resid <-lim_fit$residuals
predict<-lim_fit$fitted.values

plot(speed2, resid, xlab="speed", ylab="Residuals", main = "Residual Plot for Non-linearity Checking")
plot(predict, resid,xlab="yhat", ylab="...", main ="Residual Plot for Error Variance Checking")

hist(resid,xlab ="Residuals", main = "Histogram of the Residuals", breaks = "FD")
plot(density(resid), xlab = "Residuals", ylab = "Density", main = "Density Estimate of Residuals") #density estimator
qqnorm(resid) #normal quantile plot

#for 99% CI
confint(lim_fit, level=0.99)

#for 95% CI given an average
predict(lm(output~speed2), data.frame(speed=3.2), interval ="confidence", level=0.95)
#ask about this one

predict(lm(output~speed2), data.frame(speed=9.05), interval ="prediction", level=0.95)

#equation: y = 2.9789 -6.935/x





