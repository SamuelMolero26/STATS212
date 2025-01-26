getwd()
setwd("/Users/samuel/Desktop/stats/212/Homeworks/Data")

data <- read.table("BacteriaDeath.txt", header = TRUE)

t <- data[, 1]
y_t <- data[, 2]

#(t,y) Graph
plot(x = t, y = y_t, xlab = "t", ylab = "y", main = "(t,y) Graph")

#(t, log(y)) graph
y_log <- log(y_t)
plot(x = t, y = y_log, xlab = "t", ylab = "log(y)", main = "(t,log(y)) Graph")

lm_fit <- lm(y_log~t)
summary(lm_fit)
#equation: log(y) = 5.9732 - 0.2184t
# solving for y = e^(5.9732 - 0.2184t)

data <- read.table("WindSpeed.txt", header = TRUE)
output <- data[,1]
speed <- data[,2]

plot(x = speed,  y= output, xlab = "speed",  ylab= "output", main = "(x,y)")

speed2 <- 1 / (data[,2])
plot(x = speed2,  y= output, xlab = "speed",  ylab= "output", main = "(1/x,y)")

lim_fit <-lm(output~speed2)
resid <-lim_fit$residuals
predict<-lim_fit$fitted.values

plot(speed2, resid, xlab="x", ylab="Residuals", main = "Residual Plot for Non-linearity Checking")
plot(predict, resid,xlab="yhat", ylab="...", main ="Residual Plot for Error Variance Checking")

hist(resid,xlab ="Residuals", main = "Histogram of the Residuals")
plot(density(speed2), xlab = "Residuals", ylab = "Density", main = "Density Estimate of Residuals") #density estimator
qqnorm(resid) #normal quantile plot


#equation: y = 2.9789 -6.935/x





