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






