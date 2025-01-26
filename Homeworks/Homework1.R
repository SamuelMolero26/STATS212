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






