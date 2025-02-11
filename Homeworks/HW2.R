getwd()
setwd("/Users/samuel/Desktop/stats/212/Homeworks/Data")

tree_age <- read.table("TreeAgeDiamSugarMaple.txt", header = T)
x <- tree_age$Diamet
y <- tree_age$Age

#function definition
AICpoly = function(x,y,kmax) {
  adjr2 = 1:kmax
  aic  = 1:kmax
  bic =1:kmax
  n=length(y)
  for(k in 1:kmax){
    fit = lm(y~poly(x,k, raw=T))
    aic[k] = AIC(fit)
    bic[k] = AIC(fit, k=log(n))
    adjr2[k] = summary(fit)$adj
  }
  kr2 = (1:kmax)[adjr2==max(adjr2)]
  kaic = (1:kmax)[aic == min(aic)]
  kbic = (1:kmax)[bic == min(bic)]
  ylim = range(c(aic, bic))
  plot(1:k, aic, xlab ='k', ylab ='aic/bic',
       ylim=ylim, col='red')
  points(1:k, bic, col ='blue')
  abline(v=kaic, col = 'red')
  abline(v = kbic, col = 'blue')
  title("Red points: AIC  | Blue Points: BIC")
  list(aic,bic, adjr2, kaic, kbic, kr2)
  
  return (list(AIC = aic, BIC = bic, Adjusted_R2 = adjr2, Best_AIC_k = kaic, Best_BIC_k = kbic, Best_Adjusted_R2_k = kr2))
}
#part A
results=AICpoly(x,y,8)
print(results)
#best one is degree 2

#part B
tree_fit <- lm(y~poly(x, degree = 2, raw = TRUE))
summary(tree_fit) #reject the null 

resid = tree_fit$residuals
fitted = tree_fit$fitted.values
plot(fitted, resid, xlab ="Fitted", ylab = "Residuals")


#Part d
newtree = data.frame(x=110)
temp. <- predict(tree_fit, newdata = newtree, interval = "predict", level =0.95)
plot(temp)


#Problem # 1
#Page 43 Lecture 1-A
#page 25 Lecture 1-A








