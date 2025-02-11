 #Ensures reproducibility of results. #value inside is the starting value in the sequence

#n = 200 # Larger sample size #

n = 30 # Original sample size used in the results shown in the lecture slides #

degree = 12  # The max degree of polynomials fitted. #

#
# Define x vector.
#
x=((1:n)-0.5)/n # this is a sequence
#
# Initialize (a) matrix of polynomial degrees and (b) aic, bic and
# adjusted R^2 vectors. 
#
degree=matrix(0,1000,3)
aic=1:12
bic=1:12
adjr2=1:12


#
# Begin the simulation.
#
for(j in 1: dim(degree)[1]){
  #
  # Define a vector of y-values.
  #
  y=11.8+28*x-70*x^2+50*x^3+rnorm(n)
  #
  # For each degree from 1 to 12, fit a polynomial model and determine
  # the values of aic, bic and adjusted-R^2.
  #
  for(deg in 1:12){
    fit=lm(y~poly(x,deg,raw=T))
    aic[deg]=AIC(fit)
    bic[deg]=AIC(fit,k=log(length(y)))
    adjr2[deg]=summary(fit)$adj
  }
  #
  # Determine the degree that minimizes each of aic and bic, and
  # maximizes adjusted-R^2.
  #
  degree[j,1]=(1:12)[aic==min(aic)]
  degree[j,2]=(1:12)[bic==min(bic)]
  degree[j,3]=(1:12)[adjr2==max(adjr2)]
  #
  # Repeat.
  #
}

summary.mat = matrix(0,nrow=deg,ncol=3)
for(j in 1:deg){
  for(i in 1:3){
    ind = which(degree[,i]==j)
    summary.mat[j,i] = length(ind)/1000*100
  }
}

summary.selected.degree = cbind(c(1:12),summary.mat)
colnames(summary.selected.degree) = c("Degree", "AIC", "BIC", "Adj.R2")

summary.selected.degree










