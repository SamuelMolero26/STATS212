data = read.table("Mesquite-Tree-Data.txt", header=F, skip=1)

## Importance of header = F and skip = 3 in this case (check how the original data file looks like and you will know!) ##


colnames(data) = c("diam1", "diam2", "toth", "canht", "leafwt")

head(data)


u1 = data[,1];
print(u1)
u1 = data[,"diam1"]; #best apprach
print(u1)
u2 = data[,"diam2"];
u3 = data[,"toth"]; 
u4 = data[,"canht"];
z = data[,"leafwt"]




x1 = log(u1); x2 = log(u2); x3 = log(u3); x4 = log(u4); 
y = log(z) 

log.data = log(data); 
colnames(log.data) = paste("l",colnames(data),sep="") #paste is to append
head (log.data)

pairs(log.data)



fit = lm(y~x1+x2+x3+x4)

summary(fit)

U0 = c(2.5,2,1.7,0.92)

X0 = log(U0)

predict(fit,newdata=data.frame(x1=X0[1],x2=X0[2],x3=X0[3],x4=X0[4]), interval="conf",level=0.95)

predict(fit,newdata=data.frame(x1=X0[1],x2=X0[2],x3=X0[3],x4=X0[4]), interval="predict",level=0.95)


anova(fit)#SSE (Sum sq) -> for the whole model


fit.red = lm(y~x2+x4)

anova(fit.red)

sse.full = anova(fit)[5,2]
print(sse.full)
sse.red = anova(fit.red)[3,2]
print(sse.red)

F.statistic = ((sse.red - sse.full)/2)/(sse.full/15)
sse.full
sse.red
F.statistic

qf(0.95, 2,15)



### Model selection using leaps package ###

install.packages("leaps")

require(leaps)


obj = leaps(y=log.data[,5],x=log.data[,-5],method="r2")

obj.best = leaps(y=log.data[,5],x=log.data[,-5],method="r2",nbest=1)

obj

obj.best

### Outlier detection via Cook's D Distance ###

 output = cooks.distance(fit)
 plot(output,ylab="Cook's D")


