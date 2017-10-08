getwd()
setwd("/Users/manishreddybendhi/Desktop/Fun/Rprogramming/Assigenment2DataScience/CHDAGE")
CHDdata=read.table("CHDAGE.txt",header = TRUE)  
CHDdata
head(CHDdata,10)
summary(CHDdata)
str(CHDdata)
CHDdata$CHD
plot(CHDdata$AGE,CHDdata$CHD,xlab = "Age",ylab = "Chd")
range(CHDdata$AGE)
(AGEbreaks =seq(from = 30,to=60,by=5))
AGEbreaks

(AGEbreaks=c(20,AGEbreaks,70))
CHDdata$AGE
AGEbreaks
CHDdata$AgeGroups = cut(CHDdata$AGE,breaks = AGEbreaks,right = FALSE)
CHDdata$AgeGroups
head(CHDdata)
tail(CHDdata)
(table1=table(CHDdata$AgeGroups))
(table2=table(CHDdata$CHD,CHDdata$AgeGroups))
(table3=tapply(CHDdata$CHD,INDEX = CHDdata$AgeGroups,FUN = mean))
table4=cbind(as.vector(table1),t(table2),table3)
table1
table2
table3
table4
colnames(table4)=c("n", "CHD absent", "CHD present", "mean % present")
table4
c(5,rep(2.5,6),5)
midpoints = AGEbreaks[-length(AGEbreaks)]+c(5,rep(2.5,6),5)
midpoints
plot(midpoints,table4[,4],col="red",xlab = "",ylab = "")

glmCHD = glm(CHD~AGE,family = binomial(link = "logit"),CHDdata)
CHDdata
summGlmCHD=summary(glmCHD)
summGlmCHD
logLik(glmCHD)
plot(CHDdata$AGE,CHDdata$CHD,xlab = "Age",ylab = "Chd")
#glmCHD$fitted
lines(CHDdata$AGE,glmCHD$fitted,type="1",col ="red",lwd=3)


plot(CHDdata$AGE, CHDdata$CHD, , xlab="Age", ylab="Chd")
lines(CHDdata$AGE, glmCHD$fitted, type="l", col="red", lwd=3)
(vcovCHD = vcov(glmCHD))


X = CHDdata$AGE
Y = CHDdata$CHD

likelihood = function(b){
  prod(exp(b[1] + b[2] * X[Y==1]) /(1 + exp(b[1] + b[2] * X[Y==1]))) * prod(1 /(1 + exp(b[1] + b[2] * X[Y==0])))  
}


b0 = seq(-8, -2, length.out = 60)
b1 = seq(0.06, 0.18, length.out = 60)

likelihood_vect = Vectorize(function(b0, b1)likelihood(c(b0, b1)))

par(mar = c(0.6, 0.1,0.6,0.1))
zp = outer(X = b0, Y = b1, likelihood_vect)
persp(b0, b1, zp, theta=75, phi=20, col="lightblue", xlab="b0", ylab="b1", zlab="Likelihood")

