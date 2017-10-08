
getwd()
setwd("/Users/manishreddybendhi/Desktop/Fun/Rprogramming/Assigenment2DataScience/Assigenment3")
getwd()

data1=read.table("heart.dat",header = TRUE)
head(data1)
data1
colnames(data1)=c("age","sex","Chestpain","rbp","serum","fbp","fendographic","maxHR","exericseInduced","oldpeak","slopepaekexce","vessels","defecttype","Diseace")
#data1$Diseace=as.integer(data1$Diseace)
data1

#glmmodel1=glm(Diseace~ age+sex+Chestpain+rbp+serum+fbp+fendographic+maxHR+exericseInduced+oldpeak+slopepaekexce+vessels+defecttype+Diseace,family = binomial(link = "logit"),data1)
#glmmodel1=glm(Diseace ~ age+sex,family = binomial(link = "logit"),data1)
#glmmodel1=glm(X2 ~ X3.0.1+X3.0+X2.0.1+X2.4+X0.0.1+X109.0+X2.0+X0.0+X322.0+X130.0+X4.0+X1.0+X70.0,family = binomial(link = "logit"),data1)

#Setting  the value of defect type to 3
data1$defecttype[data1$defecttype==3]=0
data1$defecttype[data1$defecttype==6]=1
data1$defecttype[data1$defecttype==7]=1
data1$defecttype


data1


glmmodel1=glm(defecttype~ age+sex+Chestpain+rbp+serum+fbp+fendographic+maxHR+exericseInduced+oldpeak+slopepaekexce+vessels+Diseace,family = binomial(link = "logit"),data1)
summary(glmmodel1)
##Using the sub set values sex and disease 
#glmmodel1=glm(defecttype~ sex+Diseace,family = binomial(link = "logit"),data1)
summary(glmmodel1)
#plot( data1$defecttype,data1$sex , xlab="sex", ylab="disease")
#lines(data1$age, glmmodel1$fitted, type="l", col="red", lwd=3)
#data1
plot(glmmodel1)
newdata1=c(data1$sex,data1$Diseace)
newdata1
#data1[2]
#data1[data1$sex,data1$Diseace]
#predict(glmmodel1,(12))
#new.df <- data.frame(sex=c(0,1),Disease=c(1,0))
predict(glmmodel1,newdata1=new)

data1$defecttype


