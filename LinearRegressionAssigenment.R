getwd()
help("getwd")
setwd("/Users/manishreddybendhi/Desktop/Git/GitMeashineLearningRepo/Liearner Regression/LinearRegressions")
getwd()
SoundinDb <- function(req1,req2) {
  setwd(req1)
  #Fitting the model  ito a table 
  Y <- read.table(req2)
  #Using the liner model to filt the data 
  sat.lmod2 <- lm(V6 ~ V1+V2+V3+V4+V5 ,data = Y)
  #Plotting a graph
  plot(Y)
  #Getting the summary of the model 
  summary(sat.lmod2)
  #Predicting the values and fitting it in the table 
  table(predict(sat.lmod2,Y[0:5]))
  
}
resultatntfun <-SoundinDb("/Users/manishreddybendhi/Desktop/Git/GitMeashineLearningRepo/Liearner Regression/LinearRegressions","airfoil_self_noise.dat")

resultatntfun

#using the iris data on the lm model to predict the outcomes of flowers 
#Reading the model in the csv file 
Z <- read.csv("iris.data")
#group1 = Z$A+Z$B+Z$C+Z$D+Z$E
colnames(Z) <- c("A","B","C","D","E")
group1
#Z<-read.table(Z)
#Using the linier model 
set.mod.iris <- lm(E~A+B+C+D,data = Z)
Z[0:4]
set.mod.iris
predict(set.mod.iris,Z[0:4])

