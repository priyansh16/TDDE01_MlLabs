#install.packages("tidyverse")
library(tidyverse)
library(kknn)
library(dbplyr)

data <- read.csv("optdigits.csv",header = FALSE)
data$V65 <- as.factor(data$V65)
num <- dim(data)[1]
set.seed(12345)
id = sample(1:num, floor(num*0.5))
trainData <- data[id,]
id_1 <- setdiff(1:num,id)
set.seed(12345)
id_2 <- sample(id_1,floor(num*0.25))
validationData <- data[id_2,]
id_3 <- setdiff(id_1,id_2)
testData <- data[id_3,]

#install.packages("kknn")

train_kknn <- kknn(V65~., train = trainData, test = trainData, kernel = "rectangular" , k=30)
test_kknn <- kknn(V65~., train = trainData, test = testData, kernel="rectangular",k=30)
confmat_train <- train_kknn$fitted.values
confmat_test <- test_kknn$fitted.values
cm_1 <- table(trainData$V65,confmat_train)
print("Confusion matrix for the training data")
cm_1
cm_2 <- table(testData$V65,confmat_test)
print("Confusion matrix for the test data")
cm_2
diagonal_values <- diag(cm_1)
print(diagonal_values)
sorted_indices <- order(diagonal_values,decreasing = TRUE)
print(sorted_indices)
sorted_values <- diagonal_values[sorted_indices]

print(sorted_values)

diagonal_values_test <- diag(cm_2)
print(diagonal_values_test)
sorted_indices_test <- order(diagonal_values_test,decreasing = TRUE)
print(sorted_indices_test)
sorted_values_test <- diagonal_values_test[sorted_indices_test]

print(sorted_values_test)

miss_class = function(y1,y2){
  n=length(y1)
  return(1-sum(diag(table(y1,y2)))/n)
}
print("Miss-classification of training data")
miss_class(trainData$V65,confmat_train)

print("Miss-classification of test data")
miss_class(testData$V65,confmat_test)

accuracy = function(y1,y2)
{
  n=length(y1)
  return(sum(diag(table(y1,y2)))/n)
   
}
print("Accuracy of the training data")
accuracy(trainData$V65,confmat_train)
print("Accuracy of the test data")
accuracy(testData$V65,confmat_test)
#install.packages("dbplyr")
trainData_new <- trainData
trainData_new$prob <- train_kknn$prob[ ,"8"]
df <- data.frame(trainData_new)
hardest <- df[order(df$prob), ] %>% head(3) # 3 hardest cases to classify
easiest <- df[rev(order(df$prob)), ] %>% head(2) #2 easiest cases to classify
heatmap(matrix(as.numeric(easiest[1,1:64]),byrow = TRUE,nrow = 8,ncol = 8),Colv = NA,Rowv = NA,col=c("white","purple"))

heatmap(matrix(as.numeric(easiest[2,1:64]),byrow = TRUE,nrow = 8,ncol = 8),Colv = NA,Rowv = NA,col=c("white","purple"))

heatmap(matrix(as.numeric(hardest[1,1:64]),byrow = TRUE,nrow = 8,ncol = 8),Colv = NA,Rowv = NA,col=c("white","red"))

heatmap(matrix(as.numeric(hardest[2,1:64]),byrow = TRUE,nrow = 8,ncol = 8),Colv = NA,Rowv = NA,col=c("white","red"))

heatmap(matrix(as.numeric(hardest[3,1:64]),byrow = TRUE,nrow = 8,ncol = 8),Colv = NA,Rowv = NA,col=c("white","red"))

missclass_training <- c()
missclass_validation <- c()
for(k in 1:30){
  kknn_training <- kknn(formula = V65~.,kernel = "rectangular",train =trainData,test = trainData,k=k )
  kknn_validation <- kknn(formula = V65~.,kernel = "rectangular",train =trainData,test = validationData,k=k)
  missclass_training <- c(missclass_training,miss_class(trainData$V65,kknn_training$fitted.values))
  missclass_validation <- c(missclass_validation,miss_class(validationData$V65,kknn_validation$fitted.values))
}

library(ggplot2)
k<-1:30
df1<-data.frame(missclass_training,k1 =k)
df2<-data.frame(missclass_validation,k2 =k)
df3 <- data.frame(df1,df2)
ggplot( ) +
  geom_line(aes(x=df3$k1,y=df3$missclass_training,
                colour="Training")) +
  geom_line(aes(x=df3$k2,y=df3$missclass_validation,
                colour="Validation")) +
  theme_minimal()+ggtitle("Finding the Optimal Parameter") +
  scale_x_continuous(breaks = seq(1,30,2)) +
  xlab("Parameter 'k'") + ylab("Missclassification") +
  geom_vline(xintercept = 3
             , linetype = "dashed",color = "green")

kknn_test <- kknn(formula = V65~.,kernel = "rectangular",train =trainData,test = testData,k=3 )
cat("for k=3 :\n")
cat("MissClassification error on Training =",missclass_training[3],"\n")
cat("MissClassification error on Validation = ",missclass_validation[3],"\n")
cat("MissClassification error on Test = ",
    miss_class(testData$V65,kknn_test$fitted.values),"\n")
library(data.table)
library(kknn)
crossEntropy_training <- c()
crossEntropy_validation <- c()
noOfClasses <- length(unique(trainData$V65))
noOfRows_training <- nrow(trainData)
noOfRows_validation <- nrow(validationData)
training_onehot <- t(sapply(as.numeric(trainData$V65),function(x)
  {c(rep(0,x-1), 1 ,rep(0 , noOfClasses-x ))}))

validation_onehot <- t(sapply(as.numeric(validationData$V65),function(x)
{c(rep(0,x-1), 1 ,rep(0 , noOfClasses-x ))}))
for(i in 1:30)
{
  fitModel_training <- kknn(V65~.,kernel = "rectangular", train = trainData, test = trainData, k=i)
  fitModel_validation <- kknn(V65~.,kernel = "rectangular", train = trainData, test = validationData, k=i)

  #cross-entropy
  crossEntropy_training <- c(crossEntropy_training,sum(training_onehot * -log(fitModel_training$prob + 10^ - 15))/noOfRows_training)
  
  crossEntropy_validation <- c(crossEntropy_validation,sum(validation_onehot * -log(fitModel_validation$prob + 10^ - 15))/noOfRows_validation)
  
}
crossEntropy_validation

crossEntropy <- melt(data.table(k = 1:30, Validation = crossEntropy_validation), "k",variable.name = " ")
ggplot(crossEntropy) + geom_line(aes(k, value),col="red") + theme_bw() +
  geom_vline(xintercept = 15, linetype = "dotted") + scale_x_continuous(breaks = seq(1,30)) +
  xlab("'k'") + ylab("Cross-Entropy") +
  ggtitle("The Optimal Parameter")




	data=read.csv("pima-indians-diabetes.csv",header = FALSE)
	colnames(data)=c("Number of times pregnant",
                  	  "Plasma",
  " Diastolic blood pressure",
	  "Triceps skinfold thickness",
	  "2-Hour serum insulin",
	  "Body mass index",
	  "Diabetes pedigree function",
	  "Age",
	  "Diabetes")
	 
	haveDiabetes= (as.integer(unlist((data$Diabetes))))
	dontHaveDiabetes= -1*(as.integer(unlist((data$Diabetes)))-1)
	plasma= as.integer(unlist((data$`Plasma`)))
	age= as.integer(unlist((data$Age)))
	plot(haveDiabetes*age,haveDiabetes*plasma,ylab = " Plasma glucose concentration",
         	     xlab = "Age",col="red")
	points(dontHaveDiabetes*age,dontHaveDiabetes*plasma,col="blue")

