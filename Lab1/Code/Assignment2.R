require(ggplot2)
require(lattice)
library(caret)
#1. Divide it into training and test data (60/40) and scale it appropriately. 
#In the coming steps, assume that motor_UPDRS is normally distributed and is a
#function of the voice characteristics, and since the data are scaled, no
#intercept is needed in the modelling.

parkinsons = read.csv("parkinsons.csv")
parkinson_data = as.data.frame(parkinsons[,c(5,7:22)])

n = dim(parkinson_data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.6))
traindata = parkinson_data[id,]
testdata = parkinson_data[-id,]
scaler <- preProcess(as.data.frame(traindata))
train_s <- predict(scaler,traindata)
test_s <- predict(scaler,testdata)

#2.compute a linear regression model from the training data, estimate training
#and test MSE and comment on which variables contribute significantly to the
#model.

lrm <- lm(motor_UPDRS ~ .-1, data = train_s)
train_predicted <- predict(lrm, newdata = train_s )
test_predicted <- predict(lrm, newdata = test_s)
MSETrain <- mean((train_predicted - train_s$motor_UPDRS)^2)
MSETest <- mean((test_predicted - test_s$motor_UPDRS)^2)
(c(MSE_of_train_data = MSETrain, MSE_of_test_data = MSETest ))
summary(lrm)

#3.Implement 4 following basic functions by using basic R commands only ( not using any external packages):
#a. loglikelihood function that for a given vector𝜽 and dispersion 𝜎 compute the log-likelihood function
#logP(T|𝜽,𝜎) for the states model and traning data.

loglikelihood <- function(parameters) {
  theta <- parameters[1:16]
  sigma <- parameters[17]
  y <- as.matrix(train_s[,-1]) %*% theta
  n <- nrow(train_s)
  return (-n/2*log(2*pi) -n/2*log(sigma^2)-sum((train_s[,1]-y)^2)/(2*sigma^2))
}

#b.Ridge function that for a given vector𝜽, scalar 𝜎 and scalar 𝜆uses function from 3a
# and adds up a Ridge penalty 𝜆‖𝜽‖^2 to the minus log likelihood.

ridgePenalty <- function(parameters, lambda){
  return(-loglikelihood(parameters)+ lambda*norm(as.matrix(parameters[1:16]),type="2")^2)
}

#c. RidgeOPt function that depends on scalar 𝜆, uses function from 3b and function optim()
# with method="BFGS" to find the optimal𝜽 and 𝜎 for given value of 𝜆.

ridgeOpt <- function(lambda){
  return (optim( par = c(rep(0,length=16),0.01), fn = ridgePenalty, lambda = lambda, method = "BFGS"))
}

#d. DF function that for a given scalar 𝜆 computes the degree of freedom of 
#the Ridgemodel based on training data.
df <- function(lambda){
  X <- as.matrix(train_s[,-1])
  P <- X %*% solve(t(X)%*%X + lambda*diag(16))%*%t(X)
  return(sum(diag(P)))
}

#4. by using RidgeOpt(), compute optimal 𝜽 for𝜆 =1, 𝜆=100, 𝜆=1000, use the estimated parameter to 
#predict the motor_UPDRS value for training and test data and report the training and Test MSE values.
# which penalty parameter is most appropriate among the selected one? compute and compare the degree of freedom
# of these model and make appropriate conclusion.

ridgeOpt(lambda = 1)
train_predict <- as.matrix(train_s[,-1]) %*% ridgeOpt(lambda = 1)$par[1:16]
test_predict <- as.matrix(test_s[,-1]) %*% ridgeOpt(lambda = 1)$par[1:16]
MSE_Train_1 <- sum((train_predict - train_s$motor_UPDRS)^2) / nrow(train_s)
MSE_Test_1 <- sum((test_predict - test_s$motor_UPDRS)^2) / nrow(test_s)


ridgeOpt(lambda = 100)
train_predict <- as.matrix(train_s[,-1]) %*% ridgeOpt(lambda = 100)$par[1:16]
test_predict <- as.matrix(test_s[,-1]) %*% ridgeOpt(lambda = 100)$par[1:16]
MSE_Train_100 <- sum((train_predict - train_s$motor_UPDRS)^2) / nrow(train_s)
MSE_Test_100 <- sum((test_predict - test_s$motor_UPDRS)^2) / nrow(test_s)


ridgeOpt(lambda = 1000)
train_predict <- as.matrix(train_s[,-1]) %*% ridgeOpt(lambda = 1000)$par[1:16]
test_predict <- as.matrix(test_s[,-1]) %*% ridgeOpt(lambda = 1000)$par[1:16]
MSE_Train_1000 <- sum((train_predict - train_s$motor_UPDRS)^2) / nrow(train_s)
MSE_Test_1000 <- sum((test_predict - test_s$motor_UPDRS)^2) / nrow(test_s)

MSE_Matrix<-matrix(c(MSE_Train_1,MSE_Test_1,MSE_Train_100,MSE_Test_100,MSE_Train_1000,MSE_Test_1000), nrow =2, ncol = 3)
rownames(MSE_Matrix) <-c("Train_MSE",'Test_MSE')
colnames(MSE_Matrix) <-c("𝜆=1","𝜆=100","𝜆=1000")
print(MSE_Matrix)

c(DF_1 = df(1),DF_100 = df(100),DF_1000 = df(1000))






practceX<- matrix(c(-5,-3,3,5,-6.5,-2.5,4.5,4.5), ncol=2, nrow=4)
practceX
practice <- matrix(c(17,19,19,22.25),nrow=2,ncol=2)
practice
eigenVector<-eigen(practice)$vectors
z<-practceX%*%eigen(practice)$vectors
AprroxX<-as.matrix(z)%*%t(as.matrix(eigenVector)) 
AprroxX

