#install.packages("neuralnet")
library(neuralnet)

set.seed(1234567890)
#part-1
# generating 500 data points between interval [0,10]
data_points <- data.frame(x=runif(500,0,10))
data_points$y <-sin(data_points$x)
# split the training and testing data
index<-sample(1:500, 25)
trainingdata<- data_points[index,]
testingdata<- data_points[-index,]
#NeuralNetwork
neuralNetwork <- neuralnet(y~x, trainingdata, hidden = c(10), linear.output = TRUE)
neuralNetwork$weights

#prediction on test data
predictTest<-predict(neuralNetwork, testingdata)

plot(testingdata$x, testingdata$y, col = "green", pch = 16, xlab = "x", ylab = "sin(x)", main = "Neural Network - sin(x)")
points(testingdata$x, predictTest, col = "blue", pch = 2)
points(trainingdata$x, trainingdata$y,col="red", pch=18)
legend("bottomright", legend = c("Training Data" ,"Testing Data", "Test Predictions"), col = c("red","green","blue"), pch = 16)

#part-2
h1<-function(x) x
h2<- function (x) ifelse(x>0,x,0)
h3<- function(x) log(1 + exp(x))

# neural netweok to use above 3 functions as activation function 
neuralNetwork_h1 <- neuralnet(y ~ x, data = trainingdata, hidden = c(10),linear.output = TRUE, act.fct = h1)
neuralNetwork_h2 <- neuralnet(y ~ x, data = trainingdata, hidden = c(10), linear.output = TRUE, act.fct = h2)
neuralNetwork_h3 <- neuralnet(y ~ x, data = trainingdata, hidden = c(10), linear.output = TRUE, act.fct = h3)

#Predicting on traoing data
PredictTestH1<- predict(neuralNetwork_h1, testingdata)
PredictTestH2<- predict(neuralNetwork_h2, testingdata)
PredictTestH3<- predict(neuralNetwork_h3, testingdata)

#Ploting the prediction with original data 
plot(testingdata$x, testingdata$y, col = "green", pch = 16, xlab = "x", ylab = "sin(x)", main = "Neural Network - sin(x) with linear activation function")
points(testingdata$x, PredictTestH1, col = "blue", pch = 2)
points(trainingdata$x, trainingdata$y,col="red", pch=18)
legend("bottomright", legend = c("Training Data" ,"Testing Data", "Test Predictions"), col = c("red","green","blue"), pch = 16)

plot(testingdata$x, testingdata$y, col = "green", pch = 16, xlab = "x", ylab = "sin(x)", main = "Neural Network - sin(x) with ReLu activation function")
points(testingdata$x, PredictTestH2, col = "blue", pch = 2)
points(trainingdata$x, trainingdata$y,col="red", pch=18)
legend("bottomright", legend = c("Training Data" ,"Testing Data", "Test Predictions"), col = c("red","green","blue"), pch = 16)

plot(testingdata$x, testingdata$y, col = "green", pch = 16, xlab = "x", ylab = "sin(x)", main = "Neural Network - sin(x) with SoftPlus activation function")
points(testingdata$x, PredictTestH3, col = "blue", pch = 2)
points(trainingdata$x, trainingdata$y,col="red", pch=18)
legend("bottomright", legend = c("Training Data" ,"Testing Data", "Test Predictions"), col = c("red","green","blue"), pch = 16)

#part 3
new_data_points <- data.frame(x=runif(500,0,50))
new_data_points$y <-sin(new_data_points$x)
predict_newdata <- predict(neuralNetwork, new_data_points)

plot(new_data_points$x, new_data_points$y, col = "green", pch = 16, xlab = "x", ylab = "sin(x)", main = "Neural Network - sin(x)", ylim=c(-10.0,2.0))
points(new_data_points$x, predict_newdata, col = "blue", pch = 2)
points(trainingdata$x, trainingdata$y,col="red", pch=18)
legend("bottomleft", legend = c("Training Data" ,"Testing Data", "Test Predictions"), col = c("red","green","blue"), pch = 16)

#part4
neuralNetwork$weights

#part 5
data_points_part5 <- data.frame(x=runif(500,0,10))
data_points_part5$y <-sin(data_points_part5$x)

#training a neural network for predicting the sin(x) from x
nn_y_to_x <-neuralnet(x~y, data = data_points_part5, hidden=c(10), linear.output = TRUE, threshold = 0.1 )

#predicting the training data fron NN
predictx <- predict(nn_y_to_x,data_points_part5 )

#ploting the result
plot(data_points_part5$x, data_points_part5$y, col = "blue", pch = 16, xlab = "x", ylab = "sin(x)", main = "Neural Network to predict x from sin(x) ", ylim=c(-1.5,6.5))
points(data_points_part5$x, predictx, col = "red", pch = 16)
legend("bottomleft", legend = c("True Data", "Predictied data"), col = c("blue", "red"), pch = 16)








