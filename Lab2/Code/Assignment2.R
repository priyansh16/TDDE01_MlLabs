#install.packages("tree")
library(tree)
library(caret)

#import data, clean it properly and divide into 40/30/30.
bankdata <- read.csv2("bank-full.csv")
data_2<-bankdata[,-12]

data_2$job<-as.factor(data_2$job)
data_2$marital<-as.factor(data_2$marital)
data_2$education<-as.factor(data_2$education)
data_2$default<-as.factor(data_2$default)
data_2$housing<-as.factor(data_2$housing)
data_2$loan<-as.factor(data_2$loan)
data_2$contact<-as.factor(data_2$contact)
data_2$month<-as.factor(data_2$month)
data_2$day<-as.factor(data_2$day)
data_2$poutcome<-as.factor(data_2$poutcome)
data_2$y<-as.factor(data_2$y)

n <- dim(data_2)[1]
set.seed(12345)
index <- sample(1:n, floor(n*0.4))
trainingData = data_2[index,]
index1 <- setdiff(1:n, index)
set.seed(12345)
index2 <- sample(index1, floor(n*0.3))
validationData = data_2[index2,]
index3<-setdiff(index1,index2)
testingData = data_2[index3,]

#define 3 different trees
library(tree)
#a. Decision tree with default settings
defaulttree <- tree(y~ ., data = trainingData)
plot(defaulttree)
text(defaulttree,pretty = 0)

#b.Decision tree with smallest allowed node size equal to 7000.
tree_min_node_size <- tree(y~.,data = trainingData,control = tree.control(nobs = 22605, minsize = 7000))
plot(tree_min_node_size)
text(tree_min_node_size,pretty = 0)

#c. Decision tree with
tree_with_deviance <- tree(y~., data = trainingData, mindev = 0.0005)
plot(tree_with_deviance)
text(tree_with_deviance,pretty = 0)

#Prediction on training data 
trainPredictDefault<- predict(defaulttree, trainingData, type = "class") #miss rate: 0.1071
validPredictDefault <- predict(defaulttree, validationData , type = "class") 

trainPredictSmallest <-predict(tree_min_node_size, trainingData, type = "class")
validPredictSmallest <- predict(tree_min_node_size, validationData, type = "class")

trainPredictMindev <-predict(tree_with_deviance, trainingData, type = "class")
validPredictMindev <- predict(tree_with_deviance, validationData, type = "class")

# calculate the misclassification rate

misclassified <- function(Y,Y1){
  total_rows<-length(Y)
  return(1-(sum(diag(table(Y,Y1)))/total_rows))
}

mscTrainDefault <- misclassified(trainingData$y, trainPredictDefault)
mscTrainSmallest <- misclassified(trainingData$y, trainPredictSmallest)
mscTrainMindev <- misclassified(trainingData$y, trainPredictMindev)
mscValidDefault <- misclassified(validationData$y, validPredictDefault)
mscValidSmallest <- misclassified(validationData$y, validPredictSmallest)
mscValidMindev <- misclassified(validationData$y, validPredictMindev)
MSE_Matrix<-matrix(c(mscTrainDefault,mscTrainSmallest,mscTrainMindev,
                     mscValidDefault,mscValidSmallest,mscValidMindev), 
                   nrow =3, ncol = 2)
colnames(MSE_Matrix) <-c("Training_data",'Validation_data')
rownames(MSE_Matrix) <-c("Default_Tree","Tree_min_node","Tree_with_deviance")
print(MSE_Matrix)

#studying the tree upto 50 leaves in 2c.
trainDeviance <- seq(1,50)
validDeviance <-seq(1,50)

for(i in 2:50){
  prunedTree <- prune.tree(tree_with_deviance, best=i)
  pred_valid <- predict(prunedTree,validationData, "tree")
  trainDeviance[i] = deviance(prunedTree)
  validDeviance[i] = deviance(pred_valid)
}

plot(2:50, trainDeviance[2:50], type = "b", col="magenta",ylim=c(5000,16000))
points(2:50, validDeviance[2:50], type = "b", col = "blue")

legend("topright", legend=c("Training data", "validation data"),
       col=c("magenta", "blue"),lty=1:1, cex=0.8)

which(validDeviance == min(validDeviance[2:50]))
optimalTree<- prune.tree(tree_with_deviance, best=24)
plot(optimalTree)
text(optimalTree,pretty = 0)
#Prediction with test data
pred_test <- predict(optimalTree, testingData,"class")
#confusion metrix
confusion<- table(testingData$y,pred_test)
confusion
#accuracy
1-misclassified(testingData$y,pred_test)
#F1
recall<-confusion[2,2]/sum(confusion[,2])
precision<-confusion[2,2]/sum(confusion[2,])
F1Score <-2*precision*recall/(precision+recall)
F1Score
Ypredict4_test_2<-predict(optimalTree, newdata = testingData,type = "vector")

final<-c()
for (i in 1:nrow(Ypredict4_test_2)){
  if(Ypredict4_test_2[i,1]/Ypredict4_test_2[i,2] > 5){
    final[i]<-"no"
  }else{
    final[i]<-"yes"
  }
}
final<-as.factor(final)
summary(final)

confusion_Loss <-table(testingData$y,final)
confusion_Loss
#accuracy
1-misclassified(testingData$y, final)
#F1 score
recall_decisiontree<- confusion_Loss[2,2]/sum(confusion_Loss[,2])
precision_decisiontree <- confusion_Loss[2,2]/sum(confusion_Loss[2,])
F1_decisiontree<- 2*precision_decisiontree*recall_decisiontree/
  (precision_decisiontree+recall_decisiontree)
F1_decisiontree
#logistic regression
lRegression <- glm(y~.,trainingData, family = "binomial")
lr_pred <- predict(lRegression, testingData,type="response")
#TPR adn FPR values for different values of treshold (pi)
TPR_tree<-c()
FPR_tree<-c()
TPR_LR<-c()
FPR_LR<-c()

j<-1
for (i in seq(0.05,0.95,0.05)){
  #for logistic
  pred<-ifelse(lr_pred>i,"yes","no")
  pred<-factor(pred,levels = c("no","yes"))
  a<-confusionMatrix(testingData$y,pred)
  
  TPR_LR[j]<- a$table[4]/(a$table[2]+a$table[4])
  FPR_LR[j]<- a$table[3]/(a$table[1]+a$table[3])
  
  # for optimal tree
  final<-c()
  for (k in 1:nrow(Ypredict4_test_2)){
    if(Ypredict4_test_2[k,2] > i){
      final[k]<-"yes"
    }else{
      final[k]<-"no"
    }
  }
  final<-factor(final,levels = c("no","yes"))
  b<-confusionMatrix(testingData$y,final)
  
  TPR_tree[j]<- b$table[4]/(b$table[2]+b$table[4])
  FPR_tree[j]<- b$table[3]/(b$table[1]+b$table[3])
  j=j+1
}

plot(FPR_tree,TPR_tree, type="l", col="magenta",lwd=2, 
     xlab ="False Positive rate", ylab="True Positive rate", main="ROC Curves")
lines(FPR_LR, TPR_LR, type = "l", col = "blue", lwd=2)
legend("bottomright", c("Optimal Tree", "Logistic Regression"), 
       fill = c("magenta", "blue"))

