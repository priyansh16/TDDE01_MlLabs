library("ggplot2")

data=read.csv("communities.csv",header = TRUE)
############## Question1 ##############

#normalizing variebles
scaled_data=as.data.frame(cbind(scale(data[-data$ViolentCrimesPerPop]),
                                data$ViolentCrimesPerPop))

#applying PCA
pca=prcomp(scaled_data,scale. = FALSE)
#pca_variation=pca$sdev^2
pca_variation= eigen(cov(scaled_data))
print(cov(scaled_data))
#pca_variation_percentage=round(pca_variation/sum(pca_variation)*100,2)

#barplot(pca_variation_percentage)

cumsum_percentage=cumsum(pca_variation$values / sum(pca_variation$values)*100)
which(cumsum_percentage >= 95)[1]

############## Question2 ##############
pca2=princomp(scaled_data)
firstPC=pca2$loadings[,1]
plot(firstPC,col="purple",pch=19)

top_feature=which(abs(firstPC) %in% head(sort(abs(firstPC)),5))
points(top_feature,pca2$loadings[top_feature,1],col="green",pch=19)

dtp=data.frame("ViolentCrimesPerPop"=scaled_data$ViolentCrimesPerPop,
               pca2$scores[,1:2])
ggplot(data = dtp) + 
  geom_point(aes(x = Comp.1, y = Comp.2, col = ViolentCrimesPerPop))

############## Question3 ##############

# Divide data
n=dim(scale(data))[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,] 

LR=lm(ViolentCrimesPerPop~.,data=train)
train_pred=predict(LR,type="response")
test_pred=predict(LR,test,type="response")

train_error=mean((train_pred-train$ViolentCrimesPerPop)^2)
test_error=mean((test_pred-test$ViolentCrimesPerPop)^2)

############## Question4 ##############

cost_function=function(theta){
  train_x=as.matrix(train[-train$ViolentCrimesPerPop])
  test_x= as.matrix(test[-test$ViolentCrimesPerPop])
  train_y=train$ViolentCrimesPerPop
  test_y=test$ViolentCrimesPerPop
  
  mse_train=mean((train_y - train_x %*% theta)^2)
  mse_test=mean((test_y - test_x %*% theta)^2)
  
  train_errors<<-c(train_errors,mse_train)
  test_errors<<-c(test_errors,mse_test)
  
  return(mse_train)
}

test_errors=c()
train_errors=c()

print(test_errors)

theta=rep(0, 100)
opt=optim(par = theta, fn = cost_function, method = "BFGS", control=list(trace=TRUE))
opt$par
plot(train_errors[400:5000],type="b",col="blue")
points(test_errors[400:5000],type="b",col="red")


