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
# Devide date
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,] 

#new features
train$phi1=phi1=train$Plasma^4
train$phi2=phi2=train$Plasma^3*train$Age
train$phi3=phi3=train$Plasma^2*train$Age^1
train$phi4=phi4=train$Plasma^1*train$Age^3
train$phi5=phi5=train$Age^4

logistic=glm(Diabetes ~ Plasma +Age+phi1+phi2+phi3+phi4+phi5
             ,data = train,family = "binomial")
LRpred=predict(logistic,newdata = test,type="response")
cutoff=0.5
LRpred[LRpred<cutoff]=0
LRpred[LRpred>=cutoff]=1
summary(logistic)

#Misclassification rate

confusionMX=table(LRpred,test$Diabetes)
misclassificationRate=(1 - sum(diag(confusionMX))/sum(confusionMX))

#Scatterplot
haveDiabetes=as.integer(unlist(LRpred))
dontHaveDiabetes= -1*(as.integer(unlist((LRpred)))-1)
plasma= as.integer(unlist((test$`Plasma`)))
age= as.integer(unlist((test$Age)))
plot(haveDiabetes*age,haveDiabetes*plasma,ylab = " Plasma glucose concentration",
     xlab = "Age",col="red")
points(dontHaveDiabetes*age,dontHaveDiabetes*plasma,col="blue")



