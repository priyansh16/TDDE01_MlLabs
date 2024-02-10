
###Seting r=0.2
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
# Train model

logistic=glm(Diabetes ~ Age + Plasma,data = train,family = "binomial")
LRpred=predict(logistic,newdata = test,type="response")
cutoff=0.2
LRpred[LRpred<cutoff]=0
LRpred[LRpred>=cutoff]=1

haveDiabetes=as.integer(unlist(LRpred))
dontHaveDiabetes= -1*(as.integer(unlist((LRpred)))-1)
plasma= as.integer(unlist((test$`Plasma`)))
age= as.integer(unlist((test$Age)))
plot(haveDiabetes*age,haveDiabetes*plasma,ylab = " Plasma glucose concentration",
     xlab = "Age",col="red")
points(dontHaveDiabetes*age,dontHaveDiabetes*plasma,col="blue")
coeff=logistic$coefficients
slope=coeff[2]/(-1*coeff[3])
intercept=coeff[1]/(-1*coeff[3])

#Plot decision boundary
abline(intercept,slope,col="purple")