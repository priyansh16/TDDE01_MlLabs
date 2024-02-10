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

