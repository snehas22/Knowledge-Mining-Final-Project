
library(margins)
library(rpart)
library(rpart.plot)

#Importing Data
Diabetes<-read.csv("diabetes_data_upload.csv",header=TRUE,sep=",")
str(Diabetes)

#Recoding the Variables
Diabetes$Polyuria<-ifelse(Diabetes$Polyuria=="Yes",1,0)
Diabetes$sudden.weight.loss<-ifelse(Diabetes$sudden.weight.loss=="Yes",1,0)
Diabetes$weakness<-ifelse(Diabetes$weakness=="Yes",1,0)
Diabetes$Polydipsia<-ifelse(Diabetes$Polydipsia=="Yes",1,0)
Diabetes$Genital.thrush<-ifelse(Diabetes$Genital.thrush=="Yes",1,0)
Diabetes$Polyphagia<-ifelse(Diabetes$Polyphagia=="Yes",1,0)
Diabetes$visual.blurring<-ifelse(Diabetes$visual.blurring=="Yes",1,0)
Diabetes$Itching<-ifelse(Diabetes$Itching=="Yes",1,0)
Diabetes$Irritability<-ifelse(Diabetes$Irritability=="Yes",1,0)
Diabetes$delayed.healing<-ifelse(Diabetes$delayed.healing=="Yes",1,0)
Diabetes$partial.paresis<-ifelse(Diabetes$partial.paresis=="Yes",1,0)
Diabetes$muscle.stiffness<-ifelse(Diabetes$muscle.stiffness=="Yes",1,0)
Diabetes$Alopecia<-ifelse(Diabetes$Alopecia=="Yes",1,0)
Diabetes$Obesity<-ifelse(Diabetes$Obesity=="Yes",1,0)
Diabetes$class<-ifelse(Diabetes$class=="Positive",1,0)
Diabetes$Gender<-ifelse(Diabetes$Gender=="Male",1,0)

#Inspecting Missing Values
complete.cases(Diabetes)
Diabetes[!complete.cases(Diabetes),]
str(Diabetes)

#Graphs of the Predictor Variables-To See Unbalance in the Dataset or any Trends

#Class (Target/Response Variable)

t <- table(Diabetes$class)

barplot(t, col=rainbow(5),main="Number of Positive and Negative Cases",xlab = "Class",ylab = "Number")   #Response is fairly balanced. We need not to worry about performing under or over sampling


#Gender Variable
t1 <- table(Diabetes$Gender)

barplot(t1, col=rainbow(5),main="Positive Cases by Gender",xlab = "Gender",ylab = "Number of Cases")  #Gender does not need to be included in the model because there is no variation in the variable and it will not be helpful in predicting out of sample diabetes presence

#Age Variable

hist(Diabetes$Age, xlab="Age", main="Distribution of Age", freq=FALSE)

#Sudden Weight Loss Variable

t3 <- table(Diabetes$sudden.weight.loss)

barplot(t3, col=rainbow(5),main="Sudden Weight Loss",xlab="Yes/No",ylab = "Number of cases") 

t4<-table(Diabetes$Obesity)
barplot(t4,col=rainbow(5),main="Obesity Cases",xlab="Neg/Pos",ylab="Count")

t5<-table(Diabetes$Polyuria)
barplot(t5,col=rainbow(5),main="Polyuria Cases",xlab="Pos/Neg",ylab="Cases")

t6<-table(Diabetes$Polydipsia)
barplot(t6,col=rainbow(5),main="Polydipsia Cases",xlab="Pos/Neg",ylab="Cases")
#Linear Modeling-Logistic Regression

t7<-table(Diabetes$sudden.weight.loss)
barplot(t7,col=rainbow(5),main="Sudden Weight Loss",xlab="Pos/Neg",ylab="Cases")

t8<-table(Diabetes$weakness)
barplot(t8,col=rainbow(5),main="Weakness",xlab="Pos/Neg",ylab="Cases")

t9<-table(Diabetes$Polyphagia)
barplot(t9,col=rainbow(5),main="Polyphagia",xlab="Pos/Neg",ylab="Cases")

t10<-table(Diabetes$Genital.thrush)
barplot(t10,col=rainbow(5),main="Genital thrush",xlab="Pos/Neg",ylab="Cases")

t11<-table(Diabetes$visual.blurring)
barplot(t11,col=rainbow(5),main="Visual Blurring",xlab="Pos/Neg",ylab="Cases")

t11<-table(Diabetes$Itching)
barplot(t11,col=rainbow(5),main="Itching",xlab="Pos/Neg",ylab="Cases")

t12<-table(Diabetes$Itching)
barplot(t12,col=rainbow(5),main="Itching",xlab="Pos/Neg",ylab="Cases")


#Stepwise Logistic Regression
reduced <- glm(class~1,Diabetes, family="binomial")
summary(reduced)
full <- glm(class~Age+Polyuria+Polydipsia+sudden.weight.loss+weakness+Polyphagia+Genital.thrush+visual.blurring+Itching+Irritability+delayed.healing+partial.paresis+muscle.stiffness+Alopecia+Obesity+Gender,data=Diabetes,family="binomial")
summary(full)

diabetes_step <- step(reduced, scope=list(lower=formula(reduced), upper=formula(full)), direction="both")


#The final model has the following predictor variables. The AIC for this model is 478.68 (the lowest in the stepwise procedure).

#Polyuria, Polydipsia, Itching, partial.paresis, Irritability, sudden.weight.loss, 
#Age, visual.blurring, Genital.thrush, Polyphagia, delayed.healing, weakness, muscle.stiffness

#Running the Final Logistic Regression Model including the predictor variables in the final iteration of stepwise procedure.

diabetes_final <-glm(class ~ Polyuria + Polydipsia + Itching + Irritability+ partial.paresis
                     + Age + Genital.thrush + 
                       Polyphagia  + weakness + Gender, data=Diabetes,
                     family="binomial")
summary(diabetes_final)

#Average Marginal Effect-Interpretation in terms of Probabilities 
margins(diabetes_final)


#Predict the Values of the Target Variable using Final Logistic Regression Model

Diabetes$probab_logistic <- predict(diabetes_final, Diabetes, type="response")


Diabetes$predic_logistic <- ifelse(Diabetes$probab_logistic > 0.5, 1, 0)

#Confusion Matrix
table(Diabetes$class, Diabetes$predic_logistic)

#Accuracy 
(180+283)/520            #89% Accuracy

#Sensitivity 
283/(283+37)             #88.43% (True Positive/True Positive+False Negative)


#Tree Based Model-Decision Tree Model

diabetes_tree <- rpart(class~Age+Polyuria+Polydipsia+sudden.weight.loss+weakness+Polyphagia+Genital.thrush+visual.blurring+Itching+Irritability+delayed.healing+partial.paresis+muscle.stiffness+Alopecia+Obesity+Gender,data=Diabetes, method="class")
rpart.plot(diabetes_tree, main="Decision Tree for Predicting the Diabetes in a Patient")

#Feature Importance

feature_import <- data.frame(importance=diabetes_tree$variable.importance)

#Plotting the Feature Importance of Variables
ggplot(feature_import, aes(x = row.names(feature_import), y = importance))+geom_col()+labs(x="Feature Importance", y="Feature", title="Feature Importance Plot")+theme(text=element_text(size=9.5))

library(ggplot2)
#Predict the Values of the Target Variable using Final Logistic Regression Model

Diabetes$predic_tree <- predict(diabetes_tree, Diabetes, type="class")

#Confusion Matrix
table(Diabetes$class, Diabetes$predic_tree)

#Accuracy 
(177+290)/520            

#Sensitivity 
290/(290+30)