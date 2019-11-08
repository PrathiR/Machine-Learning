setwd("C:/Users/HP/Desktop/Mini Project3")
mydata = read.csv("Factor-Hair-Revised.csv",header = TRUE)
mydata
attach(mydata)
names(mydata)

#Libraries used
library(DataExplorer)
library(ggplot2)
library(ppcor)
library(nFactors)
library(psych)
library(dplyr)

#Exploratory Analysis
dim(mydata)
str(mydata)
summary(mydata)

#check missing values
#anyNA(mydata)
plot_missing(mydata)


#Multicollinearity
#Pearson correlation method
data1 = mydata[,2:13]
data2 = mydata[,2:12]
corr_mat = cor(mydata)
library(corrplot)
corrplot(corr_mat)
corr = pcor(data1, method = "pearson")
corr

#Build Regression model using these independent variables
Model = lm(Satisfaction~., data = mydata)
summary(Model)


#Factor Analysis
#step -1 Eigenvalue computation
ev = eigen(cor(data2))
ev
print(ev, digits = 5)
Eigenvalues = ev$values
Eigenvalues


#Scree plot


Factor = c(1:11)
Factor

Scree = data.frame(Factor, Eigenvalues)
Scree
plot(Scree, main = "Scree plot", col = "Blue")
lines(Scree, col="Red")

#Unrotated loadings
unrotate = principal(data2, nfactors = 4, rotate = "none")
unrotate
print(unrotate, digits = 4)

#Rotated loadings
Rotate = principal(data2, nfactors = 4, rotate="varimax")
Rotate
print(Rotate, digits = 4)

#Visualize the factors
fa.diagram(Rotate)
Rotate$scores

#multiple Regression 
#Get the data frame to be used in Regression
target = mydata[13]
target
indep = Rotate$scores

dataf = cbind(target,indep)
head(dataf)

#Change the columnn names
newdata = dataf %>% 
  rename(
    Purchase = RC1,
    Marketing = RC2,
    Product = RC3,
    Support = RC4
  )
head(newdata)
attach(newdata)
model = lm(Satisfaction~Purchase+Marketing+Product+Support, data = newdata)
summary(model)


confint(model, "Purchase")
confint(model, "Marketing")
confint(model, "Product")
confint(model, "Support")

#divide the data into test and training data
train<-sample_frac(newdata, 0.7)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-newdata[-sid,]
head(train)
head(test)

#train regression model
train_model = lm(Satisfaction~.,data = train)
summary(model)

#calculate rmse
rmse_train = sqrt(mean(train_model$residuals^2))
rmse_train

#predict with test data
pred=predict(train_model, newdata = test, type = "response") 
pred
head(test)
test$Satisfaction.Predict <- pred

#calculate R squared
cor(test$Satisfaction, test$Satisfaction.Predict)^2 

#calculate rmse
rmse_test = sqrt(mean((test$Satisfaction - pred)^2)) 
rmse_test

#Plot
plot(train$Satisfaction, col="Red")
lines(train$Satisfaction, col = "Red")

plot(test$Satisfaction.Predict, col = "Blue")
lines(test$Satisfaction.Predict, col = "Blue")
plot(test$Satisfaction, col = "red")
lines(test$Satisfaction, col = "red")
lines(test$Satisfaction.Predict, col = "Blue")
