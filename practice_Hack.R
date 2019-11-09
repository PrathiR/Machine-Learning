#Install required libraries
library(SDMTools)
library(pROC)
library(Hmisc)
library(ggplot2)
library(DataExplorer)
library(PerformanceAnalytics)
library(car)
library(nFactors)
library(psych)
library(dplyr)
library(pscl)
#Loading the mice package
install.packages("VIM")
library(mice)

#Loading the following package for looking at the missing values
library(VIM)
library(lattice)

setwd("C:/Users/HP/Desktop/Hack")
getwd()

#as.is to retain the original data type of a column when we use stringAsFactors
#unique to get the levels/factors --> if not unique try strip.white = TRUE
#na.strings to replace ? as NA 
mydata = read.csv("train.csv", header = T, stringsAsFactors = TRUE, 
                  as.is = c(3,15), na.strings = '?')
testdata = read.csv("test.csv", header = T, stringsAsFactors = TRUE, 
                    as.is = c(3,15), na.strings = '?')

attach(mydata)
str(mydata)

#Exploratory Data Analysis
View(mydata)
str(mydata)  
names(mydata)
dim(mydata)
summary(mydata)
str(mydata)
#View(mydata)
colSums(is.na(mydata))
plot_missing(mydata)


#Missing Value treatment
table(mydata$Male)
plot(mydata$Male)
#as b is majority in Male, change NA to b
mydata$Male[is.na(mydata$Male)] <- 'b'
colSums(is.na(mydata))
plot_missing(mydata)

#Treat all missing values
#miss_df = data.frame(Age, ZipCode, Married, BankCustomer, EducationLevel, Ethnicity)
#md.pattern(miss_df)
md.pattern(mydata)
imputed_Data <- mice(mydata, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)
out_data = complete(imputed_Data)
View(out_data)

newdata = mydata
colSums(is.na(newdata))
plot_missing(mydata)
newdata$Age = out_data$Age
newdata$Married = out_data$Married
newdata$BankCustomer = out_data$BankCustomer
newdata$EducationLevel = out_data$EducationLevel
newdata$Ethnicity = out_data$Ethnicity
newdata$ZipCode = out_data$ZipCode
colSums(is.na(newdata))
plot_missing(newdata)

newdata = newdata[-1]
attach(newdata)

dim(newdata)
dim(testdata)

colSums(is.na(testdata))
View(testdata)

table(testdata$Male)
plot(testdata$Male)
#as b is majority in Male, change NA to b
testdata$Male[is.na(testdata$Male)] <- 'b'
colSums(is.na(testdata))
plot_missing(testdata)

#Proportion
table(Approved)
prop.table(table(Approved))

#Feature engineering
table(newdata$Male)
table(newdata$BankCustomer)
#From this it can be observed that data'gg' is recorded wrongly as it contains only 2 rows. 
#moreover a bankcustomer can be yes or no value(i.e) only 2 levels.
newdata$BankCustomer[newdata$BankCustomer == "gg"] = 'g'
str(newdata)
newdata$BankCustomer = factor(newdata$BankCustomer)
str(testdata)

#Data visualization



#Data visualization
ggplot(data = newdata, aes(x= Approved, y=Age, fill=Approved)) + geom_boxplot()
#Age doesn't have an impact on Approval of Loan

ggplot(data = newdata, aes(x= Approved, y=CreditScore, fill=Approved)) + geom_boxplot()
#Loan is approved based on the credit score. The more credit score a person has
#Loan can be approved

ggplot(data = newdata, aes(x= Approved, y=Debt, fill=Approved)) + geom_boxplot()
#If a person has more debt,  loan is approved more

ggplot(data = newdata, aes(x= Approved, y=YearsEmployed, fill=Approved)) + geom_boxplot() 
#Experienced employees has a higher chance of getting loan approved

ggplot(data = newdata, aes(x= Approved, y=ZipCode, fill=Approved)) + geom_boxplot() 
#The area zip code between 0 and 150 has a higher chance of getting a loan approved.

ggplot(data = newdata, aes(x= Approved, y=Male, fill=Approved)) + geom_boxplot()
ggplot(data = newdata, aes(x= Approved, y=Married, fill=Approved)) + geom_boxplot()
ggplot(data = newdata, aes(x= Approved, y=PriorDefault, fill=Approved)) + geom_boxplot()

#Build regression model 
str(newdata)
logit = glm(newdata$Approved ~ .-BankCustomer, data = newdata, family = "binomial")

summary(logit)
 
#test multicollinearity
vif(logit)#this shows the presence of multicollinearity.

#It seems that Male, Age and Married are not significant. 
#hence we remove them, 
#attach(mydata)
logit1= glm(Approved~.- BankCustomer - Male - Married - Age, data = newdata, family = "binomial")
summary(logit1)
vif(logit1)
 
 
#removing Drivers license and citizen from model
logit2= glm(Approved~.- BankCustomer - Male - Married - Age - DriversLicense - Citizen,
            data = newdata, family = "binomial")
summary(logit2)
vif(logit2)

 

 
#removing credit score and yearsemploted as it is shown to be insignificant
attach(mydata)
logit3= glm(Approved~.- BankCustomer - Male - Married - Age - DriversLicense - Citizen
            -CreditScore - YearsEmployed,
            data = newdata, family = "binomial")
summary(logit3)
vif(logit3)
 
#Overall Significance
#install.packages("lmtest")
library(lmtest)
lrtest(logit3)
#The overall significance is more. p value is very low.Hence the null hypothesis
#is rejected. Alteast one variable is a predictor of Churn.
 
 
#McFaden Rsquare computation
#install.packages("pscl")
pR2(logit3)
#57% of the variations in Approval is explained by the model. So the model is good.
 

 
#Individual coefficient significance
summary(logit3)
odds = exp(logit3$coefficients)
prob = odds/(1+odds)

newdf = data.frame(coeff = logit3$coefficients, pval = summary(logit3)$coefficients[ ,4], odds, prob)
newdf
#filter by probabilities(take only significant ones)
newdf[newdf$pval <= 0.05, ]
#sort by odds
sorted <- newdf[order(-odds),] 
sorted

predict(logit3, type = "response", data = newdata)

#prediction at a cutoff value
pred_train = floor(predict(logit3, type = "response", data = newdata)+0.5)
confusionmat = table(Actual = newdata$Approved, Predicted = pred_train)
confusionmat

#predict test data
pred_test = floor(predict(logit3, type = "response", newdata = testdata[-1])+0.5)
pred_test

testdata$Approved = pred_test
str(testdata)

table(newdata$Approved)
table(testdata$Approved)

 
confusionmat
accuracy.logit<-sum(diag(confusionmat))/sum(confusionmat)
accuracy.logit
#The model is 88% accurate

loss.logit<-confusionmat[1,2]/(confusionmat[1,2]+confusionmat[1,1])
loss.logit
opp.loss.logit<-confusionmat[2,1]/(confusionmat[2,1]+confusionmat[2,2])
opp.loss.logit
tot.loss.logit<-0.95*loss.logit+0.05*opp.loss.logit
tot.loss.logit
 




