setwd("C:/Users/HP/Desktop/R MiniProject2")
getwd()

#install necessary packages and invoke them
install.packages("readxl")
library(readxl)

golf_details = read_xls("Golf.xls")
attach(golf_details)

dim(golf_details)
summary(golf_details)
str(golf_details)
#missing values
is.na(golf_details)

#Descriptive Statistics
mean(Current)
mean(New)


median(Current)
median(New)

#Range
range(Current)
range(New)


sd(Current)
sd(New)
var(Current)
var(New)

#coefficient of variation
CurrentCV = sd(Current)/mean(Current)
CurrentCV
NewCV = sd(New)/mean(New)
NewCV

#Visualization
par(mfrow=c(1,2))
hist(Current, col="Blue")
hist(New, col="Red")
boxplot(Current, col="Green", horizontal = TRUE, main = "Boxplot of Current")
boxplot(New, col = "Cyan", horizontal = TRUE, main = "Boxplot of new")

#T test 
t.test(Current, New, var.equal = TRUE)

SD = sd(Current-New)
Delta = mean(Current - New)

power.t.test(power = 0.95, delta = Delta, sd = SD, sig.level = 0.05)


