setwd("C:/Users/HP/Desktop/Mini Project4")
getwd()
mydata = read.csv("Bank Personal Loan Dataset.csv", header = TRUE)
install.packages("PerformanceAnalytics")
install.packages("latex2exp")
install.packages("latexpdf")
library(tinytex)
library(latexpdf)

library(latex2exp)
library(car)
library(carData)
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(DataExplorer)
library(ggplot2)
library(ppcor)
library(nFactors)
library(psych)
library(dplyr)
library(tidyverse)
library(purrr)
library(grid)
library(REdaS)
library(foreign)
library(PerformanceAnalytics)
attach(mydata)
names(mydata)
dim(mydata)
summary(mydata)
table(Education)
str(mydata)
colSums(is.na(mydata))
plot_missing(mydata)


describe(mydata)
hist(data = mydata)



#Missing value treatment
newdata = na.omit(mydata)
newdata
dim(newdata)
mean(newdata$Family.members)
#Replace with 2(Round off value of mean)
mydata[is.na(mydata)] <- 2
colSums(is.na(mydata))
mydata$Personal.Loan
#Split data
set.seed(123)
train1 = createDataPartition (mydata$Personal.Loan, p = .7, list = FALSE, times = 1)
head(train1)
trainingdata = mydata[train1,]
testData = mydata[-train1, ]
dim(trainingdata)
dim(testData)
prop.table((table(trainingdata$Personal.Loan)))
prop.table((table(testData$Personal.Loan)))

pairs(mydata)

chart.Correlation(mydata, histogram = TRUE, pch = 19)
corr_mydata1 = cor(mydata[-1])
library(corrplot)
round(corrplot(corr_mydata1, method = "number"), 2)


install.packages("randomForest")

library(randomForest)


RF = randomForest(as.factor(trainingdata$Personal.Loan) ~ ., data = trainingdata[,-1],
                   ntree = 501, mtry = 3, nodesize = 50,importance = TRUE )
print(RF)
plot(RF, main = "")

RF = randomForest(as.factor(trainingdata$Personal.Loan) ~ ., data = trainingdata[,-1],
                  ntree = 501, mtry = 3, nodesize = 45,importance = TRUE )
print(RF)
plot(RF, main = "")

RF = randomForest(as.factor(trainingdata$Personal.Loan) ~ ., data = trainingdata[,-1],
                  ntree = 501, mtry = 3, nodesize = 55,importance = TRUE )
print(RF)
plot(RF, main = "")

RF = randomForest(as.factor(trainingdata$Personal.Loan) ~ ., data = trainingdata[,-1],
                  ntree = 501, mtry = 3, nodesize = 40,importance = TRUE )
print(RF)
plot(RF, main = "")

legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3) 
title(main="Error Rates Random Forest Training data")
RF$err.rate

# List the importance of the variables. 
impVar <- round(randomForest::importance(RF), 2) 
# impVar[order(impVar[,3], decreasing=TRUE),] 
# impVar[order(impVar[,4], decreasing=TRUE),] 
impVar[order(impVar[,1], decreasing=TRUE),]
#VariableImportancce
varImpPlot(RF)


#Build model using the important variables

RF = randomForest(as.factor(trainingdata$Personal.Loan) ~ 
                    trainingdata$Income..in.K.month.+
                  trainingdata$Education + trainingdata$Family.members +
                  trainingdata$CCAvg + trainingdata$CD.Account + 
                  trainingdata$Mortgage + trainingdata$Age..in.years. +
                  trainingdata$Experience..in.years.,
                  data = trainingdata[,c(1,10)],
                  ntree = 501, mtry = 3, nodesize = 45,importance = TRUE )
print(RF)
plot(RF, main = "")


## Tuning Random Forest
tRF <- tuneRF(x = trainingdata[,-c(1,10)], 
              y=as.factor(trainingdata$Personal.Loan),
              mtryStart = 2, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 100, 
              importance=TRUE
)

tRF$importance
varImpPlot(tRF)

RF = randomForest(as.factor(trainingdata$Personal.Loan) ~ 
                    trainingdata$Income..in.K.month.+
                    trainingdata$Education + trainingdata$Family.members +
                    trainingdata$CCAvg + trainingdata$CD.Account + 
                    trainingdata$Mortgage + trainingdata$Age..in.years. +
                    trainingdata$Experience..in.years.,
                  data = trainingdata[,c(1,10)],
                  ntree = 400, mtry = 4, nodesize = 50,importance = TRUE )
print(RF)
plot(RF, main = "")


trainingdata$predict.class <- predict(tRF, trainingdata, type="class")
trainingdata$predict.class


trainingdata$predict.score <- predict(tRF, trainingdata, type="prob")
trainingdata$predict.score
head(trainingdata)
class(trainingdata$predict.score)



## deciling code
decile <- function(x){
  deciles <- vector(length=14)
  for (i in seq(0.1,1,.1)){
    deciles[i*14] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
    ifelse(x<deciles[2], 2,
    ifelse(x<deciles[3], 3,
    ifelse(x<deciles[4], 4,
    ifelse(x<deciles[5], 5,
    ifelse(x<deciles[6], 6,
    ifelse(x<deciles[7], 7,
    ifelse(x<deciles[8], 8,
    ifelse(x<deciles[9], 9,
    ifelse(x<deciles[10], 10,
    ifelse(x<deciles[11], 11,
    ifelse(x<deciles[12], 12,
    ifelse(x<deciles[13], 13,14
    ))))))))))))))
}

trainingdata$deciles <- decile(trainingdata$predict.score[,2])
trainingdata$deciles

library(data.table)
tmp_DT = data.table(trainingdata)
rank <- tmp_DT[, list(
  cnt = length(trainingdata$Personal.Loan), 
  cnt_resp = sum(trainingdata$Personal.Loan), 
  cnt_non_resp = sum(Personal.Loan == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

sum(trainingdata$Personal.Loan) / nrow(trainingdata)


library(ROCR)
pred <- prediction(trainingdata$predict.score[,2], trainingdata$Personal.Loan)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(trainingdata$predict.score[,2], type="Gini")
gini

## Classification Error
with(trainingdata, table(trainingdata$Personal.Loan, predict.class))


## Scoring syntax
testData$predict.class <- predict(tRF, testData, type="class")
testData$predict.score <- predict(tRF, testData, type="prob")

testData$deciles <- decile(testData$predict.score[,2])

tmp_DT = data.table(testData)
h_rank <- tmp_DT[, list(
  cnt = length(testData$Personal.Loan), 
  cnt_resp = sum(testData$Personal.Loan), 
  cnt_non_resp = sum(testData$Personal.Loan == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)

sum(testData$Personal.Loan) / nrow(testData)

pred <- prediction(testData$predict.score[,2], testData$Personal.Loan)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(testData$predict.score[,2], type="Gini")
gini

#Classification error on test data
with(testData, table(testData$Personal.Loan, testData$predict.class))
