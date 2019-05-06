# LOGISTIC REGRESSION
rm(list = ls())

# Read  Data
data <- read.csv("./Data/HeartDisease.csv", header = T, sep = ",")
head(data)
str(data)
data$Target<- as.factor(data$Target)

vitals<-subset(data, select=-ID)
table(vitals[vitals==-99])
vitals[vitals=="-99"]<-NA
summary(vitals)

#plotting variables with huge range
library(ggplot2)
p1<-ggplot(vitals,aes(y=A1))+geom_boxplot()
p2<-ggplot(vitals,aes(y=A3))+geom_boxplot()
p3<-ggplot(vitals,aes(y=A4))+geom_boxplot()
p4<-ggplot(vitals,aes(y=A5))+geom_boxplot()
p5<-ggplot(vitals,aes(y=A6))+geom_boxplot()
p6<-ggplot(vitals,aes(y=A7))+geom_boxplot()
p7<-ggplot(vitals,aes(y=A8))+geom_boxplot()
p8<-ggplot(vitals,aes(y=A9))+geom_boxplot()
p9<-ggplot(vitals,aes(y=A10))+geom_boxplot()
p10<-ggplot(vitals,aes(y=A12))+geom_boxplot()
p11<-ggplot(vitals,aes(y=A14))+geom_boxplot()

require(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,ncol=4)

#removing outliers
which.max(vitals$A5)
vitals[14017,]
vitals<-vitals[-14017,]


which.max(vitals$A9)
vitals[14289,]
vitals<-vitals[-14289,]


library(DMwR)
#manyNAs(vitals, 0.1) 

#percentage of errors
2233/34281
2103/34281
1743/34281

#central imputation
vitals_imputed<-centralImputation(vitals)


str(vitals_imputed)
sum(is.na(vitals_imputed))

#splitting data
library(caTools)
library(car)

set.seed(1000)
split = sample.split(vitals_imputed$Target, SplitRatio = 0.70)

# Split up the data using subset
train = subset(vitals_imputed, split==TRUE)
test = subset(vitals_imputed, split==FALSE)

# Check the proportions of Target in both sets
#cat(sum(train$Target)/nrow(train),sum(test$Target)/nrow(test))
tail(train$A11)

vitalslog <- glm(Target~., data = train, family = binomial)
summary(vitalslog)

#checking correlation between variables
library(corrplot)
corMat <- cor(train[,sapply(train, is.numeric)])
par(mfrow=c(1,1))
corrplot::corrplot(corMat)
corrplot::corrplot(corMat,tl.cex = 0.7)
corrplot::corrplot(corMat, tl.cex = 0.7, method = "number")

#removing singularities
train$A11<-NULL

#model 
vitalslog1 <- glm(Target~., data = train, family = binomial)
summary(vitalslog1)

#predictions
predictTrain=predict(vitalslog1,newdata=train,type='response')
table(train$Target,predictTrain > 0.5)

# our importance is predicting the heart disease correctly. so we need to put more 
#emphasis on False Negatives

PredictTest=predict(vitalslog1,newdata=test,type="response")
table(test$Target,PredictTest>0.5)
logLik(vitalslog1)
deviance(vitalslog1)
AIC(vitalslog1)


# Test set AUC 
library(ROCR)
ROCRpred = prediction(PredictTest, test$Target)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


table(test$Target,PredictTest>0.3)

#collinearity
car::vif(vitalslog1)

vitalslog2 <- glm(Target~IV+A1+A2+A3+A4+A5+A8+A9+A12+A13+A14+A15+A16+A17+A18+A19+A20+A21+A22, data = train, family = binomial)
summary(vitalslog2)
car::vif(vitalslog2)

vitalslog3 <- glm(Target~IV+A1+A2+A3+A4+A5+A8+A13+A14+A15+A16+A17+A18+A19+A20+A21+A22, data = train, family = binomial)
summary(vitalslog3)

car::vif(vitalslog3)


predictTrain=predict(vitalslog3,newdata=train,type='response')
table(train$Target,predictTrain > 0.3)
x<-ifelse(predictTrain > 0.3,1,0)
head(x)
PredictTest=predict(vitalslog3,newdata=test,type="response")
table(test$Target,PredictTest>0.3)

library(caret)
confusionMatrix(x, train$Target)

independentattr<-setdiff(names(train),c("Target"))
std_model <- preProcess(train[, independentattr], method = c("scale"))
std_model

train[, independentattr] <- predict(object = std_model, newdata = train[, independentattr])
test[, independentattr] <- predict(object = std_model, newdata = test[, independentattr])

vitalslog5 <- glm(Target~IV+A2+A3+A4+A8+A13+A14+A15+A16+A17+A18+A19+A20+A21+A22, data = train, family = binomial)
summary(vitalslog5)

vitalslog6<-glm(Target~.,data=train,family=binomial)
summary(vitalslog6)
predictTrain=predict(vitalslog5,newdata=train,type='response')
table(train$Target,predictTrain > 0.3)

x<-ifelse(predictTrain > 0.3,1,0)

confusionMatrix(x, train$Target)

