rm(list=ls())

data<- read.csv("./Data/CustomerRevenue.csv")

head(data)

str(data)

sum(is.na(data))

data$TransactionMode<-as.factor(data$TransactionMode)
data$Occupation<-as.factor(data$Occupation)
str(data)

#removing Personal ID's 
custid<-data['PersonID']
cust<-subset(data,select = -c(PersonID))
str(cust)

#checking the spread of data
library(ggplot2)
ggplot(cust,aes(x=Occupation,y=Amount))+
  geom_boxplot()

require(gridExtra)

#plotting variables
ggplot(cust,aes(x=FamilySize,y=NumberofFrequentItems))+
  geom_point()

ggplot(cust,aes(x=FamilySize,y=Amount))+
  geom_point(size=2,aes(colour=Distance))+scale_colour_gradient2(low = "red", mid = "green",
                                                                 high = "blue", midpoint = 13, space = "Lab")
ggplot(cust,aes(x=Distance,fill=Area))+
  geom_bar()


#grid.arrange(p1,p2,ncol=2)
library(dplyr)
cust<-mutate(cust, spendperperson = Amount/(FamilySize))


ggplot(cust,aes(x=FamilySize,y=spendperperson))+
  geom_point(size=2)

#summarizing 
cust%>%
  group_by(Area,Occupation)%>%
  summarise_each(funs(sum), Amount)

cust%>%
  group_by(TransactionMode)%>%
  summarise_each(funs(sum),DirectVisits,OnlineVisits)

cust%>%
  group_by(Area)%>%
  summarise_each(funs(max,min,mean), Distance)
cust%>%
  group_by(Area)%>%
  summarise(n())

#removing the outliers
which.max(cust$Amount)
cust[2282,]
cust<-cust[-2282,]

train$spendperperson<-NULL

###Splitting the data
rows=seq(1,nrow(cust),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(cust))/100)
train = cust[trainRows,] 
validation = cust[-trainRows,]

#taking out numerical attributes
numattr<- subset(train,select = -c(TransactionMode,Occupation,Area))

#building model using all variables
mod_lm<-lm(Amount~.,data=train)
summary(mod_lm)


#building model after removing insignificant variables
mod_lm1<-lm(Amount~Duration+FamilySize+DirectVisits+Quantity+OnlineVisits+NumberofFrequentItems+TransactionMode+Area+spendperperson,data=train)
summary(mod_lm1)

#checking residuals
par(mfrow=c(2,2))
plot(mod_lm1)

#Let's deal with the leverages and res outliers
lev= hat(model.matrix(mod_lm1)) #gives allleverages
par(mfrow=c(1,1))
plot(lev)
#Method1 : Manual - From the plot we made a decision to obtain leverage values greater than 0.1
train[lev>0.1,]
nrow(train[lev>0.1,])
#Let's remove these three points
train<-train[-which(lev>0.1),]

#cooks distance
cook = cooks.distance(mod_lm1)
plot(cook,ylab="Cooks distances")
max=as.numeric(which.max(cook))
points(max,cook[max],col='red', pch=19)
train[max,]
train <- train[-max,]

#Residual outliers
residuals = mod_lm1$residuals
outliers <- boxplot(residuals,plot=T)$out
sort(outliers)
length(outliers)

#checking correlation between variables
library(corrplot)
corMat <- cor(train[,sapply(train, is.numeric)])
par(mfrow=c(1,1))
corrplot::corrplot(corMat)
corrplot::corrplot(corMat,tl.cex = 0.7)
corrplot::corrplot(corMat, tl.cex = 0.7, method = "number",)

library(car)
vif(mod_lm1)
#We see DirectVisits and NumberofFreequentItems have VIF >10; So ignoring them for model building
mod_lm2<-lm(Amount~FamilySize+Duration+Quantity+OnlineVisits+TransactionMode+Area+spendperperson,data=cust)
summary(mod_lm2)

#But we even see there's huge reduction in Adjusted R-square, so we add NumberofFrequentItems and check
mod_lm3<-lm(Amount~FamilySize+Duration+Quantity+OnlineVisits+NumberofFrequentItems+TransactionMode+Area+spendperperson,data=cust)
summary(mod_lm3)

#Predictions on train, vaildation, and test data
model3_train_preds <- mod_lm3$fitted.values #OR
model3_train_preds <- predict(object = mod_lm3, newdata = train)
model3_validation_preds <- predict(object = mod_lm3, newdata = validation)

library(DMwR)
regr.eval(trues = train$Amount, preds = model3_train_preds)
regr.eval(trues = validation$Amount, preds = model3_validation_preds)
#there is 20 percent error in our predictions

###--- Model 4 : Using StepAIC   ---###
mod_lm4 <- lm(formula = Amount ~ ., data = train)
summary(mod_lm4)
# Stepwise Regression
library(MASS)
step <- stepAIC(mod_lm4, direction="both")
step #the model ignored Distance and Occupation attributes

mod_lm5<-lm(formula = Amount ~ FamilySize+Duration + DirectVisits + OnlineVisits + 
              Quantity + NumberofFrequentItems + TransactionMode + Area+spendperperson, 
            data = train)
summary(mod_lm5)

#Predictions on train, vaildation, and test data
model5_train_preds <- mod_lm5$fitted.values #OR
model5_train_preds <- predict(object = mod_lm5, newdata = train)
model5_validation_preds <- predict(object = mod_lm5, newdata = validation)

regr.eval(trues = train$Amount, preds = model5_train_preds)
regr.eval(trues = validation$Amount, preds = model5_validation_preds)
# slight reduce in errors

#standardizing data
library(caret)
# The "preProcess()" function creates a model object required for standardizing unseen data
# Do not standardize the target variable

train_nonstd = train
test_nonstd = validation

independentattr<-setdiff(names(train),c("Amount"))
std_model <- preProcess(train[, independentattr], method = c("range"))
std_model
# The predict() function is used to standardize any other unseen data

train[, independentattr] <- predict(object = std_model, newdata = train[, independentattr])
validation[, independentattr] <- predict(object = std_model, newdata = validation[, independentattr])


# Model3- Build linear regression with all standardized attributes 
LinReg_std1<-lm(Amount~., data=train)
summary(LinReg_std1)

#Error verification on train data
regr.eval(train$Amount, LinReg_std1$fitted.values) 

#Error verification on test data
Pred<-predict(LinReg_std1,validation)
regr.eval(validation$Amount, Pred)
plot(LinReg_std1)


library(tidyr)
library(purrr)
library(GGally)

#pair plot 
cust %>%
  keep(is.numeric)%>%
  ggpairs()



