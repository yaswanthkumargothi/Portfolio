#Quantity forecast for next 6 months 


CustDem<-read.csv("./Data/ExisitngCustomerDemographics.csv",header=T)
str(CustDem)

CustTrans<-read.csv("./Data/ExistingCustomersTransactionsData.csv",header=T)
str(CustTrans)



library(lubridate)
CustTrans$BillDate<-strptime(CustTrans$BillDate,format="%Y-%m-%d")
str(CustTrans$BillDate)


#number of records in each Product Category 
table(CustTrans$ProductCategory)
#tx<-table(CustTrans$ProductID)
#tx[order(-tx)] 

CustTrans$BillDate<-as.Date(CustTrans$BillDate,format="%Y-%m-%d")
str(CustTrans$BillDate)

#missing values
sum(is.na(CustTrans))
sum(is.na(CustDem))

#Subsetting data into each productcategory
library(dplyr)
Furniture=filter(CustTrans,ProductCategory =="Furniture")
Officesup=filter(CustTrans,ProductCategory=="Office Supplies")
Technology=filter(CustTrans,ProductCategory=="Technology")
head(Furniture)
tail(Furniture)

#checking minimum and maximum date's for Furniture and the whole dataset 
minDate=min(as.Date(Furniture$BillDate,format="%Y-%m-%d")) # To find the maximum of the dates 
minDate
maxDate =max(as.Date(Furniture$BillDate,format="%Y-%m-%d"))
maxDate

minDate=min(as.Date(CustTrans$BillDate,format="%Y-%m-%d")) # To find the maximum of the dates 
minDate
maxDate =max(as.Date(CustTrans$BillDate,format="%Y-%m-%d"))
maxDate

#we have same bill no. with same date with the same customerID with different productID



# EDA
#particular product is sold regularly or not
Furniture%>%
  group_by(ProductID)%>%
  summarise_each(funs(sum), Quantity)%>%arrange(desc(Quantity))

Officesup%>%
  group_by(ProductID)%>%
  summarise_each(funs(sum), Quantity)%>%arrange(desc(Quantity))

Technology%>%
  group_by(ProductID)%>%
  summarise_each(funs(sum), Quantity)%>%arrange(desc(Quantity))

#certain customer spending more than rest?
Furniture%>%
  group_by(CustomerID)%>%
  summarise_each(funs(sum), Sales)%>%arrange(desc(Sales))

Officesup%>%
  group_by(CustomerID)%>%
  summarise_each(funs(sum), Sales)%>%arrange(desc(Sales))

Technology%>%
  group_by(CustomerID)%>%
  summarise_each(funs(sum), Sales)%>%arrange(desc(Sales))

#Billdate vs sales

CustTrans%>%
  group_by(BillDate)%>%
  summarise_each(funs(sum),Sales )%>%arrange(desc(Sales))%>%plot()

CustTrans%>%
  group_by(BillDate)%>%
  summarise_each(funs(sum),Sales )%>%arrange(desc(Sales))
#on 2014-03-18 the highest number of sales happened 

#numberof bills on each date
CustTrans%>%
  group_by(BillDate)%>%
  summarise(n=n())%>%arrange(desc(n))

sum(is.na(CustTrans))


library(ggplot2)

#Quantity spread of each product category 
ggplot(CustTrans,aes(x=ProductCategory,y=Quantity))+
  geom_boxplot(aes(group=ProductCategory))
  #ggtitle("Percent of STD calls Made by each Age group")+
  #scale_y_continuous(name = "Average STD calls")

#number of records in each category
ggplot(CustTrans,aes(x=ProductCategory))+
  geom_bar()


#number of records with familysize
ggplot(CustDem,aes(x=Familysize))+
  geom_bar(aes(fill=gender))
#we could see the number of customers with family size 4 are more than any
#other familysize


ggplot(CustDem,aes(x=gender))+
  geom_bar(aes(fill=MaritalStatus))
#we could see most of the customers are married


#merging datasets using left outer join on CustDem 
MergedData<-merge(CustDem,CustTrans,by="CustomerID", all.x = TRUE)

sum(is.na(MergedData))
head(MergedData)

#gender vs quantity or sales
MergedData%>%
  group_by(gender)%>%
  summarise_each(funs(sum), Quantity)
#quanity bought by female customers more than male customers

ggplot(MergedData)+
  geom_bar(aes(x=gender))

#Quantity grouping by date 
FurnDt<-setNames(aggregate(Furniture$Quantity, by=list(Billdt=Furniture$BillDate), FUN=sum),c("BillDate","Quantity"))
FurnDt$month <- as.numeric(format(FurnDt$BillDate, format="%Y.%m"))#month
head(FurnDt)
FurnMonth<-setNames(aggregate(FurnDt$Quantity, by=list(month=FurnDt$month), FUN=sum),c("Billmonth","Quantity"))
FurnMonth

#splitting Furniture data by month
Train=FurnMonth[which(FurnMonth$Billmonth<=2017.06),] 
validation=FurnMonth[which(FurnMonth$Billmonth>2017.06),]

#timeseries
TrainFurn <- ts(Train$Quantity, frequency =12)

plot(TrainFurn,type="l",lwd=3,xaxt="n",col="blue",xlab="year",ylab="Quantity", main="Time series plot")
axis(1,at=1:5, labels=c( "2014","2015","2016","2017","2018"))

TrainFurndecomp <- decompose(TrainFurn)
plot(TrainFurndecomp,las=1)



#smoothing averages
library(TTR) 
fitsma <- SMA(TrainFurn,n=1)
fitwma<- WMA(TrainFurn,n=2,1:2)
fitEma <- EMA(TrainFurn, n =2)

par(mfrow=c(2,2))
plot(Train$Quantity, type="l", col="black")
plot(fitsma, type="l", col="red") 
plot(fitwma, type="l", col="blue") 
plot(fitEma, type="l", col="brown")

#building Holtz-Winters models
holtcustqunatforecast <- HoltWinters(Train$Quantity, beta=TRUE, gamma=FALSE) # Look the fitted or forecasted values
head(holtcustqunatforecast)
par(mfrow=c(1,1))
plot(holtcustqunatforecast)

#Building a model considering beta,gamma,and seasonality
Furnquantholtforecast <- HoltWinters(TrainFurn, beta=TRUE, gamma=TRUE, seasonal="additive") # Look the fitted or forecasted values . Did you notice the 
head(Furnquantholtforecast,100)
plot(Furnquantholtforecast)

head(Furnquantholtforecast$fitted)

holtforecastTrain <- data.frame(Furnquantholtforecast$fitted)
holtforecastTrainpredictions <- holtforecastTrain$xhat

#forecastig for next 6 months
library(forecast)
Quantforecast <- forecast(Furnquantholtforecast, h=6)



#predictions on train data
train_actuals <- TrainFurn[1:30] 
train_pred <- data.frame(hw_stockprice$fitted)[1] 
Furn_quant <- HoltWinters(TrainFurn , beta=TRUE, gamma=TRUE) #hw_price_gamma <- HoltWinters(Price[1:260], beta=TRUE, gamma=TRUE, seasonal="additive") 
Furn_quant$fitted
train_pred <- data.frame(Furn_quant$fitted)[1] 
DMwR::regr.eval(train_actuals,train_pred) 

# Forecasting on validation data 
Furn_forecasts = forecast(Furn_quant,h=6) 
validation_preds <- data.frame(Furn_forecasts)$Point.Forecast #forecast.HoltWinters(hw_price,h=1)
validation_actuals <- validation$Quantity
validation_actuals
validation
validation_preds
DMwR::regr.eval(validation_actuals,validation_preds)

plot(Furn_forecasts,ylab = "Quantity",xlab = "years")


#building a forecast model using auto arima
Furn_ARIMA <- auto.arima(TrainFurn, ic='aic')
Furn_ARIMA

#checking residuals 
par(mfrow=c(1,2))
acf(as.numeric(Furn_ARIMA$residuals) ,lag.max = 20, main = "Residuals ACF plot") 
pacf(as.numeric(Furn_ARIMA$residuals) ,lag.max = 20, main = "Residuals PACF plot")
#as we could see the residuals are independent 

#forecasting Furniture quantity for nexr 6 months
furn_forecast = forecast(Furn_ARIMA, h=6) 
par(mfrow=c(1,1))
plot(furn_forecast)

#error metrics 
DMwR::regr.eval(validation$Quantity,data.frame(furn_forecast)$Point.Forecast)














