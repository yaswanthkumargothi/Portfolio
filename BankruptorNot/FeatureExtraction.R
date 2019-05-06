data<-read.csv("./Data/bankData.csv")
head(data)
fixedassets<- data$Attr55/data$Attr28 #working capital/(working capital/fixed assets)
constantcapital<-data$Attr54*fixedassets
equity<-data$Attr53*fixedassets     #from here play with equity,constantcapital,fixedassets
totalassets<- equity/data$Attr10
sales<-data$Attr9*totalassets
EBIT<-totalassets*data$Attr7
netprofit<-totalassets*data$Attr1
totalliabilities<-totalassets*data$Attr2
shorttermliabilities<-sales/data$Attr63
inventory<-sales/data$Attr60
recievables<-sales/data$Attr61
currentassets<-totalliabilities*data$Attr50
grossprofit<-sales*data$Attr19
depreciation<-(sales*data$Attr13)-grossprofit
retainedearnings<-totalassets*data$Attr6
longtermliabilities<-equity*data$Attr59
cash<-((data$Attr30*sales)-totalliabilities)*-1
profitonopa<-data$Attr22*totalassets
financialexp<-profitonopa/data$Attr27
costofprodsold<-inventory*365/data$Attr47
grossprofit3yrs<-data$Attr24*totalassets
operatingexp<-data$Attr33*shorttermliabilities
inventorotation<-data$Attr43
profitonsales<-data$Attr39*sales
sharecapital<-((data$Attr25*totalassets)-equity)*-1
shorttermsec<-data$Attr5 
bookvaloeq<-data$Attr8*totalliabilities
extraord<-data$Attr11
EBTIDA<-sales*data$Attr49
workingcap<-data$Attr55
target<-data$target

bank<-data.frame(cash,constantcapital,totalcosts,costofprodsold,currentassets,depreciation,EBIT,equity,bookvaloeq,
    financialexp,fixedassets,grossprofit,grossprofit3yrs,inventory,longtermliabilities,netprofit,
    operatingexp,profitonopa,profitonsales,recievables,sales,sharecapital,shorttermliabilities,totalassets,totalliabilities,
    extraord,shorttermsec,inventorotation,retainedearnings,EBTIDA,workingcap,interest,currentliabilities,target)

totalcosts<-data$Attr58*totalassets


sum(is.na(bank))
interest<-(totalassets*data$Attr14)-grossprofit

currentliabilities<-data$Attr32*costofprodsold/365

str(bank)

write.csv(bank, file = "MyBank.csv",row.names = FALSE)



