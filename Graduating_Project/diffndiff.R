#libraries
library(ggplot2)
library(dplyr)

# importing data
data<-read.csv("vital_experiment2.csv",header=T,sep=",")
head(data)

#creating two different dataframes for treatment and control to plot trend lines 
treat=data[data$treatment==1,]
cont=data[data$control2==1,]

#aggregating data
accum<-treat %>% group_by(Day)%>%summarise(acc=sum(accum_tot_comment))
accumneg<-treat %>% group_by(Day)%>%summarise(acc=sum(Total_accum_negative))
accumpos<-treat %>% group_by(Day)%>%summarise(acc=sum(Total_accum_positive))
comday<-treat %>% group_by(Day)%>%summarise(acc=sum(comments_of_day))
compos<-treat %>% group_by(Day)%>%summarise(acc=sum(pos_comment))
comneg<-treat %>% group_by(Day)%>%summarise(acc=sum(neg_comment))

accum_cont<-cont %>% group_by(Day)%>%summarise(acc=sum(accum_tot_comment))
accumneg_cont<-cont %>% group_by(Day)%>%summarise(acc=sum(Total_accum_negative))
accumpos_cont<-cont %>% group_by(Day)%>%summarise(acc=sum(Total_accum_positive))

#plotting trend lines
newdata<-as.data.frame(accum[order(accum$acc),])
n<-ggplot(newdata,aes(reorder(Day, acc), acc))+geom_point(stat = "identity")+geom_line(aes(group=1))+
  ggtitle('Accumulated total reviews in treatment group')+xlab("Day")+ylab("Accumulated number")
print(n)

n<-ggplot(accumneg,aes(reorder(Day, acc), acc))+geom_point(stat = "identity")+geom_line(aes(group=1))+
  ggtitle('Accumulated negative reviews in treatment group')+xlab("Day")+ylab("Accumulated number")
print(n)

n<-ggplot(accumpos,aes(reorder(Day, acc), acc))+geom_point(stat = "identity")+geom_line(aes(group=1))+
ggtitle('Accumulated positive reviews in treatment group')+xlab("Day")+ylab("Accumulated number")
print(n)

n<-ggplot(accum_cont,aes(reorder(Day, acc), acc))+geom_point(stat = "identity")+geom_line(aes(group=1))+
  ggtitle('Accumulated total reviews in control group')+xlab("Day")+ylab("Accumulated number")
print(n)

n<-ggplot(accumneg_cont,aes(reorder(Day, acc), acc))+geom_point(stat = "identity")+geom_line(aes(group=1))+
  ggtitle('Accumulated negative reviews in control group')+xlab("Day")+ylab("Accumulated number")
print(n)

n<-ggplot(accumpos_cont,aes(reorder(Day, acc), acc))+geom_point(stat = "identity")+geom_line(aes(group=1))+
ggtitle('Accumulated positive reviews in control group')+xlab("Day")+ylab("Accumulated number")
print(n)

#diffndiffmodels

#difference in difference variable
data$difndif<- data$treatment*data$diff_date

table(data$difndif)

#regression on total accumulated comments
reviewcountReg = lm(accum_tot_comment ~  stars+Chinese+Italian+Thai+Mexican+Japanese+Caribbean+Mediterranean+LatinAmerican+Saturday+Sunday+difndif+treatment+diff_date, data=data)
summary(reviewcountReg)

#regression on total accumulated positive comments
reviewposReg = lm(Total_accum_positive ~  stars+Chinese+Italian+Thai+Mexican+Japanese+Caribbean+Mediterranean+LatinAmerican+Saturday+Sunday+difndif+treatment+diff_date, data=data)
summary(reviewposReg)

#regression on total accumulated negative comments
reviewnegReg = lm(Total_accum_negative ~  stars+Chinese+Italian+Thai+Mexican+Japanese+Caribbean+Mediterranean+LatinAmerican+Saturday+Sunday+difndif+treatment+diff_date, data=data)
summary(reviewnegReg)
