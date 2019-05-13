### SEGMENTATION of Customers

rm(list=ls())

CustDem<-read.csv("./Data/ExisitngCustomerDemographics.csv",header=T)
str(CustDem)

CustTrans<-read.csv("./Data/ExistingCustomersTransactionsData.csv",header=T)
str(CustTrans)

CustTrans$BillDate<-as.Date(CustTrans$BillDate,format="%Y-%m-%d")
str(CustTrans$BillDate)

#complete dataset
MergedData<-merge(CustDem,CustTrans,by="CustomerID", all.x = TRUE)
str(MergedData)

#removing ID's and Billnumber
MergedData$CustomerID<- NULL
MergedData$BillNumber<- NULL

MergedData$ProductID <- as.integer(gsub('[a-zA-Z]', '', MergedData$ProductID))
MergedData$prodbin = cut(MergedData$ProductID,c(6000,6200,6400,6600,6800,7000,7200,7400,7600,7800,8000))
table(MergedData$prodbin)

str(MergedData$prodbin)

MergedData$ProductID <- NULL

#Date conversion
MergedData$DOB<- as.Date(MergedData$DOB,format="%Y-%m-%d")

#extracting new parameters
library(lubridate)
MergedData$Age<-(year(MergedData$BillDate)-year(MergedData$DOB))


MergedData$Billyear<- year(MergedData$BillDate)

MergedData$Billmonth<- month(MergedData$BillDate)

#removing dates 
MergedData$DOB<- NULL
MergedData$BillDate<- NULL

#checking data spread 
library(ggplot2)

ggplot(MergedData,aes(x=ProductCategory,y=Sales))+
  geom_boxplot()

#Dummyfying categorical data
#install.packages("dummies")
library(dummies)
genderDummyVars<-dummy(MergedData$gender)
MaritalDummyvars<-dummy(MergedData$MaritalStatus)
ProductDummyvars<-dummy(MergedData$ProductCategory)
prodbindummy<-dummy(MergedData$prodbin)

MergedDatadum<-data.frame(MergedData,genderDummyVars,MaritalDummyvars,ProductDummyvars)
MergedDatadum<-data.frame(MergedDatadum,prodbindummy)

#removing categorical variables
MergedDatadum$gender<-NULL
MergedDatadum$MaritalStatus<-NULL
MergedDatadum$ProductCategory<-NULL
MergedDatadum$prodbin<-NULL

str(MergedDatadum)

#standardizing numerical variables
#install.packages("vegan")
library(vegan)
standData<-decostand(MergedDatadum[,-c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)],"standardize")#using z score
# standardize variables 
standmergeData<-cbind(standData,MergedDatadum[,c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)])

#---------------------------------K-Means-----------------------------#

install.packages("factoextra")
library(factoextra)

set.seed(123)
fit <- kmeans(standmergeData, 5) # 5 cluster solution
fit$withinss
fit$betweenss
#study the mdoel
fit$cluster
fit$tot.withinss
fit
fit$centers

#visualizing clusters
library(factoextra)
fviz_cluster(fit, standmergeData)

#MergedData <- data.frame(MergedData, fit$cluster)
#write.csv(mydata,"kmeans_2.csv")
#head(MergedData)

#optimized number of clusters
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(standmergeData,centers=i)$withinss)
}

#plotting Number of clusters vs WSS
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

factoextra::fviz_nbclust(standmergeData, kmeans, method = "wss")
#k=6!?
set.seed(123)
final_fit_kmeans <- kmeans(standmergeData, 10)



###-------------------------    Gower's distance     ------------------------###
#  To deal with mixture of attribute types

# distance using gower measure
distMat <- daisy(MergedData, metric = "gower")
# hierarchical clustering
fitGower<-hclust(distMat,method="single")
dev.off()
plot(fitGower)
# naming the clusters 



#using the grower distance Matrix and building model using Partioning Around Medoids algorithm
pam_fit1 <- pam(distMat,
               diss = TRUE,
               k = 7,keep.data=TRUE)
pam_fit1


pam_fit1$data
#clust plot


#silhoutte width
sil_width <- pam_fit1$silinfo$avg.width

sil_width
# RStudio sometimes does not display silhouette plots correctly

plot(silhouette(pam_fit1),  col = 1:7,border=NA) 

for (k in 2:10)
  asw[k] <- pam(distMat, k) $ silinfo $ avg.width
k.best <- which.max(asw)

plot(1:10,asw,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, asw)

#final model
pam_fit2 <- pam(distMat,
                diss = TRUE,
                k = 6)


pam_results <- MergedData %>%
  mutate(cluster = pam_fit2$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

#plotting a t-sne plot to visulize multidimentional data
install.packages("Rtsne")
library(Rtsne)

tsne_obj <- Rtsne(distMat, is_distance = TRUE)

library(dplyr)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit2$clustering),
         name = MergedData$ProductCategory)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#medoids
MergedData[pam_fit2$medoids, ]

  
#stabilitycheck
set.seed(123)
index <- (sample(nrow(standmergeData),.70*nrow(standmergeData)))
data <- MergedData[index,]

#dissimillarity matrix calculation on newsample
distMat1<- daisy(data, metric = "gower")
pam_fit3 <- pam(distMat1,
               diss = TRUE,
               k = 6)

data$clusters <- pam_fit3$clustering

group1 <- pam_fit2$clustering[index]
group2 <- data$clusters


install.packages("fossil")
library(fossil)
stabilitycheck <- adj.rand.index(group1, group2)
stabilitycheck
#across samples: avg_stabilitycheck
#Index value between 0 and 1, where 1 means the two clustering outcomes match identically.


install.packages("clusteval")
library(clusteval)
Stabindex <- cluster_similarity(group1, group2, similarity = "jaccard", method="independence")
Stabindex

MergedDataclus<-cbind(MergedData,pam_fit2$clustering)
write.csv(MergedDataclus,file = "Clustered.csv")


#------------------------------------clara---------------------------------------#
library(cluster)
clarax <- clara(MergedDatadum, 6, samples=200)
clarax
clarax$clusinfo

plot(clarax)


fviz_cluster(clarax, stand = FALSE, geom = "point",
             pointsize = 1)

plot(silhouette(clarax),  col = 2:7, main = "Silhouette plot")  

#optimum number of clusters
asw <- numeric(10) 
for (k in 2:10) 
    asw[k] <- clara(MergedDatadum, k,metric = "grower",samples = 200) $ silinfo $ avg.width 
k.best <- which.max(asw) 
cat("silhouette-optimal number of clusters:", k.best, "\n")

asw

#Number of clusters vs silhoutee plot
silht<-cbind(1:10,asw)
plot(silht,xlab = "Number of clusters",ylab = "Average Silhouette Width")
lines(silht)

## checking  with more numer of samples

(clx3 <- clara(MergedDatadum, 4))
## "better" number of samples
cl.3 <- clara(MergedDatadum, 4, samples=400)
## but that did not change the result here:
stopifnot(cl.3$clustering == clx3$clustering)


plot(clx3, ask = TRUE)


## Trying  100 times different random samples for reliability:
nSim <- 100
nCl <- 4 # = no.classes
set.seed(421)# (reproducibility)
cl <- matrix(NA,nrow(MergedDatadum), nSim)
for(i in 1:nSim)
  cl[,i] <- clara(MergedDatadum, nCl, medoids.x = FALSE, rngR = TRUE)$cluster
tcl <- apply(cl,1, tabulate, nbins = nCl)

cl
tcl

#----------------------predictions on new dataset --------------------#

#data preparation for predictions
NewMergeData<-read.csv("./NewMergeData.csv",header=T)
str(NewMergeData)
NewMergeData$Age<-as.integer(NewMergeData$Age)

#dummify
library(dummies)
genderDummyVars<-dummy(NewMergeData$gender)
MaritalDummyvars<-dummy(NewMergeData$MaritalStatus)
ProductDummyvars<-dummy(NewMergeData$ProductCategory)


NewMergeDatadum<-data.frame(NewMergeData,genderDummyVars,MaritalDummyvars,ProductDummyvars)


NewMergeDatadum$gender<-NULL
NewMergeDatadum$MaritalStatus<-NULL
NewMergeDatadum$ProductCategory<-NULL

#calculating gower distances from medoids we got from model and assigning the closest cluster
install.packages('gower')
library(gower)
med<-MergedData[pam_fit2$medoids,]

NewMergeData<-NewMergeData[,c(4,5,6,1,2,3,7,8,9)]  
head(NewMergeData)

#Predictions using minimum dissimilarity 
pred<-apply(sapply(1:7, function(i) gower_dist(med[i,], NewMergeData)),1,which.min)

NewDatapred<-cbind(NewMergeData,pred)

#saving predictions
write.csv(NewDatapred,file="NewunsupPred.csv",row.names = FALSE)
