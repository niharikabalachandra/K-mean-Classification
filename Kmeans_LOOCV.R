##Problem Statement: 
##The two outcomes for the given input data "germancredit.csv," 
##are success and failure, which represent defaulting on the loan and not defaulting, respectively. 
##You need to perform logistic regression by considering the features of the loan and 
##characteristics of the borrower as the explanatory variables.

##After this, you are required to perform k-means classification with the help of three 
##continuous variables-duration, amount, and instalment.

##Finally, take help of the cross-validation method to determine the % of identification 
##that happens correctly with k = 5 nearest neighbors.



#Solution


##Dataset considered is the German Credit Data with data on 1000 loans

library(textir) ## needed to standardize the data
library(class) ## needed for knn
library(dplyr)
library(OpenRepGrid)

## read data and isolate variables, change path to where you have stored the .csv file
credit_info <- read.csv("~/germancredit_info.csv")
credit_info 
credit_info$Default <- factor(credit_info$Default)

## re-level the credit_info history and a few other variables
credit_info$history = factor(credit_info$history,
                        levels=c("A30","A31","A32","A33","A34"))
levels(credit_info$history) = c("good","good","poor","poor","terrible")
credit_info$foreign <- factor(credit_info$foreign, levels=c("A201","A202"),
                         labels=c("foreign","german"))
credit_info$rent <- factor(credit_info$housing=="A151")
credit_info$purpose <- factor(credit_info$purpose,
                         levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit_info$purpose) <-
  c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

## for demonstration, cut the dataset to these variables
credit_info <- credit_info[,c("Default","duration","amount","installment","age",
                    "history", "purpose","foreign","rent")]
credit_info[1:3,]
summary(credit_info) # check out the data
## for illustration we consider just 3 loan characteristics:
## amount,duration,installment
## Standardization of the data is preferable, especially if
## units of the features are quite different
## We use the normalize function in the R-package textir;
## it converts data frame columns to mean-zero stdeviation-one

f <- normalize(credit_info[,c(2,3,4)])
f[1:3,]

## training and prediction datasets
## training set of 900 borrowers; want to classify 100 new ones
set.seed(1)
train <- sample(1:1000,900) ## this is training set of 900 borrowers
ftrain <- f[train,]
fnew <- f[-train,]
gtrain <- credit_info$Default[train]
gnew <- credit_info$Default[-train]

## k-nearest neighbor method
library(class)
nearest_1 <- knn(train=ftrain, test=fnew, cl=gtrain, k=1)
nearest_3 <- knn(train=ftrain, test=fnew, cl=gtrain, k=3)
data.frame(gnew,nearest_1,nearest_3)[1:10,]
37
## calculate the proportion of correct classifications
cor_classification1=100*sum(gnew==nearest_1)/100
cor_classification3=100*sum(gnew==nearest_3)/100
cor_classification1
cor_classification3

## plot for 3nn
plot(ftrain[,c("amount","duration")],col=c(4,3,6,2)[credit_info[train,"installment
                                                           "]],pch=c(1,2)[as.numeric(gtrain)],main="Predicted default, by 3 nearest
     neighbors",cex.main=.95)
points(fnew[,c("amount","duration")],bg=c(4,3,6,2)[credit_info[train,"installment"
                                                          ]],pch=c(21,24)[as.numeric(nearest_3)],cex=1.2,col=grey(.7))
legend("bottomright",pch=c(1,16,2,17),bg=c(1,1,1,1),legend=c("data 0","pred
                                                             0","data 1","pred 1"),title="default",bty="n",cex=.8)
legend("topleft",fill=c(4,3,6,2),legend=c(1,2,3,4),title="installment
       %",horiz=TRUE,bty="n",col=grey(.7),cex=.8)

## above was for just one training set
## cross-validation (leave one out)
cor_classification=dim(10)
for (k in 1:10) {
  pred=knn.cv(f,cl=credit_info$Default,k)
  cor_classification[k]=100*sum(credit_info$Default==pred)/1000
}
cor_classification


a=sum(cor_classification)
b=length(cor_classification)
answer= a/b
answer
