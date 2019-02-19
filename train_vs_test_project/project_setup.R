rm(list = ls())
cat("\14")
graphics.off()

#install.packages("ggfortify", lib="C:/Users/Annyutka/Documents/RPackages")

#Libraries
library(reshape2)#for melting dataframes for ggplot2
library(ggplot2)#for plotting
library(ggfortify)#for plotting PCA
library(gridExtra)
library(glmnet)#for ridge and lasso
library(doParallel)#For running cv.net in parallel
library(randomForest)#For the random forest
library("e1071")#for SVM
library("caret")#for SVM; must install from github to avoid error
library("kernlab")#for SVM
library(plyr)#for progress bar
library(scales)#for axis formatting
library(latex2exp)#for pretty LaTeX-type expressions in charts


k.cores= detectCores()
k.retune = FALSE # set to true to tune SVM and "tune" RF again

#theme for ggplot2 charts
theme_set(theme_gray(base_size = 20))
mytheme <- theme(panel.background = element_rect(fill = rgb(0.9,0.9,0.9)),
                 panel.grid.major = element_line(size = (0.2), colour="white"),
                 panel.grid.minor = element_line(size = (0.2), colour="white"))


colnames=c(
  #Static features
  "Fraction of clauses that are unit clauses.",
  "Fraction of clauses that are Horn clauses. ",
  "Fraction of clauses that are ground Clauses.",
  "Fraction of clauses that are demodulators.",
  "Fraction of clauses that are rewrite rules (oriented demodulators).",
  "Fraction of clauses that are purely positive.",
  "Fraction of clauses that are purely negative.",
  "Fraction of clauses that are mixed positive and negative.",
  "Maximum clause length.",
  "Average clause length.",
  "Maximum clause depth.",
  "Average clause depth.",
  "Maximum clause weight.",
  "Average clause weight.",
  #Dynamic features
  "Proportion of generated clauses kept. (Subsumed or trivial clauses are discarded.)",
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  10,
  11,
  12,
  13,
  14,
  15,
  16,
  17,
  18,
  19,
  20,
  21,
  22,
  23,
  24,
  25,
  26,
  27,
  28,
  29,
  30,
  31,
  32,
  33,
  34,
  35,
  36,
  37,
  38,
  39,
  "H1",
  "H2",
  "H3",
  "H4",
  "H5"
)

Data      =   read.csv(paste(k.path, "ml-prove/all-data-raw.csv", sep=""), header=FALSE, sep=",")
#Summarize the data
summary(Data)
#Prep the Data
#add labels to last five columns
#convert times in the last five columns to classes
#0 if not proved within 100 secs (indicated by -100)
#1 if proved within 100 secs
n<-c(nrow(Data)) 

Data.responses.temp<-data.frame()
Data.responses.temp<-Data[,(ncol(Data)-5+1):(ncol(Data))]
for (i in (1:5)){
  colnames(Data)[(ncol(Data)-i+1)]=paste("H",  5-i+1, sep="")
  Data[,ncol(Data)-i+1][Data.responses.temp[ ,5-i+1]==-100]=0
  Data[,ncol(Data)-i+1][Data.responses.temp[ ,5-i+1]!=-100]=1
  
}
#sum of five responses for each row
Sum <-rowSums(Data[,(ncol(Data)-5+1):(ncol(Data))])

#number of response observations that are either all 0 or all 1
NumEqual<-sum(Sum==0)+sum(Sum==5)
print(paste("The percentage of equal values among the five response variables is",  sprintf("%.1f %%",100*NumEqual/nrow(Data))))
print(paste("The percentage of 1s in the first response variable",  sprintf("%.1f %%",100*mean(Data$H1==1))))

#remove V5 and v35 as they are all zeros
Data<-Data[ ,!(colnames(Data) %in% c("V5","V35")) ]

for (i in (1:5)){
  Data[,ncol(Data)-i+1]=as.factor(Data[,ncol(Data)-i+1])
}



#Percentage of 1s