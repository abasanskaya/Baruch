# Prep
################################################
rm(list = ls())
cat("\14")
graphics.off()
################################################


#Load Libraries
################################################
library("reshape2")#for melting dataframes for ggplot2
library("ggplot2")#for plotting
library("ggfortify")#for plotting PCA
library("gridExtra")
library("glmnet")#for ridge and lasso
library("doParallel")#For running cv.net in parallel
library("randomForest")#For the random forest
library("e1071")#for SVM
library("caret")#for SVM; must install from github to avoid error
library("kernlab")#for SVM
library("plyr")#for progress bar
library("scales")#for axis formatting
library("latex2exp")#for pretty LaTeX-type expressions in charts
################################################

#Constants and Settings
################################################
k.path   <-"H:/Baruch/Fall 2017/Advanced Data Mining/Project/" #path with project files
k.cores  <- detectCores() #number of logical cores (Windows)
k.retune <- FALSE # set to true to tune SVM and "tune" RF again
theme_set(theme_gray(base_size = 20))#ggplot base font
################################################

#Data Pre-processing
################################################
Data <- read.csv(paste(k.path, "ml-prove/all-data-raw.csv", sep=""), header=FALSE, sep=",")
summary(Data) #Summarize the data
#Prep the Data
#remove V5 and v35 as they are all zeros
Data<-Data[ ,!(colnames(Data) %in% c("V5","V35")) ]


#Convert response variables from censored to binary
  #0 if not proved within 100 secs (indicated by -100)
  #1 if proved within 100 secs
#add labels to the response variables
Data.responses.temp<-data.frame()
Data.responses.temp<-Data[,(ncol(Data)-5+1):(ncol(Data))]
for (i in (1:5)){
  colnames(Data)[(ncol(Data)-i+1)]<-paste("H",  5-i+1, sep="")
  
  Data[,ncol(Data)-i+1][Data.responses.temp[ ,5-i+1]==-100]<-0
  Data[,ncol(Data)-i+1][Data.responses.temp[ ,5-i+1]!=-100]<-1
}#End of: for (i in (1:5))
rm(Data.responses.temp)

#Examine response variables
Sum <-rowSums(Data[,(ncol(Data)-5+1):(ncol(Data))])#sum of five responses for each row
NumEqual<-sum(Sum==0)+sum(Sum==5)#number of response observations that are either all 0 or all 1
print(paste("The percentage of equal values among the five response variables is",  sprintf("%.1f %%",100*NumEqual/nrow(Data))))

#Percentage of 1s in the first response variable (the one to be selected)
print(paste("The percentage of 1s in the first response variable",  sprintf("%.1f %%",100*mean(Data$H1==1))))

for (i in (1:5)){
  Data[,ncol(Data)-i+1]=as.factor(Data[,ncol(Data)-i+1])
}#End of:for (i in (1:5))

#Basic setup
################################################
################################################
#Put data into matrix form for glmnnet
################################################
X         =   model.matrix(Data$H1~., Data[, 1:(ncol(Data)-4)])[, -1]
X         =   scale(X)#standardixze
y         =   Data$H1
n         =   nrow(X) # sample size
p         =   ncol(X) # number of predictors/features
################################################

# Determine the train/validation split and the # of iterations
################################################
#set.seed(1)
n.train.vector       =     c(2*p, 10*p, floor(n/2))
names(n.train.vector)=     c("2p", "10p", "n/2")
n.train              =     n.train.vector[3]#manually change this
n.test               =     n-n.train
m.samp               =     100;
################################################


#Arrays/vectors to hold misclassification and variable imporptance results
################################################
#Ridge/Lasso/SVM arrray dimensions
mod.types                                      <- c("lasso", "ridge")
validation.types                               <- c("train", "test")
cv.types                                       <- c(10, n.train)
names(cv.types)                                <- c("10fold", "loocv")
hparam.sel.types                               <- c("min", "1se")

#array for lasso/ridge
misclass.array                                 <- array(rep(NA, 4*m.samp),
                                                  dim=c(m.samp, 2, 2, 2, 2),
                                                  list( seq(1:m.samp),
                                                        validation.types,
                                                        mod.types,
                                                        cv.types,
                                                        hparam.sel.types <- c("min", "1se")
                                                      )
                                                  )
#array for models w/o different selextion methods
mod.no.method.types      <-  c("logistic", "logistic.glmnet", "lasso.aic", "svm", "rf")
misclass.array.no.method <-  array(rep(NA, 5*m.samp),
                                 dim=(c(m.samp, 2, 5)),
                                 list(seq(1:m.samp),
                                      validation.types,
                                      mod.no.method.types
                                      )
                                 )

#array for variables
mod.types.for.var   <- c("lasso", "ridge", "rf")
variable.array      <- array(rep(NA, p*length(mod.types.for.var)),
                        dim=c(p, length(mod.types.for.var)),
                        list(colnames(X),
                              mod.types.for.var)
                         )
################################################

#Wrapper Function for Lasso/Ridge
################################################
classLogisticPenalized <-function(bLasso, numfolds, X.tr, y.tr){
  closeAllConnections()
  require(doParallel)
  registerDoParallel(k.cores)#turn on parallel computing, using all  available logical cores
  my.cv  <- cv.glmnet(X.tr, y.tr, alpha=as.numeric(bLasso), family = "binomial", 
                      nfolds=numfolds, type.measure="class", parallel=TRUE) 
  closeAllConnections()
  return(my.cv)
}
################################################
################################################


#Loop through all iterations
################################################
################################################
#Create a progress bar
require(plyr)
progress.bar <- create_progress_bar("text")
progress.bar$init(m.samp)
#Start the loop to run through all the samples
for (i in 1:m.samp){
  #assign observations to train/test for the current sample
  ################################################
  train    =     sample(1:n, floor(n.train))
  test     =     (-train)
  

  X.train  =     X[train, ]
  y.train  =     y[train]
  X.test   =     X[test, ]
  y.test   =     y[test]
  
  #Write the observations in the first training sampel to a csv for future inspection
  #   since many charts are made from the first sample.
  if(i==1){
    write.csv(train, paste0(k.path, gsub("/", "_", names(n.train)),
                            "_1_st_training_set_observations",".csv"))
  }
  
  ################################################
  message("##################################################") 
  message("Sample: ", i, ", n: ", n, ", n.train: ", n.train)
  ################################################
  #Logistic Regression######################
  #Note: Theoretically, logistic regression is the same as lasso/ridge with lambda=0.
  #      However, according to the ISLR footnote, "In order for glmnet() to yield the 
  #      exact least squares coefficients when ?? = 0,
  #      we use the argument exact=T when calling the predict() function. Otherwise, the
  #      predict() function will interpolate over the grid of ?? values used in fitting the 
  #      glmnet() model, yielding approximate results."
  #      
  #      In the current implementation, exact=T requires refitting the model, so 
  #      the glm function was used due to computational considerations. Based on some
  #       experimenation, the difference between glm and glmnet with lambda =0 
  #       does not seem to be huge.
  glm.result                                       <- glm(formula = as.factor(y.train)~., family=binomial, 
                                                          data=as.data.frame(cbind(X.train, y.train)))
  #Train Error
  train.pred                                       <- predict(glm.result, as.data.frame(X.train), type="response")
  train.pred                                       <- ifelse(glm.result$fit>0.5,1,0)
  misclass.array.no.method[i, "train", "logistic"] <- mean(train.pred!=as.numeric(levels(y.train))[y.train])
  message("The training logistic misclassification eror rate is ", 
          misclass.array.no.method[i, "train", "logistic"])
  
  #Test Error
  test.pred                                        <- predict(glm.result, as.data.frame(X.test), type="response")
  test.pred                                        <- ifelse(test.pred>0.5,1,0)
  misclass.array.no.method[i, "test", "logistic"]  <- mean(test.pred!=as.numeric(levels(y.test))[y.test])
  message("The test logistic misclassification eror rate is ", 
          misclass.array.no.method[i, "test", "logistic"])
  ###############################################
  for (t in (mod.types)){#lasso and ridge
    for (c in (cv.types)){#10fold and loocv cv
      message("Performing ", t, " logistic classification with ", names(which(cv.types == c)), " cross-validation." )
      glmnet.result             <-    classLogisticPenalized(ifelse(t=="lasso",TRUE, FALSE), numfolds=c, X.tr=X.train, y.tr=y.train)
      glm.fits                  <-   glmnet.result$glmnet.fit
      for (h in hparam.sel.types) {#min and 1se
         message("Using ", h, " the lambda is ", get(paste(" lambda", h, sep="."), glmnet.result))
         
         #Train Error
         train.pred                                         <- predict(glm.fits , newx = X.train, type="class", 
                                                                       s=get(paste("lambda", h, sep="."), glmnet.result))
         misclass.array[i, "train", t, as.character(c), h]  <- mean(train.pred!=y.train)
         message("For ", t, ", using ", names(which(cv.types==c)), 
                 " validation with the ", h, " rule, the training misclassiciation rate was ", 
                 misclass.array[i, "train", t, as.character(c), h])
         
         #Test Error
         test.pred                                          <- predict(glm.fits , newx = X.test, type="class", 
                                                                       s=get(paste("lambda", h, sep="."), glmnet.result))
         misclass.array[i, "test", t, as.character(c), h]   <- mean(test.pred!=y.test)
         message("For ", t, ", using ", names(which(cv.types==c)), 
                 " validation with the ", h, " rule, the test misclassiciation rate was ", 
                 misclass.array[i, "test", t, as.character(c), h])
       }#End of:  for (h in hparam.sel.types)
      #Plot 10-fold cv errors 
      ###############################################
      if ((i==1)){
        if(c==10){
          variable.array[,t]<-glm.fits$beta[, which(glm.fits$lambda==glmnet.result$lambda.min)]
          png(filename=paste(k.path, gsub("/", "_", names(n.train)), "_", t, "_CV_curve", ".png", sep=""))
          
          #trick glmnet into plotting log base 10
          glmnet.result1<-glmnet.result
          glmnet.result1$lambda<-exp(log(glmnet.result1$lambda)/log(10))
          glmnet.result1$lambda.min <- exp(log(glmnet.result$lambda.min)/log(10))
          glmnet.result1$lambda.1se <- exp(log(glmnet.result$lambda.1se)/log(10))
          
          plot(glmnet.result1, xlab= expression(paste("log(",lambda,")"), sep=""), 
               yaxt="n", cex=2.5)
          yLabels <- seq(floor((min(glmnet.result$cvm)*100/5))*5/100, 
                         ceiling((max(glmnet.result$cvm)*100/5))*5/100, length.out=5)
          axis(2, at=yLabels, labels=sprintf(round(100*yLabels), fmt="%.0f%%"), las=1)
          legend("topleft",lty=c(2,3),
                 legend=c(sprintf("Usual rule: %4.2e", glmnet.result$lambda.min),
                          sprintf("One SE rule: %4.2e",glmnet.result$lambda.1se)))
                dev.off()
          rm(glmnet.result1)#no longer needed
        }#End of:  (c=="10")
      }#End of: if ((i==1))
      ###############################################
      #AIC for Lasso
      ###############################################
      if (t=="lasso" & c==10){#only run for lasso with 10-fold
        #Get indices
        index.min <- which(glm.fits$lambda==glmnet.result$lambda.min)
        index.1se <- which(glm.fits$lambda==glmnet.result$lambda.1se)
        # only include same elments in fit as in glmnet; technically, these steps are 
        # unecessary as glmnet is always the first lambdas in the fit (glment stops using a
        # stopping rule)
        fits.lambda <- glm.fits$lambda[(glm.fits$lambda %in% glmnet.result$lambda)]
        devs        <- deviance(glm.fits)
        devs        <- devs[(glm.fits$lambda %in% glmnet.result$lambda)]
        #get log likelihood (up to a constant)
        LL.times.2  <- rep(glm.fits$nulldev, length(devs))-devs
        #get df
        k           <- glm.fits$df[(glm.fits$lambda %in% glmnet.result$lambda)]#non-zero predictors for lasso
        #calculate AIC and get the model selected
        AIC          <- -LL.times.2 +2*k
        index.min.AIC<- which.min(AIC)
        
        #Train Error
        aic.train.pred = predict(glm.fits , newx = X.train, type="class", s= glmnet.result$lambda[index.min.AIC])
        misclass.array.no.method[i, "train", "lasso.aic"]<-mean(aic.train.pred!=y.train)
        
        #Test Error
        aic.test.pred = predict(glm.fits , newx = X.test, type="class", s= glmnet.result$lambda[index.min.AIC])
        misclass.array.no.method[i, "test", "lasso.aic"]<-mean(aic.test.pred!=y.test)
        
        message("The test model selected by cross-validation is ", index.min, " with lambda ",  glmnet.result$lambda.min, " and p=", glmnet.result$nzero[index.min],  
                " vs \n", index.min.AIC, "with AIC and lambda ", glmnet.result$lambda[index.min.AIC], "and p=", glmnet.result$nzero[index.min.AIC])
        
        if(i==1){#create AIC plot and lasso
          png(filename=paste(k.path, gsub("/", "_", names(n.train)), "_", t, "_AIC_plot", ".png", sep=""))
          par(mar=c(5.1,4.1,4.1,5.1))
          plot(x=log10(fits.lambda), y=AIC,  xlab=(expression(log(lambda))), las=1)
          points(log10(fits.lambda), AIC, pch = 20, 
                 col = "blue")
          
          axis(side = 3, at = log10(glmnet.result$lambda), labels = paste(glmnet.result$nz), 
               tick = FALSE, line = 0)
          abline(v = log10(fits.lambda[index.min.AIC]), lty = 3, col="blue")
          par(new = TRUE)
          plot(log10(glmnet.result$lambda), glmnet.result$cvm, axes = FALSE, bty = "n", xlab = "", ylab = "")
          axis(side=4, at = pretty(range(glmnet.result$cvm)), labels=sprintf(round(100*pretty(range(glmnet.result$cvm))), fmt="%.0f%%"), las=1)
          
          mtext("CV Misclassification Rate", side=4, line=3)
          points(log10(glmnet.result$lambda), glmnet.result$cvm, pch = 20, 
                 col = "red")
          
          legend("topleft",legend=c(sprintf("AIC: Min %0.2f, p = %d", min(AIC), glmnet.result$nzero[index.min.AIC]),
                                    "10-Fold CV", 
                                    sprintf("Usual rule: %4.2e, p = %d", glmnet.result$lambda.min, glmnet.result$nz[index.min]),
                                    sprintf("One SE rule: %4.2e, p = %d",glmnet.result$lambda.1se, glmnet.result$nz[index.1se])),
                 text.col=c("blue","red", "red", "red"),pch=c(16,15,NA, NA),col=c("blue","red", "red", "red"))
          abline(v = log10(glmnet.result$lambda.min), lty = 3, col="red")
          abline(v = log10(glmnet.result$lambda.1se), lty = 3, col="red")
          dev.off()
          
          #Also create plot of betas vs. lambda just in case.
          png(filename=paste(k.path, gsub("/", "_", names(n.train)), "_", t, "_lasso_variables", ".png", sep=""))
          plot(glm.fits, xvar="lambda", label=TRUE)
          dev.off()
          
        }#End of: if(i==1)
      }#End of:if (t=="lasso")
    }#End of: for (c in (cv.types))
  }#End of: for (t in (mod.types))
 #######################################################
 #SVM
 #######################################################
  #get initial SVM parameters
  ################################################
  if (i==1 & k.retune==TRUE){#Initial SVM Parameters from the first sample
    #Use e1071
    ptm <- proc.time()
    #Cost should depend on sample size
    cost.n.2 = 10^seq(-3,4, by=1)
    cost.2p  = 10^seq(-3,5, by=1)
    if(n.train == 2*p){#Use larger grid for 2p
      cost <-  cost.2p
    }else{
      cost <-  cost.n.2
    }
    
    tuned.ctrl.10    <-tune.control(random = FALSE, nrepeat = 1, sampling="cross", 
                                    sampling.dispersion = sd, cross = 10)
    
    
    tuned.svm        <- tune(svm, X.train, y.train, 
                             ranges = list(gamma = 10^seq(-4,1, length.out=6), cost = cost), 
                             tuned.ctrl.10)
    svm.best.model   <- tuned.svm$best.model
    #Parameters are the same using loocv, so commented out
    # tuned.ctrl.loocv <-tune.control(random = FALSE, nrepeat = 1, sampling="cross", 
    #                                 sampling.dispersion = sd, cross = n.train)
    # tuned.svm.loocv  <- tune(svm, X.train, y.train,
    #                  ranges = list(gamma = 10^seq(-4,1, length.out=6),
    #                                cost = cost,
    #                                tuned.ctrl.loocv))
    
    run.time <- proc.time() - ptm
    message("It took ", run.time, "to tune the SVM.")
    
    
    #SVM Param Heat Map
    png(filename=paste(k.path, gsub("/", "_", names(n.train)), "SVM_initial_tuning_param_heatmap", ".png", sep=""),  width = 600, height=400)
    plot(tuned.svm, type="contour", transform.x = log10, transform.y = log10, xlab= expression(paste("log(",gamma, ")", sep="")), ylab="log(C)",
         main = "")
    dev.off()
    C.tuned.init = tuned.svm$best.parameters$cost
    gamma.tuned.init= tuned.svm$best.parameters$gamma
    message("Using e1071, the initial gamma estimate is ", gamma.tuned.init)
    message("Using e1071, the initial cost estimate is ", C.tuned.init)
    
    #Line plot
    mylabel <- TeX(sprintf("C: %0.2f, $\\gamma$: %0.4f", C.tuned.init, gamma.tuned.init))
    p.line  <- ggplot(tuned.svm$performances, aes(x=cost, y=error, colour=factor(gamma)))
    p.line  <- p.line+geom_line()+geom_point()
    p.line  <- p.line + scale_x_log10(labels = scales::trans_format("log10", scales::math_format(.x)))+ scale_y_continuous(labels = scales::percent)
    p.line  <- p.line + labs(colour=expression(gamma))+ xlab("log(C)") + ylab("Error")
    p.line  <- p.line + annotate("text", x = min(tuned.svm$performances$cost), y = max(tuned.svm$performances$error), label=as.character(mylabel), parse=T, hjust=-0.2, vjust=1)
    p.line  <- p.line + theme(axis.line = element_line(colour="black"))
    p.line
    ggsave(paste(gsub("/", "_", names(n.train)), "SVM_initial_tuning_param_plot", ".png", sep=""), plot=p.line, path=k.path, device="png", scale=1, width=5, height=5, units="in")
    unlink(paste(gsub("/", "_", names(n.train)), "SVM_initial_tuning_param_plot", ".png", sep=""))
    
  }#End of: if (i==1 & k.retune==TRUE)
  
  if (k.retune==FALSE){#use pre-tuned parameters
    C.tuned.init    <- switch(names(n.train),
                          "2p" = 10,
                          "10p" = 1000,
                          "n/2" = 100)
    
    gamma.tuned.init<- switch(names(n.train),
                             "2p" = 0.001,
                             "10p" = 0.001,
                             "n/2" = 0.1)
    svm.best.model  <- svm(X.train, y.train, gamma=gamma.tuned.init, cost = C.tuned.init, type="C-classification", kernel="radial")
  }
  #SVM Errors Using Model Tuned in Sample 1
  ##########################################################
  cl <- makePSOCKcluster(k.cores)
  svm.train.pred                               <-predict(svm.best.model, newdata = X.train)
  svm.test.pred                                <-predict(svm.best.model, newdata = X.test)
  
  #Train Error 
  misclass.array.no.method[i, "train", "svm"]  <- mean(svm.train.pred!=y.train)
  message("The SVM training error is ", misclass.array.no.method[i, "train", "svm"])
  
  #Test Error
  misclass.array.no.method[i, "test", "svm"]   <- mean(svm.test.pred!=y.test)
  message("The SVM test error is ", misclass.array.no.method[i, "test", "svm"])
 
  stopCluster(cl)
  registerDoSEQ()#insert serial backend to prevent errors
  #Visualize SVM Results (not required)
  ##########################################################
  if(i==1 & k.retune==TRUE){
    png(filename=paste(k.path, gsub("/", "_", names(n.train)), "SVM_training", ".png", sep=""),  width = 600, height=400)
    plot(cmdscale(dist(X.train[,])),
         col = as.integer(y.train[]),
         pch = c("o","+")[1:n.train %in% svm.best.model$index + 1], xlab="1st C", ylab="2nd C")
    dev.off()
    
    png(filename=paste(k.path, gsub("/", "_", names(n.train)), "SVM_training_pred", ".png", sep=""),  width = 600, height=400)
    plot(cmdscale(dist(X.train[,])),
         col = as.integer(y.train[]),
         pch = as.integer(svm.train.pred),  xlab="1st C", ylab="2nd C")
    dev.off()
    
    png(filename=paste(k.path, gsub("/", "_", names(n.train)), "SVM_test_pred", ".png", sep=""),  width = 600, height=400)
    plot(cmdscale(dist(X.test[,])),
         col = as.integer(y.test[]),
         pch = as.integer(svm.test.pred),  xlab="1st C", ylab="2nd C")
    dev.off()
  }#End of: if(i==1 & k.retune==TRUE)
  ############################################
  
  #Random Forest
  ############################################
  #Find optimal mtry (variables to consider at each split)
  ############################################
  if(i==1 & k.retune==TRUE){
    require(caret)
    control                  <- trainControl(method="cv", number=10, search="grid")
    metric                   <- "Accuracy"
    tunegrid                 <- expand.grid(.mtry=c(1,2,3,4,5,6,7,10,20,30,40,50))
    
    cl                       <- makePSOCKcluster(k.cores)
    rf.gridsearch            <- train(H1~., data=Data, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
    stopCluster(cl)
    registerDoSEQ()#insert serial backend to prevent errors
    
    #Extract errors
    rf.gridsearch.err1       <- data.frame(row.names=seq(1:nrow(rf.gridsearch$results)))
    rf.gridsearch.err1$mtry  <- rf.gridsearch$results$mtry
    rf.gridsearch.err1$Error <- 1- rf.gridsearch$results$Accuracy
    mtry.min                 <- which.min(rf.gridsearch.err1$Error)
    #print(rf.gridsearch)
    
    
    #Plot the errors vs. the number of variables
    theme_set(theme_gray(base_size = 12))
    mylabel1  <- sprintf("Optimal mtry: %d", mtry.min)
    p.rf.mtry <- ggplot(rf.gridsearch.err1, aes(x=mtry, y=Error, colour=Error))+geom_point()
    p.rf.mtry <- p.rf.mtry + theme(axis.line = element_line(colour="black"), panel.background = element_rect(fill = "white"))
    p.rf.mtry <- p.rf.mtry + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,10,20,30,40,50))
    p.rf.mtry <- p.rf.mtry +  scale_y_continuous(labels = scales::percent) + guides(colour=FALSE)
    p.rf.mtry <- p.rf.mtry +  geom_smooth(linetype="dotted", method=loess, se=FALSE, colour="black", size=1)
    p.rf.mtry <- p.rf.mtry + guides(linetype=FALSE)
    p.rf.mtry <- p.rf.mtry + labs(title=mylabel1)
    p.rf.mtry
    ggsave(paste(gsub("/", "_", names(n.train)), "RF_mtree_plot", ".png", sep=""), plot=p.rf.mtry, path=k.path, device="png", scale=1, width=4, height=3, units="in")
    unlink(paste(gsub("/", "_", names(n.train)), "RF_mtree_plot", ".png", sep=""))
    ############################################
    
    #Check error vs. number of trees
    ############################################
    rf.result <- randomForest(as.factor(y.train)~.,data=cbind(X.train, y.train), mtry = mtry.min, ntree=500, importance=TRUE)
    rf.legend <- colnames(rf.result$err.rate)
  
      numtrees<-c(500, 400, 300)
      for (x in numtrees){
        if(rf.result$err.rate[x, 1]/rf.result$err.rate[x-100, 1] < 
           rf.result$err.rate[x-100, 1]/rf.result$err.rate[x-200, 1] + 0.05){
          ntree.selected = x; break#stop if the performance improvement is less than the previous, within margin
        }
      }#End of: for (x in numtrees)
    
      #Make a plot
      png(filename=paste(k.path, gsub("/", "_", names(n.train)), "RF_ntree_plot", ".png", sep=""),  width = 400, height=300)
      plot(rf.result, main = paste("Selected:", ntree.selected),  yaxt="n", cex=2.5, adj = 0)
      legend("topright", cex =1, legend=rf.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)
      yLabels <- seq(floor(min(rf.result$err.rate)/0.05)*0.05, ceiling(max(rf.result$err.rate)/0.05)*0.05, by=0.05)
      axis(2, at=yLabels, labels=sprintf(round(100*yLabels), fmt="%.0f%%"), las=1, cex =2)
      dev.off()
      ############################################
  }#End of: if(i==1 & k.retune==TRUE)
  if(k.retune==FALSE){
    mtry.min = 7
    ntree.selected=500
  }
  ############################################
  #Random Forest Errors
  require(randomForest)
  rf.result                                 <-randomForest(as.factor(y.train)~.,data=cbind(X.train, y.train), mtry = mtry.min, ntree= ntree.selected, importance=TRUE)
  
  #Train Error 
  train.pred.rf                             <- predict(rf.result,newdata=X.train)
  train.pred.rf                             <- factor(train.pred.rf, levels =c(1,2), labels=c("0","1"))
  misclass.array.no.method[i, "train", "rf"]<-mean(train.pred.rf!=y.train)
  print(paste("The training Random Forrest misclassification is", misclass.array.no.method[i, "train", "rf"]))
  
  #Test Error   
  test.pred.rf                              <- predict(rf.result,newdata=X.test)
  test.pred.rf                              <- factor(test.pred.rf, levels =c(1,2), labels=c("0","1"))
  misclass.array.no.method[i, "test", "rf"] <-mean(test.pred.rf!=y.test)
  print(paste("The test Random Forrest misclassification is", misclass.array.no.method[i, "test", "rf"]))
  
  #Get variable importance
  if(i==1){#Betas
    rf.importance<-importance(rf.result)
    #use mean decreasingGini index
    #variable.array[,"rf"]<-sort(rf.importance[ ,4], decreasing=FALSE)#don't sort for now
    variable.array[,"rf"]<-rf.importance[ ,4]
  }
}#End of: for (i in 1:m.samp) 
################################################
################################################
theme_set(theme_gray(base_size = 20))#default font size
#Boxplots
############################################
############################################
############################################
#Create dfs for train and test boxplots
for (v in validation.types){
  assign(paste(v,"misclass", sep="."), as.data.frame(misclass.array[,v,,,]))
  assign(paste(v,"misclass", sep="."), cbind(get(paste(v,"misclass", sep=".")), as.data.frame(misclass.array.no.method[, v,])))
  #Save outputs in case the boxplot needs to be re-formatted 
  write.csv(get(paste(v,"misclass", sep=".")), paste0(k.path, gsub("/", "_", names(n.train)),"_", v, "_cvm_", as.character(Sys.Date()),".csv"))
}

test.misclass$logistic.glmnet<-NULL

#Uncomment lines below to read in previously written results
# train.misclass<-read.csv(paste0(k.path,"2p_train_cvm_2017-12-19.csv"))
# test.misclass<-read.csv(paste0(k.path,"2p_test_cvm_2017-12-19.csv"))
# train.misclass$X<-NULL
# test.misclass$X<-NULL

#Function for creating boxplots
create.Box.Plot <-function(df.cv, v){
  v.misclass <- df.cv
  v.misclass <-na.omit(v.misclass)#in case want to make an interim boxlplot
  v.misclass$logistic.glmnet<-NULL#delete glmnet
  print(head(v.misclass))
  v.misclass.melt<-melt(v.misclass, v.names="id", direction="long")
  v.misclass.melt[,"model.type"]<-rep("logistic", nrow(v.misclass.melt))
  v.misclass.melt[,"model.type"][grepl("logistic.glmnet", v.misclass.melt[ ,"variable"])]="logistic.glmnet"
  v.misclass.melt[,"model.type"][grepl("lasso", v.misclass.melt[ ,"variable"])]="lasso"
  v.misclass.melt[,"model.type"][grepl("ridge", v.misclass.melt[ ,"variable"])]="ridge"
  v.misclass.melt[,"model.type"][grepl("svm", v.misclass.melt[ ,"variable"])]="svm"
  v.misclass.melt[,"model.type"][grepl("rf", v.misclass.melt[ ,"variable"])]="rf"
  v.misclass.melt$model.type.f = factor(v.misclass.melt$model.type, levels=c("logistic", "logistic.glmnet", "lasso", "ridge", "rf", "svm"))
  
  v.misclass.melt[,"criteria"]<-rep("NA", nrow(v.misclass.melt))
  v.misclass.melt[,"criteria"][grepl("1se", v.misclass.melt[ ,"variable"])]="1se"
  v.misclass.melt[,"criteria"][grepl("min", v.misclass.melt[ ,"variable"])]="min"
  v.misclass.melt[,"criteria"][grepl("aic", v.misclass.melt[ ,"variable"])]="AIC"
  v.misclass.melt$criteria.f = factor(v.misclass.melt$criteria, levels=c("min", "1se", "AIC", "NA"))
  
  v.misclass.melt[,"valid"]<-rep("", nrow(v.misclass.melt))
  v.misclass.melt[,"valid"][grepl(10, v.misclass.melt[ ,"variable"])]="10-fold"
  v.misclass.melt[,"valid"][grepl(n.train, v.misclass.melt[ ,"variable"])]="loocv"
  v.misclass.melt[,"valid"][grepl("aic", v.misclass.melt[ ,"variable"])]="AIC"
  v.misclass.melt$valid.f = factor(v.misclass.melt$valid, levels = c("10-fold", "loocv", "AIC", ""))
 
  
  final.model.types<-unique(v.misclass.melt$model.type)
  means            <- aggregate(v.misclass.melt$value,  list(model.type.f=v.misclass.melt$model.type.f, criteria.f=v.misclass.melt$criteria.f, valid.f=v.misclass.melt$valid.f), mean)
  medians          <- aggregate(v.misclass.melt$value,  list(model.type.f=v.misclass.melt$model.type.f, criteria.f=v.misclass.melt$criteria.f, valid.f=v.misclass.melt$valid.f), median)
  v.misclass       <-v.misclass[c("logistic", "lasso.10.min", "lasso.10.1se", paste0("lasso.", n.train, ".min"), paste0("lasso.", n.train, ".1se"), "lasso.aic", "ridge.10.min", "ridge.10.1se", paste0("ridge.", n.train, ".min"), paste0("ridge.", n.train, ".1se"), "rf", "svm" )]
  v.df.summary     <-do.call(cbind, lapply(v.misclass, summary))#get summary as df

  print(v.df.summary)
  v.df.q4 <-v.df.summary["Max.", ] 
  v.df.q3 <-v.df.summary["3rd Qu.", ] 
  v.df.q2 <-v.df.summary["Median", ] 
  v.df.q1 <-v.df.summary["1st Qu.", ] 
  v.df.q0 <-v.df.summary["Min.", ] 
  y.pos<-ifelse(grepl("1se", names(v.df.q3)),v.df.q4, v.df.q0-0.03)
  print("###")
  print("Medians")
  print(v.df.q2)
  #####
  num.facets<-length(levels(v.misclass.melt$model.type.f))
  p.box <-ggplot(v.misclass.melt)+aes(x=valid.f, y=value, fill=as.factor(criteria.f)) + stat_boxplot(geom ="errorbar") 
  p.box <- p.box + geom_boxplot(outlier.size=1.5, outlier.shape=21, position="dodge")
  p.box <-p.box + geom_text(aes(x=valid.f, y=y.pos,  label = sprintf("%0.1f%%",x*100)), 
                            data = medians,
                            position=position_jitterdodge(dodge.width = 0.75, jitter.width=0.00, jitter.height=0.00), vjust=-1, inherit.aes = TRUE,  size=2.5)#sprintf("%.2g%%",x*100))
  p.box<- p.box+theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust=0.25))
  p.box<- p.box+facet_wrap(~model.type.f, ncol = num.facets, scales = "free_x")#blocks
  
  
  p.box <- p.box +  xlab("")
  p.box<- p.box +  ylab("Misclassification Rate") 
  p.box<- p.box +   scale_fill_manual(values=c("#FF6699",  "#33CCFF", "#9999CC", "#00FFCC"), name = "Model Selection\n")
  p.box <- p.box + scale_y_continuous(labels = scales::percent)
  p.box <-p.box +  theme(legend.position="none")
   # p.box <- p.box +  theme(legend.direction = "horizontal", 
   #                          legend.position = "top",
   #                          legend.box = "horizontal")
  
  p.box
  ggsave(paste(gsub("/", "_", names(n.train), "_"), v , "_Boxplot_v10", ".png", sep=""), plot=p.box, path=k.path, device="png", scale=1, width=7.15, height=4.7, units="in")
  unlink(paste(gsub("/", "_", names(n.train), "_"), v , "_Boxplot_v10", ".png", sep=""))
  
  
}
for (v in validation.types){
  create.Box.Plot(get(paste(v,"misclass", sep=".")), v)
}
############################################
############################################
############################################
#Variable importance plot
################################################
################################################
theme_set(theme_gray(base_size = 14))#set font size

df.variables <- as.data.frame(variable.array)
#Write data to CSV in case chart needs to be remade
write.csv(df.variables, paste0(k.path, gsub("/", "_", names(n.train)),"_variable_import",".csv"))

#Uncomment below lineS to read in previously written variables
# df.variables <-read.csv(paste0(k.path,"2p_variable_import.csv"))
# rownames(df.variables)<-df.variables$X
# df.variables$X<-NULL

#make sure number of lasso variables matches lasso cv plot
message(sum(abs(df.variables$lasso)>0), " lasso predictors")

df.variables$varnames        <- rownames(df.variables)
df.variables.melt            <-melt(df.variables, v.names="id", direction="long")
df.variables.melt$value      <- abs(df.variables.melt$value)
df.variables.melt$varnames[] <- substring(df.variables$varnames[], 2,length(df.variables$varnames))
df.variables.melt$varnames   <-factor(df.variables.melt$varnames, levels=unique(df.variables.melt$varnames))
p.variables <- ggplot(df.variables.melt, aes(x=varnames, y =value, fill=variable))+ geom_bar(stat="identity", colour="black")
p.variables <- p.variables + facet_wrap(~variable, nrow = 3, strip.position="left", scales = "free_y")
p.variables <- p.variables + scale_y_reverse(limits = c(NA, 0)) + guides(fill=FALSE)
p.variables <- p.variables +  xlab("") + scale_x_discrete(labels = df.variables.melt$varnames)
p.variables <- p.variables +   theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  scale_fill_manual(values=c("#FF6699", "#9999CC", "#33CCFF"))+
  theme(axis.text.x  = element_text(size=8, angle=90, vjust = 0.5))
p.variables
ggsave(paste(gsub("/", "_", names(n.train)), "_Varimport_v9", ".png", sep=""), plot=p.variables, path=k.path, device="png", scale=1, width = 4, height=6, units="in")
unlink(paste(gsub("/", "_", names(n.train)), "_Varimport_v9", ".png", sep=""))



