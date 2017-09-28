# Individual Project 2
# Andrea Bruckner
# Predict 422, Sec 56

# 1
# Load the csv
data.all <- read.csv(file.path("/Users/annie/Desktop/Northwestern/PREDICT_422/Individual_Project_02","classification-output-data.csv"),sep=",",header=TRUE)
attach(data.all)

# Get overview of data
summary(data.all) # no missing data
str(data.all) # all int or numeric
head(data.all)
class(data.all) # data.frame
names(data.all)
nrow(data.all) # 181 rows
ncol(data.all) # 11 variables

# 2
table(class,scored.class)

#       scored.class
# class   0   1
#     0 119   5
#     1  30  27

# rows = actual class (class)
# columns = predicted class (scored.class)

# left column = NEGATIVES
# right column = POSITIVES

# upper left = True Negative  | Upper right = False Positive
# lower left = False Negative | lower right = True Positive

# The diagonal elements of the confusion matrix indicate correct predictions,
# while the off-diagonals represent incorrect predictions. (pg 172/440)

# Set up for functions -- If I include this code within each function, the function would be more self-contained.
c.matrix <- data.frame(table(class,scored.class))
c.matrix
str(c.matrix) # column 3 = int
TN <- c.matrix[1,3] # 119, TN
FN <- c.matrix[2,3] # 30, FN
FP <- c.matrix[3,3] # 5, FP
TP <- c.matrix[4,3] # 27, TP

# 3 -- Accuracy = (TP + TN)/(TP + FP + TN + FN)
accuracy <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  (TP + TN)/(TP + FP + TN + FN)
}
accuracy(data.all,class,scored.class) # 0.8066298

# 4 -- Classification Error Rate = (FP + FN)/(TP + FP + TN + FN)
class.error.rate <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  (FP + FN)/(TP + FP + TN + FN)
}
class.error.rate(data.all,class,scored.class) # 0.1933702

# Verify that you get an accuracy and an error rate that sums to one.
accuracy.out <- accuracy(data.all,class,scored.class)
class.error.rate.out <- class.error.rate(data.all,class,scored.class)
accuracy.out + class.error.rate.out # 1

# 5 -- Precision = TP/(TP + FP)
precision <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  TP/(TP + FP)
}
precision(data.all,class,scored.class) # 0.84375

# 6 -- Sensitivity = TP/(TP + FN)       ## aka True Positive Rate (TPR)
sensitivity <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  TP/(TP + FN)
}
sensitivity(data.all, class,scored.class) # 0.4736842

# 7 -- Specificity = TN/(TN + FP)       ## aka True Negative Rate (TNR)
specificity <- function(df, actual, predicted){
  c.matrix <- data.frame(table(actual, predicted))
  TN <- c.matrix[1,3] # 119, TN
  FN <- c.matrix[2,3] # 30, FN
  FP <- c.matrix[3,3] # 5, FP
  TP <- c.matrix[4,3] # 27, TP
  
  TN/(TN + FP)
}
specificity(data.all, class,scored.class) # 0.9596774

# 8 -- F1 Score = (2*precision*sensitivity)/(precision + sensitivity)
f1.score <- function(df, actual, predicted){
  (2*precision(df, actual, predicted)*sensitivity(df, actual, predicted))/(precision(df, actual, predicted) + sensitivity(df, actual, predicted))
}
f1.score(data.all, class, scored.class) # 0.6067416

# 9. Let’s consider the following question: What are the bounds on the F1 score?
# Show that the F1 score will always be between 0 and 1.
# (Hint: If 0 < 𝑎 < 1 and 0 < 𝑏 < 1 then 𝑎𝑏 < 𝑎.)

# 10
my.roc <- function(df,actual,probability){
  
  # http://freakonometrics.hypotheses.org/9066
  roc.curve=function(t,print=FALSE){
    p=probability
    a=actual
    Pt=(p>t)*1
    FP=sum((Pt==1)*(a==0))/sum(a==0) # false positive
    TP=sum((Pt==1)*(a==1))/sum(a==1) # true positive
    if(print==TRUE){
      print(table(Observed=a,Predicted=Pt))
    }
    vect=c(FP,TP)
    names(vect)=c("FPR","TPR")
    return(vect)
  }
  threshold = 0.5
  roc.curve(threshold,print=TRUE)
  
  ROC.curve=Vectorize(roc.curve)
  
  M.ROC=ROC.curve(seq(0,1,by=.01))
  plot(M.ROC[1,],M.ROC[2,],type="l",xlab = "1 - Specificity", ylab = "Sensitivity")
  abline(0,1, col="gray", lty=2)
  
  # https://www.kaggle.com/c/GiveMeSomeCredit/forums/t/942/r-script-for-auc-calculation
  my.auc<-function(df,actual,probability){
    N=length(probability)
    N_pos=sum(actual)
    df=data.frame(out=actual,prob=probability)
    df=df[order(-df$prob),]
    df$above=(1:N)-cumsum(df$out)
    return(1-sum(df$above*df$out)/(N_pos*(N-N_pos)))
  }
  my.auc(df,actual,probability)
}

my.roc(data.all, class, scored.probability) # 0.8503113

# 11
accuracy(data.all,class,scored.class) # 0.8066298
class.error.rate(data.all,class,scored.class) # 0.1933702
precision(data.all,class,scored.class) # 0.84375
sensitivity(data.all,class,scored.class) # 0.4736842
specificity(data.all,class,scored.class) # 0.9596774
f1.score(data.all,class,scored.class) # 0.6067416

my.roc(data.all,class,scored.probability) # 0.8503113
# AUC = # 0.8503113

# 12
# use caret
# install.packages("caret")
library(caret)
ls("package:caret") # lists objects in package
lsf.str("package:caret") # lists functions in package

confusionMatrix(data.all$scored.class,data.all$class)
# This results in sensitivity and specificity values that are flipped from the values I got using my functions.

confusionMatrix(data.all$scored.class,data.all$class, positive="1")
# This results in the same sensitivity and specificity values I got using my functions.

# Because I named my sensitivity and specificity functions the same as the functions in the caret package, I have to specify the package.
caret::sensitivity(as.factor(data.all$scored.class),as.factor(data.all$class)) # 0.9596774
caret::sensitivity(as.factor(data.all$scored.class),as.factor(data.all$class), positive = "1") # 0.4736842

caret::specificity(as.factor(data.all$scored.class),as.factor(data.all$class)) # 0.4736842
caret::specificity(as.factor(data.all$scored.class),as.factor(data.all$class), negative = "0") # 0.9596774

# 13

# code from ch. 11 page 23/27
# install.packages("pROC")
library(pROC)
ls("package:pROC") # lists objects in package
lsf.str("package:pROC") # lists functions in package
rocCurve <- function(actual, probability){
  class <- as.factor(class)
  roc.plot <- roc(response = class,
                  predictor = scored.probability,
                  ## This function assumes that the second
                  ## class is the event of interest, so we
                  ## reverse the labels.
                  levels = rev(levels(class)),
                  auc = TRUE)
  plot(roc.plot, legacy.axes = TRUE)
}

rocCurve(class,scored.probability) # 0.8503
# AUC = 0.8503


# A shorter way to do this?
plot.roc(class, scored.probability, legacy.axes = TRUE) # 0.8503





#### 10 TRIALS ####

# 1tall.packages("dplyr")
library(dplyr)

# http://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html
my.roc <- function(actual,probability){
  actual <- actual[order(probability, decreasing=TRUE)]
  roc.df <- data.frame(TPR=cumsum(actual)/sum(actual), FPR=cumsum(!actual)/sum(!actual), actual)
  plot(roc.df$FPR,roc.df$TPR, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
  abline(0,1, col="red", lty=2)
  # axis(side=1, at=seq(0,1, by = 0.01))
  # axis(side=2, at=seq(0,1, by = 0.01))
  
  # https://www.kaggle.com/c/GiveMeSomeCredit/forums/t/942/r-script-for-auc-calculation
  my.auc<-function(actual,probability){
    N=length(probability)
    N_pos=sum(actual)
    df=data.frame(out=actual,prob=probability)
    df=df[order(-df$prob),]
    df$above=(1:N)-cumsum(df$out)
    return(1-sum(df$above*df$out)/(N_pos*(N-N_pos)))
  }
  my.auc(actual,probability)
}

my.roc(class,scored.probability)
# AUC = 0.5090549



my.roc <- function(actual,probability){

  # https://www.r-bloggers.com/roc-curves-and-classification/
  # http://freakonometrics.hypotheses.org/9066
  roc.curve=function(s,print=FALSE){
  S=probability
  Y=actual
  Ps=(S>s)*1
  FP=sum((Ps==1)*(Y==0))/sum(Y==0)
  TP=sum((Ps==1)*(Y==1))/sum(Y==1)
  if(print==TRUE){
    print(table(Observed=Y,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
  threshold = 0.5
  roc.curve(threshold,print=TRUE)
  
  ROC.curve=Vectorize(roc.curve)
  
  M.ROC=ROC.curve(seq(0,1,by=.01))
  plot(M.ROC[1,],M.ROC[2,],type="l",xlab = "1 - Specificity", ylab = "Sensitivity")
  abline(0,1, col="red", lty=2)

  # https://www.kaggle.com/c/GiveMeSomeCredit/forums/t/942/r-script-for-auc-calculation
  my.auc<-function(actual,probability){
    N=length(probability)
    N_pos=sum(actual)
    df=data.frame(out=actual,prob=probability)
    df=df[order(-df$prob),]
    df$above=(1:N)-cumsum(df$out)
    return(1-sum(df$above*df$out)/(N_pos*(N-N_pos)))
  }
  my.auc(actual,probability)
}

my.roc(class, scored.probability) # 0.8503113




# S=data.all$scored.probability
# Y=data.all$class

roc.curve=function(s,print=FALSE){
  Ps=(S>s)*1
  FP=sum((Ps==1)*(Y==0))/sum(Y==0)
  TP=sum((Ps==1)*(Y==1))/sum(Y==1)
  if(print==TRUE){
    print(table(Observed=Y,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold = 0.5
roc.curve(threshold,print=TRUE)

ROC.curve=Vectorize(roc.curve)

I=(((S>threshold)&(Y==0))|((S<=threshold)&(Y==1)))
plot(S,Y,col=c("red","blue")[I+1],pch=19,cex=.7,,xlab="",ylab="")
abline(v=threshold,col="gray")

M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],lwd=2,type="l")
abline(0,1, col="red", lty=2)







I=(((S>threshold)&(Y==0))|((S<=threshold)&(Y==1)))
plot(S,Y,col=c("red","blue")[I+1],pch=19,cex=.7,,xlab="",ylab="")
abline(v=threshold,col="gray")

M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],type="l",xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1, col="red", lty=2)






# https://www.kaggle.com/c/GiveMeSomeCredit/forums/t/942/r-script-for-auc-calculation
my.auc<-function(actual,probability){
  N=length(probability)
  N_pos=sum(actual)
  df=data.frame(out=actual,prob=probability)
  df=df[order(-df$prob),]
  df$above=(1:N)-cumsum(df$out)
  return(1-sum(df$above*df$out)/(N_pos*(N-N_pos)))
}
my.auc(actual,probability)






my.roc <- function(df){
for(t in seq(0,1,by=0.01))  #I want to do this for every t in the sequence
{
  t <- t
  TP <- 0
  FP <- 0
  p <- sum(df$class==1, na.rm=TRUE)  #Total number of true positives
  n <- sum(df$class==0, na.rm=TRUE)   #Total number of true negatives
  list <- df$scored.probability #Column to vector 
  test <- ifelse(list > t, 1, 0)  #Make prediction vector
  
  for(i in 1:nrow(data)){
  if(test==1 & df$class==1){
    TP <- TP + 1
    }  #Count number of correct predictions
    if(test==1 & df$class==0){
    FP <- FP + 1
    }   #Count number of false positives
  }
  plot(x=(FP/n),y=(TP/p))    #Plot every FP,TP pair
}
}
my.roc(data.all)

data.all$scored.probability
fix(data.all)









predicted.classes = data.frame(row.names = 1:nrow(data.all))
predicted.classes

my.roc <- function(df){
  threshold <- seq(0,1,by=0.01)
  predictions = data.frame(row.names = 1:nrow(df))
  predictions
  x = seq_along(threshold)
  y = seq_along(threshold)
  for (i in threshold) {
    yhat = as.numeric(df$scored.probability>i)
    predictions = cbind(predictions, yhat)
  }
  for (j in 1:length(threshold)){
    class.factor = factor(df$class,levels = c(0,1))
    predicted.class.factor = factor(predictions[,j], levels = c(0,1))
    t = table(class.factor, predicted.class.factor)
    sensitivity = t[2,2] / (t[2,2] + t[2,1])
    specificity = t[1,1] / (t[1,1] + t[1,2])
    y[j] = sensitivity
    x[j] = 1 - specificity
  }
  
  roc.data = data.frame(fpr = x, tpr = y)
  roc.plot = ggplot(roc.data, aes(x=fpr, y=tpr)) + geom_step()
  roc.plot = roc.plot + geom_abline(slope = 1, intercept = c(0,0), colour="red", lty=2)
  
  
  threshold <- seq(0,1,by=0.01)
  predictions = data.frame(row.names = 1:nrow(data.all)) # creates df with 0 col and 181 rows
  fpr <- seq(threshold)
  tpr <- seq(threshold)
  for(i in threshold){
    above.threshold <- as.numeric(df$scored.probability>i)
    predictions <- cbind(predictions, above.threshold)
  }
  for(j in 1:length(threshold)){
    class.factor = factor(df$class,levels = c(0,1))
    predicted.class.factor = factor(predictions[,j], levels = c(0,1))
    t = table(class.factor, predicted.class.factor)
    sensitivity = t[2,2] / (t[2,2] + t[2,1])
    specificity = t[1,1] / (t[1,1] + t[1,2])
    y[j] = sensitivity
    x[j] = 1 - specificity  
  }
  
  
  
  
  df$class <- df$class[order(df$scored.probability, decreasing=TRUE)]
  roc.df <- data.frame(TP=cumsum(df$class)/sum(df$class), FPR=cumsum(!df$class)/sum(!df$class), df$class)
  plot(roc.df$FPR,roc.df$TPR, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
  abline(0,1, col="red", lty=2)
  # axis(side=1, at=seq(0,1, by = 0.01))
  # axis(side=2, at=seq(0,1, by = 0.01))
  
  # https://www.kaggle.com/c/GiveMeSomeCredit/forums/t/942/r-script-for-auc-calculation
  my.auc<-function(df){
    N=length(df$scored.probability)
    N_pos=sum(df$class)
    newdf=data.frame(out=df$class,prob=df$scored.probability)
    newdf=newdf[order(-newdf$prob),]
    newdf$above=(1:N)-cumsum(newdf$out)
    return(1-sum(newdf$above*newdf$out)/(N_pos*(N-N_pos)))
  }
  my.auc(df)
}

my.roc(data.all)

my.roc<-function(df){
  threshold <- seq(0,1,by=0.01)
  sens <- (rep(0.0, 100))
  spec <- rep(0.0, 100)
  
  for(i in 1:100){
    threshold.predictions <- rep(0, nrow(df))
    threshold.predictions[df$scored.probability > threshold[i]] = 1
    sens[i] <- sensitivity(df$class, as.matrix(threshold.predictions))
    spec[i] <- 1-specificity(df$class, as.matrix(threshold.predictions))
    
    # record 0.50 spot for plotting
    if (threshold[i] == 0.5){
      sens50 <- sens[i]
      spec50 <- spec[i]
    }
  }

















c<-function(df){
  threshold <- seq(0,1,by=0.01)
  sens <- (rep(0.0, 100))
  spec <- rep(0.0, 100)

for(i in 1:100){
  threshold.predictions <- rep(0, nrow(df))
  threshold.predictions[df$scored.probability > threshold[i]] = 1
  sens[i] <- sensitivity(df$class, as.matrix(threshold.predictions))
  spec[i] <- 1-specificity(df$class, as.matrix(threshold.predictions))
 
   # record 0.50 spot for plotting
  if (threshold[i] == 0.5){
    sens50 <- sens[i]
    spec50 <- spec[i]
  }
}
  par(mfrow = c(1,1))
  roc.plot <- plot(sens~spec, xlim=c(0,1), ylim=c(0,1), type = 'l',
            xlab = "1 - Specificity", ylab = "Sensitivity")
  roc.plot
  abline(0,1, col="red", lty=2)
  
  # get max points for extending horizontal line to diagonal
  xm <- max(spec, na.rm = TRUE)
  ym <- max(sens, na.rm = TRUE)
  
  # plot horizontal line to diagonal
  segments(x0 = xm, y0 = ym, x1 = 1, y1 = ym )
  
  # add diagonal line
  # lines(x = c(0,1), y = c(0,1))
  
  # add point for 0.50 location
  points(spec50, sens50, type = 'p', pch = 19, col = "blue", cex = 1.5)
  
  t.spec <- as.character(round(1 - spec50, 2))
  t.sens <- as.character(round(sens50, 2))
  
  
}  

my.roc(data.all)
  
  as.matrix(
  




# 11
cplot <- function(hw2.d){
  
  # Function generates roc plot and aoc calculation
  # accepts data frame
  # returns list of plot output
  
  # Generate a vector of values beteen 0 and 1 by .01
  thresh <- seq(0.01, 1, by = .01)
  
  # initialize a vector to be used for accumulating the sensitivities and specificities
  
  speci.i <- rep(0.0, 100)
  sensi.i <- rep(0.0, 100)
  
  # loop through all 100 threshold values
  for(k in 1:100) {
    
    # initialize vector for storing thresholded scores
    thresh.pred <- rep(0, nrow(hw2.d))
    
    # use threshhold to set predicted value to 1 if scored.probability > threshhold
    thresh.pred[hw2.d$scored.probability > thresh[k] ] <- 1
    
    # now run specificity and sensitivity and store results
    speci.i[k] <- 1 - specificity(hw2.d$class, as.matrix(thresh.pred) )
    sensi.i[k] <- sensitivity(hw2.d$class, as.matrix(thresh.pred) )
    
    # record 0.50 spot for plotting
    if (thresh[k] == 0.5) {
      speci.MID <- speci.i[k]
      sensi.MID <- sensi.i[k]
    }
    
  } # end for k
  
  # now create ROC plot
  par(mfrow = c(1,1))
  p <- plot(sensi.i ~ speci.i, xlim=c(0,1), ylim=c(0,1), type = 'l', lty=1,lwd=0.5, 
            xlab = "1 - Specificity", ylab = "Sensitivity")
  
  # get max points for extending horizontal line to diagonal
  xm <- max(speci.i, na.rm = TRUE)
  ym <- max(sensi.i, na.rm = TRUE)
  
  # plot horizontal line to diagonal
  segments(x0 = xm, y0 = ym, x1 = 1, y1 = ym )
  
  # add diagonal line
  lines(x = c(0,1), y = c(0,1))
  
  # add point for 0.50 location
  points(speci.MID, sensi.MID, type = 'p', pch = 19, col = "blue", cex = 1.5)
  
  t.speci <- as.character(round(1 - speci.MID, 2))
  t.sensi <- as.character(round(sensi.MID, 2))
  
  # add text label for 0.50 point in plot
  p.label <- sprintf("0.50 (Specificity = %s, Sensitivity = %s)", t.speci, t.sensi )
  text(speci.MID, sensi.MID, labels= p.label, pos = 4, cex = .8)
  
  # -----------------------------------------------
  # calcualte area under curve (auc)
  
  speci.i[is.na(speci.i)] <- 0
  sensi.i[is.na(sensi.i)] <- 0
  
  auc <- 0
  speci.prev <- 0
  
  #reverse orders of vectors to enable more straigthforward area calculation
  speci.i <- rev(speci.i)
  sensi.i <- rev(sensi.i)
  
  # aggregate incremental distance under curve of kth point
  # multiplied by the incremental distance between the current and previous speci.i values 
  for(k in 1:100) {  
    auc <- auc + ( sensi.i[k] * (speci.i[k] - speci.prev) )
    
    # update p-speci
    speci.prev <- speci.i[k]
    
  } # end for
  
  # now add final rectangle beyond max value of speci.i to ensure coverage of entire AUC
  auc <- auc + (1 - max(speci.i))
  
  # return results in a list
  l <- list(p,auc)
  return(l)
  
} # end function

roclist <- rocplot(data.all) 
roclist[2]


