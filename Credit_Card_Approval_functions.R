# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  CREDIT CARD APPROVAL PREDICTION
#
# Data Wizards
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# ************************************************
# PREPROCESSING_removePunctuation()
# Code obtained from lab3
# INPUT: String - fieldName - name of field
#
# OUTPUT : String - name of field with punctuation removed
# ************************************************

# Pre-Processing a Dataset functions

# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()
#************************************************
# readDataset() :
# Part of this code is taken from lab3
# Read a CSV file from working directory
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
readDataset<-function(csvFilename){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  
  #Change columns to lowercase for ease of use
  #names(dataset)<-tolower(names(dataset))
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}
# ************************************************
# PREPROCESSING_initialFieldType() :
# This code is from lab3
# Test each field for NUMERIC or SYMBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
PREPROCESSING_initialFieldType<-function(dataset){
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }
    
    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}
# ************************************************
# PREPROCESSING_prettyDataset()
# Code obtained from lab3
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
PREPROCESSING_prettyDataset<-function(dataset,...){
  
  params <- list(...)
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}
# ***********************************************
# PREPROCESSING_modifyColumns
# Change column types, encode categorical values,remove negative values
# INPUT- datasframe - dataset
# OUTPUT- dataframe -dataset

# In our case factor data types contains character data.So We'll transform factor data types to numeric data type 
# since it'll be more handy to use for our functions ahead. First do the data transformation wherever text data 
# is available in the columns like CODE_GENDER,FLAG_OWN_CAR, etc..
# CODE_GENDER        : "F","M": Transformed to "0","1"
# FLAG_OWN_CAR       : "N","Y": Transformed to "0","1"
# FLAG_OWN_REALTY    : "N","Y": Transformed to "0","1"
# CNT_CHILDREN       : "No children","1 children","2+ children" : Transformed to "0","1","2"
# NAME_EDUCATION_TYPE: "Academic degree", "Higher education", "Incomplete higher", "Lower secondary", 
#                      "Secondary / secondary special": Transformed to "0","1","2","3","4",
# NAME_FAMILY_STATUS : "Civil marriage", "Married", "Separated", "Single / not married", "Widow" 
#                    : Transformed to "0","1","2","3","4"
# NAME_HOUSING_TYPE  : "Co-op apartment", "House / apartment", "Municipal apartment", "Office apartment", 
#                      "Rented apartment", "With parents":Transformed to "0","1","2","3","4","5"
#JOB                 : "Accountants", "Cleaning staff", "Cooking staff", "Core staff", "Drivers", etc..
#                    : Transformed:"0","1","2","3","4","5", "6", "7","8","9".."17"
#STATUS              : Here we will replace the value C and X with 0 as it is the same type
#                        1,2,3,4,5 are classified as 1 because they are the same type

# *************************************************
PREPROCESSING_modifyColumns<-function(dataset){

# Change daysbirth to age
dataset$AGE<- as.integer(abs(dataset$DAYS_BIRTH)/365)

# Change daysemployed to monthemployed (in months)
dataset$DAYS_EMPLOYED<-   round(dataset$DAYS_EMPLOYED*(-1)/30)
names(dataset)[names(dataset)=='DAYS_EMPLOYED'] <- 'MONTH_EMPLOYED'

# Change the negative from the values in beginmonths
dataset$BEGIN_MONTHS<-abs(dataset$BEGIN_MONTHS)

# Encoding gender column Female=1 Male=0
dataset$CODE_GENDER<-as.integer(ifelse(dataset$CODE_GENDER=='F',1,0))

# Encode flag_own_car column; Y=1 N=0
dataset$FLAG_OWN_CAR <- as.integer(ifelse(dataset$FLAG_OWN_CAR=='Y',1,0))

# Encode flagownrealty; Y=1 N=0
dataset$FLAG_OWN_REALTY <- as.integer(ifelse(dataset$FLAG_OWN_REALTY=='Y',1,0))

# Encode cnt_children; No children=0, 1 children=1, 2+ children=2
dataset$CNT_CHILDREN<- as.integer(ifelse(dataset$CNT_CHILDREN=="No children",0,
                                           ifelse(dataset$CNT_CHILDREN=='1 children',1,2)))
# Encode name_education_type
dataset$NAME_EDUCATION_TYPE  <- as.integer(as.factor(dataset$NAME_EDUCATION_TYPE))

# Encode name_family_status
dataset$NAME_FAMILY_STATUS <- as.integer(as.factor(dataset$NAME_FAMILY_STATUS))

# Encode name_housing_type
dataset$NAME_HOUSING_TYPE <- as.integer(as.factor(dataset$NAME_HOUSING_TYPE))

# Encode job
dataset$JOB <- as.integer(as.factor(dataset$JOB))

#Encode Status
dataset$STATUS[which(dataset$STATUS %in% c('C','X'))]<- 0
dataset$STATUS[which(dataset$STATUS %in% c('2','3','4','5'))]<-1
dataset$STATUS               <-  as.integer(dataset$STATUS)
#Droppimg column DAYS_BIRTH,
dataset<-subset(dataset,select=-c(DAYS_BIRTH))

# Re-order the columns so that target is at the last
print(colnames(dataset))
dataset<-dataset[,c(1,2,3,4,5,6,7,8,9,19,10,11,12,13,14,15,16,17,18)]
return(dataset)
}
#******************************************
# reduceFields()
## We will delete the variables of extremely week correlation based on analyzing our data,
# We will start with factor fields first

reduceFields<-function(dataset){
  dataset <- select(dataset, -one_of("ID","STATUS" , "JOB", "FLAG_EMAIL", "FLAG_WORK_PHONE",
                                               "NAME_HOUSING_TYPE","FLAG_MOBIL" ))
  
  return(dataset)
}

# ******************************************
# reduceData()
# Reduces the dataset byt taking all the risk users and a sample of non-risk users
# Input: dataframe - dataset
# Output: reduced dataset
reduceData<-function(dataset){
  set.seed(123)
  credit_approved    <- dataset[which(dataset$TARGET==0),]
  credit_fail        <- dataset[which(dataset$TARGET==1),] 
  i <-sample(1:nrow(credit_approved),0.033*nrow(credit_approved))
  sample_approved <- credit_approved[i,]
  
  #combine the 2 separated datasets:
  credit_new <- rbind(sample_approved,credit_fail)
  
  #now the first 10178 data of customer_new are the data about the approved credit cards
  #and the rest are about the fail ones
  #so we try to shuffle them 
  set.seed(123)
  rows<-sample(nrow(credit_new),)
  c <- credit_new[rows,]
  return(c) 
}
# *************************************************************
# create_train_test()
#
# splits data into train and test
# INPUT : dataset
# Output: Returns train dataset when the parameter train = TRUE
#         Return test dataset when the parameter train=FALSE
#***************************************************************
create_train_test <- function(dataset, size = 0.75, train = TRUE) {
  n_row = nrow(dataset)
  total_row = size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (dataset[train_sample, ])
  } else {
    return (dataset[-train_sample, ])
  }
}

# ************************************************
# determineThreshold() :
# code obtained from lab 4
# For the range of threholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Deterime "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - test_predicted   - probability of being class 1
#         :   vector double  - test_expected    - dataset to evaluate
#         :   boolean        - plot             - TRUE=output charts
#         :   string         - title            - chart title
#
# OUTPUT  :   List       - Named evaluation measures from confusion matrix
#                        - Threshold at min Euclidean distance
#                        - AUC - area under the ROC curve
#                        - Predicted class probability
# ************************************************
determineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){
  toPlot<-data.frame()
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-EvaluateClassifier(test_predicted=test_predicted,
                                 test_expected=test_expected,
                                 threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }
  
  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # Youdan = sensitivty + specificity -1
  #        = TPR + (1-FPR) -1
  
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  
  #  max Youdan
  # use which.max() to return a single index to the higest value in the vector
  maxYoudan<-toPlot$x[which.max(toPlot$youdan)]
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))
  
  #Euclidean distance to "perfect" classifier (smallest the best)
  # use which.min() to return a single index to the lowest value in the vector
  minEuclidean<-toPlot$x[which.min(toPlot$distance)]
  
  # ************************************************
  # Plot threshold graph
  
  if (plot==TRUE){
    # Sensitivity (TPR)
    plot(toPlot$x,toPlot$tpr,
         xlim=c(0, 1), ylim=c(0, 100),
         type="l",lwd=3, col="blue",
         xlab="Threshold",
         ylab="%Rate",
         main=paste("Threshold Perfomance Classifier Model",title))
    
    # Plot the specificity (1-FPR)
    lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
    
    # The point where specificity and sensitivity are the same
    crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
    
    if (!is.na(crosspoint)){
      if ((crosspoint<1) & (crosspoint>0))
        abline(v=crosspoint,col="red",lty=3,lwd=2)
    }
    
    # Plot the Euclidean distance to "perfect" classifier (smallest the best)
    lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
    
    # Plot the min distance, as might be more (311019NRT check it is within range)
    if ((minEuclidean<1) & (minEuclidean>0))
      abline(v=minEuclidean,col="green",lty=3,lwd=2)
    
    # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
    lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
    
    if ((maxYoudan<1) & (maxYoudan>0))
      abline(v=maxYoudan,col="purple",lty=3,lwd=2)
    
    legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
    text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nEuclidean=",minEuclidean,"\nYoudan=",maxYoudan))
    
    # ************************************************
    #ROC graph
    
    sensitivityROC<-toPlot$tpr[which.min(toPlot$distance)]
    specificityROC<-100-toPlot$fpr[which.min(toPlot$distance)]
    auc<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC
    
    # Set origin point for plotting
    toPlot<-rbind(toPlot,data.frame(x=0,fpr=0,tpr=0, youdan=0,distance=0))
    
    plot(100-toPlot$fpr,toPlot$tpr,type="l",lwd=3, col="black",
         main=paste("ROC:",title),
         xlab="Specificity (1-FPR) %",
         ylab="Sensitivity (TPR) %",
         xlim=c(100,0),
         ylim=c(0,100)
    )
    
    axis(1, seq(0.0,100,10))
    axis(2, seq(0.0,100,10))
    
    #Add crosshairs to the graph
    abline(h=sensitivityROC,col="red",lty=3,lwd=2)
    abline(v=specificityROC,col="red",lty=3,lwd=2)
    
    annotate<-paste("Threshold: ",round(minEuclidean,digits=4L),
                    "\nTPR: ",round(sensitivityROC,digits=2L),
                    "%\n1-FPR: ",round(specificityROC,digits=2L),
                    "%\nAUC: ",round(auc,digits=2L),sep="")
    
    text(x=specificityROC, y=sensitivityROC, adj = c(-0.2,1.2),cex=1, col="red",annotate)
    
  } # endof if plotting
  
  # Select the threshold - distance is selected here
  
  myThreshold<-minEuclidean      # Min Distance should be the same as analysis["threshold"]
  
  #Use the "best" distance threshold to evaluate classifier
  results<-EvaluateClassifier(test_predicted=test_predicted,
                               test_expected=test_expected,
                               threshold=myThreshold)
  
  results$threshold<-myThreshold
  results$AUC<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC
  
  return(results)
} #endof determineThreshold()

# ************************************************
# printMeasures()
# code obtained from lab 4
# Output measures to the Viewer
#
# INPUT:    list -   results - results from calcConfusion()
#           string - title   - title of the table
#
# OUTPUT :  NONE
# ************************************************
printMeasures<-function(results,title){
  
  #This outputs our results into the "Viewer" in RStudio
  tidyTable<-data.frame(t(t(results)))
  names(tidyTable)[1]<-title
  
  t<-formattable::formattable(tidyTable,list(
    TP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TP)),
    FN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FN)),
    TN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TN)),
    FP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FP))))
  print(t)
}
# ************************************************
# calcConfusion() :
# code obtained from lab 4
# Calculate a confusion matrix for 2-class classifier
# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#
# OUTPUT: A list with the  entries from NcalcMeasures()
#
# A credit card approval is indicated when $TARGET=0 (i.e. non-risky customer) and credit card decline when $TARGET=1 (risky customer)

#                    ACTUAL
#               ------------------
# PREDICTED   Declined=1   |  Approval=0
#               ------------------
#   Declined=1      TP     |    FP
#               ==================
#   Approval=0      FN     |    TN
#
#
# ************************************************
calcConfusion<-function(expectedClass,predictedClass){
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  # This "converts" the above into our preferred format
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  
  return(calcMeasures(TP,FN,FP,TN))
  
} #endof calcConfusion()

# ************************************************
# calcMeasures() :
# code obtained from lab 4
# Evaluation measures for a confusion matrix
#
# INPUT: numeric  - TP, FN, FP, TN
#
# OUTPUT: A list with the following entries:
#        TP        - double - True Positive records
#        FP        - double - False Positive records
#        TN        - double - True Negative records
#        FN        - double - False Negative records
#        accuracy  - double - accuracy measure
#        pgood     - double - precision for "good" (values are 1) measure
#        pbad      - double - precision for "bad" (values are 1) measure
#        FPR       - double - FPR measure
#        TPR       - double - FPR measure
#        TNR       - double - TNR measure
#        MCC       - double - Matthew's Correlation Coeficient
# ************************************************
calcMeasures<-function(TP,FN,FP,TN){
  
  accuracy<-round(100.0*((TP+TN)/(TP+FP+FN+TN)), digits=2)
  pgood   <-round(100.0*(TP/(TP+FP)),digits=2)
  pbad    <-round(100.0*(TN/(FN+TN)),digits=2)
  fpr     <-round(100.0*(FP/(FP+TN)),digits=2)
  tpr     <-round(100.0*(TP/(TP+FN)),digits=2)
  tnr     <-round(100.0*(TN/(FP+TN)),digits=2)
  mcc     <-round( ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)),digits=3)
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=accuracy,
                  "pgood"=   pgood,
                  "pbad"=    pbad,
                  "FPR"=     fpr,
                  "TPR"=     tpr,
                  "TNR"=     tnr,
                  "MCC"=     mcc
  )
  return(retList)
}


# ************************************************
# auroc() :
# code obtained from lab 4
# Calculate the Area Under Curve (AUC) for ROC
#
# INPUT   :   vector double     - score            - probability of being class 1
#             vector double     - bool             - Expected class of 0 or 1
#
# OUTPUT  :   double   - AUC
#
# ************************************************


auroc <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}



