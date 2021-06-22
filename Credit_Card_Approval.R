# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#
# Data Wizards
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
#
# ************************************************

# clears all objects in "global environment"
rm(list=ls())
# ************************************************
# Global Environment 'constant' objects
# i.e. available to all functions
DATASET_FILENAME  <- "credit_card_approval.csv"          #Name of input dataset file
OUTPUT_FIELD      <- "TARGET"

TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
#TYPE_IGNORE      <- "IGNORE"             # field is not encoded
KFOLDS            <- 6                    # Number of folded experiments


PDF_FILENAME      <- "tree.pdf"           # Name of PDF with graphical tree diagram
RESULTS_FILENAME  <- "results.csv"        # Name of the CSV results file


NODE_LEVEL        <- 1                    # The number is the node level of the tree to plot
BOOST             <- 20                   # Number of boosting iterations. 1=single model
FOREST_SIZE       <- 1000                 # Number of trees in the forest
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage


## Define and load libraries used in this project
# ************************************************
# Library from CRAN     
# pacman	               
# outliers	             
# formattable 	         
# ggplot2                
# tensorflow
# keras
# knitr
# tidyverse
# mice
# lattice
# DataExplorer
# caret
# reticulate
# rpart
# data.table
# reshape2
# plyr
# dplyr
# randomForest


#install.packages("tensorflow")
#library(tensorflow)
#install_tensorflow()
MYLIBRARIES<-c("outliers","tensorflow","keras",
               "data.table","knitr","tidyverse","mice","lattice",
               "reshape2","DataExplorer","caret","reticulate","rpart","rpart.plot",
                "plyr","dplyr","randomForest","formattable","ggplot2","pROC")

# User defined functions are next
#********************************************************
# allocateFoldID() :
#
# Append a column called "foldID" that indicates the fold number
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
allocateFoldID<-function(dataset){
  recordsPerFold<-ceiling(nrow(dataset)/KFOLDS)
  
  foldIds<-rep(seq(1:KFOLDS),recordsPerFold)
  
  foldIds<-foldIds[1:nrow(dataset)]
  
  dataset$foldId<-foldIds
  return(dataset)
} #endof allocateFoldID()

# ************************************************
# stratifiedSplit() :
#
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - dataset
#
# OUTPUT  :   list               - train & test datasets
# ************************************************

stratifiedSplit<-function(newDataset,fold){
  test<-subset(newDataset, subset= foldId==fold, select=-foldId)
  train<-subset(newDataset, subset= foldId!=fold,select=-foldId)
  return(list(
    train=train,
    test=test))
}

#*************************************************************
# stratifiedDataset() :
#
# Split dataset by the class (We have 2-class)
# Calculate the number of records that will appear in each fold
# Give each of these blocks a unique foldID
# combine the datasets & randomise
# The dataset now has a foldID from which we can select the data
# for the experiments
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
stratifiedDataset<-function(originalDataset){
  positionClassOutput=which(names(originalDataset)==OUTPUT_FIELD)
  # Get the unique class values
  classes<-unique(originalDataset[,positionClassOutput])
  # Split dataset into the two classes (so as to keep the class balance the same in the datasets)
  indexClass1<-which(originalDataset[,positionClassOutput]==classes[1])
  split1<-originalDataset[indexClass1,]
  split2<-originalDataset[-indexClass1,]
  
  # Append a column that indicates the fold number for each class
  split1<-allocateFoldID(split1)
  split2<-allocateFoldID(split2)
  
  # Combine the two datasets
  
  newDataset<-rbind(split1,split2)
  
  #Randomise the classes
  newDataset<-newDataset[order(runif(nrow(newDataset))),]
  return(newDataset)
}

# ************************************************
# runExperiment() :
#
#
# INPUT   :   data frame         - dataset        - dataset
#             object function    - FUN            - name of function
#             ...                - optional        - parameters are passed on
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
runExperiment<-function(dataset,FUN, ...){
  allResults<-data.frame()
  
  for (k in 1:KFOLDS){
    
    print(paste("Processing fold #",k))
    
    splitData<-stratifiedSplit(newDataset=dataset,fold=k)
    
    measures<-FUN(train=splitData$train,
                  test=splitData$test,
                  plot=(k==KFOLDS),...)
    
    allResults<-rbind(allResults,data.frame(measures))
  } #endof for()
  
  # Return the means from all the experiments back as a list
  # round metrics to 2 decimal places
  getMeans<-round(colMeans(allResults),digits=2)
  
  getMeans[1:4]<-as.integer(getMeans[1:4])  # TP, FN, TN, FP are rounded to ints
  
  # return the above with the rounded values
  return(as.list(getMeans))
  
} #endof runExperiment()
# ************************************************
# fullDT() :
#
# Create C5 Decision Tree
#
# INPUT   :
#             Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             int            - boost       - number of trees to boost
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
fullDT<-function(train,test,boost=1,plot=TRUE){
  # boost=1 implies one tree is created
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expected output
  train_expected<-train[,positionClassOutput]
  
  # ************************************************
  # Create a standard Decision Tree using the C5.0 algorithm
  # Uses library C50
  # Outputs the tree in the format of rules
  
  myTitle<-"DT C5.0"
  if (boost>1)
    myTitle<-paste(myTitle,"BOOSTED=",boost)
  
  print(myTitle)
  
  tree<-C50::C5.0(x=train_inputs,
                  factor(train_expected),
                  rules=TRUE,
                  trials=boost)
  plotTree(train)
  print("Tree plotted in PDF file")
  
  # Use the created decision tree with the test dataset
  # to determine best classification threshold & calculate metrics
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    
    print(summary(tree))
    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.5,
            main=myTitle)
    
  }
  return(measures)
} #endof fullDT()


# ************************************************
# randomForest() :
#
# Create Random Forest on pre-processed dataset
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
randomForest<-function(train,test,plot=TRUE){
  
  myTitle<-(paste("Random Forest=",FOREST_SIZE,"trees"))
  print(myTitle)
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=FOREST_SIZE ,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(train_inputs)))
  
  
  # ************************************************
  # Use the created decision tree with the test dataset
  measures<-getTreeClassifications(myTree = rf,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    # Get importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Strength"
    
    barplot(t(importance),las=2, border = 0,
            cex.names =0.5,
            main=myTitle)
    
    print(formattable::formattable(data.frame(importance)))
  }
  
  return(measures)
} #endof randomForest()

# ************************************************
# plotTree() :
#
# Plots tree to a PDF file
#
# INPUT   :
#             Data Frame    - train - to create the DT
#             int           - boost - optional parameter
#
# OUTPUT  :
#             File is created
#
# ************************************************
plotTree<-function(train,boost=1){
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  Global_train_inputs<<-train[,-positionClassOutput]
  
  # train data: vector with the expedcted output
  Global_train_expected<<-train[,positionClassOutput]
  
  tree<-C50::C5.0(x=Global_train_inputs,
                  factor(Global_train_expected),
                  rules=FALSE,
                  trials=boost)
  
  # ::: is used to directly access a member of a package that is internal
  suppressWarnings(graphtree<-C50:::as.party.C5.0(tree))
  
  # The plot is large - so print to a big PDF file
  pdf(PDF_FILENAME, width=100, height=50, paper="special", onefile=F)
  
  # The number is the node level of the tree to print
  plot(graphtree[NODE_LEVEL])
  
  #This closes the PDF file
  dev.off()
  
  # Remove the global level objects
  rm(Global_train_inputs,pos=1)
  rm(Global_train_expected,pos=1)
}
# ************************************************
# getTreeClassifications() :
#
# Put in test dataset and get out class predictions of the decision tree
# Determine the threshold, plot the results and calculate metrics
#
# INPUT   :   object         - myTree       - tree
#         :   Data Frame     - testDataset  - dataset to evaluate
#         :   string         - title        - string to plot as the chart title
#         :   int            - classLabel   - lable given to the positive (TRUE) class (For our dataset 1 implies customer is risky so credit card not approved)
#         :   boolean        - plot         - TRUE to output results/charts
#
# OUTPUT  :   List       - Named evaluation measures
#
# ************************************************
getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (non_risky customers) and column 2 is for class 1 (risky customers)
  
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying the risky customers
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-determineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  if (plot==TRUE)
    printMeasures(results=measures,title=title)
  print(measures)
  
  return(measures)
} #endof getTreeClassifications()
# ************************************************
# EvaluateClassifier() :
#
# Use dataset to generate predictions from model
# Evaluate as classifier using threshold value
#
# INPUT   :   vector double     - probs        - probability of being class 1
#             Data Frame        - testing_data - Dataset to evaluate
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
EvaluateClassifier<-function(test_predicted,test_expected,threshold) {
  
  predictedClass<-ifelse(test_predicted<threshold,0,1)
  
  results<-calcConfusion(expectedClass=test_expected,
                          predictedClass=predictedClass)
  
  return(results)
} #endof EvaluateClassifier()

#********************************************************
# main() :
#
# Entry point to execute your data analytics
#
# INPUT:  None
#
# OUTPUT :None
# ************************************************
main<-function(){
  # Loading the data
  CCA<-readDataset(DATASET_FILENAME)
  print(names(CCA))
  print(summary(CCA))
  PREPROCESSING_prettyDataset(CCA)
  
  # Determine the field types
  field_types<-PREPROCESSING_initialFieldType(CCA)
  
  #Viewing numeric field types
  numeric_fields<-names(CCA)[field_types=="NUMERIC"]
  print(paste("NUMERIC FIELDS=",length(numeric_fields)))
  print(numeric_fields)
  
  #Viewing symbolic fields
  symbolic_fields<-names(CCA)[field_types=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)
  # ************************************************
  # Modifying columns and datatypes
  creditData<-PREPROCESSING_modifyColumns(CCA)
  print(head(creditData))
  
# *******************************************************
  # Check the total number of risk users
  print("Total number of risk users:")
  print(nrow(creditData[creditData$TARGET==1,]))
  print("Total number of non-risk users:")
  print(nrow(creditData[creditData$TARGET==0,]))
  # Number of risk customers= 1962 and non-risk customers is 535705
  # Dataset is highly biased 
  # In order to address this problem, first we will separate the positive and negative ones
  # Then we will take all the negative ones and a sample positive ones.
  

  # Reduce Dataset  
  CCA<-reduceData(creditData)
  print(dim(CCA))
  print(head(CCA))
#*******************************************************************************
  # first we plot the features which are continuous to see if there is any outlier or not:
  par(mfrow=c(1,1))
  plot(CCA$ID,CCA$AMT_INCOME_TOTAL,col='orange')
  plot(CCA$ID,CCA$AGE)
  plot(CCA$ID,CCA$BEGIN_MONTHS)
  plot(CCA$ID,CCA$MONTH_EMPLOYED, col='orange')
  # From the plots we can see that there are some outliers in the "Income" and "Month Employed" columns.
  # So we will remove the data which are bigger than 98% of the whole data.
  
  q_high1   <-  quantile(CCA$AMT_INCOME_TOTAL,0.98)
  q_high2   <-  quantile(CCA$MONTH_EMPLOYED,0.98)
  print(dim(CCA[which(CCA$AMT_INCOME_TOTAL < q_high1),]))
  
  CCA       <-  CCA[which(CCA$AMT_INCOME_TOTAL < q_high1),]
  print(dim(CCA))
  
  CCA       <-  CCA[which(CCA$MONTH_EMPLOYED < q_high2),]
  print(dim(CCA))
  
  par(mfrow=c(1,1))
  plot(CCA$ID,CCA$AMT_INCOME_TOTAL,col='light blue')
  plot(CCA$ID,CCA$AGE)
  plot(CCA$ID,CCA$BEGIN_MONTHS)
  plot(CCA$ID,CCA$MONTH_EMPLOYED, col='light blue')
  
  
  # As we can see by plots, the outliers were removed from our dataset.
  
  
  
  # Individual Variable's effect on Approval
  #*******************************************************************************
  par(mfrow=c(1,1))
  counts_child <- table(CCA$TARGET,CCA$CNT_CHILDREN)
  barplot(counts_child, col=c("dark blue","orange"),
          legend = c("Approved", "Failed"))
  
  
  counts_gender <- table(CCA$TARGET,CCA$CODE_GENDER)
  barplot(counts_gender, col=c("dark blue","orange"))
  
  
  counts_edu <- table(CCA$TARGET,CCA$NAME_EDUCATION_TYPE)
  barplot(counts_edu, col=c("dark blue","orange"))
  
  counts_house <- table(CCA$TARGET,CCA$NAME_HOUSING_TYPE)
  barplot(counts_house, col=c("dark blue","orange"),
          legend = c("Approved", "Failed"))
  
# ******************************************************************************
#
# Feature Engineering
# ******************************************************************************
  
  # In this part we will find the relations in our dataset
  
  # The introduce() function will give basic information about the data frame, 
  # including the number of missing values in each variable. Fill the empty values with median
  
  print(introduce(CCA))
  
  # We will do multi-variate analysis of our variables and draw a correlation heat map from DataExplorer library
  plot_correlation(na.omit(CCA[-c(1,17)]), maxcat = 5L)
  
  
  # let's have a univariate analysis of our variables
  print(plot_histogram(CCA[-c(1,17)]))
  
  # we will use the pairs function to plot the relations between each column of the dataset
  print(pairs(CCA[-c(1,17)], col = CCA$TARGET))
  
  
  # We can observe the week correlation of "BEGIN_MONTHS","JOB", "FLAG_EMAIL", "FLAG_WORK_PHONE",
  # "DAYS_BIRTH", "NAME_HOUSING_TYPE","FLAG_OWN_CAR", "AMT_INCOME_TOTAL", "NAME_EDUCATION_TYPE","STATUS" 
  # with our target variable.
  # As we know, the "TARGET" column in the dataset had been added later to the orginal dataset. So, in the Orginal
  # Dataset the "STATUS" column used to be the prediction result. Because of that the STATUS column and the TARGET
  # columns are nearly the same. Therefore we will remove the Status Column.
  
  # We will delete the variables of extremely weak correlation based on analyzing our data above,
  # We will start with factor fields first
  
  CCA <- select(CCA, -one_of("ID","STATUS" , "JOB", "FLAG_EMAIL", "FLAG_WORK_PHONE","FLAG_MOBIL",
                             "NAME_HOUSING_TYPE"))
  head(CCA)
  
  
  
# ******************************************************************************
  # Visualising the dataset:
  # ******************************************************************************
  # AGE 
  ggplot(CCA, aes(AGE))+
    geom_histogram(binwidth = 1,color='dark green',fill='light green')
  
  # Begin Month
  ggplot(CCA, aes(BEGIN_MONTHS))+
    geom_histogram(binwidth = 1,color='dark blue',fill='light blue')
  
  # Total income of the applicants
  ggplot(CCA, aes(AMT_INCOME_TOTAL))+
    geom_histogram(binwidth = 20000,color='purple',fill='yellow')
  
  
  # Month Employed
  ggplot(CCA, aes(MONTH_EMPLOYED))+
    geom_histogram(binwidth = 10,color='dark blue',fill='dark red')
  
  
  #Lets plot a histogram for all the columns:
  
  par(mfrow=c(2,3))
  for (i in 1:12) {
    hist(CCA[,i],main=names(CCA)[i])
  }
  
#*******************************************
# Modelling
#*******************************************

# Decision Trees
# Code for decision tree is taken from lab 4
  # We use STRATIFIED K-FOLD
  dataset_trees<-stratifiedDataset(CCA)
  
  # Experiment with C5 decision tree
  
  measures<-runExperiment(dataset = dataset_trees,FUN = fullDT)
  
  # Test if the results are empty or not
  allResults<-data.frame(DT_Basic=unlist(measures))
  print(formattable::formattable(allResults))
  
  # ************************************************
  # Creates a boosted tree
  # The algorithm allows for "boosting"
  # Many trees are built using all the input fields and these then "vote"

  measures<-runExperiment(dataset = dataset_trees,FUN = fullDT,boost=BOOST)
  tree_results<-data.frame(DT_boost=unlist(measures))
  if (is.null(allResults)) {
    allResults<-tree_results
  }
  else {
    allResults<-cbind(allResults,tree_results)
  }
  print(formattable::formattable(tree_results))
  # Keep a note of all our results - append to the all results
  print(formattable::formattable(allResults))
  # ************************************************
  # Try a random forest model
  
  measures<-runExperiment(dataset = dataset_trees,FUN = randomForest)
  tree_results<-data.frame(RandomForest=unlist(measures))
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,tree_results)
  print(formattable::formattable(tree_results))
  allResults<-data.frame(t(allResults))
  
  # Sort by highest MCC
  allResults<-allResults[order(allResults$MCC,decreasing = TRUE),]
  
  # Output results to compare all classifiers
  allResults[,1:4]<-sapply(allResults[,1:4], as.integer)
  allResults$folds<-KFOLDS
  print(formattable::formattable(allResults))
  
  # Write frame to a CSV files
  write.csv(allResults,file=RESULTS_FILENAME)

# ******************************************************************************
# Data Pre-processing
# ******************************************************************************
  
  # For logistic regression and NN we'll split the data into training and testing. 
  # Take 75% random sample of approved and non-approved each into training data, 
  # 25% into test data, and then split training data into train and validation sets
  

  
  Train_Data <- create_train_test(CCA,0.75,TRUE)
  Test_Data  <- create_train_test(CCA,0.75,FALSE)
  
  
  table(Train_Data$TARGET)
  
  prop.table(table(CCA$TARGET))
  
  print(str(Train_Data))
  # ******************************************************************************
  #
  # Logistic Regression
  # ******************************************************************************
  
  Logistic_Model <- glm(TARGET~.,Train_Data,family=binomial())
  print(summary(Logistic_Model))
  
  par(mfrow=c(2,2))
  plot(Logistic_Model)
  
  
  # To predict using logistic regression model, probabilities obtained
  predicted.data <- predict(Logistic_Model, Test_Data, type="response")
  
  # Look at probability output
  head(predicted.data, 10)
  
  
  log.prediction.rd <- ifelse(predicted.data < 0.27 , 0,1)
  head(log.prediction.rd, 10)
  
  lr.predict <- predict(Logistic_Model,Test_Data, probability = TRUE)
  par(mfrow=c(1,1))
  auc.gbm <- roc(Test_Data$TARGET, lr.predict, plot = TRUE, col = "blue")
  
  
  # now we can plot the data of logistic2
  predicted.data <- data.frame(probability.of.TARGET=Logistic_Model$fitted.values,
                               TARGET=Train_Data$TARGET)
  
  predicted.data <- predicted.data[
    order(predicted.data$probability.of.TARGET, decreasing=FALSE),]
  predicted.data$rank <- 1:nrow(predicted.data)
  
  # Lastly, we can plot the predicted probabilities
  print(ggplot(data=predicted.data, aes(x=rank, y=probability.of.TARGET)) +
    geom_point(aes(color=TARGET), alpha=1, shape=4, stroke=2) +
    xlab("Index") +
    ylab("Predicted probability of getting a credit card"))
  
  
  conf_lg <- table(log.prediction.rd, Test_Data[,12])
  print(conf_lg)
  
  lg_accuracy <- sum(diag(conf_lg)) / sum(conf_lg)
  print(paste('The Accuracy of the Logistic Regression Model is', round(lg_accuracy*100,2),'%'))
  
  # ******************************************************************************
  #
  # Neural Networks
  # ******************************************************************************
  
  
  # Here we are going to split the Features and targets in Train and test data.
  # In the train data we will split the Features and targets, respectively, in X_train and y_train
  # and again in the Test data we will split the Features and targets, respectively, in X_test and
  # y_Test. Also, we convert them to the arrays as it is required for working with Keras
  
  set.seed(1234)
  X_train <- Train_Data %>% select(-TARGET) %>% scale()
  y_train <- to_categorical(Train_Data$TARGET , num_classes = NULL) 
  set.seed(1234)
  X_test <- Test_Data %>% select(-TARGET) %>% scale()
  y_test <- to_categorical(Test_Data$TARGET , num_classes = NULL)
  
  
  # Build the model with 2 hidden layers, of 256 and 128 units each with "Relu" activation.
  
  model <- keras_model_sequential() 
  
  model %>% 
    layer_dense(units = 256, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 2, activation = 'sigmoid')
  
  
  history <- model %>% compile(loss = 'binary_crossentropy', optimizer = 'adam',
                               metrics = c('accuracy'))
  set.seed(1234)
  model %>% fit(X_train, y_train, epochs = 20, batch_size = 5, 
                validation_split = 0.2)
  
  
  # Display the model structure.
  print(summary(model))
  
  
  predictions <- model %>% predict_classes(X_test)
  
  conf_mat <- table(factor(predictions,levels=min(Test_Data$TARGET):max(Test_Data$TARGET)),
                    factor(Test_Data$TARGET,levels=min(Test_Data$TARGET):max(Test_Data$TARGET)))
  
  print(conf_mat)
  
  nn_accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
  print(paste('The Accuracy of the Neural networks Model is', round(nn_accuracy*100,2),'%'))
  
  
  
  
  
  accuracy_all <- c(lg_accuracy,nn_accuracy)
  
  Acc_table = data.frame('Logistic Regression'=round(lg_accuracy*100,2),
                         'Neural Networks'=round(nn_accuracy*100,2),row.names = 'Accuracy')
  print(Acc_table)
  
  
 

}# ************************************************
# This is where R starts execution

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")



library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files 
source("Credit_Card_Approval_functions.R")

set.seed(123)
options(warn = -1)
# ************************************************
main()

