# **********************************************************************************************
# Create variables and Save libraries in it as required and install using library()
# INPUT: Packages
# OUTPUT : Installation of Packages
# **********************************************************************************************

# Installing packages and loading into the system
pkgsKeras <- c("keras", "tidyquant", "rsample", "recipes", "yardstick", "corrr")
install.packages(pkgsKeras)

# Loading libraries
library(keras)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)

# **********************************************************************************************
# Read a CSV file from working directory for Implementing Keras
# INPUT: text - filename
# OUTPUT : Frame - dataset
# **********************************************************************************************

# Importing the dataset
df_churn_keras <- read_csv("Telco-Customer-Churn.csv")

# **********************************************************************************************
# Prune_Function() : Pre-processing
# INPUT: Frame - dataset
# OUTPUT : Pre-processed data frame
# **********************************************************************************************

# Pruning with Prune_Function
Prune_Function <- function(df){
  df <- df %>% 
    select(-customerID) %>% 
    drop_na()
}

# Calling Prune_Function
df_keras <- Prune_Function(df_churn_keras)

# **********************************************************************************************
# Split_Function() : Pre-processing for splitting data in train and test
# INPUT: Frame - dataset
# OUTPUT : Train and Test data frame
# **********************************************************************************************

# Splitting with Split_Function
Split_Function <- function(df,prop){
  set.seed(100)
  split <- initial_split(df, prop)
}

# Calling the Split_Function to create Train and Test Dataset
split <- Split_Function(df_keras,0.7)
train_data <- training(split)
test_data  <- testing(split)

# **********************************************************************************************
# Pre-processing for transforming train data
# INPUT: Frame - train data
# OUTPUT : Trasformed Train Data
# **********************************************************************************************

# Transforming the data
train_data %>%
  select(Churn, TotalCharges) %>%
  mutate(
    Churn = Churn %>% as.factor() %>% as.numeric(),
    LogTotalCharges = log(TotalCharges)
  ) %>%
  correlate() %>%
  focus(Churn) %>%
  fashion()

# **********************************************************************************************
# Creating a recipe using recipe() function
# INPUT: Frame - train data
# OUTPUT : Recipe Object
# **********************************************************************************************

# Creating a recipe
recipe_object <- recipe(Churn ~ ., data = train_data) %>%
  step_discretize(tenure, options = list(cuts = 6)) %>%
  step_log(TotalCharges) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train_data)

# Printing the recipe object
recipe_object

# **********************************************************************************************
# Baking the recipe using bake() function
# INPUT: Recipe Object
# OUTPUT : Baked Train and Test Data and Separated Target vectors
# **********************************************************************************************

# Baking the recipe
train_data_x <- bake(recipe_object, newdata = train_data) %>% select(-Churn)
test_data_x  <- bake(recipe_object, newdata = test_data) %>% select(-Churn)

# Separating the target vector
train_vec <- ifelse(pull(train_data, Churn) == "Yes", 1, 0)
test_vec  <- ifelse(pull(test_data, Churn) == "Yes", 1, 0)

# **********************************************************************************************
# Creating Keras Model
# INPUT: Train Data
# OUTPUT : Keras Model
# **********************************************************************************************

# Initialising the Keras Model
model_keras <- keras_model_sequential()

# Running Keras Model with 2 hidden layers
model_keras %>% 
  
  # First hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu", 
    input_shape        = ncol(train_data_x)) %>% 
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Second hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu") %>% 
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", 
    activation         = "sigmoid") %>% 
  
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )

# Printing the model
model_keras

# Generating the history by using fit() function
history <- fit(
  object           = model_keras, 
  x                = as.matrix(train_data_x), 
  y                = train_vec,
  batch_size       = 50, 
  epochs           = 35,
  validation_split = 0.30
)

# Printing the history
print(history)

# Plotting the model
plot(history)

# **********************************************************************************************
# Making Predictions with the Keras Model using predict_classes(), predict_proba(), tibble(),
#                                               yardstick, conf_mat() and metrics() functions
# INPUT: Keras Model
# OUTPUT : Predictions like Estimates, Recall, Precision, Accuracy, AUC
# **********************************************************************************************

# Creating Predictions Vectors
keras_class <- predict_classes(object = model_keras, x = as.matrix(test_data_x)) %>%
  as.vector()
keras_prob  <- predict_proba(object = model_keras, x = as.matrix(test_data_x)) %>%
  as.vector()

# Creating estimates
keras_estimates <- tibble(
  truth      = as.factor(test_vec) %>% fct_recode(yes = "1", no = "0"),
  estimate   = as.factor(keras_class) %>% fct_recode(yes = "1", no = "0"),
  class_prob = keras_prob
)

# Printing estimates
keras_estimates

# Inspect performance with yardstick
options(yardstick.event_first = FALSE)

# Creating Confusion Matrix
keras_estimates %>% conf_mat(truth, estimate)

# Generating Accuracy which is 80% approximately
keras_estimates %>% metrics(truth, estimate)

# Printing Recall and Precision
tibble(
  precision = keras_estimates %>% precision(truth, estimate),
  recall    = keras_estimates %>% recall(truth, estimate)
)



# **********************************************************************************************
# Create variables and Save libraries in it as required and install using library()
# INPUT: Packages
# OUTPUT : Installation of Packages
# **********************************************************************************************

# Installing packages and loading into the system
pkgsLR <- c("corrplot", "ggplot2", "gridExtra", "ggthemes", "caret", "MASS",
            "party", "miscset", "magrittr", "dplyr", "tidyverse")

library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(party)

# **********************************************************************************************
# Read a CSV file from working directory for implementic Logistic Regression
# INPUT: text - filename
# OUTPUT : Frame - dataset
# **********************************************************************************************

# Importing the dataset
df_reg <- read.csv('Telco-Customer-Churn.csv')

# **********************************************************************************************
# Pre-processing to remove null values
# INPUT: Frame - dataset
# OUTPUT : Pre-processed data frame
# **********************************************************************************************

# Preprocessing
df_reg <- df_reg[complete.cases(df_reg), ]
df_reg$MultipleLines <- ifelse(df_reg$MultipleLines=="Yes","Yes","No")
df_reg[10:15] <- ifelse(df_reg[10:15]=="Yes","Yes","No")

# **********************************************************************************************
# tenure_group() : To sort tenure into 6 groups
# INPUT: tenure variable
# OUTPUT : 6 groups of tenure
# **********************************************************************************************

# Creating a tenure_group function
tenure_group <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-1 Year')
  }else if(tenure > 12 & tenure <= 24){
    return('1-2 Years')
  }else if (tenure > 24 & tenure <= 48){
    return('2-4 Years')
  }else if (tenure > 48 & tenure <=60){
    return('4-5 Years')
  }else if (tenure > 60){
    return('5+ Years')
  }
}

# Calling the fucntion
df_reg$tenure_group <- sapply(df_reg$tenure,tenure_group)
df_reg$tenure_group <- as.factor(df_reg$tenure_group)

# **********************************************************************************************
# Further Pre-processing steps
# INPUT: dataframe
# OUTPUT : dataframe
# **********************************************************************************************

# Converting SeniorCitizen field to binary
df_reg$SeniorCitizen <- ifelse(df_reg$SeniorCitizen==1,"Yes","No")

# Converting SeniorCitizen field to factor
df_reg$SeniorCitizen <- as.factor(df_reg$SeniorCitizen)

# Removing unique identifiers
df_reg$customerID <- NULL
df_reg$tenure <- NULL
df_reg$MonthlyCharges <- NULL

# **********************************************************************************************
# createDataPartition(): Pre-processing for splitting data in train and test
# INPUT: Frame - dataset
# OUTPUT : Train and Test data frame
# **********************************************************************************************

# Partitioning the data
data_partition <- createDataPartition(df_reg$Churn,p=0.8,list=FALSE)
set.seed(100)

# Generating train data
train_data_reg <- df_reg[data_partition,]

# Generating test data
test_data_reg <- df_reg[-data_partition,]

# **********************************************************************************************
# Creating Logistic Regression Model
# INPUT: Train Data
# OUTPUT : Logistic Regression Model
# **********************************************************************************************

# Creating Model
LReg_Model <- glm(Churn ~ .,family=binomial(link="logit"),data=train_data_reg)

# Printing the model
print(summary(LReg_Model))

# Creating Anova
anova(LReg_Model, test="Chisq")

# Generating Log Odds Ratio
exp(cbind(OR=coef(LReg_Model), confint(LReg_Model)))

# **********************************************************************************************
# Making Predictions with the LR Model using predict()
# INPUT: LR Model
# OUTPUT : Predictions like Estimates, Recall, Precision, Accuracy
# **********************************************************************************************

# Transforming the target to character
test_data_reg$Churn <- as.character(test_data_reg$Churn)

# Changing to binary
test_data_reg$Churn[test_data_reg$Churn=="No"] <- "0"
test_data_reg$Churn[test_data_reg$Churn=="Yes"] <- "1"

# Making Predictions
fitted.results <- predict(LReg_Model,newdata=test_data_reg,type='response')

# Formatting to 0s and 1s
fitted.results <- ifelse(fitted.results > 0.5,1,0)

# Accuracy Calculations 80%
misClasificError <- mean(fitted.results != test_data_reg$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# Confusion Matrix
table(test_data_reg$Churn, fitted.results > 0.5)

# **********************************************************************************************
# NcalcConfusion() : Calculate a confusion matrix for 2-class classifier
# INPUT: vector - expected - {0,1}, Expected outcome from each row (labels)
#        vector - predicted - {0,1}, Predicted outcome from each row (labels)
#        vector - actualp - [0,1], predicted real values (scores)
#
# OUTPUT: A list with the following entries:
#        TP - int - True Positive records
#        FP - int - False Positive records
#        TN - int - True Negative records
#        FN - int - False Negative records
#        accuracy - float - accuracy measure
#        pgood - float - precision for "good" (values are 1) measure
#        pbad - float - precision for "bad" (values are 1) measure
#        FPR - float - FPR measure
#        TPR - float - FPR measure
# **********************************************************************************************

NcalcConfusion<-function(expected,predicted){
  
  TP<-length(which((predicted==0) & (expected==0)))
  FN<-length(which((predicted==1) & (expected==0)))
  FP<-length(which((predicted==0) & (expected==1)))
  TN<-length(which((predicted==1) & (expected==1)))
  RMSE<-round(Nrmse(expected,predicted),digits=2)
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=NcalcAccuracy(TP,FP,TN,FN),
                  "pgood"=NcalcPgood(TP,FP,TN,FN),
                  "pbad"=NcalcPbad(TP,FP,TN,FN),
                  "FPR"=NcalcFPR(TP,FP,TN,FN),
                  "TPR"=NcalcTPR(TP,FP,TN,FN),
                  "RMSE"=RMSE
  )
  return(retList)
}

NcalcAccuracy<-function(TP,FP,TN,FN){return(100.0*((TP+TN)/(TP+FP+FN+TN)))}
NcalcPgood<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FP)))}
NcalcPbad<-function(TP,FP,TN,FN){return(100.0*(TN/(FN+TN)))}
NcalcFPR<-function(TP,FP,TN,FN){return(100.0*(FP/(FP+TN)))}
NcalcTPR<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FN)))}

# **********************************************************************************************
# Nrmse() : Calculate the RMSE statistic
# INPUT: actual_y vector of real numbers indicating the known class
#        y_predicted vector of real numbers indicating the predicted class
# OUTPUT : Frame - dataset
# **********************************************************************************************

Nrmse<-function(actual_y,y_predicted){
  
  return(sqrt(mean((actual_y-y_predicted)^2)))
}

# **********************************************************************************************
# Threshold for optimisation
# INPUT: for loop, actual vs predicted values
# OUTPUT : Frame - dataset
# **********************************************************************************************

for(threshold in seq(0,1,0.1))
{
  print(threshold)
  fitted.results <- predict(LReg_Model,newdata=test_data_reg,type='response')
  fitted_results <- ifelse(fitted.results>threshold,1,0)
  measures <- NcalcConfusion(as.numeric(test_data_reg$Churn),fitted_results)
  print(measures)
}

# Selection of Threshold
fitted_results <- ifelse(fitted.results>0.3,1,0)

# Confusion Matrix
measures <- NcalcConfusion(as.numeric(test_data_reg$Churn),fitted_results)
table(as.numeric(test_data_reg$Churn),fitted_results)

# Printing Confusion Matrix
print(measures)

# **********************************************************************************************
# Estimations_Function(): Calculating Profit, Uplift, Nomodel, ROI
# INPUT: for loop, actual vs predicted values
# OUTPUT : Profit, Uplift, Nomodel, ROI
# **********************************************************************************************

# Transforming total charges field
df_charges <- read.csv("Telco-Customer-Churn.csv")
df_charges <- df_charges[complete.cases(df_charges), ]
df_charges <- df_charges$TotalCharges

# Calculating the mean of total charges
charges_avg <- mean(df_charges)
charges_avg <- round(charges_avg)

# Printing total charges
charges_avg

Estimations_Function <- function(TP,FP,TN, FN, charges_avg){
  
  # Generating Net Revenue
  net_revenue <- TN * 0.9 * charges_avg
  net_revenue <- round(net_revenue)
  
  # Printing Net Revenue
  print(paste('Net revenue from subscribers we retain by spending the 10%:',net_revenue))
  
  # Generating Wrongly Enticed
  wrongly_enticed <- -FN * 0.1 * charges_avg
  wrongly_enticed <- round(wrongly_enticed)
  
  # Printing Wrongly Enticed
  print(paste('Wrongly Enticed Subsribers:', wrongly_enticed))
  
  # Generating Lost Revenue
  lost_revenue <- -FP * charges_avg
  lost_revenue <- round(lost_revenue)
  
  # Printing Lost Revenue
  print(paste('Lost revenue due to missed subscribers that we did not entice and so lost:', lost_revenue))
  
  # Generating Replace Cost
  cost_replace <- -FP * 750
  cost_replace <- round(cost_replace)
  
  # Printing Replace Cost
  print(paste('Cost to replace the lost subsribers:', cost_replace))
  
  # Generating Total Uplift
  total_uplift <- net_revenue + wrongly_enticed + lost_revenue + cost_replace
  
  # Printing Total Uplift
  print(paste('Total Uplift is:', total_uplift))
  
  # Generating number who churned
  P <- TN + FP
  
  # Printing Number Who Churned
  print(paste('Number of Subscribers that churned are:', P))
  
  # Generating Nomodel
  nomodel <- P * 750
  
  # Printing Nomodel
  print(paste('Nomodel value:', nomodel))
  
  # Generating Profit
  profit <- nomodel - total_uplift
  profit <- round(profit)
  
  # Printing Profit
  print(paste('The additional ‘profit’ to the business when the model is in place is nomodel-uplift which is:', profit))

  # Generating Retain
  retain <- TN * 0.1 * charges_avg
  retain <- round(retain)
  
  # Printing Retain
  print(paste('Subsribers we retain:', retain))
  
  # Generating Invested amount based on model churn predictors
  investment_predictors <- retain - wrongly_enticed - cost_replace
  investment_predictors <- round(investment_predictors)
  
  # Printing above
  print(paste('The amount invested by business:', investment_predictors))
  
  # Generating Revenue gained
  revenue_gained <- net_revenue + lost_revenue
  revenue_gained <- round(revenue_gained)
  
  # Printing Revenue gained
  print(paste('Total Revenue gained:', revenue_gained))
  
  # Generating ROI
  roi <- (revenue_gained / investment_predictors) * 100
  roi <- round(roi)
  # Printing ROI
  print(paste('ROI:', roi))
}

# Calling the Estimations_Function
finalEstimates <- Estimations_Function(measures$TP,measures$FP,measures$TN,measures$FN, charges_avg)
