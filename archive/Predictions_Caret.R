## Initialize Libraries and set a seed

library(dplyr)
library(stats)
library(gmodels)
library(caret)
library(data.table)
library(ggplot2)
library(DMwR)


### Cleaning up raw data!

RawLoanData <- read.csv("LoanStats3a_securev1.csv", skip = "1", sep = ",", header = TRUE)

## Removing extra fields and NA errors

#Removing columns that are mostly NA's.  Lending club has too many columns that are mostly empty.  They are more useful in later loan vintages.
dropcolumnindex <- lapply(RawLoanData, function(x) sum(is.na(x))) >= 42300
RawLoanData <- RawLoanData[ , dropcolumnindex == FALSE]


#Fixing NA's in records with 0's
RawLoanData$int_rate <- as.numeric(gsub("\\%", "", RawLoanData$int_rate))*.01
RawLoanData$revol_util <- as.numeric(gsub("\\%", "", RawLoanData$revol_util))*.01
RawLoanData$delinq_2yrs[is.na(RawLoanData$delinq_2yrs)] <- 0
RawLoanData$inq_last_6mths[is.na(RawLoanData$inq_last_6mths)] <- 0
RawLoanData$open_acc[is.na(RawLoanData$open_acc)] <- 0
RawLoanData$pub_rec[is.na(RawLoanData$pub_rec)] <- 0
RawLoanData$total_acc[is.na(RawLoanData$total_acc)] <- 0
RawLoanData$acc_now_delinq[is.na(RawLoanData$acc_now_delinq)] <- 0
RawLoanData$mths_since_last_delinq[is.na(RawLoanData$mths_since_last_delinq)] <- 0
RawLoanData$open_acc[is.na(RawLoanData$open_acc)] <- 0
RawLoanData$mths_since_last_record[is.na(RawLoanData$mths_since_last_record)] <- 0
RawLoanData$revol_util[is.na(RawLoanData$revol_util)] <- 0
RawLoanData$pub_rec_bankruptcies[is.na(RawLoanData$pub_rec_bankruptcies)] <- 0

## Adding new features.  

# Percentage of monthly pay used to pay loan
RawLoanData$dti <- RawLoanData$dti * 0.01
RawLoanData$dti_lc <- RawLoanData$installment / (RawLoanData$annual_inc/12)


## Removing Outcome Fields
RawLoanData$recoveries <- NA
RawLoanData$collection_recovery_fee <- NA
RawLoanData$total_pymnt <- NA
RawLoanData$total_rec_prncp <- NA
RawLoanData$total_rec_int <- NA
RawLoanData$total_rec_late_fee<- NA
RawLoanData$last_pymnt_amnt <- NA
RawLoanData$last_fico_range_high <- NA
RawLoanData$pymnt_plan <- NA
RawLoanData$acc_now_delinq <- NA
RawLoanData$delinq_amnt <- NA


#Removing text heavy fields
RawLoanData$url <- NA
RawLoanData$desc <- NA
RawLoanData$emp_title <- NA
RawLoanData$title <- NA
RawLoanData$application_type <- NA
RawLoanData$initial_list_status <- NA
RawLoanData$grade <- NA

# Need to figure out how to use the dates more efficiently.  Perhaps as time after loan?
RawLoanData$last_pymnt_d <- NA
RawLoanData$last_credit_pull_d <- NA
RawLoanData$earliest_cr_line <- NA
RawLoanData$zip_code <- NA
RawLoanData$issue_d <- NA
RawLoanData$addr_state <- NA
RawLoanData$next_pymnt_d <- NA
RawLoanData$id <- NA
RawLoanData$member_id <- NA
RawLoanData$funded_amnt_inv <- NA
RawLoanData$funded_amnt <- NA
RawLoanData$policy_code <- NA
RawLoanData$fico_range_low <- NA
RawLoanData$last_fico_range_low <- NA
RawLoanData$out_prncp <-NA
RawLoanData$out_prncp_inv <- NA
RawLoanData$total_pymnt_inv <- NA
RawLoanData$chargeoff_within_12_mths <- NA
RawLoanData$collections_12_mths_ex_med <- NA
RawLoanData$tax_liens <- NA


## Cleaning up factored fields

#Fixing Home ownership fields
levels(RawLoanData$home_ownership)
RawLoanData$home_ownership[which(RawLoanData$home_ownership == "NONE")] <- NA
RawLoanData$home_ownership[which(RawLoanData$home_ownership == "")] <- NA
levels(RawLoanData$home_ownership)

na_index4 <- which(is.na(RawLoanData$home_ownership))
RawLoanData <- RawLoanData[-na_index4, ]
RawLoanData$home_ownership <- droplevels(RawLoanData$home_ownership)
levels(RawLoanData$home_ownership)


# Removing current loans as they are out of analysis range. 

RawLoanData$loan_status[which(RawLoanData$loan_status == "Current")] <- NA
RawLoanData$loan_status[which(RawLoanData$loan_status == "")] <- NA
RawLoanData$loan_status[which(RawLoanData$loan_status == "Late (16-30 days)")] <- NA
RawLoanData$loan_status[which(RawLoanData$loan_status == "Late (31-120 days)")] <- NA
RawLoanData$loan_status[which(RawLoanData$loan_status == "In Grace Period")] <- NA
na_index3 <- which(is.na(RawLoanData$loan_status))
RawLoanData <- RawLoanData[-na_index3, ]

levels(RawLoanData$loan_status)

# Consolidating defaulted loans.  0 will signify paid back loan.  1 will equal default. 

RawLoanData$loan_status <- as.factor(gsub("Does not meet the credit policy. Status:Fully Paid", "0", RawLoanData$loan_status))
RawLoanData$loan_status <- as.factor(gsub("Does not meet the credit policy. Status:Charged Off", "1", RawLoanData$loan_status))
RawLoanData$loan_status <- as.factor(gsub("Fully Paid", "0", RawLoanData$loan_status))
RawLoanData$loan_status <- as.factor(gsub("Charged Off", "1", RawLoanData$loan_status))
RawLoanData$loan_status <- as.factor(gsub("Default", "1", RawLoanData$loan_status))

levels(RawLoanData$loan_status)



#Dopping final empty variable columns

RawLoanData$loan_amnt <- NA
RawLoanData$annual_inc <- NA

dropcolumnindex <- lapply(RawLoanData, function(x) sum(is.na(x))) >= 42300
RawLoanData <- RawLoanData[ , dropcolumnindex == FALSE]

#Fixing Factors
RawLoanData$term <- droplevels(RawLoanData$term)
RawLoanData$sub_grade <- droplevels(RawLoanData$sub_grade)
RawLoanData$emp_length <- droplevels(RawLoanData$emp_length)
RawLoanData$verification_status <- droplevels(RawLoanData$verification_status)
RawLoanData$purpose <- droplevels(RawLoanData$purpose)

RawLoanData$term <- as.factor(gsub("\\months", "", RawLoanData$term))
RawLoanData$loan_status <- ifelse(RawLoanData$loan_status == 1, "Y", "N")


# Making Predictions using the data sets


# Rebalancing data set using SMOTE.  Undersampling method.

RawLoanData$loan_status <- as.factor(RawLoanData$loan_status)
RawLoanData <- SMOTE(loan_status ~ ., RawLoanData, perc.over = 100, perc.under=200)


# Caret Predictive Models

set.seed(345)
index_train <- sample(1:nrow(RawLoanData), .7 * nrow(RawLoanData))
training_set <- RawLoanData[index_train, ]
test_set <- RawLoanData[-index_train, ]


myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)



# glm model --- WORKS

model_glm <- train(loan_status ~ ., 
                      test_set,
                      metric = "ROC",
                      method = "glm",
                      preProcess = c("zv", "center", "scale"),
                      family = "binomial",
                      trControl = myControl)

model_glm
max(model_glm[["results"]][["ROC"]])



# glmnet model  ---- WORKS

model_glmnet <- train(loan_status ~ ., 
                      test_set,
                      metric = "ROC",
                      method = "glmnet",
                      preProcess = (c("zv", "center", "scale")),
                      trControl = myControl)

model_glmnet
max(model_glmnet[["results"]][["ROC"]])

# logit model --- works, not well

model_logit <- train(loan_status ~ ., 
                     test_set,
                     metric = "ROC",
                     method = "LogitBoost",
                     preProcess = (c("zv", "center", "scale")),
                     trControl = myControl)

model_logit
max(model_logit[["results"]][["ROC"]])

# gbm model --- WORKS


grid.gbm <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))


model_gbm <- train(loan_status ~ ., 
                   test_set,
                   metric = "ROC",
                   method = "gbm",
                   preProcess = c("zv", "center", "scale"), 
                   trControl = myControl,
                   tuneGrid = grid.gbm)

model_gbm
max(model_gbm[["results"]][["ROC"]])





model_gbm2 <- train(loan_status ~ ., 
                   test_set,
                   metric = "ROC",
                   method = "gbm",
                   preProcess = c("zv", "center", "scale"), 
                   trControl = myControl,
                   tunelength = 10)

model_gbm2
max(model_gbm2[["results"]][["ROC"]])




## ctree model --- WORKS

model_ctree <- train(
  loan_status ~ ., 
  test_set,
  metric = "ROC",
  method = "ctree",
  preProcess = (c("nzv", "center", "scale")),
  trControl = myControl
)

model_ctree
max(model_ctree[["results"]][["ROC"]])


# Fit rpart: rpart ---- WORKS
model_rpart <- train(
  loan_status ~ ., 
  test_set,
  metric = "ROC",
  method = "rpart",
  preProcess = (c("zv", "center", "scale")),
  tuneGrid = expand.grid(cp=0.0001),
  trControl = myControl
)

model_rpart
max(model_rpart[["results"]][["ROC"]])



# Fit random forest: model_rf  ----  WORKS

rf.grid <- expand.grid(mtry = c(1:10))


model_rf <- train( loan_status ~ .,
  test_set,
  method = "rf",
  preProcess = (c("zv", "center", "scale")),
  tuneGrid = rf.grid,
  metric = "ROC",
  ntree = 100, 
  trControl = myControl
  )

model_rf
max(model_rf[["results"]][["ROC"]])

# Model Evaluations

model_list <- list(glm = model_glm, glmnet = model_glmnet, gbm = model_gbm, logit = model_logit, ctree = model_ctree, rpart = model_rpart, rf = model_rf)

resamples <- resamples(model_list)
summary(resamples)

bwplot(resamples, metric = "ROC")
dotplot(resamples, metric = "ROC")

# Importance Variables

#Checking variable importance for GLM
varImp(object=model_glm)
plot(varImp(object=model_glm),main="GLM - Variable Importance")


#Checking variable importance for RF
varImp(object=model_rf)
plot(varImp(object=model_rf),main="RF - Variable Importance")


#Testing Predictions

predictions <- predict.train(object=model_gbm,test_set,type="raw")
table(predictions)
confusionMatrix(predictions,test_set$loan_status)

predictions_rf <- predict.train(object=model_rf,test_set,type="raw")
table(predictions_rf)
confusionMatrix(predictions_rf,test_set$loan_status)

