## Initialize Libraries and set a seed

library(dplyr)
library(stats)
library(gmodels)
library(data.table)
library(ggplot2)
library(pROC)
library(rpart)
library(randomForest)

RawLoanData <- read.csv("LoanStats3a_securev1.csv", skip = "1", sep = ",", header = TRUE)
## RawLoanData <- fread("LoanStats3a_securev1.csv", skip = "1", sep = ",", header = TRUE, stringsAsFactors = TRUE)
## fread.  import as factors

str(RawLoanData)
summary(RawLoanData)

#Removing extra fields

#Removing columns that are mostly NA
dropcolumnindex <- lapply(RawLoanData, function(x) sum(is.na(x))) >= 42000
RawLoanData <- RawLoanData[ , dropcolumnindex == FALSE]

#Removing text heavy fields
RawLoanData$url <- NA
RawLoanData$desc <- NA
RawLoanData$emp_title <- NA
RawLoanData$title <- NA
RawLoanData$application_type <- NA
RawLoanData$initial_list_status <- NA
RawLoanData$pymnt_plan <- NA
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
RawLoanData$revol_util <- as.numeric(RawLoanData$revol_util)
RawLoanData$int_rate <- as.numeric(RawLoanData$int_rate)

#Creating a credit utilization variable
#RawLoanData$Credit_Usage <- RawLoanData$revol_util/RawLoanData$revol_bal


#Cleaning up empty comment rows and empty income rows
RawLoanData$annual_inc <- as.numeric(as.character(RawLoanData$annual_inc))
na_index2 <- which(is.na(RawLoanData$annual_inc))
RawLoanData <- RawLoanData[-na_index2, ]


#Fixing NA's in records with 0's
RawLoanData$delinq_2yrs  <- as.numeric(gsub(NA, 0, RawLoanData$delinq_2yrs))
RawLoanData$inq_last_6mths  <- as.numeric(gsub(NA, 0, RawLoanData$inq_last_6mths))
RawLoanData$open_acc  <- as.numeric(gsub(NA, 0, RawLoanData$open_acc))
RawLoanData$pub_rec  <- as.numeric(gsub(NA, 0, RawLoanData$pub_rec))
RawLoanData$total_acc  <- as.numeric(gsub(NA, 0, RawLoanData$total_acc))
RawLoanData$delinq_amnt <- as.numeric(gsub(NA, 0, RawLoanData$delinq_amnt))
RawLoanData$acc_now_delinq  <- as.numeric(gsub(NA, 0, RawLoanData$acc_now_delinq))
RawLoanData$mths_since_last_delinq <- as.numeric(gsub(NA, 0, RawLoanData$mths_since_last_delinq))
RawLoanData$mths_since_last_record <- as.numeric(gsub(NA, 0, RawLoanData$mths_since_last_record))

## Cleaning up Loan Status

levels(RawLoanData$loan_status)

# Removing current loans as they are out of analysis range. 
RawLoanData$loan_status[which(RawLoanData$loan_status == "Current")] <- NA
RawLoanData$loan_status[which(RawLoanData$loan_status == "")] <- NA
RawLoanData$loan_status[which(RawLoanData$loan_status == "Late (16-30 days)")] <- NA
RawLoanData$loan_status[which(RawLoanData$loan_status == "Late (31-120 days)")] <- NA
RawLoanData$loan_status[which(RawLoanData$loan_status == "In Grace Period")] <- NA
na_index3 <- which(is.na(RawLoanData$loan_status))
RawLoanData <- RawLoanData[-na_index3, ]

levels(RawLoanData$loan_status)

levels(RawLoanData$home_ownership)
RawLoanData$home_ownership[which(RawLoanData$home_ownership == "NONE")] <- NA
RawLoanData$home_ownership[which(RawLoanData$home_ownership == "")] <- NA
levels(RawLoanData$home_ownership)

na_index4 <- which(is.na(RawLoanData$home_ownership))
RawLoanData <- RawLoanData[-na_index4, ]
RawLoanData$home_ownership <- droplevels(RawLoanData$home_ownership)
levels(RawLoanData$home_ownership)

#Fixing Factors
RawLoanData$term <- droplevels(RawLoanData$term)
RawLoanData$sub_grade <- droplevels(RawLoanData$sub_grade)
RawLoanData$emp_length <- droplevels(RawLoanData$emp_length)
RawLoanData$verification_status <- droplevels(RawLoanData$verification_status)
RawLoanData$purpose <- droplevels(RawLoanData$purpose)

# refactor.  factor function.  Give it levels. 
# Consolidating defaulted loans.  0 will signify paid back loan.  1 will equal default. 

RawLoanData$loan_status <- as.factor(gsub("Does not meet the credit policy. Status:Fully Paid", "0", RawLoanData$loan_status))
RawLoanData$loan_status <- as.factor(gsub("Does not meet the credit policy. Status:Charged Off", "1", RawLoanData$loan_status))
RawLoanData$loan_status <- as.factor(gsub("Fully Paid", "0", RawLoanData$loan_status))
RawLoanData$loan_status <- as.factor(gsub("Charged Off", "1", RawLoanData$loan_status))
RawLoanData$loan_status <- as.factor(gsub("Default", "1", RawLoanData$loan_status))

levels(RawLoanData$loan_status)

dropcolumnindex <- lapply(RawLoanData, function(x) sum(is.na(x))) >= 42000
RawLoanData <- RawLoanData[ , dropcolumnindex == FALSE]


#use an if an else
## Plotting Loan Status. 

CrossTable(x = RawLoanData$loan_status)

CrossTable(x = RawLoanData$sub_grade, y = RawLoanData$loan_status, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

CrossTable(x = RawLoanData$term, y = RawLoanData$loan_status, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

CrossTable(x = RawLoanData$home_ownership, y = RawLoanData$loan_status, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)


hist_1 <- hist(RawLoanData$loan_amnt, breaks = 20, xlab = "Loan amount", main = "Histogram of the loan amount")

hist(RawLoanData$fico_range_high, breaks = 50, xlab = "Fico Score", main = "Histogram of the fico score")

plot(RawLoanData$loan_amnt, log(RawLoanData$annual_inc), xlab = "Loan Ammount", ylab = "Annual Income")

ggplot(data.frame(RawLoanData), aes(x=loan_status)) +
  geom_bar()



## Splitting a training and testing set

set.seed(345)
index_train <- sample(1:nrow(RawLoanData), .7 * nrow(RawLoanData))

# Create training set: training_set
training_set <- RawLoanData[index_train, ]

# Create test set: test_set
test_set <- RawLoanData[-index_train, ]

## Building a logistic model.

#log_model_cat <- glm(loan_status ~ loan_amnt+ term + installment + grade + emp_length + annual_inc + purpose +  dti + fico_range_high + inq_last_6mths + mths_since_last_delinq + open_acc + pub_rec + revol_bal + revol_util + total_acc + last_fico_range_high, family= "binomial", data = training_set)

# Fit the logit, probit and cloglog-link logistic regression models
# New Factor.  Had a deliquency

## corrplot


log_model_logit <- glm(loan_status ~ loan_amnt + term + int_rate + installment + sub_grade + home_ownership + annual_inc + purpose +  verification_status + dti + fico_range_high + pub_rec_bankruptcies , family = binomial(link = logit) , data = training_set)
log_model_logit2 <- glm(loan_status ~ term + sub_grade + home_ownership + annual_inc + purpose + dti + fico_range_high + pub_rec_bankruptcies , family = binomial(link = logit) , data = training_set)

#old_log_model_logit2 <- glm(loan_status ~ term + sub_grade + annual_inc + purpose + dti + pub_rec_bankruptcies , family = binomial(link = logit) , data = training_set)

log_model_all <- glm(loan_status ~ ., family = binomial(link = probit), data = training_set)



summary(log_model_logit)
summary(log_model_logit2)
summary(log_model_all)

## Not sure why error happens here 
# log_model_probit <- glm(loan_status ~ loan_amnt+ term + installment + grade + emp_length + annual_inc + purpose +  dti + fico_range_high ,family = binomial(link = probit), data = training_set)
# log_model_cloglog <- glm(loan_status ~ loan_amnt+ term + installment + grade + emp_length + annual_inc + purpose +  dti + fico_range_high ,family = binomial(link = cloglog), data = training_set)

# Make predictions for all models using the test set
predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")
predictions_logit2 <- predict(log_model_logit2, newdata = test_set, type = "response")
predictions_logitall <- predict(log_model_all, newdata = test_set, type = "response")
predictions_probit <- predict(log_model_probit, newdata = test_set, type = "response")
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response")

# Assesing model with ROC Curve
ROC_Logit <- roc(test_set$loan_status, predictions_logit)
ROC_Logit2 <- roc(test_set$loan_status, predictions_logit2)
ROC_LogitAll <- roc(test_set$loan_status, predictions_logitall)

plot(ROC_Logit)
auc(ROC_Logit)
plot(ROC_Logit2)
auc(ROC_Logit2)
plot(ROC_LogitAll)
auc(ROC_LogitAll)



# Use a cut-off of 30% to make binary predictions-vectors
cutoff <- 0.27
class_pred_logit <- ifelse(predictions_logit > cutoff, 1, 0)
class_pred_logit2 <- ifelse(predictions_logit2 > cutoff, 1, 0)
class_pred_probit <- ifelse(predictions_probit > cutoff, 1, 0)
class_pred_cloglog <- ifelse(predictions_cloglog > cutoff, 1, 0)

# Make a confusion matrix for the three models
tab_class_logit <- table(test_set$loan_status,class_pred_logit)
tab_class_logit2 <- table(test_set$loan_status,class_pred_logit2)
tab_class_probit <- table(true_val,class_pred_probit)
tab_class_cloglog <- table(true_val,class_pred_cloglog)

# Compute the classification accuracy for all three models
acc_logit <- sum(diag(tab_class_logit)) / nrow(test_set)
acc_logit2 <- sum(diag(tab_class_logit2)) / nrow(test_set)
acc_probit <- sum(diag(tab_class_probit)) / nrow(test_set)
acc_cloglog <- sum(diag(tab_class_cloglog)) / nrow(test_set)

#print reults
acc_logit
acc_logit2
acc_probit
acc_cloglog

tab_class_logit
tab_class_logit2

## Prediction Trees

# Creating the first base classification tree. 
tree1 <- rpart(loan_status ~ term + sub_grade + annual_inc + purpose + dti, method = "class", data =  training_set, control = rpart.control(cp = 0.001))

# Plot the decision tree
plot(tree1, uniform = TRUE)
text(tree1)

#Changing Weights
tree_prior <- rpart(loan_status ~ ., method = "class",data = training_set, parms = list(prior=c(0.7, 0.3)), control = rpart.control(cp = 0.001))

# Plot the decision tree
plot(tree_prior, uniform = TRUE)
text(tree_prior)

# Loss Matrix Tree
tree_loss_matrix <- rpart(loan_status ~ ., method = "class", data =  training_set, parms = list(loss = matrix(c(0, 10, 1, 0), ncol=2)), control = rpart.control(cp = 0.001))
plot(tree_loss_matrix, uniform = TRUE)
text(tree_loss_matrix)

plotcp(tree1)
printcp(tree1)


## Random Forests

fit <- randomForest(as.factor(loan_status) ~ term + sub_grade + annual_inc + purpose + dti + pub_rec,
                    data=training_set, 
                    importance=TRUE, 
                    ntree=2000)





