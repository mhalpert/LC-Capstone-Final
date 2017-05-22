## Initialize Libraries and set a seed

library(dplyr)
library(stats)
library(gmodels)
library(data.table)
library(ggplot2)
library(pROC)
library(rpart)
library(randomForest)

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
#RawLoanData$loan_amnt <- NA
#RawLoanData$annual_inc <- NA



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



#Cleaning up empty comment rows and empty income rows
#RawLoanData$annual_inc <- as.numeric(as.character(RawLoanData$annual_inc))
#na_index2 <- which(is.na(RawLoanData$annual_inc))
#RawLoanData <- RawLoanData[-na_index2, ]


## Cleaning up factored fields

#Fixing Factors
RawLoanData$term <- droplevels(RawLoanData$term)
RawLoanData$sub_grade <- droplevels(RawLoanData$sub_grade)
RawLoanData$emp_length <- droplevels(RawLoanData$emp_length)
RawLoanData$verification_status <- droplevels(RawLoanData$verification_status)
RawLoanData$purpose <- droplevels(RawLoanData$purpose)

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
dropcolumnindex <- lapply(RawLoanData, function(x) sum(is.na(x))) >= 42300
RawLoanData <- RawLoanData[ , dropcolumnindex == FALSE]

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

hist(RawLoanData$revol_bal, breaks = 5, xlab = "Balance", main = "Histogram of the balances")




### Building Predictive Models

## Splitting a training and testing set

set.seed(345)
index_train <- sample(1:nrow(RawLoanData), .7 * nrow(RawLoanData))

# Create training set: training_set
training_set <- RawLoanData[index_train, ]

# Create test set: test_set
test_set <- RawLoanData[-index_train, ]

## Building a logistic model.

log_model_all <- glm(loan_status ~ ., family = binomial, data = training_set)
summary(log_model_all)
predictions_all <- predict(log_model_all, newdata = test_set, type = "response")
ROC_all <- roc(training_set$loan_status, predictions_all)
plot(ROC_all)
auc(ROC_all)




# Log Model 1.  Removing unneccesary variables. 

# All = log_model_logit <- glm(loan_status ~  loan_amnt + term + int_rate + installment + sub_grade + emp_length + home_ownership + annual_inc + verification_status + purpose +  dti + fico_range_high + mths_since_last_delinq + mths_since_last_record + total_acc + dti_lc + too_many_accts + had_heavy_inq + had_delinq + had_pub_rec + had_pub_rec_bankruptcies + had_large_bal + had_high_util, family = binomial(link = logit) , data = training_set)

#logit model
#log_model_logit <- glm(loan_status ~  term + installment + sub_grade + emp_length + home_ownership + purpose + fico_range_high + dti_lc + had_heavy_inq + had_pub_rec + had_large_bal + had_high_util, family = binomial(link = logit) , data = training_set)
# number 2 - log_model_logit <- glm(loan_status ~  loan_amnt + term + int_rate + installment + sub_grade + emp_length + home_ownership + annual_inc + verification_status + purpose + dti + delinq_2yrs + fico_range_high + inq_last_6mths + mths_since_last_delinq + mths_since_last_record + open_acc + pub_rec + revol_bal + revol_util + total_acc + pub_rec_bankruptcies + dti_lc, family = binomial(link = logit) , data = training_set)
log_model_logit <- glm(loan_status ~  term + installment + sub_grade + emp_length + home_ownership + purpose + fico_range_high + inq_last_6mths + mths_since_last_record + revol_bal + revol_util + dti_lc, family = binomial(link = logit) , data = training_set)
summary(log_model_logit)
predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")
ROC_Logit <- roc(test_set$loan_status, predictions_logit)
plot(ROC_Logit)
auc(ROC_Logit)

#probit model
#log_model_probit <- glm(loan_status ~  term + installment + sub_grade + emp_length + home_ownership + purpose +  dti + fico_range_high + dti_lc + had_heavy_inq + had_pub_rec + had_large_bal + had_high_util, family = binomial(link = probit) , data = training_set)
log_model_probit <- glm(loan_status ~   term + installment + sub_grade + emp_length + home_ownership + purpose + fico_range_high + inq_last_6mths + mths_since_last_record + revol_bal + revol_util + dti_lc, family = binomial(link = probit) , data = training_set)
summary(log_model_probit)
predictions_probit <- predict(log_model_probit, newdata = test_set, type = "response")
ROC_probit <- roc(test_set$loan_status, predictions_probit)
plot(ROC_probit)
auc(ROC_probit)

#log_model_cloglog <- glm(loan_status ~  term + installment + sub_grade + emp_length + home_ownership + purpose +  dti + fico_range_high + dti_lc + had_heavy_inq + had_pub_rec + had_large_bal + had_high_util, family = binomial(link = cloglog) , data = training_set)
log_model_cloglog <- glm(loan_status ~   term + installment + sub_grade + emp_length + home_ownership + purpose + fico_range_high + inq_last_6mths + mths_since_last_record + revol_bal + revol_util + dti_lc, family = binomial(link = cloglog) , data = training_set)
summary(log_model_cloglog)
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response")
ROC_cloglog <- roc(test_set$loan_status, predictions_cloglog)
plot(ROC_cloglog)
auc(ROC_cloglog)


plot(ROC_all)
lines(ROC_probit, col = "blue")
lines(ROC_cloglog, col = "red")
lines(ROC_Logit, col = "green")




# Use a cut-off of 20% to make binary predictions-vectors
cutoff <- 0.2
class_pred_all <- ifelse(predictions_all > cutoff, 1, 0)
class_pred_logit <- ifelse(predictions_logit > cutoff, 1, 0)
class_pred_probit <- ifelse(predictions_probit > cutoff, 1, 0)
class_pred_cloglog <- ifelse(predictions_cloglog > cutoff, 1, 0)

# Make a confusion matrix for the three models
tab_class_all <- table(test_set$loan_status,class_pred_all)
tab_class_logit <- table(test_set$loan_status,class_pred_logit)
tab_class_probit <- table(test_set$loan_status,class_pred_probit)
tab_class_cloglog <- table(test_set$loan_status,class_pred_cloglog)

# Compute the classification accuracy for all three models
acc_all <- sum(diag(tab_class_all)) / nrow(test_set)
acc_logit <- sum(diag(tab_class_logit)) / nrow(test_set)
acc_probit <- sum(diag(tab_class_probit)) / nrow(test_set)
acc_cloglog <- sum(diag(tab_class_cloglog)) / nrow(test_set)

#print reults
acc_all
acc_logit
acc_probit
acc_cloglog

tab_class_all
tab_class_logit
tab_class_probit
tab_class_cloglog



## Prediction Trees

# Creating the first base classification tree. FOR SOME REASON THIS ISNT WORKING!
#tree1 <- rpart(loan_status ~ ., method = "class", data =  training_set, minsplit=2, minbucket=1, control = rpart.control(cp = 0.001))
#tree1 <- rpart(loan_status ~ term + int_rate + installment + sub_grade + emp_length + home_ownership +  verification_status + purpose + dti + delinq_2yrs + fico_range_high + inq_last_6mths + mths_since_last_delinq + mths_since_last_record + open_acc + pub_rec + total_acc + pub_rec_bankruptcies + dti_lc, method = "class", data =  training_set, control = rpart.control(cp = 0.001))
#tree1 <- rpart(loan_status ~ term + int_rate + installment + sub_grade + emp_length + home_ownership , method = "class", data =  training_set, control = rpart.control(cp = 0.001))


# Plot the decision tree
plot(tree1)
text(tree1)

tree_1 <- rpart(loan_status ~ ., method = "class",data = training_set, control = rpart.control(minsplit=50, minbucket=5, cp=0.0001))
pred_tree1 <- predict(tree1, newdata = test_set, type = "class")
prp(tree_1)

plot(tree_1, uniform = TRUE)
text(tree_1)

ROC_tree1<- roc(test_set$loan_status, as.numeric(pred_tree1))
plot(ROC_tree1)
auc(ROC_tree1)

# Pruning the tree
plotcp(tree_1)
printcp(tree_1)
cp_index2 <- which.min(tree_1$cptable[, "xerror"])
tree_min2 <- tree_1$cptable[cp_index2, "CP"]
ptree_1 <- prune(tree_1, cp = tree_min2)
plot(ptree_1)
text(ptree_1)




#Changing Weights
tree_prior <- rpart(loan_status ~ ., method = "class",data = training_set, parms = list(prior=c(0.6, 0.4)), control = rpart.control(cp = 0.001))
plot(tree_prior, uniform = TRUE)
text(tree_prior)

# Pruning the tree

pred_prior <- predict(ptree_prior, newdata = test_set, type = "class")
confmat_prior <- table(test_set$loan_status, pred_prior)
acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)
confmat_prior
acc_prior

ROC_ptree_prior<- roc(test_set$loan_status, as.numeric(pred_prior))
plot(ROC_ptree_prior)
auc(ROC_ptree_prior)



# Loss Matrix Tree
tree_loss_matrix <- rpart(loan_status ~ ., method = "class", data =  training_set, parms = list(loss = matrix(c(0, 7.5, 1, 0), ncol=2)), control = rpart.control(cp = 0.001))
plot(tree_loss_matrix, uniform = TRUE)
text(tree_loss_matrix)

# Pruning the tree
plotcp(tree_loss_matrix)
printcp(tree_loss_matrix)
cp_index2 <- which.min(tree_loss_matrix$cptable[, "xerror"])
tree_min2 <- tree_loss_matrix$cptable[cp_index2, "CP"]
ptree_loss_matrix <- prune(tree_loss_matrix, cp = tree_min2)
plot(ptree_loss_matrix)
text(ptree_loss_matrix)

pred_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set,  type = "class")
confmat_ptree_loss_matrix <- table(test_set$loan_status, pred_loss_matrix)
acc_ptree_loss_matrix <- sum(diag(confmat_ptree_loss_matrix)) / nrow(test_set)
confmat_ptree_loss_matrix
acc_ptree_loss_matrix

ROC_ptree_loss_matrix<- roc(test_set$loan_status, as.numeric(pred_loss_matrix))
plot(ROC_ptree_loss_matrix)
auc(ROC_ptree_loss_matrix)



## Random Forests

rf <- randomForest(as.factor(loan_status) ~ fico_range_high + pub_rec + revol_util + inq_last_6mths,
                   type="classification", data=training_set, importance=TRUE, na.action=na.omit)


varImpPlot(rf)
predictions_rf<- predict(rf, newdata = test_set, type = "class")
confmat_rf <- table(test_set$loan_status, predictions_rf)
acc_rf <- sum(diag(confmat_rf)) / nrow(test_set)
acc_rf


ROC_rf <- roc(test_set$loan_status, as.numeric(predictions_rf))
plot(ROC_rf)
auc(ROC_rf)




forest_all <- randomForest(as.factor(loan_status) ~ ., data = training_set, importance=TRUE, ntree=500, cutoff=c(.85,.15))
varImpPlot(forest_all)
predictions_forest_all <- predict(forest_all, newdata = test_set, type = "class")
confmat_forest_all <- table(test_set$loan_status, predictions_forest_all)
acc_forest_all <- sum(diag(confmat_forest_all)) / nrow(test_set)
acc_forest_all


ROC_forest_all <- roc(test_set$loan_status, as.numeric(predictions_forest_all))
plot(ROC_forest_all)
auc(ROC_forest_all)








