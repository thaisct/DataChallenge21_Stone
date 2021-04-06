#Top Minds - Stone Challenge

#########################################################Settings##################################################

#Loading library functions

source("./library/functions.R")

#Installing and loading necessary packages

packages = c("tidyverse", "rmarkdown","arrow", "data.table", "caret", "e1071", "corrplot","xgboost", "cowplot",
             "lubridate", "gmodels","rlist","ROCR", "rpart","rpart.plot","rattle","RColorBrewer","Hmisc", "partykit", 
             "randomForest", "caTools")

package_check(packages)

rm(packages)

###############################################################################################################################
################################################ Logistic Regression ##########################################################
###############################################################################################################################


#Splitting train database

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(train$id, p=0.80, list=FALSE)
train.test <- train[-validation_index,]
train.train <- train[validation_index,]

#################################################### Statistical Inference 

#Checking Statistical Significance for each variable

model_train <- glm(y ~ ., family = "binomial", data = train)
summary(model_train)


#Fitting Statistical Significance Variables into a Logistic Regression Model

logit_model <- glm(y ~ pgto_diario_esperado + valor_emprestado + maturidade + pgto_esperado_pp + diasdepois_pgto_nivel + diasdepois_trans_nivel + taxa_t_nivel + liq_75 + media_pgto_25 + freq_50_pgto + freq_50_trans + freq_90_trans, family = "binomial", data = train.train)


###################################################### Predictions 

predictions_train <- as.data.frame(predict(logit_model_train, newdata = train.test%>%select(-y), type = "response"))

#################################################### Setting Cut-off Level
#Cut Off Level = 20%
cutoff_train <- 0.20
pred_cutoff_0.20_train <- as.data.frame(ifelse(predictions_train > cutoff_train, 1, 0))
colnames(pred_cutoff_0.20_train) <- "default"

conf_20_train <- table(train.test$y,pred_cutoff_0.20_train$default)

accuracy_20_train <- sum(diag(conf_20_train))/sum(conf_20_train)


#Cut Off Level = 25%
cutoff_train <- 0.25
pred_cutoff_0.25_train <- as.data.frame(ifelse(predictions_train > cutoff_train, 1, 0))
colnames(pred_cutoff_0.25_train) <- "default"

conf_25_train <- table(train.test$y,pred_cutoff_0.25_train$default)

accuracy_25_train <- sum(diag(conf_25_train))/sum(conf_25_train)


#Cut Off Level = 30%
cutoff_train <- 0.30
pred_cutoff_0.30_train <- as.data.frame(ifelse(predictions_train > cutoff_train, 1, 0))
colnames(pred_cutoff_0.30_train) <- "default"

conf_30_train <- table(train.test$y,pred_cutoff_0.30_train$default)

accuracy_30_train <- sum(diag(conf_30_train))/sum(conf_30_train)


#Cut Off Level = 10%
cutoff_train <- 0.10
pred_cutoff_0.10_train <- as.data.frame(ifelse(predictions_train > cutoff_train, 1, 0))
colnames(pred_cutoff_0.10_train) <- "default"

conf_10_train <- table(train.test$y,pred_cutoff_0.10_train$default)

accuracy_10_train <- sum(diag(conf_10_train))/sum(conf_10_train)

#Cut Off Level = 5%
cutoff_train <- 0.05
pred_cutoff_0.05_train <- as.data.frame(ifelse(predictions_train > cutoff_train, 1, 0))
colnames(pred_cutoff_0.05_train) <- "default"

conf_5_train <- table(train.test$y,pred_cutoff_0.05_train$default)

accuracy_5_train <- sum(diag(conf_5_train))/sum(conf_5_train)

accuracy_table <- cbind(c(5,10,15,20,25,30), c(accuracy_5_train,accuracy_10_train,accuracy_15_train,accuracy_20_train,accuracy_25_train,accuracy_30_train))

#################################################### ACU 

##Assessing model robustness

#AUC (Area under the curve)

predROC <- prediction(predictions_train, train.test$y)

perfROC <- performance(predROC,"tpr","fpr")

plot(perfROC, colorize = TRUE,
     print.cutoffs.at = seq(0,1,0.1),text.adj = c(-0.2,1.7))

AUC_full <- as.numeric(performance(predROC , "auc")@y.values)



###############################################################################################################################
################################################ Decision Tree #############################################################
###############################################################################################################################


# Changing the prior probabilities of loan default and non-loan default

#Modifying train datasets
train.test <- train.test %>% select(-subsegmento)
train.train <- train.train %>% select(-subsegmento)


#training a model
creditDecTree <- rpart(train.train$y ~ ., data = train.train, method = "class", minbucket = 1) #min bucket is minimum number of observations in a terminal nore
summary(creditDecTree) #summary of the model output

#plotting a decision tree to see splits
prp(creditDecTree)

#predicting on test data
predictCreditDecTree <- predict(creditDecTree, newdata = train.test, type = "class") #getting classes rather than probability

#computing the accuracy of the model
table(train.test$y,predictCreditDecTree) #since we don't have a probability here so we don't set a threshold

accuracyCreditDecTree <- ((as.matrix(table(train.test$y, predictCreditDecTree))[1,1]) + (as.matrix(table(train.test$y, predictCreditDecTree))[2,2]))/nrow(train.test)

#computing the baseline model for comparison
baseLineAccuracy <- max(table(train.test$y))/nrow(train.test)

print(accuracyCreditDecTree)
print(baseLineAccuracy)
#Note: Our decision tree model beats the baseline model in terms of accuracy


###############################################################################################################################
########################## Modelo Logit para o dataset test.parquet#############################################################
###############################################################################################################################
##################################################### Modelo Logit para o dataset test.parquet

#Modelo de Regressão com dataset train.train
logit_model <- glm(y ~ pgto_diario_esperado + valor_emprestado + maturidade + pgto_esperado_pp + diasdepois_pgto_nivel + diasdepois_trans_nivel + taxa_t_nivel + liq_75 + media_pgto_25 + freq_50_pgto + freq_50_trans + freq_90_trans, family = "binomial", data = train)

predictions <- as.data.frame(predict(logit_model, newdata = test, type = "response"))

#Cut Off Level = 15%
cutoff <- 0.15
pred_cutoff_0.15 <- as.data.frame(ifelse(predictions > cutoff, 1, 0))
colnames(pred_cutoff_0.15) <- "ypred"

submission <- cbind(test$id,pred_cutoff_0.15,predictions)
colnames(submission) <- c("id", "ypred", "yprob")

#Salvando submission as .parquet

write_parquet(submission, "./database/submission.parquet")
example <- read_parquet("./database/submission.parquet", as_tibble = TRUE)










