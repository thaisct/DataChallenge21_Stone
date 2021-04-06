#Top Minds - Stone Challenge

#########################################################Settings###############################################################

#Loading library functions

source("./library/functions.R")

#Installing and loading necessary packages

packages = c("tidyverse", "rmarkdown","arrow", "data.table", "caret", "e1071", "corrplot","xgboost", "cowplot",
             "lubridate", "gmodels","rlist","ROCR", "rpart","rpart.plot","rattle","RColorBrewer","Hmisc", "partykit", "randomForest")

package_check(packages)

rm(packages)


################################################################################################################################
####################################################### TRAIN PARQUET ##########################################################
################################################################################################################################

############################################### Summarize Dataset ##################################################

#Loading Database

train.parquet <- read_parquet("./database/train.parquet", as_tibble = TRUE)

#Dimensions of the Dataset
dim(train.parquet)

#Evaluating Database: structure
str(train.parquet)

#Statistical Summary
summary(train.parquet)

#Description
describe(train.parquet)

############################################### Cleaning Dataset ######################################################

#Checking for missing values

colSums((is.na(train.parquet)))
train.parquet <- train.parquet %>% replace_na(list(subsegmento = "Sem Classificação"))

#Reshaping Date variables
train.parquet <- train.parquet %>% 
    mutate(desembolso = as.Date(desembolso, format = "%Y-%m-%d")) %>%
    mutate(vencimento = as.Date(vencimento, format = "%Y-%m-%d")) %>%
    mutate(dia = as.Date(dia, format = "%Y-%m-%d"))

###############################################Feature Construction##################################################

#Temporary Database

temp <- train.parquet %>%
    group_by(id) %>%
    group_split()


#Taking the first 90 days of all id's (even if they finished to pay before)

train_90 <-train.parquet %>%
    group_by(id) %>%
    filter(dias_pos_desembolso <=90)


#Loan amount (valor_emprestado), pagamento diário esperado (pgto_diario_esperado), subsegmento, maturity (maturidade)

extract_raw <- train_90 %>% 
    select(id,valor_emprestado, pgto_diario_esperado, subsegmento, y, desembolso, vencimento) %>%
    mutate(maturidade = as.numeric(vencimento -  desembolso)) %>%
    select(-vencimento, -desembolso) %>%
    distinct(id, .keep_all = TRUE)


#Expected Payment vs Daily payment (pgto_esperado_pp)

pgto_esperado_90_total <- train_90  %>% 
    select(id,pgto_diario_esperado) %>% 
    group_by(id) %>% 
    summarise(pgto_esperado_90_total = sum(pgto_diario_esperado))

pgto_90_diario <- train_90  %>% 
    select(id, pagamento_diario) %>% 
    group_by(id) %>% 
    summarise(pgto_90_diario = sum(pagamento_diario))

pgto_pp <- pgto_esperado_90_total%>%
    left_join(pgto_90_diario, by = c("id" = "id"))%>%
    mutate(pgto_esperado_pp = pgto_90_diario / pgto_esperado_90_total)%>%
    mutate(pgto_esperado_pp = replace_na(pgto_esperado_pp, 0))%>%
    select(-pgto_esperado_90_total, - pgto_90_diario)

rm(pgto_90_diario, pgto_esperado_90_total)


#Daily Interest Rate (juros) and separating them into bins

juros_d <- juros(temp)

    ##Classes de Juros
    
    juros_d$juros_d_nivel <- rep(NA, length(juros_d$juros))
    
    juros_d$juros_d_nivel[which(juros_d$juros <= 0.0010)] <- "0 - 0.1"
    juros_d$juros_d_nivel[which(juros_d$juros > 0.0010 & juros_d$juros <= 0.0015)] <- "0.10 - 0.15"
    juros_d$juros_d_nivel[which(juros_d$juros > 0.0015 & juros_d$juros <= 0.0020)] <- "0.15 - 0.20"
    juros_d$juros_d_nivel[which(juros_d$juros > 0.0020 & juros_d$juros <= 0.0025)] <- "0.20 - 0.25"
    juros_d$juros_d_nivel[which(juros_d$juros > 0.0025 & juros_d$juros <= 0.0030)] <- "0.25 - 0.30"
    juros_d$juros_d_nivel[which(juros_d$juros > 0.0030)] <- "0.30+"
    
    juros_d$juros_d_nivel <- as.factor(juros_d$juros_d_nivel)
    juros_d$juros_d_nivel <- factor(juros_d$juros_d_nivel, ordered = TRUE)
    
    
    juros_d <- juros_d[,-c(2)]


#Rate over transactions (taxa_transacao)

temp1 <- train.parquet %>%
    #filter(id %in% id_indices)%>%
    select(id, dia, pagamento_diario, transacionado) %>%
    filter(pagamento_diario != 0 | transacionado != 0) %>%
    mutate(taxa_transacao = pagamento_diario / lag(transacionado)) %>%
    select(id, taxa_transacao) %>%
    filter( !is.na(taxa_transacao) & taxa_transacao != 0 & !is.infinite(taxa_transacao) & taxa_transacao < 1) %>%
    mutate(taxa_transacao = round(taxa_transacao,3))%>%
    group_by(id) %>%
    group_split()

taxa_t <- taxa(temp1)

#Including all id's
taxa_t <- juros_d%>%
    left_join(taxa_t, by = c("id" = "id"))%>%
    select(-juros_d_nivel)

#Classifying rate of transaction into levels

taxa_t$taxa_t_nivel <- rep(NA, length(taxa_t$taxa_transacao))

taxa_t$taxa_t_nivel[which(taxa_t$taxa_transacao <= 0.05)] <- "0 - 0.05"
taxa_t$taxa_t_nivel[which(taxa_t$taxa_transacao > 0.05 & taxa_t$taxa_transacao <= 0.10)] <- "0.05 - 0.10"
taxa_t$taxa_t_nivel[which(taxa_t$taxa_transacao > 0.10 & taxa_t$taxa_transacao <= 0.15)] <- "0.10 - 0.15"
taxa_t$taxa_t_nivel[which(taxa_t$taxa_transacao > 0.15 & taxa_t$taxa_transacao <= 0.20)] <- "0.15 - 0.20"
taxa_t$taxa_t_nivel[which(taxa_t$taxa_transacao > 0.20 & taxa_t$taxa_transacao <= 0.25)] <- "0.20 - 0.25"
taxa_t$taxa_t_nivel[which(taxa_t$taxa_transacao > 0.25)] <- "0.25+"
taxa_t$taxa_t_nivel[which(is.na(taxa_t$taxa_transacao))] <- "Missing"

taxa_t$taxa_t_nivel <- as.factor(taxa_t$taxa_t_nivel)
taxa_t$taxa_t_nivel <- factor(taxa_t$taxa_t_nivel, ordered = TRUE)

taxa_t <- taxa_t[,-c(2)]
rm(temp1)


#How many days after the loan, clients perform first Pagamento, Amortização and Transação -  (diasdepois_pgto, diasdepois_amort, diasdepois_trans)
#If no payment was identified in the first 90 days, all variables will be 0

##Pagamento_diario

diasdepois_pgto <- diasdepois(train_90,"p")

    #Classifying days into bins
    
    diasdepois_pgto$id <- as.numeric(diasdepois_pgto$id)
    
    diasdepois_pgto$diasdepois_pgto <- as.numeric(diasdepois_pgto$diasdepois_pgto)
    
    diasdepois_pgto$diasdepois_pgto_nivel <- rep(NA, length(diasdepois_pgto$diasdepois_pgto))
    
    diasdepois_pgto$diasdepois_pgto_nivel[which(diasdepois_pgto$diasdepois_pgto <= 2)] <- "0 - 2"
    diasdepois_pgto$diasdepois_pgto_nivel[which(diasdepois_pgto$diasdepois_pgto > 2 & diasdepois_pgto$diasdepois_pgto <= 5)] <- "3 - 5"
    diasdepois_pgto$diasdepois_pgto_nivel[which(diasdepois_pgto$diasdepois_pgto > 5 & diasdepois_pgto$diasdepois_pgto <= 8)] <- "6 - 8"
    diasdepois_pgto$diasdepois_pgto_nivel[which(diasdepois_pgto$diasdepois_pgto > 8 & diasdepois_pgto$diasdepois_pgto <= 12)] <- "9 - 12"
    diasdepois_pgto$diasdepois_pgto_nivel[which(diasdepois_pgto$diasdepois_pgto > 12 & diasdepois_pgto$diasdepois_pgto <= 15)] <- "13 - 15"
    diasdepois_pgto$diasdepois_pgto_nivel[which(diasdepois_pgto$diasdepois_pgto > 15 & diasdepois_pgto$diasdepois_pgto <= 20)] <- "16 - 20"
    diasdepois_pgto$diasdepois_pgto_nivel[which(diasdepois_pgto$diasdepois_pgto > 20 & diasdepois_pgto$diasdepois_pgto <= 40)] <- "21 - 40"
    diasdepois_pgto$diasdepois_pgto_nivel[which(diasdepois_pgto$diasdepois_pgto > 40)] <- "41 - 90"
    diasdepois_pgto$diasdepois_pgto_nivel[which(is.na(diasdepois_pgto$diasdepois_pgto))] <- "Missed 90 days"
    
    diasdepois_pgto$diasdepois_pgto_nivel <- as.factor(diasdepois_pgto$diasdepois_pgto_nivel)
    diasdepois_pgto$diasdepois_pgto_nivel <- factor(diasdepois_pgto$diasdepois_pgto_nivel, 
                                                    levels = c("0 - 2","3 - 5", "6 - 8","9 - 12", "13 - 15","16 - 20", "21 - 40",
                                                               "41 - 90","Missed 90 days" ))

    diasdepois_pgto <- diasdepois_pgto[,-c(2)]


##Amortização

diasdepois_amort <- diasdepois(train_90,"a")

#Classifying days into bins

diasdepois_amort$id <- as.numeric(diasdepois_amort$id)

diasdepois_amort$diasdepois_amort <- as.numeric(diasdepois_amort$diasdepois_amort)

diasdepois_amort$diasdepois_amort_nivel <- rep(NA, length(diasdepois_amort$diasdepois_amort))

diasdepois_amort$diasdepois_amort_nivel[which(diasdepois_amort$diasdepois_amort <= 2)] <- "0 - 2"
diasdepois_amort$diasdepois_amort_nivel[which(diasdepois_amort$diasdepois_amort > 2 & diasdepois_amort$diasdepois_amort <= 5)] <- "3 - 5"
diasdepois_amort$diasdepois_amort_nivel[which(diasdepois_amort$diasdepois_amort > 5 & diasdepois_amort$diasdepois_amort <= 8)] <- "6 - 8"
diasdepois_amort$diasdepois_amort_nivel[which(diasdepois_amort$diasdepois_amort > 8 & diasdepois_amort$diasdepois_amort <= 12)] <- "9 - 12"
diasdepois_amort$diasdepois_amort_nivel[which(diasdepois_amort$diasdepois_amort > 12 & diasdepois_amort$diasdepois_amort <= 15)] <- "13 - 15"
diasdepois_amort$diasdepois_amort_nivel[which(diasdepois_amort$diasdepois_amort > 15 & diasdepois_amort$diasdepois_amort <= 20)] <- "16 - 20"
diasdepois_amort$diasdepois_amort_nivel[which(diasdepois_amort$diasdepois_amort > 20 & diasdepois_amort$diasdepois_amort <= 40)] <- "21 - 40"
diasdepois_amort$diasdepois_amort_nivel[which(diasdepois_amort$diasdepois_amort > 40)] <- "41 - 90"
diasdepois_amort$diasdepois_amort_nivel[which(is.na(diasdepois_amort$diasdepois_amort))] <- "Missed 90 days"

diasdepois_amort$diasdepois_amort_nivel <- as.factor(diasdepois_amort$diasdepois_amort_nivel)
diasdepois_amort$diasdepois_amort_nivel <- factor(diasdepois_amort$diasdepois_amort_nivel, 
                                                  levels = c("0 - 2","3 - 5", "6 - 8","9 - 12", "13 - 15","16 - 20", "21 - 40",
                                                             "41 - 90","Missed 90 days" ))

diasdepois_amort <- diasdepois_amort[,-c(2)]

##Transacionado
diasdepois_trans <- diasdepois(train_90,"t")

#Classifying days into bins

diasdepois_trans$id <- as.numeric(diasdepois_trans$id)

diasdepois_trans$diasdepois_trans <- as.numeric(diasdepois_trans$diasdepois_trans)

diasdepois_trans$diasdepois_trans_nivel <- rep(NA, length(diasdepois_trans$diasdepois_trans))

diasdepois_trans$diasdepois_trans_nivel[which(diasdepois_trans$diasdepois_trans <= 2)] <- "0 - 2"
diasdepois_trans$diasdepois_trans_nivel[which(diasdepois_trans$diasdepois_trans > 2 & diasdepois_trans$diasdepois_trans <= 5)] <- "3 - 5"
diasdepois_trans$diasdepois_trans_nivel[which(diasdepois_trans$diasdepois_trans > 5 & diasdepois_trans$diasdepois_trans <= 8)] <- "6 - 8"
diasdepois_trans$diasdepois_trans_nivel[which(diasdepois_trans$diasdepois_trans > 8 & diasdepois_trans$diasdepois_trans <= 12)] <- "9 - 12"
diasdepois_trans$diasdepois_trans_nivel[which(diasdepois_trans$diasdepois_trans > 12 & diasdepois_trans$diasdepois_trans <= 15)] <- "13 - 15"
diasdepois_trans$diasdepois_trans_nivel[which(diasdepois_trans$diasdepois_trans > 15 & diasdepois_trans$diasdepois_trans <= 20)] <- "16 - 20"
diasdepois_trans$diasdepois_trans_nivel[which(diasdepois_trans$diasdepois_trans > 20 & diasdepois_trans$diasdepois_trans <= 40)] <- "21 - 40"
diasdepois_trans$diasdepois_trans_nivel[which(diasdepois_trans$diasdepois_trans > 40)] <- "41 - 90"
diasdepois_trans$diasdepois_trans_nivel[which(is.na(diasdepois_trans$diasdepois_trans))] <- "Missed 90 days"

diasdepois_trans$diasdepois_trans_nivel <- as.factor(diasdepois_trans$diasdepois_trans_nivel)
diasdepois_trans$diasdepois_trans_nivel <- factor(diasdepois_trans$diasdepois_trans_nivel, 
                                                  levels = c("0 - 2","3 - 5", "6 - 8","9 - 12", "13 - 15","16 - 20", "21 - 40",
                                                             "41 - 90","Missed 90 days" ))

diasdepois_trans <- diasdepois_trans[,-c(2)]


#How many days took them to liquidate their debt -  25%, 50%, 75% and 100% of 90 days 

liquidacao_db <- liquidacao(train_90)



#Mean of value paid in "Pagamento Diário" in the quantile 25, 50 and 75 of 90 days - % with respect to "Pagamento Esperado"

media_pgto_pp <- media_pgto(train_90)


#Mean Weekly Frequency of "Pagamento Diário", "Amortização" and "Transacionado" in the 25%, 50% and 75% quantile of "maturidade"
#(25_freq_pgto, 25_freq_amort, 25_freq_trans, 50_freq_pgto, 50_freq_amort, 50_freq_trans, 75_freq_pgto, 75_freq_amort, 75_freq_trans)

freq_tempo_quartil <- freq_quartil(train_90)


################################################### Setting Final Train database #############################################

#Joining all tables

train <- extract_raw %>% 
    left_join(pgto_pp, by = c("id" = "id"))%>%
    left_join(diasdepois_pgto, by = c("id" = "id")) %>%
    left_join(diasdepois_amort, by = c("id" = "id")) %>% 
    left_join(diasdepois_trans, by = c("id" = "id")) %>%
    left_join(juros_d, by = c("id" = "id"))%>%
    left_join(taxa_t, by = c("id" = "id"))%>%
    left_join(liquidacao_db, by = c("id" = "id"))%>%
    left_join(media_pgto_pp, by = c("id" = "id"))%>%
    left_join(freq_tempo_quartil, by = c("id" = "id"))

#checking proportions across train
prop.table(table(train$y))


################################################################################################################################
####################################################### TEST PARQUET ###########################################################
################################################################################################################################

#Loading Test Database

test <- read_parquet("./database/test.parquet", as_tibble = TRUE)

#Evaluating Database
str(test)

#Checking for missing values
colSums((is.na(test)))
test <- test %>% replace_na(list(subsegmento = "Sem Classificação"))

#Reshaping Date variables
test <- test %>% 
    mutate(desembolso = as.Date(desembolso, format = "%Y-%m-%d")) %>%
    mutate(vencimento = as.Date(vencimento, format = "%Y-%m-%d")) %>%
    mutate(dia = as.Date(dia, format = "%Y-%m-%d"))

###############################################Feature Construction##################################################

#Temporary Database

temp <- test %>%
    group_by(id) %>%
    group_split()

#Taking the first 90 days of all id's (even if they finished to pay before)

test_90 <-test %>%
    group_by(id) %>%
    filter(dias_pos_desembolso <=90)

#Loan amount (valor_emprestado), pagamento diário esperado (pgto_diario_esperado), subsegmento, maturity (maturidade)
extract_raw2 <- test_90 %>% 
    select(id,valor_emprestado, pgto_diario_esperado, subsegmento, desembolso, vencimento) %>%
    mutate(maturidade = as.numeric(vencimento - desembolso)) %>%
    select(-vencimento, -desembolso) %>%
    distinct(id, .keep_all = TRUE)

#Expected Payment vs Daily payment (pgto_90_pp)

pgto_esperado_90_total <- test_90  %>% 
    select(id,pgto_diario_esperado) %>% 
    group_by(id) %>% 
    summarise(pgto_esperado_90_total = sum(pgto_diario_esperado))

pgto_90_diario <- test_90  %>% 
    select(id, pagamento_diario) %>% 
    group_by(id) %>% 
    summarise(pgto_90_diario = sum(pagamento_diario))

pgto_pp_2 <- pgto_esperado_90_total%>%
    left_join(pgto_90_diario, by = c("id" = "id"))%>%
    mutate(pgto_esperado_pp = pgto_90_diario / pgto_esperado_90_total)%>%
    mutate(pgto_esperado_pp = replace_na(pgto_esperado_pp, 0))%>%
    select(-pgto_esperado_90_total, - pgto_90_diario)

rm(pgto_90_diario, pgto_esperado_90_total)


#Daily Interest Rate (juros) and separating them into bins

juros_d2 <- juros(temp)

###############################################################################

##Classes de Juros

juros_d2$juros_d_nivel <- rep(NA, length(juros_d2$juros))

juros_d2$juros_d_nivel[which(juros_d2$juros <= 0.0010)] <- "0 - 0.1"
juros_d2$juros_d_nivel[which(juros_d2$juros > 0.0010 & juros_d2$juros <= 0.0015)] <- "0.10 - 0.15"
juros_d2$juros_d_nivel[which(juros_d2$juros > 0.0015 & juros_d2$juros <= 0.0020)] <- "0.15 - 0.20"
juros_d2$juros_d_nivel[which(juros_d2$juros > 0.0020 & juros_d2$juros <= 0.0025)] <- "0.20 - 0.25"
juros_d2$juros_d_nivel[which(juros_d2$juros > 0.0025 & juros_d2$juros <= 0.0030)] <- "0.25 - 0.30"
juros_d2$juros_d_nivel[which(juros_d2$juros > 0.0030)] <- "0.30+"

juros_d2$juros_d_nivel <- as.factor(juros_d2$juros_d_nivel)
juros_d2$juros_d_nivel <- factor(juros_d2$juros_d_nivel, ordered = TRUE)

juros_d2 <- juros_d2[,-c(2)]


#Rate over transactions (taxa_transacao)

temp1 <- test %>%
    select(id, dia, pagamento_diario, transacionado) %>%
    filter(pagamento_diario != 0 | transacionado != 0) %>%
    mutate(taxa_transacao = pagamento_diario / lag(transacionado)) %>%
    select(id, taxa_transacao) %>%
    filter( !is.na(taxa_transacao) & taxa_transacao != 0 & !is.infinite(taxa_transacao) & taxa_transacao < 1) %>%
    mutate(taxa_transacao = round(taxa_transacao,3))%>%
    group_by(id) %>%
    group_split()

taxa_t2 <- taxa(temp1)

#Including all id's
taxa_t2 <- juros_d2 %>%
    left_join(taxa_t2, by = c("id" = "id"))%>%
    select(-juros_d_nivel)

#Classifying rate of transaction into levels

taxa_t2$taxa_t_nivel <- rep(NA, length(taxa_t2$taxa_transacao))

taxa_t2$taxa_t_nivel[which(taxa_t2$taxa_transacao <= 0.05)] <- "0 - 0.05"
taxa_t2$taxa_t_nivel[which(taxa_t2$taxa_transacao > 0.05 & taxa_t2$taxa_transacao <= 0.10)] <- "0.05 - 0.10"
taxa_t2$taxa_t_nivel[which(taxa_t2$taxa_transacao > 0.10 & taxa_t2$taxa_transacao <= 0.15)] <- "0.10 - 0.15"
taxa_t2$taxa_t_nivel[which(taxa_t2$taxa_transacao > 0.15 & taxa_t2$taxa_transacao <= 0.20)] <- "0.15 - 0.20"
taxa_t2$taxa_t_nivel[which(taxa_t2$taxa_transacao > 0.20 & taxa_t2$taxa_transacao <= 0.25)] <- "0.20 - 0.25"
taxa_t2$taxa_t_nivel[which(taxa_t2$taxa_transacao > 0.25)] <- "0.25+"
taxa_t2$taxa_t_nivel[which(is.na(taxa_t2$taxa_transacao))] <- "Missing"

taxa_t2$taxa_t_nivel <- as.factor(taxa_t2$taxa_t_nivel)
taxa_t2$taxa_t_nivel <- factor(taxa_t2$taxa_t_nivel, ordered = TRUE)

taxa_t2 <- taxa_t2[,-c(2)]
rm(temp1)


#How many days after the loan, clients perform first Pagamento, Amortização and Transação -  (diasdepois_pgto, diasdepois_amort, diasdepois_trans)
#If no payment was identified in the first 90 days, all variables will be 0

##Pagamento_diario

diasdepois_pgto2 <- diasdepois2(test_90,"p")

    #Classifying days into bins
    
    diasdepois_pgto2$id <- as.numeric(diasdepois_pgto2$id)
    
    diasdepois_pgto2$diasdepois_pgto <- as.numeric(diasdepois_pgto2$diasdepois_pgto)
    
    diasdepois_pgto2$diasdepois_pgto_nivel <- rep(NA, length(diasdepois_pgto2$diasdepois_pgto))
    
    diasdepois_pgto2$diasdepois_pgto_nivel[which(diasdepois_pgto2$diasdepois_pgto <= 2)] <- "0 - 2"
    diasdepois_pgto2$diasdepois_pgto_nivel[which(diasdepois_pgto2$diasdepois_pgto > 2 & diasdepois_pgto2$diasdepois_pgto <= 5)] <- "3 - 5"
    diasdepois_pgto2$diasdepois_pgto_nivel[which(diasdepois_pgto2$diasdepois_pgto > 5 & diasdepois_pgto2$diasdepois_pgto <= 8)] <- "6 - 8"
    diasdepois_pgto2$diasdepois_pgto_nivel[which(diasdepois_pgto2$diasdepois_pgto > 8 & diasdepois_pgto2$diasdepois_pgto <= 12)] <- "9 - 12"
    diasdepois_pgto2$diasdepois_pgto_nivel[which(diasdepois_pgto2$diasdepois_pgto > 12 & diasdepois_pgto2$diasdepois_pgto <= 15)] <- "13 - 15"
    diasdepois_pgto2$diasdepois_pgto_nivel[which(diasdepois_pgto2$diasdepois_pgto > 15 & diasdepois_pgto2$diasdepois_pgto <= 20)] <- "16 - 20"
    diasdepois_pgto2$diasdepois_pgto_nivel[which(diasdepois_pgto2$diasdepois_pgto > 20 & diasdepois_pgto2$diasdepois_pgto <= 40)] <- "21 - 40"
    diasdepois_pgto2$diasdepois_pgto_nivel[which(diasdepois_pgto2$diasdepois_pgto > 40)] <- "41 - 90"
    diasdepois_pgto2$diasdepois_pgto_nivel[which(is.na(diasdepois_pgto2$diasdepois_pgto))] <- "Missed 90 days"
    
    diasdepois_pgto2$diasdepois_pgto_nivel <- as.factor(diasdepois_pgto2$diasdepois_pgto_nivel)
    diasdepois_pgto2$diasdepois_pgto_nivel <- factor(diasdepois_pgto2$diasdepois_pgto_nivel, 
                                                    levels = c("0 - 2","3 - 5", "6 - 8","9 - 12", "13 - 15","16 - 20", "21 - 40",
                                                               "41 - 90","Missed 90 days" ))
    
    diasdepois_pgto2 <- diasdepois_pgto2[,-c(2)]

##Amortização

diasdepois_amort2 <- diasdepois2(test_90,"a")

    #Classifying days into bins
    
    diasdepois_amort2$id <- as.numeric(diasdepois_amort2$id)
    
    diasdepois_amort2$diasdepois_amort <- as.numeric(diasdepois_amort2$diasdepois_amort)
    
    diasdepois_amort2$diasdepois_amort_nivel <- rep(NA, length(diasdepois_amort2$diasdepois_amort))
    
    diasdepois_amort2$diasdepois_amort_nivel[which(diasdepois_amort2$diasdepois_amort <= 2)] <- "0 - 2"
    diasdepois_amort2$diasdepois_amort_nivel[which(diasdepois_amort2$diasdepois_amort > 2 & diasdepois_amort2$diasdepois_amort <= 5)] <- "3 - 5"
    diasdepois_amort2$diasdepois_amort_nivel[which(diasdepois_amort2$diasdepois_amort > 5 & diasdepois_amort2$diasdepois_amort <= 8)] <- "6 - 8"
    diasdepois_amort2$diasdepois_amort_nivel[which(diasdepois_amort2$diasdepois_amort > 8 & diasdepois_amort2$diasdepois_amort <= 12)] <- "9 - 12"
    diasdepois_amort2$diasdepois_amort_nivel[which(diasdepois_amort2$diasdepois_amort > 12 & diasdepois_amort2$diasdepois_amort <= 15)] <- "13 - 15"
    diasdepois_amort2$diasdepois_amort_nivel[which(diasdepois_amort2$diasdepois_amort > 15 & diasdepois_amort2$diasdepois_amort <= 20)] <- "16 - 20"
    diasdepois_amort2$diasdepois_amort_nivel[which(diasdepois_amort2$diasdepois_amort > 20 & diasdepois_amort2$diasdepois_amort <= 40)] <- "21 - 40"
    diasdepois_amort2$diasdepois_amort_nivel[which(diasdepois_amort2$diasdepois_amort > 40)] <- "41 - 90"
    diasdepois_amort2$diasdepois_amort_nivel[which(is.na(diasdepois_amort2$diasdepois_amort))] <- "Missed 90 days"
    
    diasdepois_amort2$diasdepois_amort_nivel <- as.factor(diasdepois_amort2$diasdepois_amort_nivel)
    
    diasdepois_amort2$diasdepois_amort_nivel <- factor(diasdepois_amort2$diasdepois_amort_nivel, 
                                                     levels = c("0 - 2","3 - 5", "6 - 8","9 - 12", "13 - 15","16 - 20", "21 - 40",
                                                                "41 - 90","Missed 90 days" ))

    diasdepois_amort2 <- diasdepois_amort2[,-c(2)]


##Transacionado
diasdepois_trans2 <- diasdepois2(test_90,"t")

    #Classifying days into bins
    
    diasdepois_trans2$id <- as.numeric(diasdepois_trans2$id)
    
    diasdepois_trans2$diasdepois_trans <- as.numeric(diasdepois_trans2$diasdepois_trans)
    
    diasdepois_trans2$diasdepois_trans_nivel <- rep(NA, length(diasdepois_trans2$diasdepois_trans))
    
    diasdepois_trans2$diasdepois_trans_nivel[which(diasdepois_trans2$diasdepois_trans <= 2)] <- "0 - 2"
    diasdepois_trans2$diasdepois_trans_nivel[which(diasdepois_trans2$diasdepois_trans > 2 & diasdepois_trans2$diasdepois_trans <= 5)] <- "3 - 5"
    diasdepois_trans2$diasdepois_trans_nivel[which(diasdepois_trans2$diasdepois_trans > 5 & diasdepois_trans2$diasdepois_trans <= 8)] <- "6 - 8"
    diasdepois_trans2$diasdepois_trans_nivel[which(diasdepois_trans2$diasdepois_trans > 8 & diasdepois_trans2$diasdepois_trans <= 12)] <- "9 - 12"
    diasdepois_trans2$diasdepois_trans_nivel[which(diasdepois_trans2$diasdepois_trans > 12 & diasdepois_trans2$diasdepois_trans <= 15)] <- "13 - 15"
    diasdepois_trans2$diasdepois_trans_nivel[which(diasdepois_trans2$diasdepois_trans > 15 & diasdepois_trans2$diasdepois_trans <= 20)] <- "16 - 20"
    diasdepois_trans2$diasdepois_trans_nivel[which(diasdepois_trans2$diasdepois_trans > 20 & diasdepois_trans2$diasdepois_trans <= 40)] <- "21 - 40"
    diasdepois_trans2$diasdepois_trans_nivel[which(diasdepois_trans2$diasdepois_trans > 40)] <- "41 - 90"
    diasdepois_trans2$diasdepois_trans_nivel[which(is.na(diasdepois_trans2$diasdepois_trans))] <- "Missed 90 days"
    
    diasdepois_trans2$diasdepois_trans_nivel <- as.factor(diasdepois_trans2$diasdepois_trans_nivel)
    diasdepois_trans2$diasdepois_trans_nivel <- factor(diasdepois_trans2$diasdepois_trans_nivel, 
                                                      levels = c("0 - 2","3 - 5", "6 - 8","9 - 12", "13 - 15","16 - 20", "21 - 40",
                                                                 "41 - 90","Missed 90 days" ))
    
    diasdepois_trans2 <- diasdepois_trans2[,-c(2)]


#How many days took them to liquidate their debt -  25%, 50%, 75% and 100% of 90 days 

liquidacao2 <- liquidacao(test_90)



#Mean of value paid in "Pagamento Diário" in the quantile 25, 50 and 75 of 90 days - % with respect to "Pagamento Esperado"

media_pgto_pp2 <- media_pgto(test_90)


#Frequency of "Pagamento Diário", "Amortização" and "Transacionado" in the 25%, 50% and 75% quantile of "maturidade"
#(25_freq_pgto, 25_freq_amort, 25_freq_trans, 50_freq_pgto, 50_freq_amort, 50_freq_trans, 75_freq_pgto, 75_freq_amort, 75_freq_trans)

freq_tempo_quartil2 <- freq_quartil(test_90)



###################################################### Setting Final database ############################################

#Joining all tables, Removing Outliers & NAs

test <- extract_raw2 %>%
    left_join(pgto_pp_2, by = c("id" = "id"))%>%
    left_join(diasdepois_pgto2, by = c("id" = "id")) %>%
    left_join(diasdepois_amort2, by = c("id" = "id")) %>% 
    left_join(diasdepois_trans2, by = c("id" = "id")) %>%
    left_join(juros_d2, by = c("id" = "id"))%>%
    left_join(taxa_t2, by = c("id" = "id"))%>%
    left_join(liquidacao2, by = c("id" = "id"))%>%
    left_join(media_pgto_pp2, by = c("id" = "id"))%>%
    left_join(freq_tempo_quartil2, by = c("id" = "id"))%>%
    filter(pgto_esperado_pp > 0)

colSums((is.na(test)))

