#Top Minds - Stone Challenge

#########################################################Settings###############################################################

#Loading library functions

source("./library/functions.R")

#Installing and loading necessary packages

packages = c("tidyverse", "rmarkdown","arrow", "data.table", "caret", "e1071", "corrplot","xgboost", "cowplot",
             "lubridate", "gmodels","rlist","ROCR", "rpart","rpart.plot","rattle","RColorBrewer","Hmisc", "partykit", "randomForest")

package_check(packages)

rm(packages)


##################################################### Visualize Dataset ######################################################

############## Análise Univariada

###Variáveis Qualitativas Nominais

t <- as.data.frame(table(train$subsegmento))

data <- t[t$Freq %in% tail(sort(t$Freq),5),]

data$Var1 <- as.character(data$Var1)
data <- rbind(data, data.frame(Freq = sum(t$Freq) - sum(data$Freq),Var1="others"))
pie(data$Freq, data$Var1)


###Variáveis Qualitativas Ordinais

##Juros
barplot(table(train$juros_d_nivel), xlab = "Níveis de Juros", main="Juros Diários")

##Taxa sobre Transações
barplot(table(train$taxa_t_nivel), xlab = "Níveis de Taxa de Transação", main="Taxa sobre Transação")

##Número de Dias pós desembolso que realizou:

#Primeiro Pagamento
barplot(table(train$diasdepois_pgto_nivel), xlab = "Poll de Dias", main="Número de Dias para realizar 1º Pagamento")

#Primeira Amortização
barplot(table(train$diasdepois_amort_nivel), xlab = "Poll de Dias", main="Número de Dias para realizar 1º Amortização")

#Primeira Transação
barplot(table(train$diasdepois_trans_nivel), xlab = "Poll de Dias", main="Número de Dias para realizar 1º Transação")


###Variáveis Quantitativas Discretas

##Maturidade
plot(table(train$maturidade),ylab = "", xlab = "Número de Dias", main="Distribuição de Maturidades")

##Frequências de Pagamento

#Amortizações

plot(table(train$freq_25_amort),ylab = "", xlab = "Número de Amortizações", main="Frequência de Amortizações Diárias - em Q1 dos primeiros 90 dias")
plot(table(train$freq_50_amort),ylab = "", xlab = "Número de Amortizações", main="Frequência de Amortizações Diárias - em Q2 dos primeiros 90 dias")
plot(table(train$freq_75_amort),ylab = "", xlab = "Número de Amortizações", main="Frequência de Amortizações Diárias - em Q3 dos primeiros 90 dias")

#Pagamentos

plot(table(train$freq_25_pgto),ylab = "", xlab = "Número de Pagamentos Diários", main="Frequência de Pagamentos Diários - em Q1 dos primeiros 90 dias")
plot(table(train$freq_50_pgto),ylab = "", xlab = "Número de Pagamentos Diários", main="Frequência de Pagamentos Diários - em Q2 dos primeiros 90 dias")
plot(table(train$freq_75_pgto),ylab = "", xlab = "Número de Pagamentos Diários", main="Frequência de Pagamentos Diários - em Q3 dos primeiros 90 dias")

#Transações

plot(table(train$freq_25_trans),ylab = "", xlab = "Número de Transações Diárias", main="Frequência de Transações Diárias - em Q1 dos primeiros 90 dias")
plot(table(train$freq_50_trans),ylab = "", xlab = "Número de Transações Diárias", main="Frequência de Transações Diárias - em Q2 dos primeiros 90 dias")
plot(table(train$freq_75_trans),ylab = "", xlab = "Número de Transações Diárias", main="Frequência de Transações Diárias - em Q3 dos primeiros 90 dias")


###Variáveis Quantitativas Contínuas


##Receita
hist(train$receita_pp, main = "Receita")
boxplot(train$receita_pp, main="Boxplot: Receita em % com relação à Receita Total na maturidade" )

#Removing Outliers
train <- train%>%filter(receita_pp <= 1)

##Amortização
hist(train$amort_pp, main = "Amortização")
boxplot(train$amort_pp)

#Removing Outliers
train <- train%>%filter(amort_pp <= 1 & amort_pp >= 0)

##Pagamentos
hist(train$pgto_diario_pp, main = "Pagamento")
boxplot(train$pgto_diario_pp)

hist(train$pgto_esperado_pp, main = "Pagamento Efetivado VS Pagamento Esperado")

#Removing Outliers
train <- train%>%filter(pgto_esperado_pp > 0)

##Liquidação da Dívida

hist(train$liq_25, main = "Quanto da dívida total foi liquidada no Q1 dos primeiros 90 dias?")   
hist(train$liq_50, main = "Quanto da dívida total foi liquidada no Q2 dos primeiros 90 dias?")
hist(train$liq_75, main = "Quanto da dívida total foi liquidada no Q3 dos primeiros 90 dias?")

##Média de Pagamento
hist(train$media_pgto_25, main = "Qual foi o valor médio do ticket de pagamento diário em Q1 dos primeiros 90 dias em comparação com o Pagamento Esperado?")
hist(train$media_pgto_50, main = "Qual foi o valor médio do ticket de pagamento diário em Q2 dos primeiros 90 dias em comparação com o Pagamento Esperado?")
hist(train$media_pgto_75, main = "Qual foi o valor médio do ticket de pagamento diário em Q3 dos primeiros 90 dias em comparação com o Pagamento Esperado?")

##Valor Emprestado
hist(train$valor_emprestado, main = "Distribuição dos Valores Emprestados") 

##Valor de Pagamento Diário Esperado
hist(train$pgto_diario_esperado, main = "Distribuição dos Valores de Pagamento Diário Esperado")


############## Análise Bivariada

#Criando uma database com todos os ID's aparecendo somente uma única vez
t2 <- train_90%>%
    distinct(y, .keep_all = TRUE) %>%
    mutate(maturidade = vencimento - desembolso)

##Quali vs Quali
barplot(with(train, table(juros_d_nivel, diasdepois_amort_nivel)), legend = TRUE, main = "Distribuição do Nível de Juros em relação ao Número de Dias que levou para realizar a 1ª Amortização")
barplot(with(train, table( y, juros_d_nivel)), legend = TRUE, main = "Distribuição do Nível de Juros em relação ao Número de Dias que levou para realizar a 1ª Amortização")

##Gráficos

default_empres <- plot(y ~ valor_emprestado, data = t2, ylab = "Default", xlab = "Valor do Empréstimo", main = "Distribuição de Empréstimo por classe de Default")

default_mat <- plot(y ~ maturidade, data = t2, ylab = "Default", xlab = "Maturidade do Empréstimo", main = "Distribuição dos clusters de Maturidade")


png(filename = "./figures/default_empres.png", width = 500, height = 480, bg = "transparent")
plot(y ~ valor_emprestado, data = t2, ylab = "Default", xlab = "Valor do Empréstimo", main = "Distribuição de Empréstimo por classe de Default")
dev.off()

png(filename = "./figures/default_mat.png", width = 500, height = 480, bg = "transparent")
plot(y ~ maturidade, data = t2, ylab = "Default", xlab = "Maturidade do Empréstimo", main = "Distribuição dos clusters de Maturidade")
dev.off()





