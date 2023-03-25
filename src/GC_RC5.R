# exemplo a partir de link:
# https://rstudio-pubs-static.s3.amazonaws.com/227022_7074da86c0a5441eb1d0869e61a8fc8c.html#
library(fBasics) #summary statistics
library(plotly) #data visulization
library(dplyr) #count function
library(C50) #C50 Decision Tree algorithm
library(gmodels) #CrossTable()
library(caret) #Confusion Matrix
library(rmarkdown)

# carrega a base do arquivo para uma tabela na variável dat
credit <- read.table("./SouthGermanCredit.asc", header=TRUE) 
nam_evtree <- c("status", "duration", "credit_history", "purpose", "amount", 
                "savings", "employment_duration", "installment_rate",
                "personal_status_sex", "other_debtors",
                "present_residence", "property",
                "age", "other_installment_plans",
                "housing", "number_credits",
                "job", "people_liable", "telephone", "foreign_worker",
                "default")
names(credit) <- nam_evtree

credit$default[credit$default == 1] <- "no"
credit$default[credit$default == 0] <- "yes"

for (i in setdiff(1:21, c(2,5,13)))
  credit[[i]] <- factor(credit[[i]])

#Below is a partial table preview of the dataset:
knitr::kable(head(credit), caption = "Credit Information Dataset")

#The general structure of the dataset:
str(credit)

#Below are some useful tables that offers summary statistics from the loan features such as checking, savings, duration, amount, and default.
knitr::kable(data.frame(table(credit$savings)), Caption = "Savings Balance", col.names = c("Savings Balance", "Frequency"))

#The data visualization will focus on identifying patterns of key features which clearly distinguishes an applicant’s default status.
credit %>% count(default, savings) %>% plot_ly(x = ~default, y = ~n, color = ~savings, type = "bar")

# To ensure results can be reproduced
set.seed(123)

# Sample 900 integers randomly from 1000
train_sample <- sample(1000,900)

#Subset randomly into training and test
credit_train <- credit[train_sample ,]
credit_test <- credit[-train_sample , ]

#Let’s check if both the train and test set have rather even split of the class levels. This is to prevent training bias.
#training set
knitr::kable(data.frame(prop.table(table(credit_train$default))), caption = "Training Set Proportion", col.names = c("Default Status", "Proportion"), digits = 3)

#testing set 
knitr::kable(data.frame(prop.table(table(credit_test$default))), caption = "Test Set Proportion", col.names = c("Default Status", "Proportion"), digits = 4)

#building the classifier

credit_model <- C5.0(credit_train[-21], credit_train$default)
credit_model
summary(credit_model)

#making prediction
credit_pred <- predict(credit_model, credit_test, type = "prob")

## inicio enxerto
Teste = cbind(credit_test,credit_pred)
pred.val = prediction(credit_pred ,Teste$credit_risk)

# calculo da auc (area under the curve)
auc = performance(pred.val,"auc")

# Plota curva ROC
performance = performance(pred.val, "tpr", "fpr")
plot(performance, col = "blue", lwd = 5)

#Calculo Estatística KS
ks <- max(attr(performance, "y.values")[[1]] - (attr(performance, "x.values")[[1]]))
ks

# Comando alternativo para plotar curva ROC:
plotROC(credit_test$credit_risk, credit_pred)

# Usa função nativa para obter o ponto de corte ideal otimizando par ambos
ponto_corte <- optimalCutoff(credit_test$credit_risk, credit_pred, optimiseFor = "Both")
#ponto_corte <- 0.381111
ponto_corte 

#confusion matrix com ponto de corte
table(credit_test$credit_risk, credit_pred > ponto_corte)

# Especificade diz quantos que são verdadeiros e são classificados como tal (taxa de acerto dos casos positivos)
# ex: specificity = .80 é capez acertar 80% dos casos que são verdadeiros, mas erra 20% dos casos (falso positivo)
specificity(credit_test$credit_risk, credit_pred, threshold = ponto_corte)

# Sensitividade diz quantos que são falsos e são classificados como tal (taxa de acerto dos casos negativos)
# ex: sensitivity = .60 é capaz de assertar 60% dos casos que não possuem a doença
sensitivity(credit_test$credit_risk, credit_pred, threshold = ponto_corte)
## fim enxerto


#Evaluation
confusionMatrix(credit_pred, unclass(credit_test$default),  dnn = c("Predicted", "Actual"))

#making the classifier
credit_boost10 <- C5.0(credit_train[-21], credit_train$default, trials = 10)

#making the prediction
credit_boost_pred10 <- predict(credit_boost10, credit_test)

#Evaluation
credit_boost_pred10
credit_test$default
confusionMatrix(credit_boost_pred10, credit_test$default)
