library(readxl)
library(tidyverse)
library(caret)
library(plotROC)
library(rpart)
library(rattle)
library(ggplot2)
library(rpart.plot)
library(rattle)
#library(sjPlot)
#library(sjmisc)
#library(sjlabelled)

load("D.RData")
head(D)

set.seed(1)

d_idx = createDataPartition(D$reg_RTE_salad, p = 0.70, list = FALSE)
train = D[d_idx, ]
test = D[-d_idx, ]


# Logit
full.model <- glm(reg_RTE_salad~gender+age3+income+seniority+hh_size+knowledge_I+
                  fitness+howlearn_adv+howlearn_social+buy_supermarket+buy_convenience_store+
                  consumption_lunch_box+consumption_snacking+SN_I,
                  data = train, family = binomial(link="logit"))

summary(full.model)

predicted_full = as.factor(ifelse(predict(full.model, newdata = test,type = "response")> 0.5, "1", "0"))
cmLogit_full <-confusionMatrix(predicted_full, test$reg_RTE_salad)
print(cmLogit_full)

ioRoc <- data.frame(pred=as.numeric(predicted_full),obs=as.numeric(test$reg_RTE_salad))

basicplot <- ggplot(ioRoc, aes(d = pred, m =obs)) + geom_roc()
basicplot + labs(title="LOGIT",
                 x ="False Positive Rate", y = "True Positive Rate")+
  annotate("text", x = .60, y = .55, 
           label = paste("AUC =", round(calc_auc(basicplot)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .1))


# NNET tuning
set.seed(50)
tuning = expand.grid(unit=round(runif(3, min = 0, max = 100 )))
tuning = cbind(tuning, performance = rep(0, nrow(tuning)))

for (g in  1:(nrow(tuning))) {
NN = train(reg_RTE_salad ~ ., 
             data=train, 
             method="nnet",#"svmLinear", 
             trControl = trainControl(method='repeatedcv', 
                                      number=tuning$unit[g], 
                                      repeats=5,
                                      search = 'random'))
  
# PREDICTION
predicted_NN = predict(NN, newdata = test)
cmNN <-confusionMatrix(predicted_NN, test$reg_RTE_salad)
print(cmNN)
tuning$performance[g] = cmRF$overall[1]
View(tuning)
}
