#libraries
install.packages('MLmetrics')
install.packages('pROC')
install.packages('ggplot')
install.packages("reshape")
install.packages("ggpubr")
install.packages('pscl')
install.packages("margins")
library(margins)
library(pscl)
library(caret)
library(rpart)
library(ggpubr)
library(MLmetrics)
library(lmtest)
library(pROC)
library(ggplot2)
library(erer)
library(vcd)
library(reshape2)
library(reshape)
library(magrittr)
library(dplyr)
library(readxl)
library(lmtest)
library(AUC)
library(DescTools)
#data preparation
data_good <- subset(dane_do_projektu1, Y == 0)
data_bad <- subset(dane_do_projektu1, Y == 1)
N <- nrow(data_good)
N_test <- 8
data_good_test <- data_good[1:N_test,] 
data_good_learn <- data_good[(N_test+1):N,]
data_bad_test <- data_bad[1:N_test,] 
data_bad_learn <- data_bad[(N_test+1):N,] 

data_learn <- rbind(data_good_learn, data_bad_learn)
data_test <- rbind(data_good_test, data_bad_test)

x=cor(dane_do_projektu1[4:9],method="pearson")
x



data_good<-dane_do_projektu1[1:41,]
data_bad<-dane_do_projektu1[42:82,]
#figure
df<- data.frame(upadla=data_bad$WO,dobra=data_good$WO)
df1<-data.frame(upadla=data_bad$OZ,dobra=data_good$OZ)
df2<-data.frame(upadla=data_bad$ROA,dobra=data_good$ROA)
df3<-data.frame(upadla=data_bad$BP,dobra=data_good$BP)
df4<-data.frame(upadla=data_bad$WRZ,dobra=data_good$WRZ)
df5<-data.frame(upadla=data_bad$WOZ,dobra=data_good$WOZ)
class(dada3$value)
dada <- melt(df)
pp<- ggplot(dada, aes(x=value, fill=variable)) +
  geom_density(alpha=.9)+
  ggtitle("WO(Wskaznik wydajnosci operacyjnej majatku ogolem)")
pp
dada1 <- melt(df1)
pp1<- ggplot(dada1, aes(x=value, fill=variable)) +
  geom_density(alpha=.9) +
  xlim(0,8)+
  ggtitle("OZ(Wskaznik ogolnego zadluzenia)")
pp1
dada2 <- melt(df2)
pp2<- ggplot(dada2, aes(x=value, fill=variable)) +
  geom_density(alpha=.9) +
  xlim(0,00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005)+
  ggtitle("ROA(rentowosc)")
pp2
dada3 <- melt(df3)
pp3<- ggplot(dada3, aes(x=value, fill=variable)) +
  geom_density(alpha=.9) +
  xlim(0,3)+
  ggtitle("BP(Plynnosc biezaca)")
pp3
dada4 <- melt(df4)
pp4<- ggplot(dada4, aes(x=value, fill=variable)) +
  geom_density(alpha=.9) +
  xlim(0,3)+
  ggtitle("WRZ(Rotacja zobowiazan)")
pp4
dada5 <- melt(df5)
pp5<- ggplot(dada5, aes(x=value, fill=variable)) +
  geom_density(alpha=.9) +
  xlim(0,8)+
  ggtitle("WOZ(wskaznik obslugi zadluzenia)")
pp5

figure <- ggarrange(pp, pp1,pp2,pp3,pp4,pp5,
                    labels = c("A", "B","C", "D","E","F"),
                    ncol = 2, nrow = 3)
figure
#models
model <- lm(Y~WO+OZ+WRZ+ROA+WOZ+BP, data = dane_do_projektu1)
summary(model)

model2 <- lm(Y~log(WOZ)+log(OZ)+WRZ+ROA+WO+BP, data = dane_do_projektu1)
summary(model2)

model.logit.learn.full <- glm(Y ~ BP+WO+OZ+WRZ, data = data_learn,  binomial(link = "logit"), x=TRUE)
summary(model.logit.learn.full)
pseudo_r2_1 <- pR2(model.logit.learn.full)
bic_value <- BIC(model.logit.learn.full)
model.logit.learn.short<- glm(Y ~ OZ+WO+BP, data = data_learn,  binomial(link = "logit"), x=TRUE)
summary(model.logit.learn.short)
bic_value <- BIC(model.logit.learn.short)
pseudo_r2_2 <- pR2(model.logit.learn.short)

lrtest(model.logit.learn.full, model.logit.learn.short)

marginal_effects <- margins(model.logit.learn.full)
summary(marginal_effects)
marginal_effects <- margins(model.logit.learn.short)
summary(marginal_effects)
model.logit.learn.short
#predictions for learn_dataset
pr_logit <- predict(model.logit.learn.short, data_learn, se = TRUE)
l <- cbind(data_learn, pr_logit)
l <- mutate(l, prob = plogis(fit))
l <- mutate(l, pred = ifelse(prob < 0.5, 0, 1))
with(data_test, table(l$pred, data_learn$Y))

confusionMatrix(as.factor(l$pred), as.factor(data_learn$Y))
#predictions for test_dataset

pr_logit1 <- predict(model.logit.learn.short, data_test, se = TRUE)
l_1 <- cbind(data_test, pr_logit1)
l_1 <- mutate(l_1, prob = plogis(fit))
l_1 <- mutate(l_1, pred = ifelse(prob < 0.5, 0, 1))
with(data_test, table(l_1$pred, data_test$Y))

confusionMatrix(as.factor(l_1$pred), as.factor(data_test$Y))
#presicion estimation

roc_curve <- roc(data_test$Y, l_1$pred)

auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")
roc_df <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

# Tworzenie wykresu ROC za pomocą ggplot2
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  labs(title = "Krzywa ROC", subtitle = paste("AUC =", round(auc_value, 2)),
       x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  coord_equal()
