library(ranger)
library(tidyverse)
donnees <- rio::import("data/data_clean1.csv")
data <- dplyr::as_tibble(donnees)

#supprimer variable ID_DOSSIER_SIN_CONTRAT
data <- data %>% select(-ID_DOSSIER_SIN_CONTRAT, 
                        -target_rcc,
                        -target_acc, 
                        -charge_acc,
                        -charge_rcc, 
                        -ALL_GARANTIE
                        )
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
data <- rfImpute(target_90k ~ ., data)
# Découpage du jeu de données
dt = sort(sample(nrow(data), nrow(data)*.7))
data <- data %>% drop_na()

library(dplyr)
subdf <- data %>% filter(rownames(data) <= 50) %>% droplevels()

train<-data[dt,] 
test<-data[-dt,] 

library(ggplot2)
library(randomForest)
listFeatures = list(colnames(data %>% select(-target_90k) ))
extractFeatures <- function(data) {
  features <- listFeatures
  fea <- data[,features]
  
  return(fea)
}


rf <- randomForest(train, as.factor(train$target_90k), ntree=100, na.action = na.roughfix,importance=TRUE)

submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)
