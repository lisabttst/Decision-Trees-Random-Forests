rm(list=ls())
install.packages('party')
install.packages('caret')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('ggplot2')
install.packages('rattle')
install.packages('e1071')
install.packages("Metrics")
library(party)
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(rattle)
library(e1071)
library(Metrics)
library(pROC)
library(Rarity)

#QUESTION 1 : ANALYSE EXPLORATOIRE DESCRIPTIVE - 5-Breast Cancer

BC <- read.csv('Winconsin.csv', sep=';')

#Démarrons l'analyse
str(BC)
parametres <- names(BC)[1:10]
print(parametres)
missing_values <- sapply(BC, function(x) sum(is.na(x)))
print(missing_values) 
# Il n'y a aucune valeurs manquantes

summary(BC)
table(BC$Diagnosis)

# Nous allons effectuer des analyses sur la moyenne des parametres, donc sur les valeurs des colonnes 1 a 10
#Nous voulons savoir l'ordre de grandeur des valeurs par parametres, pour visualiser des variables de tailles similaires ensemble
max_val <- apply(BC[, 1:10], 2, max)
ordegrandeur<- floor(log10(max_val))
print(ordegrandeur)
par(mfrow=c(1,5))
boxplot(BC[, 1:2], main="Boxplots of Columns 1 and 2", las=2)
boxplot(BC[, 3], main="Boxplots of Column 3 = mean perimeters", las=2)
boxplot(BC[, 4], main="Boxplots of Column 4 = mean area ", las=2)
boxplot(BC[, 5:9], main="Boxplots of Columns 5 to 9", las=2)
boxplot(BC[, 10], main="Boxplots of Column 10 = mean fractal dimension", las=2)
par(mfrow=c(1,1))

Val_AB <- sapply(BC[, 1:10], function(col) {
  if (is.numeric(col)) {
    Q1 <- quantile(col, 0.25, na.rm = TRUE)
    Q3 <- quantile(col, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    return(sum(col < lower_bound | col > upper_bound, na.rm = TRUE))
  } else {
    return(NA)
  }
})
sum(Val_AB)

#Nous pouvons analyser les correlations entre les differents parametres
cor_matrice <- cor(BC[, sapply(BC, is.numeric)]) 
print(cor_matrice)

# Nous regardons comment les données sont distribuées
skewness_vals <- sapply(BC[, sapply(BC, is.numeric)], skewness)
kurtosis_vals <- sapply(BC[, sapply(BC, is.numeric)], kurtosis)
print(skewness_vals[1:10])
print(kurtosis_vals[1:10])

#QUESTION 2 : CONSTRUCTION CLASSIFIEUR


#1 Division des données
set.seed(123) 
splitIndex <- createDataPartition(BC$Diagnosis, p = 0.7, list = FALSE)
trainData <- BC[splitIndex, ]
testData <- BC[-splitIndex, ]
trainData$Diagnosis <- factor(trainData$Diagnosis, levels = c("M", "B"))
testData$Diagnosis <- factor(testData$Diagnosis, levels = c("M", "B"))


#2 Construction et optimisation de l'arbre en utilisant la validation croisée
fitControl <- trainControl(method = "cv", number = 5)
fit <- train(Diagnosis ~ ., data=trainData, method="rpart", trControl=fitControl,
             tuneGrid=data.frame(cp=seq(0.01, 0.5, 0.01)))
best_cp <- fit$bestTune$cp
tree <- rpart(Diagnosis ~ ., data=trainData, cp=best_cp)

# Visualiser l'arbre élagué
rpart.plot(tree)
print(tree)

#predictions
predictions <- predict(tree, testData, type="class")
cm <- confusionMatrix(predictions, testData$Diagnosis)
print(cm)

#Predictions avec probabilités
#Recherche du seuil pour maximiser la sensibilité
library(pROC)
roc_result <- roc(testData$Diagnosis, predict(tree, newdata = testData, type = "prob")[, 2], levels = c("B", "M"))
plot(roc_result)
roc_coords <- coords(roc_result, "all", ret = c("threshold", "sensitivity", "specificity"))
filtered_coords <- roc_coords[roc_coords$threshold >= 0 & roc_coords$threshold <= 1, ]
optimal_coords <- filtered_coords[which.max(filtered_coords$sensitivity), ]
optimal_threshold <- optimal_coords$threshold
optimal_sensitivity <- optimal_coords$sensitivity
optimal_sensitivity
optimal_threshold

# QUESTION 3 : EVALUATION DU CLASSIFIEUR

#A
print(cm)

#B
roc_obj <- roc(testData$Diagnosis, as.numeric(predictions), levels=c("B", "M"))
plot.roc(roc_obj, main="Courbe ROC", col="blue")
auc(roc_obj)

##  PROBLEME IV ##
library(randomForest)
library(ranger)
library(pROC)


#Question 1
#1 RANDOM FOREST
# Réglage de l'hyperparamètre pour randomForest
tuneGridRandomForest <- expand.grid(mtry = 2:7)
fitControlRandomForest <- trainControl(method = "cv", number = 5, search = "grid")

# Entraînement du modèle avec randomForest
set.seed(123)
randomForestFit <- train(Diagnosis ~ ., data = trainData, method = "rf", 
                         trControl = fitControlRandomForest, tuneGrid = tuneGridRandomForest)
# Prédiction sur le jeu de test pour randomForest
predictions_rf_raw <- predict(randomForestFit, newdata = testData)
predictions_rf_prob <- predict(randomForestFit, newdata = testData, type='prob')

# Matrice de confusion pour randomForest
conf_matrix_rf <- confusionMatrix(predictions_rf_raw, testData$Diagnosis)
conf_matrix_rf

print(randomForestFit)
plot(randomForestFit)

#2 RANGER
# Réglage de l'hyperparamètre pour ranger
tuneGridRanger <- expand.grid(mtry = c(2:7), splitrule = c("gini", "extratrees"), min.node.size = c(1, 5, 10))
fitControlRanger <- trainControl(method = "cv", number = 5, search = "grid")

# Entraînement du modèle avec ranger
set.seed(123)
rangerFit <- ranger(Diagnosis ~ ., data = trainData, num.trees = 200, mtry = 3, min.node.size = 1, importance = 'impurity')

# Prédiction sur le jeu de test pour ranger
predictions_ranger <- predict(object = rangerFit, data = testData, type = "response", num.threads = 1, verbose = TRUE)

predicted_labels <- predictions_ranger$predictions

# Matrice de confusion pour ranger
conf_matrix_ranger <- confusionMatrix(predicted_labels, testData$Diagnosis)
conf_matrix_ranger

#Obtenir les probabilites de prediction
rangerFit_prob <- ranger(Diagnosis ~ ., data = trainData, num.trees = 200, mtry = 3, min.node.size = 1, importance = 'impurity', probability = TRUE)
predictions_ranger_prob <- predict(object = rangerFit_prob, 
                              data = testData, 
                              type = "response", 
                              num.threads = 1, 
                              verbose = TRUE) 
print(rangerFit_prob)

#QUESTION 2
# Calcul et tracé des courbes ROC pour chaque modèle
probabilities_ranger <- predictions_ranger_prob$predictions[, "M"]
probabilities_rf <- predictions_rf_prob[, "M"]
roc_rf <- roc(response = testData$Diagnosis, predictor = probabilities_rf, levels = c("B", "M"))
roc_ranger <- roc(response = testData$Diagnosis, predictor = probabilities_ranger, levels = c("B", "M"))

# Calcul de l'AUC pour chaque modèle
auc_rf <- auc(roc_rf)
auc_ranger <- auc(roc_ranger)

# Tracer les courbes ROC
plot(roc_rf, col = "red")
plot(roc_ranger, add = TRUE, col = "blue")
legend("bottomright", legend = c(paste("Random Forest, AUC :", round(auc_rf, digits = 3)), paste("Rager, AUC :", round(auc_ranger, digits = 3))), col = c("red", "blue"), lwd = 2)

# Calcul des métriques de précision et de rappel
precision_rf <- conf_matrix_rf$byClass['Precision']
recall_rf <- conf_matrix_rf$byClass['Recall']
precision_ranger <- conf_matrix_ranger$byClass['Precision']
recall_ranger <- conf_matrix_ranger$byClass['Recall']
print(precision_rf)
print(precision_ranger)
print(recall_rf)
print(recall_ranger)

# Random Forest importance
importance_rf <- varImp(randomForestFit)
plot(importance_rf)

# Ranger importance
importance_ranger <- rangerFit$variable.importance
barplot(importance_ranger, main="Variable Importance - Ranger", las=2)


# Créer une nouvelle dataframe avec les prédictions et les vraies valeurs
comparison_df <- data.frame(
  TrueValue = testData$Diagnosis,
  RF_Prediction = predictions_rf_prob[,2], # la classe positive pour RF
  Ranger_Prediction = predictions_ranger_prob$predictions[,2] # la classe positive pour Ranger
)

# Calculer un score de prédiction binaire basé sur un seuil, par exemple 0.5
comparison_df$RF_Class = ifelse(comparison_df$RF_Prediction > 0.5, "Positive", "Negative")
comparison_df$Ranger_Class = ifelse(comparison_df$Ranger_Prediction > 0.5, "Positive", "Negative")

# Créer une table de contingence pour comparer les classifications
table(comparison_df$RF_Class, comparison_df$Ranger_Class)

# Visualiser les prédictions avec un graphique à points
library(ggplot2)
ggplot(comparison_df, aes(x = RF_Prediction, y = Ranger_Prediction, color = TrueValue)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("Positive" = "red", "Negative" = "blue")) +
  theme_minimal() +
  labs(title = "Comparaison des prédictions Random Forest vs Ranger",
       x = "Probabilité prédite Random Forest",
       y = "Probabilité prédite Ranger",
       color = "Valeur vraie")

