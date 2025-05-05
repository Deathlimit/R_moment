library(e1071)     
library(caret)       
library(rpart)       
library(rpart.plot) 
library(randomForest) 
library(party)
library(klaR)
library(factoextra)
library(NbClust)
library(cluster)
library(scatterplot3d)
library(parameters)
library(mclust)
library(easystats)


growth_data <- read.csv("C:/Users/rim/Documents/R_lab5/GrowthSW.csv", row.names = 1)
head(growth_data)


#Дескриптивный анализ
summary(growth_data)

#Нормализация данных
scaled_data <- scale(growth_data)

optimal_k <- 3
dist_matrix <- dist(scaled_data, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
clusters <- cutree(hc, k = optimal_k)



dataset <- as.data.frame(growth_data)
dataset$group <- factor(clusters, labels = paste0("Cluster", 1:length(unique(clusters))))

#70/30
set.seed(123)
trainIndex <- createDataPartition(dataset$group, p = 0.75, list = FALSE)
trainData <- dataset[trainIndex, ]
testData  <- dataset[-trainIndex, ]

#Наивный Байесовский классификатор
nb_model <- naiveBayes(group ~ ., data = trainData)
nb_pred <- predict(nb_model, newdata = testData)
nb_confmat <- confusionMatrix(b_nb_pred, testData$group)
nb_confmat

klaR_nb_model <- NaiveBayes(group ~ ., data = trainData)
old.par <- par()                            
layout(matrix(c(1,2,3,4,5,6),3, 2, byrow = TRUE))
plot(klaR_nb_model)


#Дерево
dt_model <- ctree(group ~ ., data = trainData)
plot(dt_model)      
dt_pred <- predict(dt_model,newdata = testData )
dt_confmat <- confusionMatrix(dt_pred, testData$group)
dt_confmat

#Случайный лес
set.seed(123)
rf_model <- randomForest(group ~ ., data = trainData, importance = TRUE)
rf_pred <- predict(rf_model, newdata = testData)
rf_confmat <- confusionMatrix(rf_pred, testData$group)
rf_confmat

#Сравнение
accuracy_results <- data.frame(
  Model = c("Naive Bayes", "Decision Tree", "Random Forest"),
  Accuracy = c(
    nb_confmat$overall["Accuracy"],
    dt_confmat$overall["Accuracy"],
    rf_confmat$overall["Accuracy"])
)
accuracy_results
