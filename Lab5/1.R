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


fviz_nbclust(scaled_data, kmeans, method = "wss") + ggtitle("Метод локтя")


fviz_nbclust(scaled_data, kmeans, method = "silhouette") + ggtitle("Метод силуэта")

gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 10, K.max = 10, B = 100)
fviz_gap_stat(gap_stat) + ggtitle("Статистика разрыва")


#Алгоритм консенсуса
n_clust <- n_clusters(
  as.data.frame(scaled_data),
  package = c("easystats", "NbClust", "mclust"),
  standardize = FALSE,
  force = TRUE
)

plot(n_clust) + labs(title = "Метод консенсуса")

optimal_k <- 3

#Дендрограмма
dist_matrix <- dist(scaled_data, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, main = "Дендрограмма", xlab = "Страны")
rect.hclust(hc, k = optimal_k, border = 2:(optimal_k + 1))
clusters <- cutree(hc, k = optimal_k)

#Столбчатая диаграмма
normalized_data <- as.data.frame(scaled_data)
agg_norm <- aggregate(normalized_data, by = list(Cluster = clusters), FUN = mean)
barplot(
  as.matrix(agg_norm[, -1]), 
  beside = TRUE, 
  col = 2:(optimal_k + 1),
  main = "Средние значения по кластерам",
  xlab = "Переменные",
  ylab = "Нормализованные значения",
  legend.text = paste("Кластер", agg_norm$Cluster),
  args.legend = list(x = "topright", inset = c(0, 0))
)

#Боксплоты
par(mfrow = c(2, 3))
for (i in 1:ncol(growth_data)) {
  boxplot(growth_data[, i] ~ clusters, 
          main = colnames(growth_data)[i],
          col = 2:(optimal_k + 1),
          xlab = "Кластер",
          ylab = "Значение")
}
par(mfrow = c(1, 1))

#K-means
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = optimal_k, nstart = 25)

#Визуализация кластеров
fviz_cluster(
  kmeans_result, 
  data = scaled_data, 
  ellipse.type = "norm", 
  geom = "point",
  main = "K-means кластеризация",
  ggtheme = theme_minimal()
)

fviz_cluster(
  kmeans_result, 
  data = scaled_data, 
  geom = "point",
  main = "K-means кластеризация",
  ggtheme = theme_minimal()
)

#Scatterplot
pairs(scaled_data, col = kmeans_result$cluster, pch = 19, main = "Матрица scatterplot")

#3D
scatterplot3d(
  scaled_data[, 1:3], 
  color = kmeans_result$cluster, 
  pch = 19,
  main = "3D кластеризация",
  xlab = "growth", 
  ylab = "rgdp60", 
  zlab = "tradeshare"
)
