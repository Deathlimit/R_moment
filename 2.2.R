library(readxl)

data_table <- read_excel("C:/Users/rim/Documents/R_lab2.2/results.xlsx")

data <- data_table[-c(1,2)]

data


for (col in names(data)) {
  column_data <- data[[col]]
  
  
  data_for_moda <- table(column_data)
  moda <- as.numeric(names(data_for_moda)[which.max(data_for_moda)])
  
  
  median <- median(column_data)
  
  mean <- mean(column_data)

  min <- min(column_data)
  
  max <- max(column_data)
  
  
  cat("Столбец:", col, "\n")
  cat("Мода:", moda, "\n")
  cat("Медиана:", median, "\n")
  cat("Среднее:", mean, "\n")
  cat("Минимальное:", min, "\n")
  cat("Максимальное:", max, "\n")
  cat("\n")
}



#---------------------------#

sorted <- data[order(data$`Добрый Cola`),]
sorted

#-------------------------#

subdataset <- data[data$`Добрый Cola` <= 3,]
subdataset


for (col in names(subdataset)) {
  column_data <- subdataset[[col]]
  
  
  data_for_moda <- table(column_data)
  moda <- as.numeric(names(data_for_moda)[which.max(data_for_moda)])
  
  
  median <- median(column_data)
  
  
  mean <- mean(column_data)
  
  
  cat("Столбец:", col, "\n")
  cat("Мода:", moda, "\n")
  cat("Медиана:", median, "\n")
  cat("Среднее:", mean, "\n")
  cat("\n")
  
}


hist(subdataset$`Добрый Cola`, main = paste("Рис 1. Гистограмма для столбца: Добрый Cola"), 
     xlab = "Добрый Cola", ylab = "Количество студентов", col = "lightblue", border = "black")

boxplot(subdataset$`Добрый Cola`, col = "lightblue", border = "black",
        main = "Рис. 2. Боксплот оценок для столбца: Добрый Cola",
        ylab = "Оценка для Добрый Cola")



#------------------------#

#Слияние#
data_fanta <- data_table[c(2, 5)] 
data_fanta

data_pepsi <- data_table[c(2,6)]
data_pepsi

data_merged <- merge(data_fanta, data_pepsi, by = "Фамилия")
data_merged



#Новая строка#
new_row <- data.frame(Фамилия = "Тестов", Fanta = 10, Pepsi = 1)
new_row

data_added <- rbind(data_merged, new_row);
data_added


#Исключение столбца#
data_without_pepsi <- data_added[!names(data_added) %in% c("Pepsi")]
data_without_pepsi


#Формирование подмножества данных, где Fanta=10#
subset_data <- subset(data_merged, Fanta == 10)
subset_data