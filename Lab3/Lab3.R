library(readxl)

data_table <- read_excel("D:/R/R_lab3/results.xlsx")

data_p <- data_table[, -c(1,2,11,20)]

data_p_matrix <- as.matrix(data_p)
rownames(data_p_matrix) <- data_table$Олимпиада 


data_p_men <- data_p_matrix[, c(1:8)]
data_p_women <- data_p_matrix[, c(9:16)]

data_p_combined <- data_p_men + data_p_women 
colnames(data_p_combined) <- c("First", "Second", "Third", "4", "5", "6", "7", "8")

#Столбчатая диаграмма по количеству мест
par(mar = c(11, 4, 4, 1))

barplot(t(data_p_combined), beside = TRUE, col = rainbow(8), 
        main = "Количество мест 1-8 по каждой Олимпиаде (мужчины и женщины)", 
        ylab = "Количество мест", 
        legend.text = paste("Место", 1:8), args.legend = list(x = "topright"),
        las = 2)

#Круговая диаграмма по количеству первых мест
data_only_first <- data_p_combined[, 1]
data_without_zeros <- data_only_first[data_only_first != 0]
pie(data_without_zeros, main = "Количество первых мест Германии по велоспорту",
    col = rainbow(length(data_without_zeros)), labels = data_without_zeros)
legend("topright", legend = names(data_without_zeros), col = rainbow(length(data_without_zeros)), lwd = 2, pch = 10, title = "Страна")




#Тенденции изменения
male_trends <- data_table[, c(2)]
female_trends <- data_table[, c(11)]
years_trends <- sapply(data_table[, 1], function(x) as.numeric(gsub("[^0-9]", "", x)))
years_trends <- as.numeric(years_trends)

plot(years_trends, male_trends$Мужчины, type = "o", col = "blue", 
     main = "Тенденции изменения количества призовых мест Германии по велоспорту",
     xlab = "Год", ylab = "Количество призовых мест", ylim = c(0, 10), xaxt = "n")
lines(years_trends, female_trends$Женщины, type = "o", col = "red")
legend("topright", legend = c("Мужчины", "Женщины"), col = c("blue", "red"), lty = 1)
axis(1, at = years_trends, labels = years_trends, las = 1)




#Столбчатая диаграмма раздельно (мужчины женшины)
par(mfrow=c(1,2))
par(mar = c(11, 4, 4, 1))
barplot(t(data_p_men[,c(1,2,3)]), beside = TRUE, col = rainbow(3), 
        main = "Количество мест 1-3 по каждой Олимпиаде (Мужчины)", 
        ylab = "Количество мест", 
        legend.text = paste("Место", 1:3), args.legend = list(x = "topright"),
        las = 2)

barplot(t(data_p_women[,c(1,2,3)]), beside = TRUE, col = rainbow(3), 
        main = "Количество мест 1-3 по каждой Олимпиаде (Женщины)", 
        ylab = "Количество мест", 
        legend.text = paste("Место", 1:3), args.legend = list(x = "topright"),
        las = 2)


#Круговая диаграмма раздельно (мужчины женшины)
par(mfrow=c(1,2))

data_mw <- data_table[, c(2,11)]
data_mw_matrix <- as.matrix(data_mw)
rownames(data_mw_matrix) <- data_table$Олимпиада 


data_male_without_zeros <- data_mw_matrix[,1]
data_male_without_zeros <- data_male_without_zeros[data_male_without_zeros != 0]

pie(data_male_without_zeros, main = "Количество призовых мест Германии по велоспорту (Мужчины)",
    col = rainbow(length(data_male_without_zeros)), labels = data_male_without_zeros)
legend("topright", legend = names(data_male_without_zeros), col = rainbow(length(data_male_without_zeros)), lwd = 2, pch = 10, title = "Страна", xpd = TRUE, inset = c(-0.3, 0))

data_female_without_zeros <- data_mw_matrix[,2]
data_female_without_zeros <- data_female_without_zeros[data_female_without_zeros != 0]

pie(data_female_without_zeros, main = "Количество призовых мест Германии по велоспорту (Женщины)",
    col = rainbow(length(data_female_without_zeros)), labels = data_female_without_zeros)
legend("topright", legend = names(data_female_without_zeros), col = rainbow(length(data_female_without_zeros)), lwd = 2, pch = 10, title = "Страна", xpd = TRUE, inset = c(-0.3, 0))

#Графики по всем странам
data_table_all <- read_excel("D:/R/R_lab3/results_all_coutries.xlsx")
filtered_all <- subset(data_table_all, Medal == "Gold")
filtered_all

#Только первые места
medals_by_year_country <- aggregate(Medal ~ Year + Country, data = filtered_all, FUN = length)

countries <- unique(medals_by_year_country$Country)

plot(NULL, 
     xlim = range(medals_by_year_country$Year), 
     ylim = c(0, max(medals_by_year_country$Medal)), 
     xlab = "Год", 
     ylab = "Количество медалей", 
     main = "График изменения спортивных достижений по золотым медалям")

for (country in countries) {
  country_data <- subset(medals_by_year_country, Country == country)
  lines(country_data$Year, country_data$Medal, type = "o", col = which(country == countries), lwd = 2, pch = 10)
}
legend("topright", legend = countries, col = seq_along(countries), lwd = 2, pch = 10, title = "Страна")

#Призовые места
medals_by_year_country_all <- aggregate(Medal ~ Year + Country, data = data_table_all, FUN = length)

countries_all <- unique(medals_by_year_country_all$Country)

plot(NULL, 
     xlim = range(medals_by_year_country_all$Year), 
     ylim = c(0, max(medals_by_year_country_all$Medal)), 
     xlab = "Год", 
     ylab = "Количество медалей", 
     main = "График изменения спортивных достижений по призовым местам")

for (country in countries_all) {
  country_data_all <- subset(medals_by_year_country_all, Country == country)
  lines(country_data_all$Year, country_data_all$Medal, type = "o", col = which(country == countries_all), lwd = 2, pch = 10)
}
legend("topright", legend = countries_all, col = seq_along(countries_all), lwd = 2, pch = 10, title = "Страна")

