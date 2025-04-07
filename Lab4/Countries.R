library(rvest)

target_countries <- c("Latvia", "Estonia", "Lithuania", "Bulgaria", "Hungary")
years <- 2014:2021


for (year in years) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  
  filename <- paste0("quality_of_life_", year, ".html")
  download.file(url, destfile = filename, quiet = TRUE)
  
  content <- read_html(filename)
  nodes <- html_nodes(content, "table")
  df <- html_table(nodes[[2]], fill = TRUE)
  df <- as.data.frame(df)
  
  df <- df[, -1]
  
  colnames(df) <- c(
    "Country", "Quality_of_Life_Index", 
    "Purchasing_Power_Index", "Safety_Index", 
    "Health_Care_Index", "Cost_of_Living_Index",
    "Property_Price_to_Income_Ratio", 
    "Traffic_Commute_Time_Index", 
    "Pollution_Index", "Climate_Index", "Extra"
  )[1:ncol(df)]
  
  df_filtered <- df[df$Country %in% target_countries, ]
  
  assign(paste0("df_", year), df_filtered)
  assign(paste0("df_filtered_", year), df_filtered)
}


#собираем данные из df_filtered 
combined_df <- do.call(rbind, lapply(years, function(year) {
  df <- get(paste0("df_filtered_", year))
  df$Year <- year
  return(df)
}))

combined_df$Purchasing_Power_Index <- as.numeric(as.character(combined_df$Purchasing_Power_Index))
colors <- rainbow(length(target_countries))



#индекс покупательной способности
plot(
  NA, xlim = range(years), ylim = range(combined_df$Purchasing_Power_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Purchasing Power Index",
  main = "Индекс покупательной способности (2014–2021)"
)


for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Purchasing_Power_Index, type = "o", col = colors[i], lwd = 2)
}

legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")

#индекс загрязнения
plot(
  NA, xlim = range(years), ylim = range(combined_df$Pollution_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Pollution Index",
  main = "Индекс загрязнения (2014–2021)"
)

for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Pollution_Index, type = "o", col = colors[i], lwd = 2)
}

legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")


#отношение цены на жилье к доходу
plot(
  NA, xlim = range(years), ylim = range(combined_df$Property_Price_to_Income_Ratio, na.rm = TRUE),
  xlab = "Год", ylab = "Property Price to Income Ratio",
  main = "Отношение цены на жилье к доходу (2014–2021)"
)

for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Property_Price_to_Income_Ratio, type = "o", col = colors[i], lwd = 2)
}

legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")


#индекс прожиточного минимума
plot(
  NA, xlim = range(years), ylim = range(combined_df$Cost_of_Living_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Cost of Living Index",
  main = "Индекс прожиточного минимума (2014–2021)"
)

for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Cost_of_Living_Index, type = "o", col = colors[i], lwd = 2)
}

legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")


#индекс безопасности
plot(
  NA, xlim = range(years), ylim = range(combined_df$Safety_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Safety Index",
  main = "Индекс безопасности (2014–2021)"
)

for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Safety_Index, type = "o", col = colors[i], lwd = 2)
}

legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")


#индекс медицинского обслуживания
plot(
  NA, xlim = range(years), ylim = range(combined_df$Health_Care_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Health Care Index",
  main = "Индекс медицинского обслуживания (2014–2021)"
)

for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Health_Care_Index, type = "o", col = colors[i], lwd = 2)
}

legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")



#индекс времени движения на дороге
plot(
  NA, xlim = range(years), ylim = range(combined_df$Traffic_Commute_Time_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Traffic Commute Time Index",
  main = "Индекс времени движения на дороге (2014–2021)"
)

for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Traffic_Commute_Time_Index, type = "o", col = colors[i], lwd = 2)
}

legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")

#климатический индекс
combined_df_filtered <- subset(combined_df, Year >= 2016)
combined_df_filtered$Climate_Index <- as.numeric(as.character(combined_df_filtered$Climate_Index))

plot(
  NA, xlim = range(2016:2021), ylim = range(combined_df_filtered$Climate_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Климатический индекс",
  main = "Климатический индекс (2016–2021)"
)

for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df_filtered, Country == target_countries[i])
  lines(country_data$Year, country_data$Climate_Index, type = "o", col = colors[i], lwd = 2)
}

legend("bottomright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")


