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
  
  assign(paste0("df_", year), df)
  assign(paste0("df_filtered_", year), df_filtered)
}


combined_df <- do.call(rbind, lapply(years, function(year) {
  df <- get(paste0("df_filtered_", year))
  df$Year <- year
  return(df)
}))

#Графики
index_list <- list(
  list(
    var = "Purchasing_Power_Index",
    title = "Индекс покупательной способности"
  ),
  list(
    var = "Pollution_Index",
    title = "Индекс загрязнения"
  ),
  list(
    var = "Property_Price_to_Income_Ratio",
    title = "Отношение цены на жилье к доходу"
  ),
  list(
    var = "Cost_of_Living_Index",
    title = "Индекс прожиточного минимума"
  ),
  list(
    var = "Safety_Index",
    title = "Индекс безопасности"
  ),
  list(
    var = "Health_Care_Index",
    title = "Индекс медицинского обслуживания"
  ),
  list(
    var = "Traffic_Commute_Time_Index",
    title = "Индекс времени движения на дороге"
  ),
  list(
    var = "Climate_Index",
    title = "Климатический индекс",
    filter = function(df) {
      subset_df <- subset(df, Year >= 2016)
      subset_df$Climate_Index <- as.numeric(as.character(subset_df$Climate_Index))
      subset_df
    }
  )
)


colors <- rainbow(length(target_countries))
par(mfrow = c(1, 1))


for (index in index_list) {
  if (!is.null(index$filter)) {
    current_df <- index$filter(combined_df)
  } else {
    current_df <- combined_df
  }
  
  plot(NA,
       xlim = range(current_df$Year), 
       ylim = range(current_df[[index$var]], na.rm = TRUE),
       xlab = "Год", 
       ylab = "Индекс",
       main = paste0(index$title, " (", min(current_df$Year), "-", max(current_df$Year), ")")
  )
  
  for (i in seq_along(target_countries)) {
    country_data <- subset(current_df, Country == target_countries[i])
    lines(country_data$Year, country_data[[index$var]], 
          type = "o", col = colors[i], lwd = 2)
  }
  
  legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")}