library(rvest)
library(stringr)

url <- "https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/"
filename <- "best_museums.html"
download.file(url, destfile = filename, quiet = TRUE)


content <- read_html(filename, encoding = "UTF-8")


museum_names <- content %>%
  html_nodes(xpath = "/html/body/div[8]/div/section/div/div[3]/div[2]/article/div[2]/h3/a/span") %>%
  html_text()

museum_addresses <- content %>%
  html_nodes(xpath = "/html/body/div[8]/div/section/div/div[3]/div[2]/article/div[2]/address/span") %>%
  html_text()

museum_addresses <- sapply(museum_addresses, function(x) {
  str_trim(str_replace_all(x, "", ""))
})

museum_descriptions <- content %>%
  html_nodes(xpath = "/html/body/div[8]/div/section/div/div[3]/div[2]/article/div[2]/div[1]/p") %>%
  html_text()

museum_link <- content %>%
  html_nodes(xpath = "/html/body/div[8]/div/section/div/div[3]/div[2]/article/div[1]/a/@href") %>%
  html_text()


museum_data <- data.frame(
  Name = museum_names,
  Address = museum_addresses,
  Description = museum_descriptions,
  Link = museum_link
)

fix(museum_data)
