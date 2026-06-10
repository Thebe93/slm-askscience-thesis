library(rvest)
library(dplyr)

setwd("C:\\Users\\thebe\\OneDrive\\Desktop\\Corsi Trento\\Tesi\\Python\\Data")

url_phy <- "https://en.wikipedia.org/wiki/Glossary_of_physics"

page <- read_html(url_phy)

phy_terms <- page %>%
  html_nodes("dl dt") %>%
  html_text(trim = TRUE) %>%
  unique()

head(terms, 20)
length(terms)


url_bio <- "https://en.wikipedia.org/wiki/Glossary_of_biology"

page_bio <- read_html(url_bio)

bio_terms <- page_bio %>%
  html_nodes("dl dt") %>%
  html_text(trim = TRUE) %>%
  unique()

head(bio_terms, 20)
length(bio_terms)

# Cleaning of data
clean_terms <- function(x){
  x <- gsub(" –.*", "", x)
  x <- gsub("\\s+\\(.*\\)", "", x)
  x <- trimws(x)
  tolower(x)
}

physics_terms <- clean_terms(terms)
biology_terms <- clean_terms(bio_terms)

write.csv(physics_terms, "physics_terms.csv", row.names = FALSE)
write.csv(biology_terms, "biology_terms.csv", row.names = FALSE)