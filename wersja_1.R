
# Instalacja pakietów ----
# install.packages(c("tm", "tidytext", "stringr", "dplyr", "readr", "SnowballC"))
# install.packages("stopwords")
# install.packages("ggplot2")

# Ładowanie wymaganych pakietów ----
library(tm)
library(tidytext)
library(stringr)
library(dplyr)
library(readr)
library(SnowballC)
library(stopwords)
library(ggplot2)
# Wczytanie danych ----

konstytucja <- read_csv("konstytucja_artykuly.csv", show_col_types = FALSE)

custom_stopwords <- c(
  "art",
  "ust",
  "pkt",
  "poz",
  "nr",
  "dnia",
  "roku",
  "r",
  "dz",
  "u",
  "kancelaria",
  "sejmu",
  "str",
  "s",
  "opracowano",
  "podstawie",
  "konstytucja",
  "rzeczypospolitej",
  "polskiej",
  "polska",
  "polski"
)

# 1. Wstepny stemming ----

popraw_formy_prawne <- function(words) {
  
  words <- ifelse(words %in% c("prawo", "prawa", "prawem", "prawu", "prawach", "prawami"),
                  "prawo", words)
  
  words <- ifelse(words %in% c("wolność", "wolnosci", "wolności", "wolnością", "wolnosciach",
                               "wolnościach", "wolnosciami", "wolnościami"),
                  "wolnosc", words)
  
  words <- ifelse(words %in% c("obowiązek", "obowiazek", "obowiązku", "obowiazku",
                               "obowiązki", "obowiazki", "obowiązkiem", "obowiazkiem",
                               "obowiązków", "obowiazkow"),
                  "obowiazek", words)
  
  words <- ifelse(words %in% c("zakaz", "zakazu", "zakazem", "zakazy", "zakazów", "zakazow",
                               "zakazuje", "zakazane", "zakazana", "zakazany"),
                  "zakaz", words)
  
  words <- ifelse(words %in% c("ograniczenie", "ograniczenia", "ograniczeniu",
                               "ograniczeniem", "ograniczeń", "ograniczen",
                               "ograniczony", "ograniczona", "ograniczone"),
                  "ograniczenie", words)
  
  words <- ifelse(words %in% c("obywatel", "obywatela", "obywatelowi", "obywatelem",
                               "obywatele", "obywateli", "obywatelom", "obywatelami"),
                  "obywatel", words)
  
  words <- ifelse(words %in% c("człowiek", "czlowiek", "człowieka", "czlowieka",
                               "człowiekowi", "czlowiekowi", "człowiekiem", "czlowiekiem"),
                  "czlowiek", words)
  
  words <- ifelse(words %in% c("państwo", "panstwo", "państwa", "panstwa",
                               "państwem", "panstwem", "państwu", "panstwu"),
                  "panstwo", words)
  
  words <- ifelse(words %in% c("ustawa", "ustawy", "ustawie", "ustawą", "ustawa",
                               "ustawach", "ustawami"),
                  "ustawa", words)
  
  words <- ifelse(words %in% c("sąd", "sad", "sądu", "sadu", "sądowi", "sadowi",
                               "sądem", "sadem", "sądy", "sady", "sądów", "sadow"),
                  "sad", words)
  
  words <- ifelse(words %in% c("trybunał", "trybunal", "trybunału", "trybunalu",
                               "trybunałem", "trybunalem"),
                  "trybunal", words)
  
  return(words)
}

# 2. Czyszczenie tekstu jednego artykułu ----


process_text <- function(text) {
  
  # Zamiana na małe litery
  text <- tolower(text)
  
  # Zamiana polskich apostrofów/cudzysłowów/myślników na prostsze znaki
  text <- gsub("[\u2019\u2018\u0060\u00B4]", "'", text)
  text <- gsub("[\u2013\u2014]", " ", text)
  
  # Usunięcie cyfr
  text <- removeNumbers(text)
  
  # Podział tekstu na słowa
  words <- unlist(strsplit(text, "\\s+"))
  
  # Usunięcie pustych elementów
  words <- words[words != ""]
  
  # Usunięcie interpunkcji
  words <- str_replace_all(words, "[[:punct:]]", "")
  
  # Usunięcie spacji z początku i końca słów
  words <- str_trim(words)
  
  # Usunięcie pustych elementów po czyszczeniu
  words <- words[words != ""]
  
  # Usunięcie bardzo krótkich elementów, ale zostawiamy "nie"
  words <- words[words == "nie" | nchar(words) > 1]
  
  # Stop words z pakietu tm dla języka polskiego
  tm_stopwords <- tolower(stopwords("pl",source="stopwords-iso"))
  
  # NIE usuwamy słowa "nie", bo jest ważne dla zakazów
  tm_stopwords <- tm_stopwords[tm_stopwords != "nie"]
  
  words <- words[!(words %in% tm_stopwords)]
  
  # Dodatkowe stop words dopasowane do naszego tekstu
  words <- words[!(words %in% custom_stopwords)]
  
  # Ręczne poprawienie najważniejszych form prawnych PRZED stemmingiem
  words <- popraw_formy_prawne(words)
  
  # Stem completion
  completed_doc <- words
  
  # Zamiana wyniku na zwykły wektor znaków
  completed_doc <- as.character(completed_doc)
  
  # Usunięcie pustych elementów
  completed_doc <- completed_doc[completed_doc != ""]
  
  # Ręczne poprawienie najważniejszych form prawnych PO stemmingu
  completed_doc <- popraw_formy_prawne(completed_doc)
  
  return(completed_doc)
}

