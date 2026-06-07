#' ---
#' title: "Raport i analiza - Konstytucja RP"
#' author: "Maciej Gwizd, Jakub Ładno, Igor Rosiński"
#' date: "2026-06-06"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: journal
#'     highlight: kate
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)


#' # 1. Instalacja pakietów
# 1. Instalacja pakietów ----

# install.packages(c("tm", "stringr", "dplyr", "readr", "stopwords","tidyr", "ggplot2","tibble","tidyverse","tidytext","widyr","igraph","ggraph","topicmodels","wordcloud", "reshape2", "ggwordcloud"))

# Ładowanie pakietów
library(tm)
library(stringr)
library(dplyr)
library(readr)
library(stopwords)
library(tidyr)
library(ggplot2)
library(tibble)
library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(topicmodels)
library(wordcloud)
library(reshape2)
library(ggwordcloud)

#' # 2. Wczytanie danych
# 2. Wczytanie danych ----
konstytucja <- read_csv("konstytucja_artykuly.csv", show_col_types = FALSE)

#' # 3. Czyszczenie danych
# 3. Czyszczenie danych ----

#' #   Customowe stopwords
#   Customowe stopwords ----
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
  "polski",
  "©",
  "©"
)

#' #Ręczny stemming
# Ręczny stemming  ----

popraw_formy_prawne <- function(words) {
  
  # KONSTYTUCJA
  words <- ifelse(words %in% c(
    "konstytucja", "konstytucji", "konstytucję", "konstytucją", "konstytucjo",
    "konstytucje", "konstytucji", "konstytucjom", "konstytucjami", "konstytucjach"
  ), "konstytucja", words)
  
  # PRAWO / PRAWA
  words <- ifelse(words %in% c(
    "prawo", "prawa", "prawem", "prawu", "prawach", "prawami",
    "praw", "prawny", "prawna", "prawne", "prawnego", "prawnej",
    "prawnym", "prawnych", "prawnymi"
  ), "prawo", words)
  
  # WOLNOŚĆ / WOLNOŚCI
  words <- ifelse(words %in% c(
    "wolność", "wolnosci", "wolności", "wolnością", "wolnoscia",
    "wolnosciach", "wolnościach", "wolnosciami", "wolnościami",
    "wolny", "wolna", "wolne", "wolnego", "wolnej", "wolnym",
    "wolnych", "wolnymi", "wolno"
  ), "wolnosc", words)
  
  # OBOWIĄZEK / OBOWIĄZKI
  words <- ifelse(words %in% c(
    "obowiązek", "obowiazek", "obowiązku", "obowiazku",
    "obowiązki", "obowiazki", "obowiązkiem", "obowiazkiem",
    "obowiązków", "obowiazkow", "obowiązkom", "obowiazkom",
    "obowiązkami", "obowiazkami", "obowiązkowy", "obowiazkowy",
    "obowiązkowa", "obowiazkowa", "obowiązkowe", "obowiazkowe"
  ), "obowiazek", words)
  
  # OBOWIĄZANY / ZOBOWIĄZANY
  words <- ifelse(words %in% c(
    "zobowiązany", "zobowiazany", "zobowiązana", "zobowiazana",
    "zobowiązane", "zobowiazane", "zobowiązani", "zobowiazani",
    "zobowiązuje", "zobowiazuje", "zobowiązują", "zobowiazuja"
  ), "zobowiazany", words)
  
  # ZAKAZ / ZAKAZUJE
  words <- ifelse(words %in% c(
    "zakaz", "zakazu", "zakazem", "zakazy", "zakazów", "zakazow",
    "zakazom", "zakazami", "zakazuje", "zakazują", "zakazuja",
    "zakazane", "zakazana", "zakazany", "zakazani",
    "zabrania", "zabronione", "zabroniony", "zabroniona"
  ), "zakaz", words)
  
  # OGRANICZENIE / OGRANICZA
  words <- ifelse(words %in% c(
    "ograniczenie", "ograniczenia", "ograniczeniu",
    "ograniczeniem", "ograniczeń", "ograniczen",
    "ograniczeniami", "ograniczeniom", "ograniczony",
    "ograniczona", "ograniczone", "ograniczonych",
    "ograniczonym", "ogranicza", "ograniczają", "ograniczaja",
    "ograniczać", "ograniczac"
  ), "ograniczenie", words)
  
  # OBYWATEL
  words <- ifelse(words %in% c(
    "obywatel", "obywatela", "obywatelowi", "obywatelem",
    "obywatele", "obywateli", "obywatelom", "obywatelami",
    "obywatelski", "obywatelska", "obywatelskie",
    "obywatelskiego", "obywatelskiej", "obywatelskich",
    "obywatelstwo", "obywatelstwa", "obywatelstwem"
  ), "obywatel", words)
  
  # CZŁOWIEK
  words <- ifelse(words %in% c(
    "człowiek", "czlowiek", "człowieka", "czlowieka",
    "człowiekowi", "czlowiekowi", "człowiekiem", "czlowiekiem",
    "ludzie", "ludzi", "ludziom", "ludźmi", "ludzmi",
    "ludzki", "ludzka", "ludzkie", "ludzkiego", "ludzkiej",
    "ludzkich", "ludzkim"
  ), "czlowiek", words)
  
  # PAŃSTWO
  words <- ifelse(words %in% c(
    "państwo", "panstwo", "państwa", "panstwa",
    "państwem", "panstwem", "państwu", "panstwu",
    "państwowy", "panstwowy", "państwowa", "panstwowa",
    "państwowe", "panstwowe", "państwowego", "panstwowego",
    "państwowej", "panstwowej", "państwowych", "panstwowych"
  ), "panstwo", words)
  
  # USTAWA
  words <- ifelse(words %in% c(
    "ustawa", "ustawy", "ustawie", "ustawą",
    "ustawach", "ustawami", "ustawom", "ustaw",
    "ustawowy", "ustawowa", "ustawowe", "ustawowego",
    "ustawowej", "ustawowych", "ustawowym"
  ), "ustawa", words)
  
  # SĄD
  words <- ifelse(words %in% c(
    "sąd", "sad", "sądu", "sadu", "sądowi", "sadowi",
    "sądem", "sadem", "sądy", "sady", "sądów", "sadow",
    "sądom", "sadom", "sądami", "sadami",
    "sądowy", "sadowy", "sądowa", "sadowa",
    "sądowe", "sadowe", "sądowego", "sadowego",
    "sądowej", "sadowej", "sądowych", "sadowych"
  ), "sad", words)
  
  # TRYBUNAŁ
  words <- ifelse(words %in% c(
    "trybunał", "trybunal", "trybunału", "trybunalu",
    "trybunałem", "trybunalem", "trybunałowi", "trybunalowi",
    "trybunały", "trybunaly", "trybunałów", "trybunalow"
  ), "trybunal", words)
  
  # RÓWNOŚĆ
  words <- ifelse(words %in% c(
    "równość", "rownosc", "równości", "rownosci",
    "równością", "rownoscia", "równy", "rowny",
    "równa", "rowna", "równe", "rowne", "równi", "rowni",
    "równego", "rownego", "równej", "rownej",
    "równym", "rownym", "równych", "rownych"
  ), "rownosc", words)
  
  # GODNOŚĆ
  words <- ifelse(words %in% c(
    "godność", "godnosc", "godności", "godnosci",
    "godnością", "godnoscia", "godny", "godna", "godne",
    "godnego", "godnej", "godnym", "godnych"
  ), "godnosc", words)
  
  # OCHRONA
  words <- ifelse(words %in% c(
    "ochrona", "ochrony", "ochronie", "ochroną",
    "ochronna", "ochronny", "ochronne",
    "chroni", "chronić", "chronic", "chroniony", "chroniona",
    "chronione", "chronionych", "chronionym"
  ), "ochrona", words)
  
  # NIETYKALNOŚĆ
  words <- ifelse(words %in% c(
    "nietykalność", "nietykalnosc", "nietykalności", "nietykalnosci",
    "nietykalnością", "nietykalnoscia", "nietykalny",
    "nietykalna", "nietykalne", "nietykalnego", "nietykalnej"
  ), "nietykalnosc", words)
  
  # WŁASNOŚĆ
  words <- ifelse(words %in% c(
    "własność", "wlasnosc", "własności", "wlasnosci",
    "własnością", "wlasnoscia", "własny", "wlasny",
    "własna", "wlasna", "własne", "wlasne",
    "własnego", "wlasnego", "własnej", "wlasnej",
    "własnych", "wlasnych"
  ), "wlasnosc", words)
  
  # ODPOWIEDZIALNOŚĆ
  words <- ifelse(words %in% c(
    "odpowiedzialność", "odpowiedzialnosc",
    "odpowiedzialności", "odpowiedzialnosci",
    "odpowiedzialnością", "odpowiedzialnoscia",
    "odpowiedzialny", "odpowiedzialna", "odpowiedzialne",
    "odpowiedzialnego", "odpowiedzialnej", "odpowiedzialnych",
    "odpowiada", "odpowiadają", "odpowiadaja"
  ), "odpowiedzialnosc", words)
  
  # KARA
  words <- ifelse(words %in% c(
    "kara", "kary", "karze", "karą",
    "kar", "karami", "karom", "karany", "karana",
    "karane", "karalny", "karalna", "karalne",
    "karalności", "karalnosci"
  ), "kara", words)
  
  # NAKAZ
  words <- ifelse(words %in% c(
    "nakaz", "nakazu", "nakazem", "nakazy", "nakazów", "nakazow",
    "nakazuje", "nakazują", "nakazuja", "nakazane",
    "nakazana", "nakazany"
  ), "nakaz", words)
  
  # MOŻE / MOŻNA / UPRAWNIENIE
  words <- ifelse(words %in% c(
    "może", "moze", "mogą", "moga", "można", "mozna",
    "uprawniony", "uprawniona", "uprawnione", "uprawnieni",
    "uprawnienia", "uprawnienie", "uprawnieniu", "uprawnieniem"
  ), "moze", words)
  
  # MUSI / POWINIEN
  words <- ifelse(words %in% c(
    "musi", "muszą", "musza", "musieć", "musiec",
    "powinien", "powinna", "powinno", "powinni",
    "powinny", "powinienem"
  ), "musi", words)
  
  return(words)
}

#' # Czyszczenie
# Czyszczenie ----
# Funkcja do czyszczenia tekstu jednego artykułu

process_text <- function(text) {
  
  # Zamiana na małe litery
  text <- tolower(text)
  
  # Zamiana apostrofów, cudzysłowów i myślników
  text <- gsub("[\u2019\u2018\u0060\u00B4]", "'", text)
  text <- gsub("[\u2013\u2014]", " ", text)
  
  # Usunięcie cyfr
  text <- removeNumbers(text)
  
  # Tokenizacja, czyli podział na słowa
  words <- unlist(strsplit(text, "\\s+"))
  
  # Usunięcie pustych elementów
  words <- words[words != ""]
  
  # Usunięcie interpunkcji
  words <- str_replace_all(words, "[[:punct:]]", "")
  
  # Usunięcie pustych elementów po czyszczeniu
  words <- words[words != ""]
  
  # Usunięcie bardzo krótkich elementów, ale zostawiamy "nie"
  words <- words[words == "nie" | nchar(words) > 1]
  
  # Stop words dla języka polskiego
  polskie_stopwords <- tolower(stopwords("pl", source = "stopwords-iso"))
  
  # Nie usuwamy słów ważnych dla analizy restrykcyjności/liberalności
  polskie_stopwords <- polskie_stopwords[
    !(polskie_stopwords %in% c("nie", "może", "moze", "musi", "powinien", "powinna", "powinno"))
  ]
  
  # Usunięcie stop words
  words <- words[!(words %in% polskie_stopwords)]
  
  # Usunięcie dodatkowych stop words
  words <- words[!(words %in% custom_stopwords)]
  
  # Usunięcie spacji
  words <- str_trim(words)
  
  # Ręczny stemming / ręczna lematyzacja
  words <- popraw_formy_prawne(words)
  
  return(words)
}


# 4. Czyszczenie wszystkich artykułów

lista_slow <- list()

for (i in 1:nrow(konstytucja)) {
  
  lista_slow[[i]] <- process_text(konstytucja$text[i])
  
}


#' # Zbudowanie tabeli
# Zbudowanie tabeli ----

tokeny_konstytucja <- data.frame()

for (i in 1:nrow(konstytucja)) {
  
  temp <- data.frame(
    id = konstytucja$id[i],
    pozycja = seq_along(lista_slow[[i]]),
    word = lista_slow[[i]]
  )
  
  tokeny_konstytucja <- rbind(tokeny_konstytucja, temp)
}

#' # Stworzenie wersji tekstowej po czyszczeniu
# Stworzenie wersji tekstowej po czyszczeniu ----

konstytucja_czysta <- data.frame()

for (i in 1:nrow(konstytucja)) {
  
  temp <- data.frame(
    id = konstytucja$id[i],
    text_clean = paste(lista_slow[[i]], collapse = " ")
  )
  
  konstytucja_czysta <- rbind(konstytucja_czysta, temp)
}


#' # Zapis wyczyszczonych danych
# Zapis wyczyszczonych danych ----

write_csv(tokeny_konstytucja, "konstytucja_tokeny_czyste.csv")
write_csv(konstytucja_czysta, "konstytucja_czysta.csv")


#' # 4. Analiza restrykcyjności i liberalności języka
# 4. Analiza restrykcyjności i liberalności języka ----


# Wczytanie oczyszczonych tokenów

# Customowe słowniki do analizy języka

# Słownik liberalny / wolnościowy:
# słowa związane z prawami, wolnościami, ochroną jednostki, równością i uprawnieniami.

slownik_liberalny <- c(
  "prawo",
  "wolnosc",
  "moze",
  "uprawnienie",
  "ochrona",
  "rownosc",
  "godnosc",
  "nietykalnosc",
  "wlasnosc",
  "swoboda",
  "obywatel",
  "czlowiek",
  "gwarantuje",
  "zapewnia",
  "przysluguje",
  "przyslugiwac",
  "korzystac",
  "korzystanie",
  "dostep",
  "bezpieczenstwo",
  "wolny",
  "rowny",
  "sprawiedliwosc",
  "solidarnosc"
)


# Słownik restrykcyjny / obowiązkowo-zakazowy:
# słowa związane z zakazami, obowiązkami, ograniczeniami, karami i odpowiedzialnością.

slownik_restrykcyjny <- c(
  "obowiazek",
  "obowiazany",
  "zakaz",
  "ograniczenie",
  "odpowiedzialnosc",
  "kara",
  "nakaz",
  "musi",
  "nie",
  "podlega",
  "podlegaja",
  "wymaga",
  "wymagane",
  "wymagac",
  "zabrania",
  "zabronione",
  "niedopuszczalne",
  "odmowa",
  "pozbawienie",
  "utrata",
  "naruszenie",
  "narusza",
  "kontrola",
  "kontroluje",
  "sankcja"
)


# Słownik instytucjonalny / państwowy:
# słowa opisujące organy państwa, procedury, instytucje i ustrój.

slownik_instytucjonalny <- c(
  "panstwo",
  "ustawa",
  "sad",
  "trybunal",
  "sejm",
  "senat",
  "prezydent",
  "rada_ministrow",
  "rzad",
  "posel",
  "senator",
  "wybory",
  "glosowanie",
  "wladza",
  "organ",
  "orzeczenie",
  "umowa",
  "narod",
  "minister",
  "administracja",
  "urząd",
  "urzad",
  "samorzad",
  "wojewoda",
  "komisja",
  "marszalek",
  "zgromadzenie",
  "budzet",
  "finanse",
  "referendum"
)


# Jeden słownik z kategoriami

slownik_kategorie <- data.frame(
  word = c(slownik_liberalny, slownik_restrykcyjny, slownik_instytucjonalny),
  kategoria = c(
    rep("liberalny", length(slownik_liberalny)),
    rep("restrykcyjny", length(slownik_restrykcyjny)),
    rep("instytucjonalny", length(slownik_instytucjonalny))
  )
)


slownik_kategorie <- slownik_kategorie %>%
  distinct(word, .keep_all = TRUE)


# Sprawdzenie
print(slownik_kategorie)


#' #NEGACJA
# NEGACJA ----

tokeny_z_kategoria <- tokeny_konstytucja %>%
  arrange(id, pozycja) %>%
  left_join(slownik_kategorie, by = "word") %>%
  mutate(
    kategoria_pierwotna = ifelse(is.na(kategoria), "inne", kategoria)
  ) %>%
  group_by(id) %>%
  mutate(
    poprzednie_slowo = lag(word),
    
    negacja_przed = ifelse(poprzednie_slowo == "nie", TRUE, FALSE),
    
    kategoria = case_when(
      negacja_przed == TRUE & kategoria_pierwotna == "liberalny" ~ "restrykcyjny",
      negacja_przed == TRUE & kategoria_pierwotna == "restrykcyjny" ~ "liberalny",
      TRUE ~ kategoria_pierwotna
    ),
    
    wyrazenie = ifelse(
      negacja_przed == TRUE & kategoria_pierwotna %in% c("liberalny", "restrykcyjny"),
      paste(poprzednie_slowo, word),
      word
    )
  ) %>%
  ungroup()

# Podgląd
head(tokeny_z_kategoria)

#Kontrola negacji

wyrazenia_z_negacja <- tokeny_z_kategoria %>%
  filter(
    negacja_przed == TRUE,
    kategoria_pierwotna %in% c("liberalny", "restrykcyjny")
  ) %>%
  select(id, pozycja, wyrazenie, word, kategoria_pierwotna, kategoria)

print(wyrazenia_z_negacja)


#' # Analiza całej Konstytucji
# Analiza całej Konstytucji ----

wynik_cala_konstytucja <- tokeny_z_kategoria %>%
  count(kategoria, name = "liczba_slow") %>%
  mutate(
    udzial_proc = round(100 * liczba_slow / sum(liczba_slow), 2)
  ) %>%
  arrange(desc(liczba_slow))

print(wynik_cala_konstytucja)

#' # Analiza tylko słów sklasyfikowanych
# Analiza tylko słów sklasyfikowanych ----

wynik_slowa_kategoryzowane <- tokeny_z_kategoria %>%
  filter(kategoria != "inne") %>%
  count(kategoria, name = "liczba_slow") %>%
  mutate(
    udzial_proc = round(100 * liczba_slow / sum(liczba_slow), 2)
  ) %>%
  arrange(desc(liczba_slow))

print(wynik_slowa_kategoryzowane)


#' # Indeks tekstu prawnego
# Indeks tekstu prawnego ----
# Interpretacja:
# indeks > 0  -> przewaga języka liberalnego
# indeks < 0  -> przewaga języka restrykcyjnego
# indeks = 0  -> równowaga
#
# Wzór:
# indeks = (liberalne - restrykcyjne) / (liberalne + restrykcyjne)

liczba_liberalnych <- sum(tokeny_z_kategoria$kategoria == "liberalny")
liczba_restrykcyjnych <- sum(tokeny_z_kategoria$kategoria == "restrykcyjny")

indeks_liberalnosci_caly <- (liczba_liberalnych - liczba_restrykcyjnych) /
  (liczba_liberalnych + liczba_restrykcyjnych)

indeks_liberalnosci_caly <- round(indeks_liberalnosci_caly, 4)


print(indeks_liberalnosci_caly)


#' #  Analiza według artykułów
#  Analiza według artykułów ----

wynik_artykuly <- tokeny_z_kategoria %>%
  filter(kategoria != "inne") %>%
  count(id, kategoria, name = "liczba_slow") %>%
  pivot_wider(
    names_from = kategoria,
    values_from = liczba_slow,
    values_fill = 0
  )

# Brakujące kolumny
if (!"liberalny" %in% names(wynik_artykuly)) {
  wynik_artykuly$liberalny <- 0
}

if (!"restrykcyjny" %in% names(wynik_artykuly)) {
  wynik_artykuly$restrykcyjny <- 0
}

if (!"instytucjonalny" %in% names(wynik_artykuly)) {
  wynik_artykuly$instytucjonalny <- 0
}

wynik_artykuly <- wynik_artykuly %>%
  mutate(
    suma_kategoryzowanych = liberalny + restrykcyjny + instytucjonalny,
    
    indeks_liberalnosci = ifelse(
      liberalny + restrykcyjny == 0,
      0,
      round((liberalny - restrykcyjny) / (liberalny + restrykcyjny), 4)
    ),
    
    dominujaca_kategoria = case_when(
      liberalny > restrykcyjny & liberalny > instytucjonalny ~ "liberalny",
      restrykcyjny > liberalny & restrykcyjny > instytucjonalny ~ "restrykcyjny",
      instytucjonalny > liberalny & instytucjonalny > restrykcyjny ~ "instytucjonalny",
      TRUE ~ "mieszany_neutralny"
    )
  ) %>%
  arrange(id)

print(head(wynik_artykuly, 20))


# Dodawanie artykułów w których nie wykryto żadnego słowa ze słownika


wszystkie_artykuly <- tokeny_konstytucja %>%
  distinct(id)

wynik_artykuly_pelny <- wszystkie_artykly <- wszystkie_artykuly %>%
  left_join(wynik_artykuly, by = "id") %>%
  mutate(
    liberalny = ifelse(is.na(liberalny), 0, liberalny),
    restrykcyjny = ifelse(is.na(restrykcyjny), 0, restrykcyjny),
    instytucjonalny = ifelse(is.na(instytucjonalny), 0, instytucjonalny),
    suma_kategoryzowanych = ifelse(is.na(suma_kategoryzowanych), 0, suma_kategoryzowanych),
    indeks_liberalnosci = ifelse(is.na(indeks_liberalnosci), 0, indeks_liberalnosci),
    dominujaca_kategoria = ifelse(is.na(dominujaca_kategoria), "brak_slownikowych", dominujaca_kategoria)
  )

print(head(wynik_artykuly_pelny, 20))


# Podsumowanie liczby artykułów według dominującej kategorii

podsumowanie_artykulow <- wynik_artykuly_pelny %>%
  count(dominujaca_kategoria, name = "liczba_artykulow") %>%
  mutate(
    udzial_proc = round(100 * liczba_artykulow / sum(liczba_artykulow), 2)
  ) %>%
  arrange(desc(liczba_artykulow))

print(podsumowanie_artykulow)


#' # Zapis wyników
# Zapis wyników ----

write_csv(tokeny_z_kategoria, "konstytucja_tokeny_z_kategoria.csv")
write_csv(wynik_cala_konstytucja, "wynik_cala_konstytucja_kategorie.csv")
write_csv(wynik_slowa_kategoryzowane, "wynik_slowa_kategoryzowane.csv")
write_csv(wynik_artykuly_pelny, "wynik_artykuly_liberalnosc_restrykcyjnosc.csv")
write_csv(podsumowanie_artykulow, "podsumowanie_artykulow_kategorie.csv")


#' # Wizualizacja
# Wizualizacja ----

# Wariant bez negacji

tokeny_bez_negacji <- tokeny_konstytucja %>%
  arrange(id, pozycja) %>%
  left_join(slownik_kategorie, by = "word") %>%
  mutate(
    kategoria = ifelse(is.na(kategoria), "inne", kategoria),
    wariant = "Bez negacji"
  )


# Wariant z negacją

tokeny_z_negacja_wariant <- tokeny_z_kategoria %>%
  mutate(
    wariant = "Z negacją"
  )


# Połączenie wariantów

tokeny_porownanie <- bind_rows(
  tokeny_bez_negacji,
  tokeny_z_negacja_wariant
)


# 1. Porównanie liczby słów w kategoriach

dane_porownanie_kategorie <- tokeny_porownanie %>%
  filter(kategoria != "inne") %>%
  count(wariant, kategoria, name = "liczba_slow") %>%
  group_by(wariant) %>%
  mutate(
    udzial_proc = round(100 * liczba_slow / sum(liczba_slow), 2)
  ) %>%
  ungroup()

ggplot(dane_porownanie_kategorie,
       aes(x = kategoria, y = liczba_slow, fill = kategoria)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = liczba_slow),
    vjust = -0.4,
    size = 4
  ) +
  facet_wrap(~ wariant) +
  scale_fill_manual(
    values = c(
      "liberalny" = "#2b8cbe",
      "restrykcyjny" = "#d7301f",
      "instytucjonalny" = "#636363"
    )
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Porównanie kategorii języka prawnego",
    subtitle = "Bez zastosowania negacji oraz z zastosowaniem negacji",
    x = "Kategoria języka",
    y = "Liczba słów",
    fill = "Kategoria"
  )


#  Porównanie udziałów procentowych kategorii

ggplot(dane_porownanie_kategorie,
       aes(x = kategoria, y = udzial_proc, fill = kategoria)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(udzial_proc, "%")),
    vjust = -0.4,
    size = 4
  ) +
  facet_wrap(~ wariant) +
  scale_fill_manual(
    values = c(
      "liberalny" = "#2b8cbe",
      "restrykcyjny" = "#d7301f",
      "instytucjonalny" = "#636363"
    )
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Struktura języka prawnego według kategorii",
    subtitle = "Udział procentowy słów sklasyfikowanych w słownikach",
    x = "Kategoria języka",
    y = "Udział procentowy",
    fill = "Kategoria"
  )


# Indeks liberalności/restrykcyjności według artykułów

indeks_artykuly_porownanie <- tokeny_porownanie %>%
  filter(kategoria != "inne") %>%
  count(wariant, id, kategoria, name = "liczba_slow") %>%
  pivot_wider(
    names_from = kategoria,
    values_from = liczba_slow,
    values_fill = 0
  )

if (!"liberalny" %in% names(indeks_artykuly_porownanie)) {
  indeks_artykuly_porownanie$liberalny <- 0
}

if (!"restrykcyjny" %in% names(indeks_artykuly_porownanie)) {
  indeks_artykuly_porownanie$restrykcyjny <- 0
}

if (!"instytucjonalny" %in% names(indeks_artykuly_porownanie)) {
  indeks_artykuly_porownanie$instytucjonalny <- 0
}

indeks_artykuly_porownanie <- indeks_artykuly_porownanie %>%
  mutate(
    suma_lib_res = liberalny + restrykcyjny,
    
    indeks_liberalnosci = ifelse(
      suma_lib_res == 0,
      0,
      round((liberalny - restrykcyjny) / suma_lib_res, 4)
    ),
    
    typ_indeksu = case_when(
      indeks_liberalnosci > 0 ~ "bardziej liberalny",
      indeks_liberalnosci < 0 ~ "bardziej restrykcyjny",
      TRUE ~ "neutralny"
    )
  ) %>%
  arrange(wariant, id)

ggplot(indeks_artykuly_porownanie,
       aes(x = id, y = indeks_liberalnosci, fill = typ_indeksu)) +
  geom_col(width = 0.8) +
  geom_hline(
    yintercept = 0,
    color = "black",
    size = 0.8
  ) +
  facet_wrap(~ wariant, ncol = 1) +
  scale_fill_manual(
    values = c(
      "bardziej liberalny" = "#2b8cbe",
      "bardziej restrykcyjny" = "#d7301f",
      "neutralny" = "#bdbdbd"
    )
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Indeks liberalności/restrykcyjności według artykułów",
    subtitle = "Indeks = (liberalne - restrykcyjne) / (liberalne + restrykcyjne)",
    x = "Numer artykułu",
    y = "Indeks liberalności",
    fill = "Charakter artykułu"
  )


#  Heatmapa kategorii według artykułów 

dane_heatmapa_porownanie <- tokeny_porownanie %>%
  filter(kategoria != "inne") %>%
  count(wariant, id, kategoria, name = "liczba_slow")

ggplot(dane_heatmapa_porownanie,
       aes(x = id, y = kategoria, fill = liczba_slow)) +
  geom_tile(color = "white") +
  facet_wrap(~ wariant, ncol = 1) +
  scale_fill_gradient(
    low = "#f7f7f7",
    high = "#08306b"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Natężenie kategorii języka prawnego według artykułów",
    subtitle = "Porównanie klasyfikacji bez negacji oraz z negacją",
    x = "Numer artykułu",
    y = "Kategoria",
    fill = "Liczba słów"
  )


# Wpływ negacji na indeks liberalności

porownanie_indeksow <- indeks_artykuly_porownanie %>%
  select(wariant, id, indeks_liberalnosci) %>%
  pivot_wider(
    names_from = wariant,
    values_from = indeks_liberalnosci
  ) %>%
  mutate(
    roznica = `Z negacją` - `Bez negacji`
  )

ggplot(porownanie_indeksow,
       aes(x = id, y = roznica)) +
  geom_col(fill = "#756bb1", width = 0.8) +
  geom_hline(
    yintercept = 0,
    color = "black",
    size = 0.8
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Wpływ negacji na indeks liberalności artykułów",
    subtitle = "Różnica = indeks z negacją - indeks bez negacji",
    x = "Numer artykułu",
    y = "Zmiana indeksu po uwzględnieniu negacji"
  )


# Najbardziej liberalne i restrykcyjne artykuły z negacją

top_artykuly <- indeks_artykuly_porownanie %>%
  filter(wariant == "Z negacją") %>%
  arrange(indeks_liberalnosci) %>%
  slice_head(n = 10) %>%
  bind_rows(
    indeks_artykuly_porownanie %>%
      filter(wariant == "Z negacją") %>%
      arrange(desc(indeks_liberalnosci)) %>%
      slice_head(n = 10)
  ) %>%
  mutate(
    id_label = paste0("Art. ", id)
  )

ggplot(top_artykuly,
       aes(x = indeks_liberalnosci,
           y = reorder(id_label, indeks_liberalnosci),
           fill = typ_indeksu)) +
  geom_col(width = 0.75) +
  geom_vline(
    xintercept = 0,
    color = "black",
    size = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "bardziej liberalny" = "#2b8cbe",
      "bardziej restrykcyjny" = "#d7301f",
      "neutralny" = "#bdbdbd"
    )
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Najbardziej liberalne i restrykcyjne artykuły Konstytucji RP",
    subtitle = "Na podstawie indeksu liberalności z uwzględnieniem negacji",
    x = "Indeks liberalności",
    y = "Artykuł",
    fill = "Charakter artykułu"
  )


#' # 5. Analiza asocjacji słów
# 5. Analiza asocjacji słów ----



# Wczytanie i przygotowanie danych do asocjacji

# Kontrola
head(konstytucja_czysta)


# Budowa macierzy TDM i liczenie częstości

# 1. Budowa korpusu
corpus <- VCorpus(VectorSource(konstytucja_czysta$text_clean))

# 2. Budowa macierzy Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)

# 3. Zamiana TDM na zwykłą macierz
tdm_m <- as.matrix(tdm)

# 4. Najczęstsze słowa
v <- sort(rowSums(tdm_m), decreasing = TRUE)

tdm_df <- data.frame(
  word = names(v),
  freq = v
)

head(tdm_df, 30)


# Wizualizacja

wykres_lizakowy_asocjacji <- function(target_word, cor_limit = 0.3, top_n = 20) {
  
  if (!(target_word %in% Terms(tdm))) {
    
    cat("Słowo:", target_word, "nie występuje w macierzy TDM.\n")
    cat("Możliwe, że po czyszczeniu/stemmingu ma inną formę.\n")
    cat("Podobne słowa w TDM:\n")
    
    print(
      Terms(tdm)[str_detect(Terms(tdm), substr(target_word, 1, 4))]
    )
    
    return(NULL)
  }
  
  
  assoc <- findAssocs(
    x = tdm,
    terms = target_word,
    corlimit = cor_limit
  )
  
  assoc_vector <- assoc[[target_word]]
  
  if (length(assoc_vector) == 0) {
    
    cat("Brak asocjacji dla słowa:", target_word, "przy progu:", cor_limit, "\n")
    cat("Spróbuj obniżyć próg, np. cor_limit = 0.2 albo cor_limit = 0.1\n")
    
    return(NULL)
  }
  
  
  assoc_df <- data.frame(
    word = names(assoc_vector),
    score = as.numeric(assoc_vector),
    row.names = NULL
  ) %>%
    arrange(desc(score)) %>%
    slice_head(n = top_n)
  
  # Wykres lizakowy z natężeniem na podstawie wartości korelacji score
  wykres <- ggplot(
    assoc_df,
    aes(
      x = score,
      y = reorder(word, score),
      color = score
    )
  ) +
    geom_segment(
      aes(
        x = 0,
        xend = score,
        y = word,
        yend = word
      ),
      size = 1.2
    ) +
    geom_point(size = 4) +
    geom_text(
      aes(label = round(score, 2)),
      hjust = -0.3,
      size = 3.5,
      color = "black"
    ) +
    scale_color_gradient(
      low = "#a6bddb",
      high = "#08306b"
    ) +
    scale_x_continuous(
      limits = c(0, max(assoc_df$score) + 0.1),
      expand = expansion(mult = c(0, 0.2))
    ) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste0("Asocjacje z terminem: '", target_word, "'"),
      subtitle = paste0("Próg r ≥ ", cor_limit),
      x = "Współczynnik korelacji Pearsona",
      y = "Słowo",
      color = "Natężenie\nskojarzenia"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "right"
    )
  
  print(wykres)
  
  return(assoc_df)
}

# Przykłady
wykres_lizakowy_asocjacji("wolnosc", cor_limit = 0.3, top_n = 20)
wykres_lizakowy_asocjacji("prawo", cor_limit = 0.3, top_n = 20)
wykres_lizakowy_asocjacji("zakaz", cor_limit = 0.3, top_n = 20)
wykres_lizakowy_asocjacji("panstwo", cor_limit = 0.3, top_n = 20)
wykres_lizakowy_asocjacji("sejm", cor_limit = 0.3, top_n = 20)


#' # 6. LDA
# 6. LDA ----

#' # Funkcja top_terms_by_topic_LDA
# Funkcja top_terms_by_topic_LDA ----

top_terms_by_topic_LDA <- function(input_corpus, plot = TRUE, k = 3) {  
  
  # 1. Tworzenie macierzy DTM
  DTM <- DocumentTermMatrix(input_corpus)
  
  # 2. Wykonanie LDA
  lda <- LDA(DTM, k = k, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") 
  
  # 3. Wyciągnięcie top 10 słów dla każdego tematu
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # 4. Bezpieczne sterowanie wykresem
  if (isTRUE(plot)) {
    # Tworzymy i od razu zwracamy/wyświetlamy wykres
    p <- top_terms %>%
      mutate(term = reorder_with_theme(term, beta, topic)) %>% 
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free_y") +
      labs(x = "Słowa kluczowe (Terminy)", y = "β (Wymiar ważności słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1") +
      scale_x_reordered()
    
    print(p) # Wymuszenie wyświetlenia wykresu w raporcie
    
  } else { 
    # Jeśli plot = FALSE, zwracamy samą tabelę danych
    return(top_terms)
  }
}
# Pomocnicza funkcja do poprawnego sortowania słów na wykresach ggplot2
reorder_with_theme <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(within, x, sep = sep)
  stats::reorder(new_x, by, fun = fun, ...)
}

# Przypisanie środowiska do poprawnego wyświetlania nazw osi
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0("^.*", sep)
  scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}



data <- read.csv(
  "konstytucja_czysta.csv",
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

# Budujemy korpus bezpośrednio z kolumny 'text_clean'
corpus <- VCorpus(VectorSource(data$text_clean))
names(corpus) <- data$id

# Macierz częstości TDM (do celów EDA i chmury słów)
tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)

v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)


#' # Eksploracyjna analiza danych
# Eksploracyjna analiza danych ----

# Globalna chmura słów dla Konstytucji
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 4, 
          max.words = 80, colors = brewer.pal(8, "Dark2"), random.order = FALSE)

print("Top 10 słów w Konstytucji:")
print(head(tdm_df, 10))


#' # UCZENIE MASZYNOWE NIENADZOROWANE (LDA)
# UCZENIE MASZYNOWE NIENADZOROWANE (LDA) ----
# Modelowanie tematów (LDA)

# Analiza dla 3 tematów
number_of_topics = 3
top_terms_by_topic_LDA(corpus, k = number_of_topics)

# Analiza dla 4 tematów
number_of_topics = 4
top_terms_by_topic_LDA(corpus, k = number_of_topics)

# Analiza dla 5 tematów
number_of_topics = 9
top_terms_by_topic_LDA(corpus, k = number_of_topics)


#' # 7. Sieć współwystępowania
# 7. Sieć współwystępowania----

konst <- read.csv(
  "konstytucja_czysta.csv",
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

# TOKENIZACJA

words <- konst %>%
  select(id, text_clean) %>%
  unnest_tokens(word, text_clean)

# USUWAMY 20 NAJCZĘSTSZYCH SŁÓW

top_words <- words %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(word)

words_net <- words %>%
  filter(!word %in% top_words)

# ZOSTAWIAMY SŁOWA WYSTĘPUJĄCE CO NAJMNIEJ 5 RAZY

freq_words <- words_net %>%
  count(word, sort = TRUE) %>%
  filter(n >= 5)

words_net <- words_net %>%
  filter(word %in% freq_words$word)

# KORELACJE MIĘDZY SŁOWAMI

word_cors <- words_net %>%
  pairwise_cor(
    item = word,
    feature = id,
    sort = TRUE
  )

# TYLKO SILNIEJSZE POWIĄZANIA

edges <- word_cors %>%
  filter(correlation >= 0.70)

# BUDOWA GRAFU

graph <- graph_from_data_frame(
  edges,
  directed = FALSE
)

# USUWAMY SŁABO POŁĄCZONE WĘZŁY

graph <- delete_vertices(
  graph,
  V(graph)[degree(graph) < 2]
)

# WYKRYWANIE KLASTRÓW (WALKTRAP)

walk <- cluster_walktrap(graph)

communities <- membership(walk)

V(graph)$community <- factor(communities)

# WIELKOŚĆ WĘZŁÓW

V(graph)$degree <- degree(graph)

# LICZBA KLASTRÓW

n_clusters <- length(unique(communities))

# UKŁAD GRAFU

set.seed(123)

layout_graph <- create_layout(
  graph,
  layout = "fr"
)

# WYKRES

ggraph(layout_graph) +
  
  geom_edge_link(
    aes(width = correlation),
    alpha = 0.25,
    colour = "grey70"
  ) +
  
  geom_node_point(
    aes(
      size = degree,
      colour = community
    )
  ) +
  
  geom_node_text(
    aes(
      label = name,
      colour = community
    ),
    repel = TRUE,
    size = 4
  ) +
  
  scale_size_continuous(
    range = c(4, 18)
  ) +
  
  scale_edge_width(
    range = c(0.3, 3)
  ) +
  
  labs(
    title = "Sieć współwystępowania pojęć w Konstytucji RP",
    subtitle = paste(
      n_clusters,
      "klastrów wykrytych metodą Walktrap"
    ),
    colour = "Klaster"
  ) +
  
  theme_void(base_size = 14) +
  
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 14
    ),
    legend.position = "right"
  )

#' # 8. TFIDF
# 8. TFIDF ----

# Wczytanie danych 

data <- konstytucja_czysta

# Kontrola nazw kolumn
print(names(data))

if (length(corpus) == 0) {
  stop("Korpus jest pusty. Sprawdź, czy kolumna text_clean istnieje i zawiera tekst.")
}

inspect(corpus[[1]])


# Zliczanie częstości słów

print("Top 10 najczęstszych słów - standardowe zliczanie:")
print(head(tdm_df, 10))


# Chmura słów - standardowe zliczanie

wordcloud(
  words = tdm_df$word,
  freq = tdm_df$freq,
  min.freq = 3,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)



tdm_tfidf <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = function(x) weightTfIdf(x, normalize = TRUE)
  )
)

tdm_tfidf_m <- as.matrix(tdm_tfidf)


v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)

tdm_tfidf_df <- data.frame(
  word = names(v_tfidf),
  freq = v_tfidf
)

print("Top 10 słów o najwyższej wadze TF-IDF:")
print(head(tdm_tfidf_df, 10))

# Wizualizacja


# Chmura słów TF-IDF

wordcloud(
  words = tdm_tfidf_df$word,
  freq = tdm_tfidf_df$freq,
  max.words = 50,
  random.order = FALSE,
  colors = brewer.pal(8, "Set1")
)


# Wykres słupkowy top słów TF-IDF

top_tfidf <- tdm_tfidf_df %>%
  slice_head(n = 20)

ggplot(top_tfidf,
       aes(x = freq, y = reorder(word, freq))) +
  geom_col(fill = "#2b8cbe") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Top 20 słów według TF-IDF",
    subtitle = "Słowa najbardziej charakterystyczne dla artykułów Konstytucji RP",
    x = "Suma wag TF-IDF",
    y = "Słowo"
  )