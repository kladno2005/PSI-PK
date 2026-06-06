# 1. Instalacja pakietów ----

# install.packages(c("tm", "stringr", "dplyr", "readr", "stopwords","tidyr", "ggplot2","tibble","tidyverse"))
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
# 2. Wczytanie danych ----
konstytucja <- read_csv("konstytucja_artykuly.csv", show_col_types = FALSE)

# 3. Czyszczenie danych ----

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


# Ręczny stemming  ----

popraw_formy_prawne <- function(words) {
  
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
    "obowiązany", "obowiazany", "obowiązana", "obowiazana",
    "obowiązane", "obowiazane", "obowiązani", "obowiazani",
    "obowiązuje", "obowiazuje", "obowiązują", "obowiazuja",
    "zobowiązany", "zobowiazany", "zobowiązana", "zobowiazana",
    "zobowiązane", "zobowiazane", "zobowiązani", "zobowiazani",
    "zobowiązuje", "zobowiazuje", "zobowiązują", "zobowiazuja"
  ), "obowiazany", words)
  
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


# Stworzenie wersji tekstowej po czyszczeniu ----

konstytucja_czysta <- data.frame()

for (i in 1:nrow(konstytucja)) {
  
  temp <- data.frame(
    id = konstytucja$id[i],
    text_clean = paste(lista_slow[[i]], collapse = " ")
  )
  
  konstytucja_czysta <- rbind(konstytucja_czysta, temp)
}



# Zapis wyczyszczonych danych ----

write_csv(tokeny_konstytucja, "konstytucja_tokeny_czyste.csv")
write_csv(konstytucja_czysta, "konstytucja_czysta.csv")



# 4. Analiza restrykcyjności i liberalności języka ----


# Wczytanie oczyszczonych tokenów ----

tokeny_konstytucja <- read_csv("konstytucja_tokeny_czyste.csv", show_col_types = FALSE)


# Customowe słowniki do analizy języka ----

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


# 4.1. NEGACJA ----

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


# Analiza całej Konstytucji ----

wynik_cala_konstytucja <- tokeny_z_kategoria %>%
  count(kategoria, name = "liczba_slow") %>%
  mutate(
    udzial_proc = round(100 * liczba_slow / sum(liczba_slow), 2)
  ) %>%
  arrange(desc(liczba_slow))

print(wynik_cala_konstytucja)


# Analiza tylko słów sklasyfikowanych ----

wynik_slowa_kategoryzowane <- tokeny_z_kategoria %>%
  filter(kategoria != "inne") %>%
  count(kategoria, name = "liczba_slow") %>%
  mutate(
    udzial_proc = round(100 * liczba_slow / sum(liczba_slow), 2)
  ) %>%
  arrange(desc(liczba_slow))

print(wynik_slowa_kategoryzowane)


# 7. Indeks tekstu prawnego
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


# Zapis wyników ----

write_csv(tokeny_z_kategoria, "konstytucja_tokeny_z_kategoria.csv")
write_csv(wynik_cala_konstytucja, "wynik_cala_konstytucja_kategorie.csv")
write_csv(wynik_slowa_kategoryzowane, "wynik_slowa_kategoryzowane.csv")
write_csv(wynik_artykuly_pelny, "wynik_artykuly_liberalnosc_restrykcyjnosc.csv")
write_csv(podsumowanie_artykulow, "podsumowanie_artykulow_kategorie.csv")


# 5. Analiza asocjacji słów ----



# Wczytanie i przygotowanie danych do asocjacji ----

konstytucja_czysta <- read_csv("konstytucja_czysta.csv", show_col_types = FALSE)

# Kontrola
head(konstytucja_czysta)


# Budowa macierzy TDM i liczenie czeętości ----

# 1. Wczytanie oczyszczonych artykułów
konstytucja_czysta <- read_csv("konstytucja_czysta.csv", show_col_types = FALSE)

# 2. Budowa korpusu
corpus <- Corpus(VectorSource(konstytucja_czysta$text_clean))

# 3. Budowa macierzy Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)

# 4. Zamiana TDM na zwykłą macierz
tdm_m <- as.matrix(tdm)

# 5. Najczęstsze słowa
v <- sort(rowSums(tdm_m), decreasing = TRUE)

tdm_df <- data.frame(
  word = names(v),
  freq = v
)

head(tdm_df, 30)


# Wizualizacja ----

wykres_lizakowy_asocjacji <- function(target_word, cor_limit = 0.3, top_n = 20) {
  
  # Sprawdzenie, czy słowo istnieje w TDM
  if (!(target_word %in% Terms(tdm))) {
    
    cat("Słowo:", target_word, "nie występuje w macierzy TDM.\n")
    cat("Możliwe, że po czyszczeniu/stemmingu ma inną formę.\n")
    cat("Podobne słowa w TDM:\n")
    
    print(
      Terms(tdm)[str_detect(Terms(tdm), substr(target_word, 1, 4))]
    )
    
    return(NULL)
  }
  
  # Asocjacje metodą findAssocs()
  assoc <- findAssocs(
    x = tdm,
    terms = target_word,
    corlimit = cor_limit
  )
  
  # Wyciągamy wynik dla danego słowa
  assoc_vector <- assoc[[target_word]]
  
  # Jeżeli brak wyników
  if (length(assoc_vector) == 0) {
    
    cat("Brak asocjacji dla słowa:", target_word, "przy progu:", cor_limit, "\n")
    cat("Spróbuj obniżyć próg, np. cor_limit = 0.2 albo cor_limit = 0.1\n")
    
    return(NULL)
  }
  
  # Zamiana na data frame w stylu GitHuba:
  # word  = słowo powiązane
  # score = korelacja
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
      linewidth = 1.2
    ) +
    geom_point(linewidth = 4) +
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