# Etap 2. Czyszczenie tekstu Konstytucji RP ----

# Instalacja pakietów, jeśli nie są zainstalowane ----
# install.packages(c("tm", "stringr", "dplyr", "readr", "stopwords"))
# install.packages("tidyr")
# Ładowanie pakietów ----
library(tm)
library(stringr)
library(dplyr)
library(readr)
library(stopwords)
library(tidyr)

# Wczytanie danych ----
konstytucja <- read_csv("konstytucja_artykuly.csv", show_col_types = FALSE)

# Sprawdzenie struktury danych ----
str(konstytucja)
head(konstytucja)

# Zakładamy, że plik ma kolumny:
# id   - numer artykułu / preambuła
# text - treść artykułu


# 1. Dodatkowe stop words dopasowane do Konstytucji ----

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


# 2. Ręczny stemming / ręczna lematyzacja ----

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


# 3. Funkcja do czyszczenia tekstu jednego artykułu ----

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


# 4. Czyszczenie wszystkich artykułów ----

lista_slow <- list()

for (i in 1:nrow(konstytucja)) {
  
  lista_slow[[i]] <- process_text(konstytucja$text[i])
  
}


# 5. Zbudowanie tabeli po czyszczeniu: jeden wiersz = jeden token ----

tokeny_konstytucja <- data.frame()

for (i in 1:nrow(konstytucja)) {
  
  temp <- data.frame(
    id = konstytucja$id[i],
    pozycja = seq_along(lista_slow[[i]]),
    word = lista_slow[[i]]
  )
  
  tokeny_konstytucja <- rbind(tokeny_konstytucja, temp)
}


# 6. Stworzenie wersji tekstowej po czyszczeniu: jeden wiersz = jeden artykuł ----

konstytucja_czysta <- data.frame()

for (i in 1:nrow(konstytucja)) {
  
  temp <- data.frame(
    id = konstytucja$id[i],
    text_clean = paste(lista_slow[[i]], collapse = " ")
  )
  
  konstytucja_czysta <- rbind(konstytucja_czysta, temp)
}



# 7. Zapis wyczyszczonych danych ----

write_csv(tokeny_konstytucja, "konstytucja_tokeny_czyste.csv")
write_csv(konstytucja_czysta, "konstytucja_czysta.csv")



# Etap 3. Analiza restrykcyjności i liberalności języka Konstytucji RP ----


# 1. Wczytanie oczyszczonych tokenów ----

tokeny_konstytucja <- read_csv("konstytucja_tokeny_czyste.csv", show_col_types = FALSE)


# 2. Customowe słowniki do analizy języka ----

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


# 3. Zbudowanie jednego słownika z kategoriami ----

slownik_kategorie <- data.frame(
  word = c(slownik_liberalny, slownik_restrykcyjny, slownik_instytucjonalny),
  kategoria = c(
    rep("liberalny", length(slownik_liberalny)),
    rep("restrykcyjny", length(slownik_restrykcyjny)),
    rep("instytucjonalny", length(slownik_instytucjonalny))
  )
)

# Jeżeli jakieś słowo przypadkiem jest w kilku słownikach,
# zostawiamy pierwsze przypisanie.
slownik_kategorie <- slownik_kategorie %>%
  distinct(word, .keep_all = TRUE)


# Kontrola słownika
print(slownik_kategorie)


# 4. Przypisanie kategorii do tokenów ----

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


# 5. Analiza całej Konstytucji ----

wynik_cala_konstytucja <- tokeny_z_kategoria %>%
  count(kategoria, name = "liczba_slow") %>%
  mutate(
    udzial_proc = round(100 * liczba_slow / sum(liczba_slow), 2)
  ) %>%
  arrange(desc(liczba_slow))

print(wynik_cala_konstytucja)


# 6. Analiza tylko słów sklasyfikowanych ----
# Czyli bez kategorii "inne".

wynik_slowa_kategoryzowane <- tokeny_z_kategoria %>%
  filter(kategoria != "inne") %>%
  count(kategoria, name = "liczba_slow") %>%
  mutate(
    udzial_proc = round(100 * liczba_slow / sum(liczba_slow), 2)
  ) %>%
  arrange(desc(liczba_slow))

print(wynik_slowa_kategoryzowane)


# 7. Indeks liberalności dla całej Konstytucji ----
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

cat("Liczba słów liberalnych:", liczba_liberalnych, "\n")
cat("Liczba słów restrykcyjnych:", liczba_restrykcyjnych, "\n")
cat("Indeks liberalności całej Konstytucji:", indeks_liberalnosci_caly, "\n")


# 8. Analiza według artykułów ----

wynik_artykuly <- tokeny_z_kategoria %>%
  filter(kategoria != "inne") %>%
  count(id, kategoria, name = "liczba_slow") %>%
  pivot_wider(
    names_from = kategoria,
    values_from = liczba_slow,
    values_fill = 0
  )

# Dodajemy brakujące kolumny, gdyby któraś kategoria nie wystąpiła
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


# 9. Dodanie artykułów, w których nie wykryto żadnego słowa ze słownika ----
# Dzięki temu w tabeli końcowej będą wszystkie artykuły.

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


# 10. Podsumowanie liczby artykułów według dominującej kategorii ----

podsumowanie_artykulow <- wynik_artykuly_pelny %>%
  count(dominujaca_kategoria, name = "liczba_artykulow") %>%
  mutate(
    udzial_proc = round(100 * liczba_artykulow / sum(liczba_artykulow), 2)
  ) %>%
  arrange(desc(liczba_artykulow))

print(podsumowanie_artykulow)


# 11. Zapis wyników ----

write_csv(tokeny_z_kategoria, "konstytucja_tokeny_z_kategoria.csv")
write_csv(wynik_cala_konstytucja, "wynik_cala_konstytucja_kategorie.csv")
write_csv(wynik_slowa_kategoryzowane, "wynik_slowa_kategoryzowane.csv")
write_csv(wynik_artykuly_pelny, "wynik_artykuly_liberalnosc_restrykcyjnosc.csv")
write_csv(podsumowanie_artykulow, "podsumowanie_artykulow_kategorie.csv")


# 12. Proste wykresy wyników ----

# Wykres 1: udział kategorii w całej Konstytucji, bez "inne"

ggplot(wynik_slowa_kategoryzowane,
       aes(x = reorder(kategoria, liczba_slow), y = liczba_slow)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Liczba słów liberalnych, restrykcyjnych i instytucjonalnych",
    x = "Kategoria",
    y = "Liczba słów"
  )


# Wykres 2: liczba artykułów według dominującej kategorii

ggplot(podsumowanie_artykulow,
       aes(x = reorder(dominujaca_kategoria, liczba_artykulow), y = liczba_artykulow)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Dominujący typ języka w artykułach Konstytucji RP",
    x = "Dominująca kategoria",
    y = "Liczba artykułów"
  )