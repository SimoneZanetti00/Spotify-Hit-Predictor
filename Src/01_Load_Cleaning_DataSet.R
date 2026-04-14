library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tidymodels)
library(leaps)
library(glmnet)
library(pROC)
library(rsample)
library(correlation)
library(DataExplorer)
library(knitr)
library(corrplot)
library(regclass)
library(rsample)
library(corrplot)
library(outliers)
library(dplyr)
library(caret)
library(class)


# 📌 1. Caricamento del dataset del 2010
dataset_2010s <- read_csv("dataset-of-10s.csv")




# 📌 2. Rinominazione delle colonne (sostituzione di spazi e caratteri non alfanumerici)
names(dataset_2010s) <- gsub("\\s+", "_", names(dataset_2010s))
names(dataset_2010s) <- gsub("[^[:alnum:]_]", "", names(dataset_2010s))

# 📌 3. Verifica della struttura del dataset
cat("📊 Struttura del dataset:\n")
str(dataset_2010s)

# 📌 4. Prime righe del dataset
cat("📊 Prime righe del dataset:\n")
head(dataset_2010s)

# 📌 5. Controlliamo la presenza di valori mancanti
missing_values <- colSums(is.na(dataset_2010s))

# 📌 Visualizziamo solo le colonne con valori mancanti

missing_values <- missing_values[missing_values > 0]

# 📌 Stampiamo il numero di valori mancanti per ciascuna colonna
cat("📊 Valori mancanti per colonna:\n")
print(missing_values)

# 📌 6. Rimozione di identificatori non utili
dataset_cleaned <- dataset_2010s %>% select(-track, -artist, -uri)

# 📌 7. Controlliamo la presenza di variabili costanti (un solo valore per tutta la colonna)
constant_cols <- dataset_cleaned %>% summarise_all(n_distinct) %>% gather() %>%
  filter(value == 1) %>% pull(key)

# 📌 Rimuoviamo eventuali variabili costanti
dataset_cleaned <- dataset_cleaned %>% select(-all_of(constant_cols))

cat("📊 Colonne rimosse perché costanti:\n")
print(constant_cols)
#head(dataset_cleaned)
#str(dataset_cleaned)
#summary(dataset_cleaned)



# 📌 8. Funzione di standardizzazione
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# 📌 Applichiamo la standardizzazione alle variabili chiave
dataset_cleaned <- dataset_cleaned %>%
  mutate(
    std_loudness = standardize(loudness),
    std_tempo = standardize(tempo),
    log_duration_ms = log(duration_ms + 1)  # Trasformazione logaritmica
  ) %>%
  select(-loudness, -tempo, -duration_ms)  # Rimuoviamo le versioni originali
# 📌 Conversione della variabile target in fattore


#prova
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Apply transformations to key variables
# 1. Standardize loudness and tempo
# 2. Log-transform duration_ms (+1 to avoid log(0))
# 3. Log-transform instrumentalness (+0.0001 to avoid log(0))
dataset_cleaned <- dataset_cleaned %>%
  mutate(
    std_loudness = standardize(loudness),
    std_tempo = standardize(tempo),
    log_duration_ms = log(duration_ms + 1),
    log_instrumentalness = log(instrumentalness + 0.0001)
  ) %>%
  dplyr::select(-loudness, -tempo, -duration_ms, -instrumentalness)








dataset_cleaned <- dataset_cleaned %>% mutate(target = as.factor(target))

# 📊 Controlliamo la distribuzione delle classi
cat("\n📊 Distribuzione della variabile target (hit/flop):\n")
print(prop.table(table(dataset_cleaned$target)))


# 📌 Controlliamo la nuova struttura del dataset
cat("\n📊 Struttura finale del dataset dopo la pulizia:\n")
str(dataset_cleaned)

# 📌 Stampiamo le prime righe del dataset pulito
cat("\n📊 Prime righe del dataset pulito:\n")
print(head(dataset_cleaned))


# 📌 Identificazione degli outlier con il metodo IQR
#identify_outliers <- function(x) {
#  Q1 <- quantile(x, 0.25, na.rm = TRUE)
#  Q3 <- quantile(x, 0.75, na.rm = TRUE)
#  IQR <- Q3 - Q1
#  lower_bound <- Q1 - 1.5 * IQR
#  upper_bound <- Q3 + 1.5 * IQR
#  return(x < lower_bound | x > upper_bound)
#}

# 📌 Selezioniamo le variabili numeriche
#numeric_features <- dataset_cleaned %>% select(where(is.numeric))

# 📌 Identificazione degli outlier per ogni variabile numerica
#outliers <- apply(numeric_features, 2, identify_outliers)

# 📌 Contiamo gli outlier per ogni variabile
#outlier_counts <- colSums(outliers)

# 📊 Stampiamo il numero di outlier per ogni variabile
#cat("📌 Numero di outlier per variabile:\n")
#print(outlier_counts)

# 📊 Visualizzazione degli outlier con boxplot
#plots <- lapply(names(numeric_features), function(var) {
#  ggplot(dataset_cleaned, aes_string(y = var)) +
#    geom_boxplot(fill = "blue", alpha = 0.5, outlier.color = "red") +
#    ggtitle(paste("Boxplot di", var)) +
#    theme_minimal()
#})

# 📊 Visualizziamo i boxplot
#do.call(grid.arrange, c(plots, ncol = 3))
# 📊 Visualizzazione degli outlier con boxplot
#plots <- lapply(names(numeric_features), function(var) {
#  ggplot(dataset_cleaned, aes_string(y = var)) +
#    geom_boxplot(fill = "blue", alpha = 0.5, outlier.color = "red") +
#    ggtitle(paste("Boxplot di", var)) +
#    theme_minimal()
#})

# 📊 Visualizziamo i boxplot
#do.call(grid.arrange, c(plots, ncol = 3))



#Funzione per rimuovere gli outlier
#remove_outliers <- function(df, cols) {
#  for (col in cols) {
#    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
#    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
#    IQR <- Q3 - Q1
#    lower_bound <- Q1 - 1.5 * IQR
#    upper_bound <- Q3 + 1.5 * IQR
#    df <- df %>% filter(df[[col]] >= lower_bound & df[[col]] <= upper_bound)
#  }
#  return(df)
#}

# Rimuoviamo gli outlier estremi per 'instrumentalness', 'chorus_hit', 'sections'
#variables_to_filter <- c("instrumentalness", "chorus_hit", "sections")
#dataset_no_outliers <- remove_outliers(dataset_cleaned, variables_to_filter)

# Applichiamo la trasformazione logaritmica
#dataset_transformed <- dataset_no_outliers %>%
#  mutate(
#    log_chorus_hit = log(chorus_hit + 1),
#    log_duration_ms = log(log_duration_ms + 1)  # Logaritmo di log_duration_ms
#  ) %>%
#  select(-chorus_hit)  # Rimuoviamo la colonna originale

# Controlliamo la struttura del dataset aggiornato
#cat("\n📊 Struttura finale del dataset dopo la rimozione degli outlier e le trasformazioni:\n")
#str(dataset_transformed)

# Stampiamo un riepilogo delle variabili numeriche
#cat("\n📊 Riepilogo delle variabili numeriche:\n")
#summary(dataset_transformed)

# Visualizziamo i nuovi boxplot per confermare
#numeric_features_transformed <- dataset_transformed %>% select(where(is.numeric))

# Creazione dei boxplot aggiornati
#plots_transformed <- lapply(names(numeric_features_transformed), function(var) {
#  ggplot(dataset_transformed, aes_string(y = var)) +
#    geom_boxplot(fill = "blue", alpha = 0.5, outlier.color = "red") +
#    ggtitle(paste("Boxplot di", var)) +
#    theme_minimal()
})

# Mostriamo i boxplot
#do.call(grid.arrange, c(plots_transformed, ncol = 3))


#Osservo. Sono ancora presenti outliers ma Gli outlier non sono sempre negativi: in un dataset musicale, i valori estremi possono rappresentare caratteristiche uniche di certe canzoni (ad esempio, brani molto rumorosi o silenziosi, brani puramente strumentali, ecc.).
#Rimuovere ulteriori outlier potrebbe causare una perdita di informazioni rilevanti, specialmente se questi valori rappresentano casi reali e non errori.


