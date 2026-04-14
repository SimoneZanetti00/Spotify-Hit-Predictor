# ─────────────────────────────────────────────
# Caricamento delle librerie necessarie

library(tidymodels)  # per initial_split, training, testing
library(dplyr)       # per la manipolazione dei dati
library(ROSE)        # per ovun.sample
library(caret)       # per confusionMatrix
library(class)       # per knn


summary(dataset_cleaned)
# ─────────────────────────────────────────────
# 1. Rimozione delle variabili collineari
# Il dataset 'dataset_cleaned' ha le seguenti colonne:
#   danceability, energy, key, mode, speechiness, acousticness,
#   instrumentalness, liveness, valence, time_signature, chorus_hit,
#   sections, target, std_loudness, std_tempo, log_duration_ms
# Supponiamo di rimuovere: energy, instrumentalness, valence e sections.
#dataset_knn <- dataset_cleaned %>% 
#  select(-energy, -instrumentalness, -valence)

dataset_knn <- dataset_cleaned %>% 
  dplyr::select(-energy, -instrumentalness, -valence, -sections)


# Verifichiamo quali colonne sono rimaste
print(names(dataset_knn))
# Dovresti avere:
# "danceability", "key", "mode", "speechiness", "acousticness",
# "liveness", "time_signature", "chorus_hit", "target",
# "std_loudness", "std_tempo", "log_duration_ms"

# ─────────────────────────────────────────────
# 2. Definizione della funzione per il min-max normalization
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# ─────────────────────────────────────────────
# 3. Split del dataset in training e test (75% / 25%)
set.seed(0607)
split_knn <- initial_split(dataset_knn, prop = 0.75)
train_knn <- training(split_knn)
test_knn  <- testing(split_knn)

# ─────────────────────────────────────────────
# 4. Normalizzazione: calcoliamo la normalizzazione min-max
# Per evitare data leakage, normalizziamo le feature (cioè le colonne numeriche eccetto "target")

numeric_cols <- train_knn %>% 
  dplyr::select(-target) %>% 
  dplyr::select(where(is.numeric)) %>% 
  names()


# Normalizziamo il training set
train_norm <- train_knn
train_norm[, numeric_cols] <- lapply(train_knn[, numeric_cols], min_max_norm)

# Calcoliamo i parametri (min e max) dal training set per ogni colonna numerica
scaling_params <- lapply(train_knn[, numeric_cols], function(x) c(min = min(x), max = max(x)))

# Funzione ausiliaria per normalizzare utilizzando i parametri calcolati
min_max_norm_with_params <- function(x, params) {
  (x - params["min"]) / (params["max"] - params["min"])
}

# Normalizziamo il test set usando i parametri del training set
test_norm <- test_knn
for (col in numeric_cols) {
  test_norm[[col]] <- min_max_norm_with_params(test_knn[[col]], scaling_params[[col]])
}

# ─────────────────────────────────────────────
# 5. Bilanciamento del training set (opzionale)
# Anche se il nostro dataset risulta già bilanciato, mostriamo come utilizzare ovun.sample.
# In questo esempio usiamo l'undersampling con p = 0.5 (la probabilità desiderata per la classe di riferimento).
set.seed(1)
train_balanced <- ovun.sample(target ~ ., data = train_norm, method = "under", p = 0.5, seed = 1)$data

# Controlliamo la distribuzione delle classi
print(prop.table(table(train_balanced$target)))

# ─────────────────────────────────────────────
# 6. Ricerca del miglior valore di k per il modello KNN
kmax <- 100
test_error <- numeric(kmax)

# Prepariamo le features (rimuoviamo la variabile target)
train_features <- train_balanced %>% dplyr::select(-target)
test_features  <- test_norm %>% dplyr::select(-target)


for(k in 1:kmax) {
  # Eseguiamo il modello KNN per il valore corrente di k
  knn_pred <- knn(train = train_features, 
                  test  = test_features, 
                  cl    = train_balanced$target, 
                  k     = k)
  
  # Calcoliamo la matrice di confusione e l'accuratezza
  cm <- confusionMatrix(data = as.factor(knn_pred), reference = test_norm$target)
  
  # Salviamo l'errore (1 - Accuracy)
  test_error[k] <- 1 - cm$overall["Accuracy"]
}

# Identifichiamo il valore di k che minimizza l'errore sul test set

k_min <- which.min(test_error)
cat("Il valore ottimale di k è:", k_min, "\n")

# ─────────────────────────────────────────────
# 7. Previsione finale con il miglior k
knn_pred_min <- knn(train = train_features, 
                    test  = test_features, 
                    cl    = train_balanced$target, 
                    k     = k_min)

# ─────────────────────────────────────────────
# 8. Valutazione: Matrice di confusione e Accuratezza sul test set
conf_matrix <- table(Actual = test_norm$target, Predicted = knn_pred_min)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuratezza del modello KNN con k =", k_min, ":", accuracy, "\n")



# ─────────────────────────────────────────────
# 1. Calcolo dell'accuratezza dalla matrice di confusione
accuracy <- function(conf_mat) {
  # La funzione calcola l'accuratezza in percentuale
  sum(diag(conf_mat)) / sum(conf_mat) * 100
}

# Supponendo che la matrice di confusione sia stata salvata in 'conf_matrix'
acc_value <- accuracy(conf_matrix)
cat("Accuracy (funzione custom):", acc_value, "%\n")


# ─────────────────────────────────────────────
# 2. Calcolo e visualizzazione della matrice di confusione con 'caret'
# (Assicurati di aver caricato la libreria caret)
library(caret)
cm <- confusionMatrix(data = knn_pred_min, reference = test_norm$target)
print(cm)


# ─────────────────────────────────────────────
# 3. Plot dell'andamento dell'errore di test in funzione di k
library(ggplot2)

# Creiamo un data frame per plottare l'errore di test
df_error <- data.frame(
  k = 1:kmax,
  test_error = test_error
)

ggplot(df_error, aes(x = k, y = test_error)) +
  geom_line(colour = "blue") +
  geom_point(colour = "blue") +
  xlab("K (#neighbors)") +
  ylab("Test error") +
  ggtitle(paste0("Best value of K = ", k_min, 
                 " (minimal error = ", 
                 format(test_error[k_min] * 100, digits = 4), "%)"))


# ─────────────────────────────────────────────
# 4. Plot a dispersione delle previsioni KNN
# Aggiungiamo le previsioni al dataset normalizzato del test
test_norm$knn_pred <- knn_pred_min

# Scegliamo due feature per il grafico (ad es. std_loudness e std_tempo)
ggplot(test_norm, aes(x = std_loudness, y = std_tempo, color = knn_pred)) +
  geom_point() +
  labs(x = "Standardized Loudness",
       y = "Standardized Tempo",
       color = "Predicted Target",
       title = paste0("KNN with K = ", k_min)) +
  theme(legend.position = c(0.8, 0.8))
