# 📌 Train-Test Split
set.seed(0607)  # Imposta un seed fisso per garantire la riproducibilità


# Non ho fatto girare parte precedente con outiliers e ho cambiato in 'datasetCleaned al posto di datasetransformed.
# Suddivisione del dataset
split <- initial_split(dataset_cleaned, prop = 0.75)  # 75% training, 25% test
train <- training(split)
test <- testing(split)

# 📊 Controllo delle proporzioni della variabile target (hit/flop) nei due set
cat("\n📊 Proporzione di hit/flop nel training set:\n")
print(prop.table(table(train$target)))

cat("\n📊 Proporzione di hit/flop nel test set:\n")
print(prop.table(table(test$target)))

# 📊 Controllo della struttura dei due dataset
cat("\n📊 Struttura del training set:\n")
str(train)

cat("\n📊 Struttura del test set:\n")
str(test)

#DATA ANALYSIS:

# 📌 Conversione temporanea di 'target' in numerico per calcolare le correlazioni
train$target_numeric <- as.numeric(as.character(train$target)) - 1  # 0 per "flop", 1 per "hit"

# 📌 Calcolo delle correlazioni con la variabile target
correlations <- train %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ cor(.x, train$target_numeric, use = "complete.obs"))) %>%
  gather(key = "Variable", value = "Correlation") %>%
  arrange(desc(abs(Correlation)))

# 📊 Stampiamo le variabili più correlate
cat("📊 Variabili più correlate con target:\n")
print(head(correlations, 10))

# 📌 Visualizzazione della distribuzione delle variabili più correlate
top_variables <- correlations %>% filter(Variable != "target_numeric") %>% top_n(6, abs(Correlation)) %>% pull(Variable)

# Creiamo i density plot
plots <- lapply(top_variables, function(var) {
  ggplot(train, aes_string(x = var, fill = "target")) +
    geom_density(alpha = 0.4) +
    ggtitle(paste("Density Plot di", var)) +
    xlab(var) +
    theme_minimal()
})

# Mostriamo i plot in una griglia
do.call(grid.arrange, c(plots, ncol = 2))



#nuovo
library(rlang)       # For 'sym' and tidy evaluation
library(gridExtra)   # For arranging multiple plots in a grid

# Select the top 6 variables correlated with 'target_numeric', excluding 'target_numeric'
top_variables <- correlations %>%
  filter(Variable != "target_numeric") %>%
  top_n(6, abs(Correlation)) %>%
  pull(Variable)

# Create density plots for each variable
plots <- lapply(top_variables, function(var) {
  ggplot(train, aes(x = !!sym(var), fill = target, group = target)) +
    geom_density(alpha = 0.4) +
    scale_fill_discrete(name = "Track Type", labels = c("Flop", "Hit")) +
    labs(
      title = paste("Density Plot di", var),
      x = var,
      y = "Density"
    ) +
    theme_minimal()
})

# Arrange the plots in a 2-column grid
do.call(grid.arrange, c(plots, ncol = 2))

#seconda prova plot. QUELLA DA TENERE!!
library(rlang)
library(gridExtra)

top_variables <- c("log_instrumentalness", "danceability", 
                   "std_loudness", "valence", 
                   "acousticness", "time_signature")

# Define custom x-limits for specific variables
# Adjust these ranges to your preference
var_limits <- list(
  #log_instrumentalness = c(0,1),   # Show the main spread of log_instrumentalness
  time_signature       = c(3, 5),    # Focus on 2 to 4 to skip the tails
  std_loudness         = c(-4, 2)    # Show the range from -4 to 0
)

plots <- lapply(top_variables, function(var) {
  
  # Create a base density plot
  p <- ggplot(train, aes(x = !!sym(var), fill = target, group = target)) +
    geom_density(alpha = 0.4) +
    scale_fill_discrete(name = "Track Type", labels = c("Flop", "Hit")) +
    labs(
      title = paste("Density Plot di", var),
      x = var,
      y = "Density"
    ) +
    theme_minimal()
  
  # Conditionally zoom in if this variable is in 'var_limits'
  if (var %in% names(var_limits)) {
    p <- p + coord_cartesian(xlim = var_limits[[var]])
  }
  
  return(p)
})

# Arrange the plots in a 2-column grid
do.call(grid.arrange, c(plots, ncol = 2))



# 📌 Rimuoviamo la colonna temporanea
train <- train %>% select(-target_numeric)

# 📌 Calcolo della matrice di correlazione per le variabili numeriche
numeric_features <- train %>% select(where(is.numeric))

# 📌 Calcoliamo la matrice di correlazione
cor_matrix <- cor(numeric_features, use = "complete.obs")

# 📌 Visualizzazione della matrice di correlazione con corrplot
corrplot(cor_matrix, method = "number", diag = FALSE, tl.cex = 0.6, number.cex = 0.6, tl.col = "black")

# 📊 Output della matrice in forma tabellare (opzionale, se vuoi ispezionare i valori numerici)
cat("\n📊 Matrice di correlazione (prime 10 righe):\n")
print(round(cor_matrix[1:10, 1:10], 2))



# Calcolo delle correlazioni parziali su tutte le variabili numeriche
partial_corr <- correlation(train %>% select(where(is.numeric)), partial = TRUE)

# 📊 Stampiamo le correlazioni parziali con valori assoluti maggiori di 0.3
cat("📊 Correlazioni parziali con valori assoluti > 0.3:\n")
top_partial_corr <- partial_corr %>%
  filter(abs(r) > 0.3) %>%  # Soglia aggiornata per includere valori > 0.3
  arrange(desc(abs(r)))

print(top_partial_corr)

# 📌 Visualizzazione delle relazioni tra variabili più correlate

# Selezioniamo le prime coppie per la visualizzazione (modificabile secondo necessità)
top_pairs <- top_partial_corr %>% slice(1:6)  # Puoi regolare il numero di coppie qui

# Genera grafici per ogni coppia
plots <- lapply(1:nrow(top_pairs), function(i) {
  ggplot(train, aes_string(x = top_pairs$Parameter1[i], y = top_pairs$Parameter2[i])) +
    geom_jitter(color = "darkgreen", alpha = 0.5) +
    xlab(top_pairs$Parameter1[i]) +
    ylab(top_pairs$Parameter2[i]) +
    ggtitle(paste("Partial Correlation:", round(top_pairs$r[i], 2))) +
    theme_minimal()
})

# Mostriamo i grafici
do.call(grid.arrange, c(plots, nrow = 2))
