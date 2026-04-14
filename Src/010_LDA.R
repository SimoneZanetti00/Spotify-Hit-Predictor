# 📌 Librerie necessarie
library(MASS)
library(ggplot2)
library(gridExtra)
library(outliers)

# 📌 Funzione per rilevare e rimuovere gli outlier utilizzando il test chi-quadro
remove_outliers <- function(data, column_name) {
  test_result <- chisq.out.test(data[[column_name]])
  if (test_result$p.value < 0.05) {
    outlier_value <- ifelse(test_result$alternative == "lowest value", min(data[[column_name]]), max(data[[column_name]]))
    return(data[data[[column_name]] != outlier_value, ])
  } else {
    return(data)
  }
}

# 📌 Rimozione degli outlier per ogni variabile rilevante
columns_to_check <- c("danceability", "energy", "speechiness", "acousticness", 
                      "instrumentalness", "liveness", "valence", "std_tempo", "log_duration_ms")

train_balanced_no_outliers <- train_balanced
for (column in columns_to_check) {
  train_balanced_no_outliers <- remove_outliers(train_balanced_no_outliers, column)
}

# 📌 Confronto con boxplot
plots <- lapply(columns_to_check, function(column) {
  ggplot(train_balanced, aes_string(y = column, fill = "2")) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
    theme(legend.position = "none") +
    ylab(column) +
    ggtitle(paste("Boxplot di", column))
})
do.call(grid.arrange, c(plots, nrow = 3))

# 📌 Riepilogo del dataset senza outlier
cat("Dimensioni originali:", dim(train_balanced), "\n")
cat("Dimensioni senza outlier:", dim(train_balanced_no_outliers), "\n")

# 📌 Riesecuzione del modello LDA
lda_model_no_outliers <- lda(target ~ . - sections - std_loudness, data = train_balanced_no_outliers)

# 📌 Sommario del modello LDA
cat("\n📊 Sommario del modello LDA senza outlier:\n")
print(lda_model_no_outliers)

# 📌 Predizioni sul test set
pred_lda_no_outliers <- predict(lda_model_no_outliers, test)
posterior_lda_no_outliers <- pred_lda_no_outliers$posterior

# 📌 Classificazione con diverse soglie
thresholds <- c(0.4, 0.5, 0.6, 0.2, 0.3)
conf_matrices_no_outliers <- list()
misclassification_rates_no_outliers <- c()

for (threshold in thresholds) {
  pred_classes <- as.factor(ifelse(posterior_lda_no_outliers[, 2] > threshold, 1, 0))
  conf_matrix <- table(test$target, pred_classes)
  misclassification_rate <- mean(pred_classes != test$target)
  
  # Salviamo i risultati
  conf_matrices_no_outliers[[paste0("Threshold_", threshold)]] <- conf_matrix
  misclassification_rates_no_outliers <- c(misclassification_rates_no_outliers, misclassification_rate)
  
  cat("\n📊 Risultati per threshold =", threshold, ":\n")
  print(conf_matrix)
  cat("Tasso di errore:", round(misclassification_rate, 4), "\n")
}

# 📌 Riassunto generale dei risultati
lda_results_no_outliers <- data.frame(
  Threshold = thresholds,
  Misclassification_Rate = misclassification_rates_no_outliers
)
cat("\n📊 Riassunto dei risultati senza outlier:\n")
print(lda_results_no_outliers)
print(lda_results)
