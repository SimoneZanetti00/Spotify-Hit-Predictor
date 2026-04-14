# 📌 Caricamento della libreria
library(MASS)

# 📌 1. Definizione del modello QDA sui dati originali
qda_model <- qda(target ~ . - sections - std_loudness, data = train)

# 📌 2. Predizioni sul test set
pred_qda <- predict(qda_model, test)
posterior_qda <- pred_qda$posterior

# 📌 3. Classificazione con diverse soglie
thresholds <- c(0.3, 0.4, 0.5, 0.6)
qda_predictions <- list()
qda_conf_matrices <- list()
qda_misclassification_rates <- c()

for (threshold in thresholds) {
  pred_classes <- as.factor(ifelse(posterior_qda[, 2] > threshold, 1, 0))
  conf_matrix <- table(test$target, pred_classes)
  misclassification_rate <- mean(pred_classes != test$target)
  
  # Salviamo i risultati
  qda_predictions[[paste0("Threshold_", threshold)]] <- pred_classes
  qda_conf_matrices[[paste0("Threshold_", threshold)]] <- conf_matrix
  qda_misclassification_rates <- c(qda_misclassification_rates, misclassification_rate)
  
  cat("\n📊 Risultati per threshold =", threshold, ":\n")
  print(conf_matrix)
  cat("Tasso di errore:", round(misclassification_rate, 4), "\n")
}

# 📌 Riassunto dei risultati
qda_results <- data.frame(
  Threshold = thresholds,
  Misclassification_Rate = qda_misclassification_rates
)

cat("\n📊 Riassunto dei risultati QDA sui dati originali:\n")
print(qda_results)
