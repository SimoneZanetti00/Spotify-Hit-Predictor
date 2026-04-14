library(MASS)
library(ggplot2)
library(dplyr)

# 📌 1. Definizione del modello LDA
lda_model <- lda(target ~ . - sections - std_loudness, data = train_balanced)

# 📌 2. Sommario del modello
cat("\n📊 Sommario del modello LDA:\n")
print(lda_model)

# 📌 3. Predizioni sul test set
pred_lda <- predict(lda_model, test)
posterior_lda <- pred_lda$posterior

# 📌 4. Classificazione con diverse soglie
thresholds <- c(0.4, 0.5, 0.6, 0.2, 0.3)
predictions_lda <- list()
conf_matrices <- list()
misclassification_rates <- c()

for (threshold in thresholds) {
  pred_classes <- as.factor(ifelse(posterior_lda[, 2] > threshold, 1, 0))
  conf_matrix <- table(test$target, pred_classes)
  misclassification_rate <- mean(pred_classes != test$target)
  
  # Salviamo i risultati
  predictions_lda[[paste0("Threshold_", threshold)]] <- pred_classes
  conf_matrices[[paste0("Threshold_", threshold)]] <- conf_matrix
  misclassification_rates <- c(misclassification_rates, misclassification_rate)
  
  cat("\n📊 Risultati per threshold =", threshold, ":\n")
  print(conf_matrix)
  cat("Tasso di errore:", round(misclassification_rate, 4), "\n")
}

# 📌 5. Istogramma delle combinazioni lineari
ldahist(pred_lda$x[, 1], g = pred_lda$class, col = c("blue", "red"))

# 📊 Visualizzazione della combinazione lineare
combination_plot <- ggplot(data.frame(x = pred_lda$x[, 1], class = pred_lda$class), 
                           aes(x = x, fill = as.factor(class))) +
  geom_histogram(binwidth = 0.5, position = "dodge", alpha = 0.7) +
  labs(x = "Combinazione lineare", y = "Frequenza", fill = "Classe", 
       title = "Combinazioni lineari e classi (LDA)") +
  theme_minimal()

print(combination_plot)

# 📌 Riassunto generale
lda_results <- data.frame(
  Threshold = thresholds,
  Misclassification_Rate = misclassification_rates
)

cat("\n📊 Riassunto dei risultati:\n")
print(lda_results)
