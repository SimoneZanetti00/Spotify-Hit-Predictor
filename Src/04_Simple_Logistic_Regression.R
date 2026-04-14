# 📌 Creazione del modello logistico completo
glm_full <- glm(target ~ ., data = train_balanced, family = "binomial")

# 📊 Calcolo dell'R-Squared e soglia del VIF
s <- summary(glm_full)
r2 <- 1 - (s$deviance / s$null.deviance)
vif_threshold <- 1 / (1 - r2)
cat("\n📊 R-Squared del modello completo:", round(r2, 3), "\n")
cat("📊 Soglia del VIF:", round(vif_threshold, 3), "\n")

# 📊 Calcolo del VIF per individuare collinearità
library(car)
vif_values <- vif(glm_full)
cat("\n📊 Valori di VIF per ciascuna variabile:\n")
print(round(vif_values, 2))

# 📌 Iterazione: Rimuoviamo variabili con VIF elevato e ricreiamo il modello
# In base ai risultati del VIF, rimuoviamo iterativamente le variabili collineari
glm_final <- glm(target ~ . - sections - std_loudness,  # Modificare in base ai risultati del VIF
                 data = train_balanced, 
                 family = "binomial")

# 📊 Riepilogo del modello finale
cat("\n📊 Riepilogo del modello finale:\n")
print(summary(glm_final))

# 📌 Predizioni sul test set
predictions <- predict(glm_final, test, type = "response")

# 📌 Conversione delle probabilità in classi in base alle soglie
thresholds <- c(0.4, 0.5, 0.6)
results <- list()

for (threshold in thresholds) {
  pred_classes <- ifelse(predictions > threshold, 1, 0)
  conf_matrix <- table(test$target, pred_classes)
  misclassification_rate <- mean(pred_classes != as.numeric(as.character(test$target)))
  
  # Salva i risultati per ogni soglia
  results[[paste0("Threshold_", threshold)]] <- list(
    ConfusionMatrix = conf_matrix,
    MisclassificationRate = misclassification_rate
  )
  
  # 📊 Mostra i risultati
  cat("\n📊 Risultati per threshold =", threshold, ":\n")
  print(conf_matrix)
  cat("📊 Tasso di misclassificazione:", round(misclassification_rate, 4), "\n")
}

# 📌 Risultati completi
cat("\n📊 Risultati finali per ogni soglia:\n")
print(results)
