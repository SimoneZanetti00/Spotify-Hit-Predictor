# 📌 Caricamento delle librerie necessarie
library(MASS)  # Per stepAIC
library(caret) # Per confusion matrix

# 📌 Creazione del modello logistico completo (partiamo dal modello precedente)
glm_full <- glm(target ~ ., data = train_balanced, family = "binomial")

# 📌 Applicazione della selezione Stepwise per minimizzare l'AIC
glm_stepwise <- stepAIC(glm_full, direction = "both", trace = FALSE)

# 📊 Osserviamo il riepilogo del modello ottenuto
cat("\n📊 Riepilogo del modello Stepwise:\n")
summary(glm_stepwise)

# 📌 Predizioni sul test set
predictions_stepwise <- predict(glm_stepwise, test, type = "response")

# 📌 Definizione delle soglie di classificazione
thresholds <- c(0.4, 0.5, 0.6)
results_stepwise <- list()

# 📊 Valutazione del modello per ogni soglia
for (threshold in thresholds) {
  
  # Conversione delle probabilità in classi binarie (hit/flop)
  pred_classes <- ifelse(predictions_stepwise > threshold, 1, 0)
  
  # Calcolo della matrice di confusione
  conf_matrix <- table(test$target, pred_classes)
  
  # Calcolo del tasso di errore
  misclassification_rate <- mean(pred_classes != as.numeric(as.character(test$target)))
  
  # Salvataggio dei risultati
  results_stepwise[[paste0("Threshold_", threshold)]] <- list(
    ConfusionMatrix = conf_matrix,
    MisclassificationRate = misclassification_rate
  )
  
  # 📊 Mostriamo i risultati per ogni soglia
  cat("\n📊 Risultati per threshold =", threshold, ":\n")
  print(conf_matrix)
  cat("📊 Tasso di misclassificazione:", round(misclassification_rate, 4), "\n")
}

# 📊 Mostriamo i risultati finali
cat("\n📊 Risultati finali del modello Stepwise per ogni soglia:\n")
print(results_stepwise)
