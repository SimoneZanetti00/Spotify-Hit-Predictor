# 📌 Calcolo del peso per la classe minoritaria (flop o hit, a seconda dei dati)
weight_factor <- sum(train$target == 0) / sum(train$target == 1)  # Se target=1 è minoritario

# 📌 Creazione del vettore dei pesi
weights <- rep(1, nrow(train))  # Peso base = 1
weights[train$target == 1] <- weight_factor  # Aumentiamo il peso della classe minoritaria

# 📌 Stampa del peso assegnato
cat("📊 Peso assegnato alla classe meno rappresentata:", round(weight_factor, 2), "\n")


# 📌 Creazione del modello con pesi
glm_weighted <- glm(target ~ ., data = train, family = "binomial", weights = weights)

# 📊 Riepilogo del modello
cat("\n📊 Riepilogo del modello con pesi:\n")
print(summary(glm_weighted))


# 📌 Predizioni sul test set
pred_weighted <- predict(glm_weighted, test, type = "response")

# 📌 Definizione delle soglie di classificazione
thresholds <- c(0.4, 0.5, 0.6)
results_weighted <- list()

# 📊 Valutazione del modello per ogni soglia
for (threshold in thresholds) {
  
  # Conversione delle probabilità in classi binarie (hit/flop)
  pred_classes <- ifelse(pred_weighted > threshold, 1, 0)
  
  # Calcolo della matrice di confusione
  conf_matrix <- table(test$target, pred_classes)
  
  # Calcolo del tasso di errore
  misclassification_rate <- mean(pred_classes != as.numeric(as.character(test$target)))
  
  # Salvataggio dei risultati
  results_weighted[[paste0("Threshold_", threshold)]] <- list(
    ConfusionMatrix = conf_matrix,
    MisclassificationRate = misclassification_rate
  )
  
  # 📊 Mostriamo i risultati per ogni soglia
  cat("\n📊 Risultati per threshold =", threshold, ":\n")
  print(conf_matrix)
  cat("📊 Tasso di misclassificazione:", round(misclassification_rate, 4), "\n")
}

# 📊 Mostriamo i risultati finali
cat("\n📊 Risultati finali del modello con pesi per ogni soglia:\n")
print(results_weighted)
