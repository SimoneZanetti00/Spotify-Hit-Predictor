numeric_vars <- train_balanced[, sapply(train_balanced, is.numeric)]
numeric_vars <- numeric_vars[, !(colnames(numeric_vars) %in% "target")]


# 📌 Applicazione del test di Shapiro-Wilk su ogni variabile per le due classi
shapiro_results <- lapply(names(numeric_vars), function(var) {
  hit_data <- train_balanced[[var]][train_balanced$target == 1]
  flop_data <- train_balanced[[var]][train_balanced$target == 0]
  
  hit_test <- shapiro.test(hit_data)
  flop_test <- shapiro.test(flop_data)
  
  list(
    Variable = var,
    Hit_W = hit_test$statistic,
    Hit_p_value = hit_test$p.value,
    Flop_W = flop_test$statistic,
    Flop_p_value = flop_test$p.value
  )
})

# 📌 Conversione dei risultati in un data frame

shapiro_df <- do.call(rbind, lapply(shapiro_results, as.data.frame))
rownames(shapiro_df) <- NULL

# 📊 Visualizzazione dei risultati
cat("📊 Risultati del test di Shapiro-Wilk:\n")
print(shapiro_df)
