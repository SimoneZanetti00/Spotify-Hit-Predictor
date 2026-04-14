# 📌 Caricamento delle librerie
library(glmnet)
library(caret)

# 📌 Conversione dei dati in matrici per glmnet
train_bal_mat <- as.matrix(train_balanced[, -which(names(train_balanced) == "target")])
test_mat <- as.matrix(test[, -which(names(test) == "target")])

# 📌 Definizione della variabile target (fattore)
train_target <- as.factor(train_balanced$target)
test_target <- as.factor(test$target)


# 📌 Ricerca del miglior valore di lambda con Cross Validation
ridge_cv <- cv.glmnet(train_bal_mat, train_target, alpha = 0, family = "binomial", type.measure = "class")

# 📌 Plot dell'errore in funzione di log(lambda)
plot(ridge_cv)

# 📌 Identificazione del miglior valore di lambda
lambda_opt_ridge <- ridge_cv$lambda.min
cat("Miglior valore di lambda per Ridge:", lambda_opt_ridge, "\n")

# 📌 Predizioni con il modello Ridge
pred_ridge <- predict(ridge_cv, test_mat, type = "class", s = lambda_opt_ridge)

# 📌 Matrice di confusione per Ridge
conf_matrix_ridge <- table(test_target, pred_ridge)
cat("\n📊 Matrice di confusione per Ridge Regression:\n")
print(conf_matrix_ridge)

# 📌 Calcolo del tasso di errore per Ridge
misclassification_ridge <- mean(pred_ridge != test_target)
cat("Tasso di errore per Ridge:", round(misclassification_ridge, 4), "\n")




# 📌 Ricerca del miglior valore di lambda con Cross Validation
lasso_cv <- cv.glmnet(train_bal_mat, train_target, alpha = 1, family = "binomial", type.measure = "class")

# 📌 Plot dell'errore in funzione di log(lambda)
plot(lasso_cv)

# 📌 Identificazione del miglior valore di lambda
lambda_opt_lasso <- lasso_cv$lambda.min
cat("Miglior valore di lambda per Lasso:", lambda_opt_lasso, "\n")

# 📌 Predizioni con il modello Lasso
pred_lasso <- predict(lasso_cv, test_mat, type = "class", s = lambda_opt_lasso)

# 📌 Matrice di confusione per Lasso
conf_matrix_lasso <- table(test_target, pred_lasso)
cat("\n📊 Matrice di confusione per Lasso Regression:\n")
print(conf_matrix_lasso)

# 📌 Calcolo del tasso di errore per Lasso
misclassification_lasso <- mean(pred_lasso != test_target)
cat("Tasso di errore per Lasso:", round(misclassification_lasso, 4), "\n")



# 📌 Creazione di un DataFrame con i risultati
results_comparison <- data.frame(
  Model = c("Ridge", "Lasso"),
  Misclassification_Rate = c(misclassification_ridge, misclassification_lasso)
)

# 📌 Visualizzazione dei risultati
cat("\n📊 Confronto tra Ridge e Lasso:\n")
print(results_comparison)

# 📊 Grafico di confronto
library(ggplot2)
ggplot(results_comparison, aes(x = Model, y = Misclassification_Rate, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Confronto tra Ridge e Lasso Regression",
       x = "Modello", y = "Tasso di errore") +
  theme_minimal()
