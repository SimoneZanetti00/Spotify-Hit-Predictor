# ─────────────────────────────────────────────
# Caricamento delle librerie necessarie
library(tidymodels)   # per roc_curve(), autoplot(), tibble, ecc.
library(dplyr)
library(ggplot2)
library(glmnet)
library(MASS)         # per lda() e qda()

# Supponiamo che i seguenti oggetti siano già definiti:
# - train_balanced: training set bilanciato (dopo normalizzazione)
# - test_norm: test set normalizzato (stesso schema di variabili di train_balanced)
# - train_bal_mat: matrice delle feature del training (usata per glmnet)
# - test_mat: matrice delle feature del test
# - lambda_opt_ridge, lambda_opt_lasso: ipotetici valori ottimali ottenuti tramite cv.glmnet

# Se non li hai già, puoi costruirli così:
train_bal_mat <- as.matrix(train_balanced %>% dplyr::select(-target))
test_mat      <- as.matrix(test_norm %>% dplyr::select(-target))
# (Assicurati che train_balanced$target sia un vettore numerico o un fattore binario compatibile con glmnet)

# ─────────────────────────────────────────────
# 1. Best GLM model  
# (Modifica la formula in base alle scelte fatte nella tua analisi; qui un esempio)
glm_best <- glm(
  target ~ danceability + std_loudness + std_tempo + log_duration_ms,
  data = train_balanced, 
  family = "binomial"
)
pred_glm_best <- predict(glm_best, test_norm, type = "response")
# pred_glm_best è un vettore numerico delle probabilità per la classe "1"

# ─────────────────────────────────────────────
# 2. Best LDA model  
# (Esempio: utilizziamo altre variabili; potrai modificare la formula)
lda_best <- lda(
  target ~ danceability + acousticness + liveness + std_tempo,
  data = train_balanced
)
pred_lda_best <- predict(lda_best, test_norm)
post_lda_best <- pred_lda_best$posterior  
# post_lda_best[,2] sono le probabilità per la classe "1"

# ─────────────────────────────────────────────
# 3. Best QDA model  
# (Nel riferimento si sceglie il modello addestrato sui dati originali; qui usiamo il training non bilanciato,
#  ipotizzando che il set "train_knn" contenga i dati originali, oppure puoi usare train se preferisci)
qda_best <- qda(
  target ~ danceability + speechiness + std_loudness + log_duration_ms,
  data = train_knn   # oppure 'data = train' se quello era il dataset originale non bilanciato
)
pred_qda_best <- predict(qda_best, test_norm)
post_qda_best <- pred_qda_best$posterior  
# post_qda_best[,2] sono le probabilità per la classe "1"

# ─────────────────────────────────────────────
# 4. Best Ridge model  
ridge_best <- glmnet(
  train_bal_mat, 
  train_balanced$target, 
  alpha = 0, 
  family = "binomial", 
  lambda = lambda_opt_ridge
)


test_norm <- test_norm %>% dplyr::select(-knn_pred)

# Ora costruiamo la matrice per il test set usando le stesse colonne dei predittori del training set.
predictor_names <- colnames(train_balanced %>% dplyr::select(-target))
test_mat <- as.matrix(test_norm[, predictor_names])

# Verifica il numero di colonne
cat("Numero di colonne in train_bal_mat:", ncol(train_bal_mat), "\n")  # Dovrebbe essere 11
cat("Numero di colonne in test_mat:", ncol(test_mat), "\n")   



pred_ridge_best <- predict(ridge_best, test_mat, type = "response", s = lambda_opt_ridge)
# pred_ridge_best è una matrice: usa as.numeric() per convertirla in vettore

# ─────────────────────────────────────────────
# 5. Best Lasso model  
lasso_best <- glmnet(
  train_bal_mat, 
  train_balanced$target, 
  alpha = 1, 
  family = "binomial", 
  lambda = lambda_opt_lasso
)
pred_lasso_best <- predict(lasso_best, test_mat, type = "response", s = lambda_opt_lasso)
# pred_lasso_best è una matrice: usa as.numeric() per convertirla in vettore

# ─────────────────────────────────────────────
# 6. Confronto dei modelli tramite ROC curve  
# Creiamo un tibble che raccoglie la "verità" e le probabilità predette per ogni modello.
# NOTA: qui usiamo test_norm$target; assicurati che le classi siano coerenti (ad es. "0" e "1")
prediction <- tibble(truth = as.factor(test_norm$target)) %>%
  mutate(pred = as.numeric(pred_glm_best)) %>%  # modello GLM
  mutate(model = "GLM") %>%
  add_row(truth = as.factor(test_norm$target), 
          pred = as.numeric(post_lda_best[, 2]), 
          model = "LDA") %>%
  add_row(truth = as.factor(test_norm$target), 
          pred = as.numeric(post_qda_best[, 2]), 
          model = "QDA") %>%
  add_row(truth = as.factor(test_norm$target), 
          pred = as.numeric(pred_ridge_best), 
          model = "Ridge") %>%
  add_row(truth = as.factor(test_norm$target), 
          pred = as.numeric(pred_lasso_best), 
          model = "Lasso")

# Calcoliamo e plottiamo le ROC curve per ciascun modello
roc <- prediction %>%
  group_by(model) %>%
  roc_curve(truth, pred, event_level = "second") %>%
  autoplot() +
  ggtitle("Confronto ROC Curve tra i modelli") +
  theme_minimal()

# Mostriamo il plot
print(roc)
