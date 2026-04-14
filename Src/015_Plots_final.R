# Carichiamo le librerie necessarie
library(ggplot2)
library(gridExtra)

# -------------------------------
# Plot per il modello GLM
# Si usa un threshold di 0.5 per convertire le probabilità in classi (0/1)
plot_glm <- ggplot(test_norm, aes(x = danceability, y = std_tempo,
                                  color = as.factor(ifelse(pred_glm_best > 0.5, 1, 0)))) +
  geom_point() +
  labs(x = "Danceability", y = "Standardized Tempo",
       color = "Predicted Target", title = "GLM") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))

# -------------------------------
# Plot per il modello LDA
# Il vettore di classe predetto è disponibile in pred_lda_best$class
plot_lda <- ggplot(test_norm, aes(x = danceability, y = std_tempo,
                                  color = as.factor(pred_lda_best$class))) +
  geom_point() +
  labs(x = "Danceability", y = "Standardized Tempo",
       color = "Predicted Target", title = "LDA") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))

# -------------------------------
# Plot per il modello QDA
# Anche in questo caso la classe predetta è in pred_qda_best$class
plot_qda <- ggplot(test_norm, aes(x = danceability, y = std_tempo,
                                  color = as.factor(pred_qda_best$class))) +
  geom_point() +
  labs(x = "Danceability", y = "Standardized Tempo",
       color = "Predicted Target", title = "QDA") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))

# -------------------------------
# Plot per il modello Ridge
# Convertiamo la probabilità in classe usando un threshold di 0.5
# Nota: pred_ridge_best viene da glmnet e potrebbe essere una matrice: usiamo as.numeric() se necessario.
plot_ridge <- ggplot(test_norm, aes(x = danceability, y = std_tempo,
                                    color = as.factor(ifelse(as.numeric(pred_ridge_best) > 0.5, 1, 0)))) +
  geom_point() +
  labs(x = "Danceability", y = "Standardized Tempo",
       color = "Predicted Target", title = "Ridge") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))

# -------------------------------
# Plot per il modello Lasso
plot_lasso <- ggplot(test_norm, aes(x = danceability, y = std_tempo,
                                    color = as.factor(ifelse(as.numeric(pred_lasso_best) > 0.5, 1, 0)))) +
  geom_point() +
  labs(x = "Danceability", y = "Standardized Tempo",
       color = "Predicted Target", title = "Lasso") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))

# -------------------------------
# Visualizzazione dei plot in una griglia
grid.arrange(plot_glm, plot_lda, plot_qda, plot_ridge, plot_lasso, nrow = 2)
