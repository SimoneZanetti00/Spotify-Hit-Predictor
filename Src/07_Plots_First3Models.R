# 📌 Confrontiamo la classificazione originale con quella stimata dai modelli

library(ggplot2)
library(gridExtra)
# 📌 Confrontiamo la classificazione originale con quella stimata dai modelli

# Visualizzazione della classificazione originale nel test set
original_plot <- ggplot(test, aes(x = danceability, y = energy, color = as.factor(target))) +
  geom_point() +
  labs(x = "Danceability", y = "Energy", color = "Hit (1) / Flop (0)", 
       title = "Distribuzione Originale dei Dati") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))

# Confrontiamo le previsioni dei modelli
plot_glm <- ggplot(test, aes(x = danceability, y = energy, color = as.factor(ifelse(predictions > 0.5, 1, 0)))) +
  geom_point() +
  labs(x = "Danceability", y = "Energy", color = "Predizione GLM", 
       title = "GLM Semplice") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))

plot_stepwise <- ggplot(test, aes(x = danceability, y = energy, color = as.factor(ifelse(predictions_stepwise > 0.5, 1, 0)))) +
  geom_point() +
  labs(x = "Danceability", y = "Energy", color = "Predizione Stepwise", 
       title = "GLM con Stepwise") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))

plot_weighted <- ggplot(test, aes(x = danceability, y = energy, color = as.factor(ifelse(pred_weighted > 0.5, 1, 0)))) +
  geom_point() +
  labs(x = "Danceability", y = "Energy", color = "Predizione GLM con Pesi", 
       title = "GLM con Pesi") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))

# 📊 Confrontiamo le quattro classificazioni
grid.arrange(original_plot, plot_glm, plot_stepwise, plot_weighted, nrow = 2)



# 📌 Funzione per creare il grafico della curva logistica stimata
plot_logistic_curve <- function(predictions, title) {
  predicted_data <- data.frame(prob_of_hit = predictions, target = test$target)
  predicted_data <- predicted_data[order(predicted_data$prob_of_hit, decreasing = FALSE),]
  predicted_data$rank <- 1:nrow(predicted_data)
  
  ggplot(data = predicted_data, aes(x = rank, y = prob_of_hit)) +
    geom_point(aes(color = as.factor(target)), alpha = 1, shape = 1, stroke = 1) +
    xlab("Index") +
    ylab("Predicted Probability") +
    ggtitle(title) +
    theme_minimal()
}

# 📊 Creiamo i grafici per ogni modello
plot_glm_curve <- plot_logistic_curve(predictions, "Estimated Logistic Curve - Simple GLM")
plot_stepwise_curve <- plot_logistic_curve(predictions_stepwise, "Estimated Logistic Curve - GLM with Stepwise")
plot_weighted_curve <- plot_logistic_curve(pred_weighted, "Estimated Logistic Curve - GLM with Weighted Data")

# 📊 Confrontiamo le curve logistiche
grid.arrange(plot_glm_curve, plot_stepwise_curve, plot_weighted_curve, nrow = 2)
