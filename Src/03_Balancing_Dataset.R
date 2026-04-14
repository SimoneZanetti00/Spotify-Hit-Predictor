# 📌 Caricamento delle librerie necessarie
library(ROSE)
library(caret)

# 📌 Bilanciamento del dataset

set.seed(1)  # Imposta un seed per la riproducibilità
train_balanced <- ovun.sample(target ~ ., data = train, method = "both", 
                              p = 0.5,  # Proporzione bilanciata tra le classi
                              N = nrow(train),  # Mantieni la dimensione del training set
                              seed = 1)$data

# 📊 Controllo delle proporzioni delle classi nel dataset bilanciato
cat("\n📊 Proporzioni delle classi nel dataset bilanciato:\n")

print(prop.table(table(train_balanced$target)))

# 📌 Definizione delle soglie di classificazione
threshold4 <- 0.4
threshold5 <- 0.5
threshold6 <- 0.6

# 📊 Output delle soglie definite
cat("\n📊 Soglie di classificazione definite:\n")
cat("Threshold 4:", threshold4, "\n")
cat("Threshold 5:", threshold5, "\n")
cat("Threshold 6:", threshold6, "\n")


