# 🎵 Spotify Hit Predictor

Binary classification project to predict whether a song becomes a **Hit or Flop** based on audio features from the [Spotify Hit Predictor Dataset](https://www.kaggle.com/datasets/theoverman/the-spotify-hit-predictor-dataset).

> **Course:** Statistical Methods for Data Science — University of Padova  
> **Author:** Simone Zanetti  
> **Date:** February 2025

---

## 📌 Objective

Predict whether a song (2010s decade) will be a **Hit** (featured on Billboard Hot-100) or a **Flop**, using Spotify audio features. The analysis emphasizes **Recall for the Hit class** to minimize missed opportunities, while maintaining overall classification accuracy.

---

## 📁 Repository Structure

```
Spotify-Hit-Predictor/
├── Src/
│   ├── 01_Load_Cleaning_DataSet.R       # Data loading, cleaning, preprocessing
│   ├── 02_Correlation_Analysis.R        # Feature correlation + partial correlations
│   ├── 03_Balancing_Dataset.R           # Class imbalance handling (ROSE)
│   ├── 04_Simple_Logistic_Regression.R  # GLM with VIF-based feature selection
│   ├── 05_LR_StepWiseSelection.R        # Stepwise logistic regression (AIC)
│   ├── 06_Weighted_GLR.R                # Class-weighted logistic regression
│   ├── 07_Plots_First3Models.R          # Visualizations for GLM models
│   ├── 08_Shapiro_Test.R                # Normality testing (Shapiro-Wilk)
│   ├── 09_LDA.R                         # Linear Discriminant Analysis
│   ├── 10_LDA.R                         # LDA with threshold analysis
│   ├── 11_QDA.R                         # Quadratic Discriminant Analysis
│   ├── 12_Lasso_Ridge.R                 # Regularized regression (glmnet)
│   ├── 13_KNN.R                         # K-Nearest Neighbors (k=24)
│   ├── 14_ROC_Analysis.R                # ROC curves + AUC comparison
│   └── 15_Plots_final.R                 # Final comparison plots + ROI analysis
├── Report/
│   └── StatisticalB_Simone_Zanetti.pdf  # Full project report
└── README.md
```

---

## 📊 Dataset

- **Source:** Spotify Hit Predictor Dataset (Kaggle)
- **Subset used:** 2010–2020 decade (~6,398 songs, 19 attributes)
- **Target variable:** `target` — 1 = Hit, 0 = Flop
- **Key features:** `danceability`, `energy`, `loudness`, `acousticness`, `instrumentalness`, `valence`, `tempo`, `speechiness`, and more

**Preprocessing steps:**
- Removed identifier columns (`track`, `artist`, `uri`)
- Standardized `loudness` and `tempo`
- Log-transformed `duration_ms` and `instrumentalness`
- Train/test split: 75% / 25% (stratified, `seed = 0607`)
- Class balancing via ROSE (`ovun.sample`)

---

## 🤖 Models Applied

| Model | Misclassification Rate | Precision (Hit) |
|---|---|---|
| Simple GLM (t=0.5) | 19.56% | 75.97% |
| Stepwise GLM (t=0.5) | **18.25%** | 77.68% |
| Weighted GLM (t=0.6) | 18.12% | **80.54%** |
| LDA (t=0.6) | 19.50% | 76.02% |
| QDA (t=0.5) | 19.31% | 76.43% |
| Ridge Regression | 19.38% | 75.84% |
| Lasso Regression | 18.44% | 77.35% |
| KNN (k=24) | 27.81% | 81.57% |

---

## 📈 Key Findings

- **Most predictive features:** `log_instrumentalness` (r = -0.61), `danceability` (r = 0.40), `loudness` (r = 0.33)
- Hit songs tend to be **vocal, danceable, loud, and less acoustic**
- **Best overall model:** Stepwise GLM (lowest error rate, 18.25%)
- **Best for precision/ROI:** Weighted GLM at threshold 0.6 (ROI ≈ 49%)
- KNN achieves high precision but suffers from high false negatives — not recommended for investment decisions

---

## 💰 ROI Analysis

Simulated investment on 100 predicted hit tracks (€10,000 each):

| Model | Precision | Net Profit | ROI |
|---|---|---|---|
| Weighted GLM (0.6) | 80.54% | €489,720 | ~49% |
| Lasso Regression | 77.35% | €430,600 | ~43% |
| KNN (k=24) | 81.57% | €512,100 | ~51%* |

*KNN's ROI is misleadingly high — its high false negative rate means many actual hits are missed entirely.

---

## 🛠️ Tools & Libraries

- **Language:** R
- **Main libraries:** `tidyverse`, `ggplot2`, `tidymodels`, `glmnet`, `MASS`, `caret`, `ROSE`, `pROC`, `class`, `corrplot`

---

## 🔮 Future Work

- Extend analysis to other decades (60s–2010s) to capture musical trend shifts
- Incorporate social media trends and streaming patterns
- Explore deep learning approaches (RNN, Transformers) for temporal pattern capture
