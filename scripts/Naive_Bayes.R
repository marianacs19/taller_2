###### NAIVE BAYES 
library(e1071)
library(caret)
library(tidyverse)
library(forcats)

# Variables candidatas
vars_modelo <- setdiff(names(train), c("id","Pobre"))

# Ensamble y limpieza
train_nb <- train %>%
  select(Pobre, all_of(vars_modelo)) %>%
  mutate(across(where(is.character), as.factor)) %>%           # texto → factor
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%     # NAs numéricos → 0 (simple)
  mutate(across(where(is.factor),  ~fct_explicit_na(.))) %>%   # NAs factor → nivel "NA"
  select(Pobre, where(~ dplyr::n_distinct(.x, na.rm = TRUE) > 1)) # quitar columnas constantes

# Target como factor "0"/"1"
train_nb <- train_nb %>% mutate(Pobre = factor(as.character(Pobre), levels = c("0","1")))

set.seed(123)
idx <- createDataPartition(train_nb$Pobre, p = 0.7, list = FALSE)
train_fit <- train_nb[idx, ]
valid_fit <- train_nb[-idx, ]

# sin balancear
# Laplace=1 evita probabilidades cero en categorías raras
modelo_nb <- naiveBayes(Pobre ~ ., data = train_fit, laplace = 1)
modelo_nb

# Probabilidad de clase positiva ("1" = pobre)
pred_val_prob <- predict(modelo_nb, newdata = valid_fit, type = "raw")[, "1"]

umbrales <- seq(0.05, 0.95, by = 0.02)
f1_scores_nb <- sapply(umbrales, function(t){
  pred_bin <- ifelse(pred_val_prob > t, 1, 0)
  cm <- confusionMatrix(as.factor(pred_bin), valid_fit$Pobre, positive = "1")
  pr <- cm$byClass["Precision"]; rc <- cm$byClass["Recall"]
  2*(pr*rc)/(pr+rc)
})
best_t_nb  <- umbrales[which.max(f1_scores_nb)]
best_f1_nb <- max(f1_scores_nb)
cat("NB — threshold* =", best_t_nb, "| F1* =", round(best_f1_nb, 3), "\n")