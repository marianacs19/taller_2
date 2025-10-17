rm(list = ls())

#Modelos con logit:

library(pacman)

p_load(rio,           # import/export data
       tidyverse,     # tidy-data
       glmnet,        # To implement regularization algorithms. 
       caret,         # Creating predictive models
       scatterplot3d, # For 3D visualization
       plotly         # For interactive 3D plots
)

load("~/GitHub/taller_2/Data/test_def.RData")
load("~/GitHub/taller_2/Data/train_def.RData")

library(dplyr)
library(readr)
library(caret)

library(dplyr)

train_def <- train_def %>%
  mutate(arriendo_compilado_hogar = coalesce(Arriendo_estimado_mensual, Arriendo_pagado_mensual))

test_def <- train_def %>%
  mutate(arriendo_compilado_hogar = coalesce(Arriendo_estimado_mensual, Arriendo_pagado_mensual))

# --- Fit model ---
mylogit <- glm(
  Pobre ~ `Años_educ_mean_hogar` +
    Asalariados_hogar  + arriendo_compilado_hogar +
    Afiliados_salud_hogar,
  data   = train_def,
  family = binomial()
)


# --- TRAIN metrics (cutoff 0.3) ---
probs_train <- predict(mylogit, newdata = train_def, type = "response")

# label with your factor levels
pred_train <- ifelse(probs_train >= 0.3, "Pobre", "No_pobre")
pred_train <- factor(pred_train, levels = c("No_pobre", "Pobre"))
true_train <- train_def$Pobre  # already has levels c("No_pobre","Pobre")

cm <- confusionMatrix(pred_train, true_train, positive = "Pobre")
cm  # prints confusion matrix, precision/recall, etc.

# F1 (from confusion matrix)
precision <- cm$byClass["Pos Pred Value"]
recall    <- cm$byClass["Sensitivity"]
F1        <- 2 * (precision * recall) / (precision + recall)
F1

# --- TEST predictions + CSV ---
probs_test <- predict(mylogit, newdata = test_def, type = "response")
pred_test  <- ifelse(probs_test >= 0.3, "Pobre", "No_pobre")

results <- test_def %>%
  transmute(id, pobre = pred_test)

results <- results %>%
  mutate(pobre = ifelse(pobre == "Pobre", 1,
                        ifelse(pobre == "No_pobre", 0, NA)))
write_csv(results, "logit_cutoff30_v1reg.csv", col_names = TRUE)
