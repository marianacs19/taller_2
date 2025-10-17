rm(list = ls())
library(forcats)
library(ranger)
library(caret)
library(tidyverse)

train <- train_def
test <- test_def

vars_modelo <- c(
  "Nivel_educativo_jefe", "Mujer_jefe", "Menores_cinco_hogar",
  "Personas_hogar", "Años_educ_mean_hogar", "Oficio_jefe", "Personas_con_Ayudas_gobierno_12m_hogar" ,
  "Asalariados_hogar", "Arriendo_sumado", "Afiliados_salud_hogar", "Urbano"
)


rf_df <- train %>%
  select(Pobre, all_of(vars_modelo)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  mutate(across(where(is.factor),  ~fct_explicit_na(.))) %>%
  mutate(Pobre = factor(ifelse(as.character(Pobre) %in% c("1","Yes","Pobre"), "Yes", "No"),
                        levels = c("No","Yes")))

# ---- Control de entrenamiento ----
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,  # para ROC
  verboseIter = TRUE,
  savePredictions = "final"
)

# ---- Grid de hiperparámetros ----
grid <- expand.grid(
  mtry = c(2, 4, 6, 8),
  splitrule = "gini",
  min.node.size = c(10, 20)
)

set.seed(1010)

rf_model <- train(
  Pobre ~ .,
  data = rf_df,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = grid,
  metric = "ROC",
  importance = "impurity"
)

# ===============================================================
# Predicciones en train (usando caret::train con CV)
# ===============================================================
# Extraer probabilidades de la clase positiva ("Yes")
pred_prob <- rf_model$pred$Yes
y_true    <- rf_model$pred$obs

# Aplicar cutoff fijo de 0.3
cutoff <- 0.3
pred_class <- ifelse(pred_prob >= cutoff, "Yes", "No") %>%
  factor(levels = c("No","Yes"))

# Matriz de confusión
cm_rf <- caret::confusionMatrix(pred_class, y_true, positive = "Yes")
cm_rf

# Métricas
precision_rf <- cm_rf$byClass["Precision"]
recall_rf    <- cm_rf$byClass["Recall"]
F1_rf <- 2 * ((precision_rf * recall_rf) / (precision_rf + recall_rf))

cat("Cutoff =", cutoff,
    "| Precision =", round(precision_rf,3),
    "| Recall =", round(recall_rf,3),
    "| F1 =", round(F1_rf,3), "\n")

# ===============================================================
# Predicciones en test
# ===============================================================

# Preprocesar test igual que train
test_mod <- test %>%
  select(all_of(vars_modelo)) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.numeric), ~replace_na(., 0)),
         across(where(is.factor),  ~fct_explicit_na(.)))

# Alinear niveles de factores con train
for (v in intersect(names(test_mod), names(rf_df))) {
  if (is.factor(rf_df[[v]]) && is.factor(test_mod[[v]])) {
    test_mod[[v]] <- factor(test_mod[[v]], levels = levels(rf_df[[v]]))
  }
}

# Probabilidades y clasificación final
pred_test_prob  <- predict(rf_model, newdata = test_mod, type = "prob")[, "Yes"]
pred_test_class <- ifelse(pred_test_prob >= cutoff, 1, 0)

# ===============================================================
# Exportar resultados
# ===============================================================
submission_rf <- tibble(id = test$id, Pobre = pred_test_class)
readr::write_csv(submission_rf, "/Users/marianacorrea/Desktop/PEG/RandomForest_cutoff03.csv")

cat("Archivo guardado: RandomForest_cutoff03.csv\n")



plot(varImp(rf_model), main = "Importancia de Variables — Random Forest")