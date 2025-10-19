# ---- Limpieza y libererías ----
rm(list = ls())
library(forcats)
library(ranger)
library(caret)
library(tidyverse)
library(pROC)

# Estableciendo rutas -----------------------------------------------------

wd_main <- "taller_2"
wd_code <- "scripts"
wd_outputs <- "Estimations"
wd_data <- "Data"

# ---- Bases de datos ----
train <- load(paste0(wd_data, "/train_def.RData"))
test <- load(paste0(wd_data, "/test_def.RData"))

train <- train_def
test <- test_def

# ---- Variables del modelo ----
vars_modelo <- c(
  "Urbano", "Dormitorios_hogar", "Propiedad_vivienda",
  "Personas_hogar", "Personas_unidad_gasto",
  "Ciudad_cat", "Asalariados_hogar", "Desocupados_hogar", "Menores_cinco_hogar",
  "Educados_hogar", "Años_educ_mean_hogar", "Ancianos_hogar",
  "Afiliados_salud_hogar", "Mujer_jefe", "Años_educ_jefe",
  "Ocupados_hogar", "Horas_trabajadas_hogar", "Edad_jefe",
  "Regimen_salud_jefe", "Oficio_C8_jefe", "Ocupado_jefe", "Arriendo_sumado"
)

rf_df <- train %>%
  select(Pobre, all_of(vars_modelo)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  mutate(across(where(is.factor),  ~fct_explicit_na(.))) %>%
  mutate(Pobre = factor(ifelse(as.character(Pobre) %in% c("1","Yes","Pobre"), "Yes", "No"),
                        levels = c("No","Yes")))

# ---- Entrenamiento ----
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,  # para ROC
  verboseIter = TRUE,
  savePredictions = "final"
)

# ---- Ajustar hiperparámetros ----
grid <- expand.grid(
  mtry = c(2, 4, 6, 8),
  splitrule = "gini",
  min.node.size = c(10, 20)
)

# ---- Entrenamiento con todo el train set ----
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

rf_model$results

# ---- Probabilidades ----
pred_prob <- rf_model$pred$Yes
y_true    <- rf_model$pred$obs

# ---- Cutoff ----
cutoff <- 0.3
pred_class <- ifelse(pred_prob >= cutoff, "Yes", "No") %>%
  factor(levels = c("No","Yes"))

# ---- Matriz de confusión ----
cm_rf <- caret::confusionMatrix(pred_class, y_true, positive = "Yes")
cm_rf

# ---- Métricas ----
precision_rf <- cm_rf$byClass["Precision"]
recall_rf    <- cm_rf$byClass["Recall"]
F1_rf <- 2 * ((precision_rf * recall_rf) / (precision_rf + recall_rf))

cat("Cutoff =", cutoff,
    "| Precision =", round(precision_rf,3),
    "| Recall =", round(recall_rf,3),
    "| F1 =", round(F1_rf,3), "\n")

# ---- Predicciones en test set ----
test_mod <- test %>%
  select(all_of(vars_modelo)) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.numeric), ~replace_na(., 0)),
         across(where(is.factor),  ~fct_explicit_na(.)))

for (v in intersect(names(test_mod), names(rf_df))) {
  if (is.factor(rf_df[[v]]) && is.factor(test_mod[[v]])) {
    test_mod[[v]] <- factor(test_mod[[v]], levels = levels(rf_df[[v]]))
  }
}

# ---- Clasificación final ----
pred_test_prob  <- predict(rf_model, newdata = test_mod, type = "prob")[, "Yes"]
pred_test_class <- ifelse(pred_test_prob >= cutoff, 1, 0)

# ---- Resultados ----
submission_rf <- tibble(id = test$id, Pobre = pred_test_class)
write.csv(submission_rf, paste0(wd_outputs, "/RandomForest2_cutoff03.csv"), row.names = FALSE)

# ---- Importancia de Variables ----
plot(varImp(rf_model), main = "Importancia de Variables — Random Forest")

# ---- Curva ROC ----
roc_rf <- roc(response = rf_model$pred$obs,
              predictor = rf_model$pred$Yes,
              levels = c("No", "Yes"),
              direction = "<")

cat("AUC del modelo Random Forest:", round(auc(roc_rf), 3), "\n")

plot(roc_rf, 
     col = "#2C7BB6",            # color azul elegante
     lwd = 2,                    # grosor de línea
     main = "Curva ROC — Modelo Random Forest",
     cex.main = 1.2,
     cex.lab = 1,
     cex.axis = 0.9)
abline(a = 0, b = 1, lty = 2, col = "gray50")
text(0.6, 0.2, paste("AUC =", round(auc(roc_rf), 3)), col = "#2C7BB6", cex = 1.1)
ggsave(
  filename = paste0(wd_outputs, "/ROC_RandomForest.png"),
  width = 6,
  height = 5,
  dpi = 300
)