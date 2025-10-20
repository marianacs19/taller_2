###############################################################################
# REPORTE: Modelos Logit para estimar probabilidad de pobreza
# Fecha: OCTUBRE 2025 
#
# Descripción:
# - Este script estima ~7 modelos logit (incluye Modelos 1–3, 4A–4C y Logit Penalizado).
# - De aquí salen los modelos "Logit f1 optimizado" y "Logit Penalizado (Elastic Net)",
#   que terminaron en el top 5 fuera de muestra por F1.
# - El código está SECCIONADO y COMENTADO para facilitar su lectura y réplica.
###############################################################################

#############################
## 0. LIMPIEZA Y LIBRERÍAS ##
#############################

rm(list = ls())

# Cargar paquetes
library(pacman)
p_load(
  rio,            # import/export
  tidyverse,      # manejo y manipulación de datos
  glmnet,         # regularización (lasso/ridge/elastic net)
  caret,          # entrenamiento y evaluación de modelos
  scatterplot3d,  # visualización 3D
  plotly,         # visualización interactiva
  pROC,           # curvas ROC/AUC
  readr,          # escritura de CSV
  dplyr,          # verbos dplyr (ya viene en tidyverse; acá por claridad)
  ggplot2         # gráficos (ya viene en tidyverse)
)

#########################################
## 1. CARGA DE DATOS Y PRE-PROCESAMIENTO
#########################################

# Cargar bases (ajustar rutas si es necesario)
load("~/GitHub/taller_2/Data/test_def.RData")
load("~/GitHub/taller_2/Data/train_def.RData")

# Variable de arriendo compilada (entrenamiento y prueba)
train_def <- train_def %>%
  mutate(arriendo_compilado_hogar = coalesce(Arriendo_estimado_mensual, Arriendo_pagado_mensual))

test_def <- test_def %>%
  mutate(arriendo_compilado_hogar = coalesce(Arriendo_estimado_mensual, Arriendo_pagado_mensual))

#########################################
## 2. MODELO LOGIT 1 (BASE PARSIMONIOSO)
#########################################

# Estimación
mylogit <- glm(
  Pobre ~ `Años_educ_mean_hogar` +
    Asalariados_hogar + arriendo_compilado_hogar +
    Afiliados_salud_hogar,
  data   = train_def,
  family = binomial()
)

# Métricas en TRAIN con corte 0.3
probs_train <- predict(mylogit, newdata = train_def, type = "response")
pred_train  <- ifelse(probs_train >= 0.3, "Pobre", "No_pobre") %>%
  factor(levels = c("No_pobre", "Pobre"))
true_train  <- train_def$Pobre

cm <- confusionMatrix(pred_train, true_train, positive = "Pobre")
cm

# F1 en TRAIN
precision <- cm$byClass["Pos Pred Value"]
recall    <- cm$byClass["Sensitivity"]
F1        <- 2 * (precision * recall) / (precision + recall)

#####################################
## 3. MODELO LOGIT 2 (AMPLIADO I) ##
#####################################

# Conjunto de predictores
pred_vars <- c(
  "Urbano","Dormitorios_hogar","Propiedad_vivienda",
  "Personas_hogar","Personas_unidad_gasto",
  "Ciudad_cat","Asalariados_hogar","Desocupados_hogar","Menores_cinco_hogar",
  "Educados_hogar","Años_educ_mean_hogar","Ancianos_hogar",
  "Afiliados_salud_hogar","Mujer_jefe","Años_educ_jefe",
  "Ocupados_hogar","Horas_trabajadas_hogar","Edad_jefe",
  "Regimen_salud_jefe","Oficio_C8_jefe","Ocupado_jefe"
)
logit_formula <- reformulate(pred_vars, response = "Pobre")

# Estimación
mylogit2 <- glm(logit_formula, data = train_def, family = binomial())

# Métricas en TRAIN con corte 0.3
probs_train2 <- predict(mylogit2, newdata = train_def, type = "response")
pred_train2  <- factor(ifelse(probs_train2 >= 0.3, "Pobre", "No_pobre"),
                       levels = c("No_pobre","Pobre"))
cm_train2 <- caret::confusionMatrix(pred_train2, train_def$Pobre, positive = "Pobre")
P <- cm_train2$byClass["Precision"]; R <- cm_train2$byClass["Recall"]
F1_train2 <- 2 * P * R / (P + R); F1_train2

#####################################
## 4. MODELO LOGIT 2 (REPETIDO)   ##
##    *Se respeta el script base* ##
#####################################

pred_vars <- c(
  "Urbano","Dormitorios_hogar","Propiedad_vivienda",
  "Personas_hogar","Personas_unidad_gasto",
  "Ciudad_cat","Asalariados_hogar","Desocupados_hogar","Menores_cinco_hogar",
  "Educados_hogar","Años_educ_mean_hogar","Ancianos_hogar",
  "Afiliados_salud_hogar","Mujer_jefe","Años_educ_jefe",
  "Ocupados_hogar","Horas_trabajadas_hogar","Edad_jefe",
  "Regimen_salud_jefe","Oficio_C8_jefe","Ocupado_jefe"
)
logit_formula <- reformulate(pred_vars, response = "Pobre")

mylogit2 <- glm(logit_formula, data = train_def, family = binomial())

probs_train2 <- predict(mylogit2, newdata = train_def, type = "response")
pred_train2  <- factor(ifelse(probs_train2 >= 0.3, "Pobre", "No_pobre"),
                       levels = c("No_pobre","Pobre"))
cm_train2 <- caret::confusionMatrix(pred_train2, train_def$Pobre, positive = "Pobre")
P <- cm_train2$byClass["Precision"]; R <- cm_train2$byClass["Recall"]
F1_train2 <- 2 * P * R / (P + R); F1_train2

#####################################
## 5. MODELO LOGIT 3 (AMPLIADO II) ##
#####################################

pred_vars <- c(
  "Urbano","Dormitorios_hogar","Propiedad_vivienda", "arriendo_compilado_hogar",
  "Personas_hogar","Personas_unidad_gasto",
  "Ciudad_cat","Asalariados_hogar","Desocupados_hogar","Menores_cinco_hogar",
  "Educados_hogar","Años_educ_mean_hogar","Ancianos_hogar",
  "Afiliados_salud_hogar","Mujer_jefe","Años_educ_jefe",
  "Ocupados_hogar","Horas_trabajadas_hogar","Edad_jefe",
  "Regimen_salud_jefe","Oficio_C8_jefe","Ocupado_jefe"
)
logit_formula <- reformulate(pred_vars, response = "Pobre")

mylogit3 <- glm(logit_formula, data = train_def, family = binomial())

probs_train3 <- predict(mylogit3, newdata = train_def, type = "response")
pred_train3  <- factor(ifelse(probs_train3 >= 0.3, "Pobre", "No_pobre"),
                       levels = c("No_pobre","Pobre"))

cm_train3 <- caret::confusionMatrix(pred_train3, train_def$Pobre, positive = "Pobre")
P <- cm_train3$byClass["Precision"]; R <- cm_train3$byClass["Recall"]
F1_train3 <- 2 * P * R / (P + R); F1_train2

# Imprimir F1s
F1
F1_train2
F1_train3

############################################
## 6. CURVAS ROC (MODELOS 1, 2 y 3 EN TRAIN)
############################################

# Función auxiliar para asegurar Pr(Pobre)
pr_pobre <- function(model, newdata) {
  p <- predict(model, newdata = newdata, type = "response")
  levs <- levels(model$y)
  if (length(levs) == 2 && levs[1] == "Pobre") {
    return(1 - p)
  } else {
    return(p)
  }
}

probs1 <- pr_pobre(mylogit,  train_def)
probs2 <- pr_pobre(mylogit2, train_def)
probs3 <- pr_pobre(mylogit3, train_def)

y <- factor(train_def$Pobre, levels = c("No_pobre","Pobre"))

roc1 <- roc(response = y, predictor = probs1, levels = c("No_pobre","Pobre"), direction = "<")
roc2 <- roc(response = y, predictor = probs2, levels = c("No_pobre","Pobre"), direction = "<")
roc3 <- roc(response = y, predictor = probs3, levels = c("No_pobre","Pobre"), direction = "<")

df1 <- coords(roc1, x = "all", ret = c("specificity","sensitivity","threshold")) %>% mutate(model="Logit 1")
df2 <- coords(roc2, x = "all", ret = c("specificity","sensitivity","threshold")) %>% mutate(model="Logit 2")
df3 <- coords(roc3, x = "all", ret = c("specificity","sensitivity","threshold")) %>% mutate(model="Logit 3")

closest_pt <- function(df, t0 = 0.3) df[which.min(abs(df$threshold - t0)), , drop = FALSE]
pt1 <- closest_pt(df1, 0.3)
pt2 <- closest_pt(df2, 0.3)
pt3 <- closest_pt(df3, 0.3)

bind_rows(df1, df2, df3) %>%
  mutate(FPR = 1 - specificity, TPR = sensitivity) %>%
  ggplot(aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = bind_rows(pt1, pt2, pt3) %>% mutate(FPR = 1 - specificity, TPR = sensitivity),
             aes(x = FPR, y = TPR, color = model), size = 2) +
  coord_equal() +
  labs(
    title = "ROC curves (train) — Logit models",
    subtitle = sprintf("AUC — Logit 1: %.3f | Logit 2: %.3f | Logit 3: %.3f",
                       auc(roc1), auc(roc2), auc(roc3)),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model"
  ) +
  theme_minimal()

# Mejor corte (más cercano a la esquina sup-izq) por modelo
best_cutoff <- function(roc_obj) {
  df <- coords(roc_obj, x = "all", ret = c("specificity","sensitivity","threshold"))
  df <- df %>%
    mutate(FPR = 1 - specificity,
           TPR = sensitivity,
           distance = sqrt((1 - TPR)^2 + (FPR)^2))
  df$threshold[which.min(df$distance)]
}

best1 <- best_cutoff(roc1)
best2 <- best_cutoff(roc2)
best3 <- best_cutoff(roc3)

cat("Best cutoff Logit1:", round(best1,3), "\n")
cat("Best cutoff Logit2:", round(best2,3), "\n")
cat("Best cutoff Logit3:", round(best3,3), "\n")

# Búsqueda de corte que maximiza F1
find_best_f1 <- function(probs, y, step = 0.01) {
  cutoffs <- seq(0, 1, by = step)
  results <- data.frame()
  for (c in cutoffs) {
    preds <- factor(ifelse(probs >= c, "Pobre", "No_pobre"),
                    levels = c("No_pobre","Pobre"))
    cm <- confusionMatrix(preds, y, positive = "Pobre")
    P <- cm$byClass["Precision"]; R <- cm$byClass["Recall"]
    F1 <- ifelse((P + R) == 0, 0, 2 * P * R / (P + R))
    results <- rbind(results, data.frame(cutoff = c, F1 = F1))
  }
  best_row <- results[which.max(results$F1), ]
  return(best_row)
}

best1_f1 <- find_best_f1(probs1, y)
best2_f1 <- find_best_f1(probs2, y)
best3_f1 <- find_best_f1(probs3, y)

best1_f1; best2_f1; best3_f1

##########################################
## 7. RESULTADOS — MODELO 3 OPTIMIZADO ##
##########################################

# Probabilidades en TEST (modelo 3)
probs_test3 <- predict(mylogit3, newdata = test_def, type = "response")

# Corte optimizado (se usa 0.33 según búsqueda de F1)
cutoffopt <- 0.33
pred_test3 <- ifelse(probs_test3 >= cutoffopt, "Pobre", "No_pobre")

# Formato de entrega
results <- test_def %>%
  transmute(
    id,
    pobre = ifelse(pred_test3 == "Pobre", 1,
                   ifelse(pred_test3 == "No_pobre", 0, NA))
  )

#########################################################
## 8. MODELOS 4A–4C (INTERACCIONES Y NO LINEALIDADES) ##
#########################################################

# Modelo 3 (baseline extendido; reestimado tal cual se definió)
mylogit3 <- glm(
  Pobre ~ Urbano + Dormitorios_hogar + Propiedad_vivienda + arriendo_compilado_hogar +
    Personas_hogar + Personas_unidad_gasto + Ciudad_cat + Asalariados_hogar +
    Desocupados_hogar + Menores_cinco_hogar + Educados_hogar +
    Años_educ_mean_hogar + Ancianos_hogar + Afiliados_salud_hogar + Mujer_jefe +
    Años_educ_jefe + Ocupados_hogar + Horas_trabajadas_hogar + Edad_jefe +
    Regimen_salud_jefe + Oficio_C8_jefe + Ocupado_jefe,
  data = train_def,
  family = binomial()
)

# Modelo 4A (interacciones capital humano y estructura del hogar)
mylogit4A <- glm(
  Pobre ~ Años_educ_mean_hogar * Asalariados_hogar +
    Años_educ_mean_hogar * Mujer_jefe +
    Asalariados_hogar * Personas_hogar +
    Propiedad_vivienda + arriendo_compilado_hogar +
    Afiliados_salud_hogar + Edad_jefe + Ancianos_hogar,
  data = train_def,
  family = binomial()
)

# Modelo 4B (vulnerabilidad estructural)
mylogit4B <- glm(
  Pobre ~ Propiedad_vivienda * arriendo_compilado_hogar +
    Asalariados_hogar * Afiliados_salud_hogar +
    Urbano * (Educados_hogar + Años_educ_mean_hogar) +
    Menores_cinco_hogar + Ancianos_hogar +
    Mujer_jefe * Regimen_salud_jefe,
  data = train_def,
  family = binomial()
)

# Modelo 4C (mercado laboral y no linealidades de ciclo de vida)
mylogit4C <- glm(
  Pobre ~ Asalariados_hogar + Ocupados_hogar + Desocupados_hogar +
    I(Edad_jefe^2) + I(Años_educ_mean_hogar^2) +
    Asalariados_hogar * Años_educ_mean_hogar +
    Propiedad_vivienda + arriendo_compilado_hogar +
    Afiliados_salud_hogar + Mujer_jefe,
  data = train_def,
  family = binomial()
)

###############################################
## 9. FUNCIONES AUXILIARES (AUC y F1 robusto) ##
###############################################

find_best_f1 <- function(probs, y, step = 0.01) {
  cutoffs <- seq(0, 1, by = step)
  results <- data.frame(cutoff = cutoffs, F1 = NA_real_)
  for (i in seq_along(cutoffs)) {
    c <- cutoffs[i]
    preds <- factor(ifelse(probs >= c, "Pobre", "No_pobre"),
                    levels = c("No_pobre","Pobre"))
    cm <- tryCatch(confusionMatrix(preds, y, positive = "Pobre"),
                   error = function(e) NULL)
    if (!is.null(cm)) {
      P <- cm$byClass["Precision"]; R <- cm$byClass["Recall"]
      results$F1[i] <- ifelse(is.na(P) || is.na(R) || (P + R) == 0, 0, 2 * P * R / (P + R))
    } else {
      results$F1[i] <- 0
    }
  }
  best_row <- results[which.max(results$F1), , drop = FALSE]
  list(best_cutoff = best_row$cutoff, best_f1 = best_row$F1)
}

evaluate_logit <- function(model, data) {
  y <- factor(data$Pobre, levels = c("No_pobre","Pobre"))
  probs <- predict(model, newdata = data, type = "response")
  roc_obj <- roc(response = y, predictor = probs, levels = c("No_pobre","Pobre"), direction = "<")
  auc_val <- as.numeric(auc(roc_obj))
  best <- find_best_f1(probs, y)
  tibble(AUC = auc_val, F1_best = best$best_f1, Cutoff_best = best$best_cutoff)
}

#################################################
## 10. LOGIT PENALIZADO (ELASTIC NET) + EXPORT ##
#################################################

# Fórmula extendida (Elastic Net)
form_m3 <- ~ Urbano + Dormitorios_hogar + Propiedad_vivienda + arriendo_compilado_hogar +
  Personas_hogar + Personas_unidad_gasto + Ciudad_cat + Asalariados_hogar +
  Desocupados_hogar + Menores_cinco_hogar + Educados_hogar +
  Años_educ_mean_hogar + Ancianos_hogar + Afiliados_salud_hogar + Mujer_jefe +
  Años_educ_jefe + Ocupados_hogar + Horas_trabajadas_hogar + Edad_jefe +
  Regimen_salud_jefe + Oficio_C8_jefe + Ocupado_jefe + Adolecentes_hogar +
  Edad_ponderada_hogar + Actividad_semana_pasada_jefe

# Asegurar niveles (No_pobre, Pobre)
train_def$Pobre <- factor(train_def$Pobre, levels = c("No_pobre","Pobre"))

# (A) Matriz de diseño para caret::train
X_train <- model.matrix(reformulate(attr(terms(form_m3), "term.labels")), data = train_def)[, -1]
train_df <- as.data.frame(X_train)
train_df$Pobre <- train_def$Pobre

# (B) Función resumen para optimizar F1 @ 0.33
f1_at_cut <- function(data, lev = NULL, model = NULL, cutoff = 0.33) {
  stopifnot(all(lev %in% names(data)))
  p <- data[[lev[2]]]
  pred <- factor(ifelse(p >= cutoff, lev[2], lev[1]), levels = lev)
  cm <- confusionMatrix(pred, data$obs, positive = lev[2])
  P <- cm$byClass["Precision"]; R <- cm$byClass["Recall"]
  F1 <- if (is.na(P) || is.na(R) || P + R == 0) 0 else 2 * P * R / (P + R)
  c(F1 = unname(F1), ROC = NA, Sens = unname(cm$byClass["Sensitivity"]), Spec = unname(cm$byClass["Specificity"]))
}
f1_033 <- function(data, lev = NULL, model = NULL) f1_at_cut(data, lev, model, cutoff = 0.33)

# (C) Control de CV y grilla de hiperparámetros
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = f1_033,
  savePredictions = "final",
  verboseIter = TRUE
)

tg <- expand.grid(
  alpha  = c(0, 0.25, 0.5, 0.75, 1),
  lambda = 10^seq(-3, 1, length.out = 30)
)

set.seed(123)
penalized_glm <- train(
  x = subset(train_df, select = -Pobre),
  y = train_df$Pobre,
  method = "glmnet",
  trControl = ctrl,
  tuneGrid = tg,
  metric = "F1"
)

penalized_glm$bestTune
max(penalized_glm$results$F1, na.rm = TRUE)

# (D) Construcción de 'mf' y 'Terms' para reproducibilidad en TEST
mf <- model.frame(
  reformulate(attr(terms(form_m3), "term.labels"), response = "Pobre"),
  data = train_def,
  na.action = na.omit
)
Terms <- terms(mf)

# (E) Preparar TEST con mismas columnas que TRAIN
ids_test <- test_def$id
mf_test  <- model.frame(delete.response(Terms), data = test_def, na.action = na.omit)
ids_test <- test_def$id[as.integer(rownames(mf_test))]

X_test <- model.matrix(delete.response(Terms), data = mf_test)[, -1, drop = FALSE]
train_cols   <- colnames(subset(train_df, select = -Pobre))
missing_cols <- setdiff(train_cols, colnames(X_test))
if (length(missing_cols)) {
  X_test <- cbind(X_test,
                  matrix(0, nrow = nrow(X_test), ncol = length(missing_cols),
                         dimnames = list(NULL, missing_cols)))
}
X_test <- X_test[, train_cols, drop = FALSE]

# (F) Predicción en TEST (penalizado) y clasificación con corte 0.33
probs_te <- predict(penalized_glm, newdata = as.data.frame(X_test), type = "prob")[, "Pobre"]
cutoff <- 0.33
pred_te <- ifelse(probs_te >= cutoff, 1L, 0L)

# (G) Resultados (id + pobre) y exportación CSV
results <- tibble(
  id    = ids_test,
  pobre = pred_te
)
write_csv(results, "penalized_logit_cutoff33.csv")

##########################################################
## 11. EXPORTS ADICIONALES                              ##
##########################################################

# Export adicional (usa objeto 'results' actual)
write_csv(results, "logit_varbestsubsetcutoff33optf1_v1reg.csv", col_names = TRUE)

# Conversión de etiquetas "Pobre"/"No_pobre" a 1/0 (si 'results$pobre' fuera factor/char)
# NOTA: En este flujo 'results$pobre' ya es 0/1; se conserva esta parte por réplica.
results <- results %>%
  mutate(pobre = ifelse(pobre == "Pobre", 1,
                        ifelse(pobre == "No_pobre", 0, pobre)))
write_csv(results, "logit_cutoff30_v1reg.csv", col_names = TRUE)
