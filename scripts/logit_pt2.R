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

test_def <- test_def %>%
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

# Logit Model 2 =============

# Build formula
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

# Fit
mylogit2 <- glm(logit_formula, data = train_def, family = binomial())

# Train predictions
probs_train2 <- predict(mylogit2, newdata = train_def, type = "response")
pred_train2  <- factor(ifelse(probs_train2 >= 0.3, "Pobre", "No_pobre"),
                       levels = c("No_pobre","Pobre"))

# F1 on TRAIN
cm_train2 <- caret::confusionMatrix(pred_train2, train_def$Pobre, positive = "Pobre")
P <- cm_train2$byClass["Precision"]; R <- cm_train2$byClass["Recall"]
F1_train2 <- 2 * P * R / (P + R); F1_train2


# Build formula
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

# Fit
mylogit2 <- glm(logit_formula, data = train_def, family = binomial())

# Train predictions
probs_train2 <- predict(mylogit2, newdata = train_def, type = "response")
pred_train2  <- factor(ifelse(probs_train2 >= 0.3, "Pobre", "No_pobre"),
                       levels = c("No_pobre","Pobre"))

# F1 on TRAIN
cm_train2 <- caret::confusionMatrix(pred_train2, train_def$Pobre, positive = "Pobre")
P <- cm_train2$byClass["Precision"]; R <- cm_train2$byClass["Recall"]
F1_train2 <- 2 * P * R / (P + R); F1_train2

# Logit model 3 ======

# Build formula
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

# Fit
mylogit3 <- glm(logit_formula, data = train_def, family = binomial())

# Train predictions
probs_train3 <- predict(mylogit3, newdata = train_def, type = "response")
pred_train3  <- factor(ifelse(probs_train3 >= 0.3, "Pobre", "No_pobre"),
                       levels = c("No_pobre","Pobre"))

# F1 on TRAIN
cm_train3 <- caret::confusionMatrix(pred_train3, train_def$Pobre, positive = "Pobre")
P <- cm_train3$byClass["Precision"]; R <- cm_train3$byClass["Recall"]
F1_train3 <- 2 * P * R / (P + R); F1_train2



F1
F1_train2
F1_train3


# ==== ROC CURVES:

pr_pobre <- function(model, newdata) {
  p <- predict(model, newdata = newdata, type = "response")
  # If the model was fit with ref="Pobre", p is already Pr(Pobre).
  # If it was fit with ref="No_pobre", then p = Pr(No_pobre) and we flip:
  # We can detect from model$y: first level is the reference
  levs <- levels(model$y)
  if (length(levs) == 2 && levs[1] == "Pobre") {
    return(1- p)               # already Pr(Pobre)
  } else {
    return(p)           # flip to get Pr(Pobre)
  }
}

probs1 <- pr_pobre(mylogit,  train_def)
probs2 <- pr_pobre(mylogit2, train_def)
probs3 <- pr_pobre(mylogit3, train_def)

# True labels with explicit ordering (No_pobre = negative, Pobre = positive)
y <- factor(train_def$Pobre, levels = c("No_pobre","Pobre"))

# --- Build ROC objects ---
roc1 <- roc(response = y, predictor = probs1, levels = c("No_pobre","Pobre"), direction = "<")
roc2 <- roc(response = y, predictor = probs2, levels = c("No_pobre","Pobre"), direction = "<")
roc3 <- roc(response = y, predictor = probs3, levels = c("No_pobre","Pobre"), direction = "<")

# Get all ROC coordinates for ggplot
df1 <- coords(roc1, x = "all", ret = c("specificity","sensitivity","threshold")) %>% mutate(model="Logit 1")
df2 <- coords(roc2, x = "all", ret = c("specificity","sensitivity","threshold")) %>% mutate(model="Logit 2")
df3 <- coords(roc3, x = "all", ret = c("specificity","sensitivity","threshold")) %>% mutate(model="Logit 3")

# Mark your operating cutoff = 0.3 on each curve
closest_pt <- function(df, t0 = 0.3) df[which.min(abs(df$threshold - t0)), , drop = FALSE]
pt1 <- closest_pt(df1, 0.3)
pt2 <- closest_pt(df2, 0.3)
pt3 <- closest_pt(df3, 0.3)

# --- Plot all three ROCs with a point at cutoff = 0.3 ---
bind_rows(df1, df2, df3) %>%
  mutate(FPR = 1 - specificity,
         TPR = sensitivity) %>%
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

# Compute best cutoff per model (closest to top-left corner)
best_cutoff <- function(roc_obj) {
  df <- coords(roc_obj, x = "all", ret = c("specificity", "sensitivity", "threshold"))
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

# Maximize F1 ======

find_best_f1 <- function(probs, y, step = 0.01) {
  cutoffs <- seq(0, 1, by = step)
  results <- data.frame()
  
  for (c in cutoffs) {
    preds <- factor(ifelse(probs >= c, "Pobre", "No_pobre"),
                    levels = c("No_pobre","Pobre"))
    cm <- confusionMatrix(preds, y, positive = "Pobre")
    P <- cm$byClass["Precision"]
    R <- cm$byClass["Recall"]
    F1 <- ifelse((P + R) == 0, 0, 2 * P * R / (P + R))
    results <- rbind(results, data.frame(cutoff = c, F1 = F1))
  }
  
  best_row <- results[which.max(results$F1), ]
  return(best_row)
}

y <- factor(train_def$Pobre, levels = c("No_pobre","Pobre"))

best1_f1 <- find_best_f1(probs1, y)
best2_f1 <- find_best_f1(probs2, y)
best3_f1 <- find_best_f1(probs3, y)

best1_f1
best2_f1
best3_f1

results <- test_def %>%
  transmute(id, pobre = pred_test)

# Model 3 optimized =====

# --- TEST predictions for Logit Model 3 ---

# Probabilities of being "Pobre"
probs_test3 <- predict(mylogit3, newdata = test_def, type = "response")


# Apply cutoff = 0.3 (or replace with best cutoff if you optimized it)
cutoffopt <- 0.33
pred_test3 <- ifelse(probs_test3 >= cutoffopt, "Pobre", "No_pobre")

# --- Format results ---
results <- test_def %>%
  transmute(
    id,
    pobre = ifelse(pred_test3 == "Pobre", 1,
                   ifelse(pred_test3 == "No_pobre", 0, NA))
  )
# Complejizando el logit====

# --- Model 3 (your current best baseline) ---
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

# --- Model 4A (Human Capital & Household Interaction) ---
mylogit4A <- glm(
  Pobre ~ Años_educ_mean_hogar * Asalariados_hogar +
    Años_educ_mean_hogar * Mujer_jefe +
    Asalariados_hogar * Personas_hogar +
    Propiedad_vivienda + arriendo_compilado_hogar +
    Afiliados_salud_hogar + Edad_jefe + Ancianos_hogar,
  data = train_def,
  family = binomial()
)

# --- Model 4B (Structural Vulnerability) ---
mylogit4B <- glm(
  Pobre ~ Propiedad_vivienda * arriendo_compilado_hogar +
    Asalariados_hogar * Afiliados_salud_hogar +
    Urbano * (Educados_hogar + Años_educ_mean_hogar) +
    Menores_cinco_hogar + Ancianos_hogar +
    Mujer_jefe * Regimen_salud_jefe,
  data = train_def,
  family = binomial()
)

# --- Model 4C (Labor Market & Life-Cycle Nonlinearities) ---
mylogit4C <- glm(
  Pobre ~ Asalariados_hogar + Ocupados_hogar + Desocupados_hogar +
    I(Edad_jefe^2) + I(Años_educ_mean_hogar^2) +
    Asalariados_hogar * Años_educ_mean_hogar +
    Propiedad_vivienda + arriendo_compilado_hogar +
    Afiliados_salud_hogar + Mujer_jefe,
  data = train_def,
  family = binomial()
)
library(pROC)
library(caret)
library(dplyr)

# (unchanged) robust F1 search you already added
find_best_f1 <- function(probs, y, step = 0.01) {
  cutoffs <- seq(0, 1, by = step)
  results <- data.frame(cutoff = cutoffs, F1 = NA_real_)
  for (i in seq_along(cutoffs)) {
    c <- cutoffs[i]
    preds <- factor(ifelse(probs >= c, "Pobre", "No_pobre"),
                    levels = c("No_pobre","Pobre"))
    cm <- tryCatch(
      confusionMatrix(preds, y, positive = "Pobre"),
      error = function(e) NULL
    )
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

# NEW: coerce AUC to numeric so bind_rows() is happy
evaluate_logit <- function(model, data) {
  # make sure positive class is "Pobre"
  y <- factor(data$Pobre, levels = c("No_pobre","Pobre"))
  probs <- predict(model, newdata = data, type = "response")
  roc_obj <- roc(response = y, predictor = probs, levels = c("No_pobre","Pobre"), direction = "<")
  auc_val <- as.numeric(auc(roc_obj))   # <-- key fix (drop 'auc' class)
  
  best <- find_best_f1(probs, y)
  tibble(
    AUC        = auc_val,
    F1_best    = best$best_f1,
    Cutoff_best= best$best_cutoff
  )
}


# Optimización elastic net ======

form_m3 <- ~ Urbano + Dormitorios_hogar + Propiedad_vivienda + arriendo_compilado_hogar +
  Personas_hogar + Personas_unidad_gasto + Ciudad_cat + Asalariados_hogar +
  Desocupados_hogar + Menores_cinco_hogar + Educados_hogar +
  Años_educ_mean_hogar + Ancianos_hogar + Afiliados_salud_hogar + Mujer_jefe +
  Años_educ_jefe + Ocupados_hogar + Horas_trabajadas_hogar + Edad_jefe +
  Regimen_salud_jefe + Oficio_C8_jefe + Ocupado_jefe + Adolecentes_hogar + Edad_ponderada_hogar +
  Actividad_semana_pasada_jefe

library(caret)
library(glmnet)
library(dplyr)

# Make the response two-class with "Pobre" as positive (second level works with twoClassSummary)
train_def$Pobre <- factor(train_def$Pobre, levels = c("No_pobre","Pobre"))

# Design matrix for caret (use same feature set as Model 3)
X_train <- model.matrix(reformulate(attr(terms(form_m3), "term.labels")), data = train_def)[, -1]
train_df <- as.data.frame(X_train)
train_df$Pobre <- train_def$Pobre

# Custom summary to compute F1 at cutoff=0.33 from probs
f1_at_cut <- function(data, lev = NULL, model = NULL, cutoff = 0.33) {
  stopifnot(all(lev %in% names(data)))     # prob columns present
  p <- data[[lev[2]]]                      # yardstick/caret: column for "Pobre" should be lev[2]
  pred <- factor(ifelse(p >= cutoff, lev[2], lev[1]), levels = lev)
  cm <- confusionMatrix(pred, data$obs, positive = lev[2])
  P <- cm$byClass["Precision"]; R <- cm$byClass["Recall"]
  F1 <- if (is.na(P) || is.na(R) || P + R == 0) 0 else 2 * P * R / (P + R)
  c(F1 = unname(F1), ROC = NA, Sens = unname(cm$byClass["Sensitivity"]), Spec = unname(cm$byClass["Specificity"]))
}
f1_033 <- function(data, lev = NULL, model = NULL) f1_at_cut(data, lev, model, cutoff = 0.33)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = f1_033,
  savePredictions = "final",
  verboseIter = TRUE
)

# α–λ grid: caret will also tune λ; keep it modest to save time
tg <- expand.grid(
  alpha = c(0, 0.25, 0.5, 0.75, 1),
  lambda = 10^seq(-3, 1, length.out = 30)
)

set.seed(123)
penalized_glm <- train(
  x = subset(train_df, select = -Pobre),
  y = train_df$Pobre,
  method = "glmnet",
  trControl = ctrl,
  tuneGrid = tg,
  metric = "F1"     # directly optimize F1@0.33 inside CV
)

penalized_glm$bestTune
max(penalized_glm$results$F1, na.rm = TRUE)

library(readr)
library(dplyr)

# Reuse training terms (contains all predictors)
Terms <- terms(mf)  # from your training model.frame

# Keep track of original test rows and ID
ids_test <- test_def$id

# Build model frame ONLY for predictors (drops rows with NA),
# then re-attach the matching id’s afterwards
mf_test <- model.frame(delete.response(Terms), data = test_def, na.action = na.omit)

# Match the filtered rows with their ids
ids_test <- test_def$id[as.integer(rownames(mf_test))]

# Build design matrix using the same predictors
X_test <- model.matrix(delete.response(Terms), data = mf_test)[, -1, drop = FALSE]

# Align columns to training matrix
train_cols <- colnames(subset(train_df, select = -Pobre))
missing_cols <- setdiff(train_cols, colnames(X_test))
if (length(missing_cols)) {
  X_test <- cbind(X_test,
                  matrix(0, nrow = nrow(X_test), ncol = length(missing_cols),
                         dimnames = list(NULL, missing_cols)))
}
X_test <- X_test[, train_cols, drop = FALSE]

# Predict probabilities and classify
probs_te <- predict(penalized_glm, newdata = as.data.frame(X_test), type = "prob")[, "Pobre"]
cutoff <- 0.33
pred_te <- ifelse(probs_te >= cutoff, 1L, 0L)

# Combine id + prediction
results <- tibble(
  id = ids_test,
  pobre = pred_te
)

# Write to CSV
write_csv(results, "penalized_logit_cutoff33.csv")


# --- Export CSV ---
write_csv(results, "logit_varbestsubsetcutoff33optf1_v1reg.csv", col_names = TRUE)


results <- results %>%
  mutate(pobre = ifelse(pobre == "Pobre", 1,
                        ifelse(pobre == "No_pobre", 0, NA)))
write_csv(results, "logit_cutoff30_v1reg.csv", col_names = TRUE)


