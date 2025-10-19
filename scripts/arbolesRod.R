rm(list = ls())

#Modelos con  Forest:

library(pacman)


p_load(rio,           # import/export data
       tidyverse,     # tidy-data
       glmnet,        # To implement regularization algorithms. 
       rpart,         # To implement decision trees.
       rpart.plot,    # To plot trees.
       caret,         # To estimate predictive models.
       Metrics        # To evaluate predictive models.
)

load("~/GitHub/taller_2/Data/test_def.RData")
load("~/GitHub/taller_2/Data/train_def.RData")

train_def <- train_def %>%
  mutate(arriendo_compilado_hogar = coalesce(Arriendo_estimado_mensual, Arriendo_pagado_mensual))

test_def <- test_def %>%
  mutate(arriendo_compilado_hogar = coalesce(Arriendo_estimado_mensual, Arriendo_pagado_mensual))

#  Forest

#Basic Tree
basic_tree <- rpart(formula = Pobre ~ `Años_educ_mean_hogar` +
                      Asalariados_hogar  + arriendo_compilado_hogar + Propiedad_vivienda + Oficio_C8_jefe +
                      Afiliados_salud_hogar,
                    data = train_def,
                      method = "class",
                      cp = 0)

#rpart.plot::prp(
#  basic_tree,      
#  under = TRUE,      # Mostrar la información debajo de cada nodo
#  branch.lty = 2,    # Tipo de línea para las ramas (2 = línea punteada)
#  yesno = 1,         # Mostrar indicadores de "sí"/"no" solo en el primer nodo.
#  faclen = 0,        # Longitud de la abreviación para niveles de factores (0 = sin abreviación)
#  varlen = 15,       # Longitud máxima para abreviar los nombres de variables
#  box.palette = "-RdYlGn"  # Paleta de colores para las hojas 
#)

probs_basic  <- predict(basic_tree, newdata = train_def, type = "prob")

probs_basic_pobre <- probs_basic[, "Pobre"]

# Apply your cutoff
cutoff <- 0.3
pred_basic_custom <- ifelse(probs_basic_pobre >= cutoff, "Pobre", "No_pobre") %>% factor(levels = c("No_pobre", "Pobre"))

# Confusion matrix
cm_basic <- confusionMatrix(pred_basic_custom, train_def$Pobre, positive = "Pobre")
cm_basic

# Extract F1 manually
precision_basic <- cm_basic$byClass["Precision"]
recall_basic    <- cm_basic$byClass["Recall"]
F1_basic <- 2 * ((precision_basic * recall_basic) / (precision_basic + recall_basic))
F1_basic

# --- Probabilities on TEST (Pr[Pobre]) ---
probs_test_basic <- predict(basic_tree, newdata = test_def, type = "prob")[, "Pobre"]

# --- Build results: only id and pobre (1/0) ---
cutoff <- 0.3
results <- test_def %>%
  transmute(
    id,                                   # <-- ensure your test set has this column name
    pobre = as.integer(probs_test_basic >= cutoff)  # TRUE->1, FALSE->0
  )

# --- Export ---
write_csv(results, "tree_basic_cutoff30_v1reg.csv", col_names = TRUE)



# Reducted tree (observations)

reducted_tree <- rpart(formula = Pobre ~ `Años_educ_mean_hogar` +
                         Asalariados_hogar  + arriendo_compilado_hogar + Propiedad_vivienda + Oficio_C8_jefe +
                         Afiliados_salud_hogar,
                       data = train_def,
                    method = "class",
                    minbucket = 200,
                    cp = 0)
prp(reducted_tree)
rpart.plot::prp(
  reducted_tree,      
  under = TRUE,      # Mostrar la información debajo de cada nodo
  branch.lty = 2,    # Tipo de línea para las ramas (2 = línea punteada)
  yesno = 1,         # Mostrar indicadores de "sí"/"no" solo en el primer nodo.
  faclen = 0,        # Longitud de la abreviación para niveles de factores (0 = sin abreviación)
  varlen = 15,       # Longitud máxima para abreviar los nombres de variables
  box.palette = "-RdYlGn"  # Paleta de colores para las hojas 
)
probs_reducted <- predict(reducted_tree, newdata = train_def, type = "prob")

probs_reducted_pobre <- probs_reducted[, "Pobre"]

pred_reducted_custom <- ifelse(probs_reducted_pobre >= cutoff, "Pobre", "No_pobre") %>% factor(levels = c("No_pobre", "Pobre"))

# Confusion matrix
cm_reducted <- confusionMatrix(pred_reducted_custom, train_def$Pobre, positive = "Pobre")
cm_reducted

# Extract F1 manually
precision_reducted <- cm_reducted$byClass["Precision"]
recall_reducted    <- cm_reducted$byClass["Recall"]
F1_reducted <- 2 * ((precision_reducted * recall_reducted) / (precision_reducted + recall_reducted))
F1_reducted

library(tidyverse)
library(caret)
library(rpart)

# 1) Ensure "Pobre" is the positive class (must be the FIRST level for caret)
train_def$Pobre <- factor(train_def$Pobre, levels = c("Pobre", "No_pobre"))

# 2) Custom summary: compute F1 using cutoff = 0.3 (and also return Accuracy for reference)
f1Summary_cutoff <- function(data, lev = NULL, model = NULL, cutoff = 0.3) {
  # data has columns: obs, pred, and class-prob columns named after lev
  stopifnot(all(lev %in% colnames(data)))  # probabilities must be present
  
  # event class (positive) is lev[1] by caret convention
  p_event <- data[[lev[1]]]                      # prob of "Pobre"
  pred_c  <- ifelse(p_event >= cutoff, lev[1], lev[2])
  pred_c  <- factor(pred_c, levels = lev)
  
  cm <- confusionMatrix(pred_c, data$obs, positive = lev[1])
  
  P  <- cm$byClass["Precision"]
  R  <- cm$byClass["Recall"]
  F1 <- ifelse((P + R) == 0, 0, 2 * P * R / (P + R))
  
  c(F1 = unname(F1), Accuracy = unname(cm$overall["Accuracy"]))
}

# Wrap with fixed cutoff = 0.3 so caret gets the right signature
f1_03 <- function(data, lev = NULL, model = NULL) f1Summary_cutoff(data, lev, model, cutoff = 0.3)

# 3) CV control
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,          # needed so 'data' includes prob columns
  summaryFunction = f1_03,    # our custom scorer (F1 @ 0.3)
  verboseIter = TRUE,
  savePredictions = "final"
)

# 4) Grid over cp (you can widen/narrow as needed)
grid <- expand.grid(cp = seq(0, 0.03, 0.001))

set.seed(123)
optimized_tree <- train(
  Pobre ~ `Años_educ_mean_hogar` +
    Asalariados_hogar + arriendo_compilado_hogar +
    Propiedad_vivienda + Oficio_C8_jefe + Afiliados_salud_hogar,
  data = train_def,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = grid,
  metric = "F1"               # optimize the custom F1 metric
)

optimized_tree
optimized_tree$bestTune       # <- best cp (pruned model)
optimized_tree$results %>% arrange(desc(F1)) %>% head(5)  # top cp by F1


# Probabilities from the tuned/pruned model
probs_train <- predict(optimized_tree, newdata = train_def, type = "prob")[, "Pobre"]
pred_train  <- factor(ifelse(probs_train >= 0.3, "Pobre", "No_pobre"),
                      levels = levels(train_def$Pobre))

cm_train <- confusionMatrix(pred_train, train_def$Pobre, positive = "Pobre")
cm_train

# Train F1 (should match CV tendency but not identical)
P <- cm_train$byClass["Precision"]
R <- cm_train$byClass["Recall"]
F1_train <- 2 * P * R / (P + R)
F1_train


F1_basic
F1_reducted
F1_train

library(pROC)
library(dplyr)
library(ggplot2)

# --- probs for each model (on train_def) ---
probs_basic_pobre    <- predict(basic_tree,    newdata = train_def, type = "prob")[, "Pobre"]
probs_reducted_pobre <- predict(reducted_tree, newdata = train_def, type = "prob")[, "Pobre"]
probs_opt_pobre      <- predict(optimized_tree, newdata = train_def, type = "prob")[, "Pobre"]

# Ensure event level is the positive class for AUC
y <- factor(train_def$Pobre, levels = c("No_pobre","Pobre"))

# roc objects
roc_basic    <- roc(response = y, predictor = probs_basic_pobre,    levels = c("No_pobre","Pobre"), direction = "<")
roc_reducted <- roc(response = y, predictor = probs_reducted_pobre, levels = c("No_pobre","Pobre"), direction = "<")
roc_opt      <- roc(response = y, predictor = probs_opt_pobre,      levels = c("No_pobre","Pobre"), direction = "<")

# data frames for ggplot
df_basic    <- coords(roc_basic,    x = "all", ret = c("specificity","sensitivity","threshold")) %>% mutate(model="Basic")
df_reducted <- coords(roc_reducted, x = "all", ret = c("specificity","sensitivity","threshold")) %>% mutate(model="Reducted")
df_opt      <- coords(roc_opt,      x = "all", ret = c("specificity","sensitivity","threshold")) %>% mutate(model="Optimized")

# point for cutoff = 0.3 (closest threshold)
closest_pt <- function(df, t0 = 0.3) df[which.min(abs(df$threshold - t0)), , drop = FALSE]
pt_basic    <- closest_pt(df_basic,    0.3)
pt_reducted <- closest_pt(df_reducted, 0.3)
pt_opt      <- closest_pt(df_opt,      0.3)

# Plot
bind_rows(df_basic, df_reducted, df_opt) %>%
  mutate(FPR = 1 - specificity,
         TPR = sensitivity) %>%
  ggplot(aes(x = FPR, y = TPR, color = model)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = bind_rows(pt_basic, pt_reducted, pt_opt) %>% mutate(FPR = 1-specificity, TPR = sensitivity),
             aes(x = FPR, y = TPR, color = model), size = 2) +
  labs(title = "ROC (train) for Trees",
       subtitle = sprintf("AUC — Basic: %.3f | Reducted: %.3f | Optimized: %.3f",
                          auc(roc_basic), auc(roc_reducted), auc(roc_opt)),
       x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal()


F1_basic
F1_reducted
F1_train


bagged_tree_prob <- ranger(
  Pobre ~ `Años_educ_mean_hogar` + Asalariados_hogar + arriendo_compilado_hogar +
    Propiedad_vivienda + Oficio_C8_jefe + Afiliados_salud_hogar,
  data            = train_def,
  num.trees       = 550,
  mtry            = 6,
  min.node.size   = 1,
  replace         = TRUE,
  sample.fraction = 1.0,
  importance      = "permutation",
  probability     = TRUE        # SOFT VOTING (average of votes → probs)
)

probs <- predict(bagged_tree_prob, data = train_def)$predictions[, "Pobre"]
cutoff <- 0.3
pred_03 <- factor(ifelse(probs >= cutoff, "Pobre", "No_pobre"),
                  levels = c("No_pobre","Pobre"))

cm_03 <- confusionMatrix(pred_03, train_def$Pobre, positive = "Pobre")
P <- cm_03$byClass["Precision"]; R <- cm_03$byClass["Recall"]
F1_03 <- 2 * P * R / (P + R); F1_03


# --- Predict on the test dataset ---

# Probabilities for "Pobre"
probs_test <- predict(bagged_tree_prob, data = test_def)$predictions[, "Pobre"]

# Apply the same cutoff = 0.3
cutoff <- 0.4
pred_test <- ifelse(probs_test >= cutoff, "Pobre", "No_pobre")

# Combine with ID or relevant columns (if you have them)
results_test <- test_def %>%
  mutate(pobre = pred_test,
         prob_pobre = probs_test) %>%  # optional: keep the probability too
  select(pobre, prob_pobre, everything())  # optional reorder

# Convert labels to numeric (as you did before)
results_test <- results_test %>%
  mutate(pobre = ifelse(pobre == "Pobre", 1,
                        ifelse(pobre == "No_pobre", 0, NA)))
results_test <- results_test %>%
  select(id, pobre)


# --- Export to CSV ---
write_csv(results_test, "bagged_cutoff30_v1reg.csv", col_names = TRUE)


F1_basic
F1_reducted
F1_train
F1_03


