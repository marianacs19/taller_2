rm(list = rm())

library(pacman)
p_load(tidyverse, caret, glmnet, readxl, writexl, leaps, bestglm)

# Establishing paths ------------------------------------------------------

wd_main <- "C:/Users/Usuario/Documents/Andes/Primer Semestre/Machine Learning/TALLER 2 GIT/taller_2"
setwd("C:/Users/Usuario/Documents/Andes/Primer Semestre/Machine Learning/TALLER 2 GIT/taller_2")
wd_data <- "/Data"
wd_output <- "/output"
wd_code <- "/code"

# Necessary definitions ---------------------------------------------------

data <- list()
set.seed(15102025)

# Importing data ----------------------------------------------------------
load(paste0(wd_main, wd_data, "/train_def.RData"))
load(paste0(wd_main, wd_data, "/test_def.RData"))

#####
# Cambio de na
train_def <- train_def %>%
  mutate(Actividad_semana_pasada_jefe = fct_explicit_na(Actividad_semana_pasada_jefe, na_level = "Otra"))
####

# Models and Results ------------------------------------------------------

# Modelo 1
# 
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = TRUE
)

set.seed(2025)

model1 <- train(
  Pobre~Urbano+Departamento+Espacios_hogar+Dormitorios_hogar+Propiedad_vivienda+Personas_hogar+Oficio_C8_jefe,
  data=train_def,
  method = "glm",
  trControl = ctrl, 
  family = "binomial"
)

# Since we're predicting with an object created by `caret`, some arguments changed.
# In particular, to predict class probabilities we use `type = 'prob'`, and to
# predict class labels we use `type = 'raw'`. 
predict_logit <- data.frame(
  Pobre = train_def$Pobre,                                           ## observed class labels
  P_hat = predict(model1, newdata = train_def, type = "prob"),    ## predicted class probabilities
  pred = predict(model1, newdata = train_def, type = "raw")      ## predicted class labels
)

head(predict_logit)

cm <- confusionMatrix(data = predict_logit$pred, reference = predict_logit$Pobre, positive = "Pobre")
cm

###Formateo a KAGGLE

predictSample <- test_def |>
  mutate(pobre_lab = predict(model1, newdata = test_def, type = "raw")) |>
  select(id,pobre_lab)

head(predictSample)

predictSample <- predictSample |> 
  mutate(pobre=ifelse(pobre_lab=="Pobre",1,0)) |>
  select(id, pobre)
head(predictSample)            

# nombre
write.csv(predictSample,"Estimations/modelo1_pruebav1_15_10_2025.csv", row.names = FALSE)


# Modelo 2
# 
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = TRUE
)

set.seed(2025)

model1 <- train(
  Pobre~Años_educ_jefe+Años_educ_mean_hogar+Edad_jefe+Horas_trabajadas_hogar+Li+Lp+Pctg_Personas_edad_productiva_hogar+Personas_hogar+Ciudad_cat+Urbano+Espacios_hogar+Subsidio_transporte_jefe+Dormitorios_hogar+Propiedad_vivienda+Personas_hogar+Oficio_C8_jefe,
  data=train_def,
  method = "glm",
  trControl = ctrl, 
  family = "binomial"
)

# Since we're predicting with an object created by `caret`, some arguments changed.
# In particular, to predict class probabilities we use `type = 'prob'`, and to
# predict class labels we use `type = 'raw'`. 
predict_logit <- data.frame(
  Pobre = train_def$Pobre,                                           ## observed class labels
  P_hat = predict(model1, newdata = train_def, type = "prob"),    ## predicted class probabilities
  pred = predict(model1, newdata = train_def, type = "raw")      ## predicted class labels
)

head(predict_logit)

cm <- confusionMatrix(data = predict_logit$pred, reference = predict_logit$Pobre, positive = "Pobre")
cm

# Cutoff out

p_load("pROC") # Paquete para calcular y visualizar curvas ROC

roc_obj_en<-roc(response=model1$pred$obs,  # Valores reales de la variable objetivo
                predictor=model1$pred$Pobre , # Probabilidades predichas por el modelo
                levels = c("No_pobre", "Pobre"),  # # Establece la referencia control y caso (No_pobre = negativo, Pobre = positivo) 
                direction = "<")  # "<" significa que "desempleado" es positivo

rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en

###Formateo a KAGGLE

predictSample <- test_def |>
  mutate(.prob = predict(model1, newdata = test_def, type = "prob") |> as.data.frame()) |>
  unnest_wider(.prob) |>
  select(id, Pobre)    # esta columna viene del nombre de la clase

head(predictSample)

predictSample <- predictSample |> 
  mutate(pobre=ifelse(Pobre>=rfThresh_en$threshold,1,0)) |>
  select(id, pobre)
head(predictSample)            

# nombre
write.csv(predictSample,"Estimations/modelo2_cutoff0.2062748.csv", row.names = FALSE)

# Modelo 3
# 
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = TRUE
)

set.seed(2025)

model1 <- train(
  Pobre~Arriendo_sumado + Años_educ_jefe+Años_educ_mean_hogar+Edad_jefe+Horas_trabajadas_hogar+Li+Lp+Pctg_Personas_edad_productiva_hogar+Personas_hogar+Ciudad_cat+Urbano+Espacios_hogar+Subsidio_transporte_jefe+Dormitorios_hogar+Propiedad_vivienda+Personas_hogar+Oficio_C8_jefe,
  data=train_def,
  method = "glm",
  trControl = ctrl, 
  family = "binomial"
)

# Since we're predicting with an object created by `caret`, some arguments changed.
# In particular, to predict class probabilities we use `type = 'prob'`, and to
# predict class labels we use `type = 'raw'`. 
predict_logit <- data.frame(
  Pobre = train_def$Pobre,                                           ## observed class labels
  P_hat = predict(model1, newdata = train_def, type = "prob"),    ## predicted class probabilities
  pred = predict(model1, newdata = train_def, type = "raw")      ## predicted class labels
)

head(predict_logit)

cm <- confusionMatrix(data = predict_logit$pred, reference = predict_logit$Pobre, positive = "Pobre")
cm

# Cutoff out

p_load("pROC") # Paquete para calcular y visualizar curvas ROC

roc_obj_en<-roc(response=model1$pred$obs,  # Valores reales de la variable objetivo
                predictor=model1$pred$Pobre , # Probabilidades predichas por el modelo
                levels = c("No_pobre", "Pobre"),  # # Establece la referencia control y caso (No_pobre = negativo, Pobre = positivo) 
                direction = "<")  # "<" significa que "desempleado" es positivo

rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en

###Formateo a KAGGLE

predictSample <- test_def |>
  mutate(.prob = predict(model1, newdata = test_def, type = "prob") |> as.data.frame()) |>
  unnest_wider(.prob) |>
  select(id, Pobre)    # esta columna viene del nombre de la clase

head(predictSample)

predictSample <- predictSample |> 
  mutate(pobre=ifelse(Pobre>=rfThresh_en$threshold,1,0)) |>
  select(id, pobre)
head(predictSample)            

# nombre
write.csv(predictSample,"Estimations/modelo3_cutoff0.2062807.csv", row.names = FALSE)

# Modelo 4
# 

multiStats <- function(...) c(twoClassSummary(...), defaultSummary(...), prSummary(...))

pos_weight  <- sum(train_def$Pobre == "No_pobre") / sum(train_def$Pobre == "Pobre")
pos_weight

wts <- ifelse(train_def$Pobre == "Pobre", pos_weight, 1) 

ctrl_multiStats <- trainControl(
  method = "cv",  # Usamos validación cruzada
  number = 5,  # 5-fold cross-validation
  summaryFunction = multiStats,  # Usamos la función de evaluación personalizada
  classProbs = TRUE,  # Habilita el cálculo de probabilidades para cada clase
  verbose = FALSE,  # Evita mensajes innecesarios en la consola
  savePredictions = TRUE  # Guarda las predicciones para análisis posterior
)

set.seed(2025)

model1 <- train(
  Pobre~Arriendo_sumado + Años_educ_jefe+Años_educ_mean_hogar+Edad_jefe+Horas_trabajadas_hogar+Li+Lp+Pctg_Personas_edad_productiva_hogar+Personas_hogar+Ciudad_cat+Urbano+Espacios_hogar+Subsidio_transporte_jefe+Dormitorios_hogar+Propiedad_vivienda+Personas_hogar+Oficio_C8_jefe,
  data=train_def,
  method = "glm",
  trControl = ctrl_multiStats, 
  family = "binomial",
  metric="Sens",
  weights    = wts
)

# Since we're predicting with an object created by `caret`, some arguments changed.
# In particular, to predict class probabilities we use `type = 'prob'`, and to
# predict class labels we use `type = 'raw'`. 
predict_logit <- data.frame(
  Pobre = train_def$Pobre,                                           ## observed class labels
  P_hat = predict(model1, newdata = train_def, type = "prob"),    ## predicted class probabilities
  pred = predict(model1, newdata = train_def, type = "raw")      ## predicted class labels
)

head(predict_logit)

cm <- confusionMatrix(data = predict_logit$pred, reference = predict_logit$Pobre, positive = "Pobre")
cm

# Cutoff out

p_load("pROC") # Paquete para calcular y visualizar curvas ROC

roc_obj_en<-roc(response=model1$pred$obs,  # Valores reales de la variable objetivo
                predictor=model1$pred$Pobre , # Probabilidades predichas por el modelo
                levels = c("No_pobre", "Pobre"),  # # Establece la referencia control y caso (No_pobre = negativo, Pobre = positivo) 
                direction = "<")  # "<" significa que "desempleado" es positivo

rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en

###Formateo a KAGGLE

predictSample <- test_def |>
  mutate(.prob = predict(model1, newdata = test_def, type = "prob") |> as.data.frame()) |>
  unnest_wider(.prob) |>
  select(id, Pobre)    # esta columna viene del nombre de la clase

head(predictSample)

predictSample <- predictSample |> 
  mutate(pobre=ifelse(Pobre>=rfThresh_en$threshold,1,0)) |>
  select(id, pobre)
head(predictSample)            

# nombre
write.csv(predictSample,"Estimations/modelo4_cutoff0.516826.csv", row.names = FALSE)

# Modelo 5
p_load('xgboost')

grid_xbgoost <- expand.grid(nrounds = c(100,250),
                            max_depth = c(2,4), 
                            eta = c(0.01,0.05), 
                            gamma = c(0,1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4,0.8))

Xgboost_tree <- train(Pobre ~ Arriendo_sumado + Años_educ_jefe+Años_educ_mean_hogar+Edad_jefe+Horas_trabajadas_hogar+Li+Lp+Pctg_Personas_edad_productiva_hogar+Personas_hogar+Ciudad_cat+Urbano+Espacios_hogar+Subsidio_transporte_jefe+Dormitorios_hogar+Propiedad_vivienda+Personas_hogar+Oficio_C8_jefe,
                      data=train_def,
                      method = "xgbTree", 
                      trControl = ctrl_multiStats,
                      tuneGrid=grid_xbgoost
)

###Formateo a KAGGLE

predictSample <- test_def |>
  mutate(pobre_lab = predict(Xgboost_tree, newdata = test_def, type = "raw")) |>
  select(id,pobre_lab)

head(predictSample)

predictSample <- predictSample |> 
  mutate(pobre=ifelse(pobre_lab=="Pobre",1,0)) |>
  select(id, pobre)
head(predictSample)            

# nombre
write.csv(predictSample,"Estimations/XBOOST_NR250_MD_4_ETA_0-05_GAM_0_CBT_066_MIN_CHI_W_10_SB04.csv", row.names = FALSE)


# Estimating best model ---------------------------------------------------

# Random forest

# Pesos
pos_weight  <- sum(train_def$Pobre == "No_pobre") / sum(train_def$Pobre == "Pobre")
pos_weight

wts <- ifelse(train_def$Pobre == "Pobre", pos_weight, 1)

# Estimación
p_load(ranger)

multiStats <- function(...) c(twoClassSummary(...), defaultSummary(...), prSummary(...))

# Validación cruzada
ctrl_multiStats <- trainControl(
  method = "cv",  # Usamos validación cruzada
  number = 5,  # 5-fold cross-validation
  summaryFunction = multiStats,  # Usamos la función de evaluación personalizada
  classProbs = TRUE,  # Habilita el cálculo de probabilidades para cada clase
  verbose = FALSE,  # Evita mensajes innecesarios en la consola
  savePredictions = TRUE  # Guarda las predicciones para análisis posterior
)

set.seed(123)  # Fijamos la semilla para la reproducibilidad, garantizando que todos los modelos se entrenen con las mismas particiones, asegurando una comparación justa.

# Entrenamiento
tree_ranger_sens <- train(Pobre~Arriendo_sumado +
                            Horas_trabajadas_hogar + Pctg_Personas_edad_productiva_hogar +
                            Edad_jefe + Años_educ_mean_hogar + Personas_hogar+
                            Inactivos_hogar + Educados_hogar +
                            Asalariados_hogar + Espacios_hogar+
                            Estudiantes_hogar +
                            Desocupados_hogar +
                            Adult_no_educ_hogar + Personas_edad_productiva_hogar +
                            Años_educ_jefe + Afiliados_salud_hogar + Educados_hogar+
                            Arriendo_sumado:Mujer_jefe +
                            Mujer_jefe + Horas_trabajadas_hogar:Mujer_jefe + Pctg_Personas_edad_productiva_hogar:Mujer_jefe +
                            Edad_jefe:Mujer_jefe + Años_educ_mean_hogar:Mujer_jefe + Personas_hogar:Mujer_jefe+
                            Inactivos_hogar:Mujer_jefe + Educados_hogar:Mujer_jefe +
                            Asalariados_hogar:Mujer_jefe + Espacios_hogar:Mujer_jefe +
                            Estudiantes_hogar:Mujer_jefe +
                            Desocupados_hogar:Mujer_jefe +
                            Adult_no_educ_hogar:Mujer_jefe + Personas_edad_productiva_hogar:Mujer_jefe +
                            Años_educ_jefe:Mujer_jefe + Afiliados_salud_hogar:Mujer_jefe + Educados_hogar:Mujer_jefe+
                            Mujer_jefe+
                            Recibio_arriendos_o_pensiones_mes_pasado_jefe+Ocupado_jefe+Ayudas_gobierno_12m_jefe+Subsidio_transporte_jefe+Actividad_semana_pasada_jefe+
                            Ciudad_cat + Urbano + Ciudad_cat:Urbano + Oficio_C8_jefe + Subsidio_transporte_jefe + Regimen_salud_jefe  +
                            Recibio_arriendos_o_pensiones_mes_pasado_jefe:Mujer_jefe+Ocupado_jefe:Mujer_jefe+Ayudas_gobierno_12m_jefe:Mujer_jefe+Subsidio_transporte_jefe:Mujer_jefe+Actividad_semana_pasada_jefe:Mujer_jefe+
                            Ciudad_cat:Mujer_jefe + Urbano:Mujer_jefe + Ciudad_cat:Urbano:Mujer_jefe + Oficio_C8_jefe:Mujer_jefe + Subsidio_transporte_jefe:Mujer_jefe + Regimen_salud_jefe:Mujer_jefe,  # Fórmula del modelo
                          data = train_def,  # Dataset de entrenamiento
                          method = "ranger",  # Usamos el motor ranger para Random Forests
                          trControl = ctrl_multiStats,  # Especificamos los controles de validación cruzada definidos antes
                          tuneGrid = expand.grid(   # Definimos la grilla de hiperparámetros a explorar
                            mtry = c(5,8,11),  # Número de predictores seleccionados al azar en cada división
                            splitrule = "gini",  # Regla de partición basada en la reducción de varianza (regresión)
                            min.node.size = c(30, 50)  # Tamaño mínimo de nodos terminales
                          ),
                          metric = "F",  # Optimiza la métrica de sensibilidad (recall para la clase positiva)
                          num.trees = 500,
                          weights = wts
)

tree_ranger_sens  

# Testeo
predict_rf <- data.frame(
  Pobre = train_def$Pobre,                                           ## observed class labels
  P_hat = predict(tree_ranger_sens, newdata = train_def, type = "prob"),    ## predicted class probabilities
  pred = predict(tree_ranger_sens, newdata = train_def, type = "raw")      ## predicted class labels
)

cm <- confusionMatrix(data = predict_rf$pred, reference = predict_rf$Pobre, positive = "Pobre")
cm

# Encontrar el mejor cutoff
find_best_f1 <- function(probs, y, step = 0.005) {
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

best1_f1 <- find_best_f1(predict_rf$P_hat.Pobre, y)

best1_f1

###Formateo a KAGGLE

test_def <- test_def %>%
  mutate(Actividad_semana_pasada_jefe = fct_explicit_na(Actividad_semana_pasada_jefe, na_level = "Otra"))


predictSample <- test_def |>
  mutate(.prob = predict(tree_ranger_sens, newdata = test_def, type = "prob") |> as.data.frame()) |>
  unnest_wider(.prob) |>
  select(id, Pobre)    # esta columna viene del nombre de la clase

head(predictSample)

predictSample <- predictSample |> 
  mutate(pobre=ifelse(Pobre>=0.655,1,0)) |>
  select(id, pobre)
head(predictSample)            

# nombre

write.csv(predictSample,"Estimations/rf_mtry_11_min_node_dise_30.csv", row.names = FALSE)