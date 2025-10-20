rm(list = rm())

library(pacman)
p_load(tidyverse, caret, glmnet, readxl, writexl, leaps, xgboost, pROC)

# Establishing paths ------------------------------------------------------

wd_main <- "C:/Users/Juan/OneDrive - Universidad de los andes/Escritorio/Python projects/EConcept/taller_2"
wd_data <- "/Data"
wd_output <- "/output"
wd_code <- "/code"

# Necessary definitions ---------------------------------------------------

data <- list()
set.seed(15102025)

# Importing data ----------------------------------------------------------

load(paste0(wd_main, wd_data, "/train_def.RData"))
load(paste0(wd_main, wd_data, "/test_def.RData"))

# Cleaning the data -------------------------------------------------------

train_def <- train_def %>%
  mutate(arriendo_compilado_hogar = coalesce(Arriendo_estimado_mensual, Arriendo_pagado_mensual))

test_def <- test_def %>%
  mutate(arriendo_compilado_hogar = coalesce(Arriendo_estimado_mensual, Arriendo_pagado_mensual))

# Models and Results ------------------------------------------------------

# Elastic Net

# Estimating best model ---------------------------------------------------

model_form <- as.formula(Pobre ~ Urbano + Dormitorios_hogar + Propiedad_vivienda +
  Personas_hogar + Personas_unidad_gasto +
  Ciudad_cat + Asalariados_hogar + Desocupados_hogar + Menores_cinco_hogar +
  Educados_hogar + Años_educ_mean_hogar + Ancianos_hogar +
  Afiliados_salud_hogar + Mujer_jefe + Años_educ_jefe +
  Ocupados_hogar + Horas_trabajadas_hogar + Edad_jefe  +
  Regimen_salud_jefe + Oficio_C8_jefe + Ocupado_jefe + arriendo_compilado_hogar)

# Model Selection ---------------------------------------------------------

forward_model <- regsubsets(model_form,
                             data = train_def, 
                             nvmax = 11, 
                             method = "forward")

backward_model <- regsubsets(model_form,
                             data = train_def, 
                             nvmax = 11, 
                             method = "backward")

# Selecting variables -----------------------------------------------------

vars_train <- c(
  "id", "Pobre", "Urbano", "Dormitorios_hogar", "Propiedad_vivienda",
  "Personas_hogar", "Personas_unidad_gasto",
  "Ciudad_cat", "Asalariados_hogar", "Desocupados_hogar", "Menores_cinco_hogar",
  "Educados_hogar", "Años_educ_mean_hogar", "Ancianos_hogar",
  "Afiliados_salud_hogar", "Mujer_jefe", "Años_educ_jefe",
  "Ocupados_hogar", "Horas_trabajadas_hogar", "Edad_jefe",
  "Regimen_salud_jefe", "Oficio_C8_jefe", "Ocupado_jefe", "arriendo_compilado_hogar")

vars_test <- c(
  "id", "Urbano", "Dormitorios_hogar", "Propiedad_vivienda",
  "Personas_hogar", "Personas_unidad_gasto",
  "Ciudad_cat", "Asalariados_hogar", "Desocupados_hogar", "Menores_cinco_hogar",
  "Educados_hogar", "Años_educ_mean_hogar", "Ancianos_hogar",
  "Afiliados_salud_hogar", "Mujer_jefe", "Años_educ_jefe",
  "Ocupados_hogar", "Horas_trabajadas_hogar", "Edad_jefe",
  "Regimen_salud_jefe", "Oficio_C8_jefe", "Ocupado_jefe", "arriendo_compilado_hogar")

train_def <- train_def %>%
  select(all_of(vars_train))

test_def <- test_def %>%
  select(all_of(vars_test))

# XGBoost -----------------------------------------------------------------

ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats,
  classProbs = TRUE,
  verboseIter = TRUE,
  savePredictions = TRUE)

grid_xgboost <- expand.grid(nrounds = c(250,500),
                            max_depth = c(1, 2),
                            eta = c(0.1,  0.01), 
                            gamma = c(0, 1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.4, 0.7), 
                            subsample = c(0.7))

set.seed(15102025)
xgboost_model <- train(
  model_form,
  data = train_def,
  method = "xgbTree",
  trControl = xgboost_ctrl, 
  tuneGrid = grid_xgboost,
  metric = "F",
  verbosity = 0)

# Exportando predicciones -------------------------------------------------

predictSample_XGBOost <- test_def %>%
  mutate(
    pobre_lab = predict(xgboost_model, newdata = test_def, type = "raw"),
    pobre = ifelse(pobre_lab == "No_pobre", 0, 1)
  ) %>%
  select(id, pobre)

