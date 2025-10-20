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

# Elastic Net -------------------------------------------------------------

ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     savePredictions = TRUE)

en_model <- train(
  model_form,
  data = train_def,
  metric  = "Accuracy",
  method = "glmnet",
  trControl = ctrl,
  tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.1),
                         lambda = 10^seq(-3, 3, length = 10)))

# F1 ----------------------------------------------------------------------

fiveStats <- function(...)  c(prSummary(...))  

xgboost_ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = fiveStats,
  savePredictions = TRUE
)

set.seed(15102025)
model_en_f1 <- train(
  model_form,
  data = train_def,
  metric = "F",
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  tuneGrid = expand.grid(
    alpha  = seq(0, 1, by= 0.1),
    lambda = 10^seq(-3, 3, length = 10)
  )   
)

# Prediction --------------------------------------------------------------

predictSample <- test_def %>%
  mutate(
    pobre_lab = predict(model_en_f1, newdata = test_def, type = "raw"),
    pobre = ifelse(pobre_lab == "No_pobre", 0, 1)
  ) %>%
  select(id, pobre)

write_csv(predictSample, paste0(wd_main, "/elastic_net_F2.csv"))
