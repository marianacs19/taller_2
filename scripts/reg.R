# Limpieza y paquetes -----------------------------------------------------
rm(list = ls())

library(pacman)
p_load(rio,       
       tidyverse,    
       glmnet,       
       caret,        
       scatterplot3d, 
       plotly       
)
library(dplyr)
library(e1071)  
library(rpart)
library(rpart.plot)
library(forcats)

# Estableciendo rutas -----------------------------------------------------

wd_main <- "taller_2"
wd_code <- "scripts"
wd_outputs <- "Estimations"
wd_data <- "Data"


# Cargando bases -----------------------------------------------------
train <- load(paste0(wd_data, "/train_def.RData"))
test <- load(paste0(wd_data, "/test_def.RData"))


# Modelo 1 regresión lineal -----------------------------------------------------
# Definir variables
names(train)
vars_modelo <- c(
  "Urbano", "Nivel_educativo_jefe", "Subsidio_familiar_hogar", 
  "Personas_hogar", "Mujer_jefe", "Años_educ_mean_hogar", "Oficio_jefe",
  "Asalariados_hogar", "Educados_hogar", "Arriendo_pagado_mensual", "Afiliados_salud_hogar" 
)

train_mod <- train %>%
  select(Pobre, all_of(vars_modelo)) %>%
  drop_na()

# Dividir la muestra
set.seed(123)
idx <- sample(1:nrow(train_mod), 0.7 * nrow(train_mod))
train_fit <- train_mod[idx, ]
valid_fit <- train_mod[-idx, ]

# Modelo 
modelo_lineal <- lm(Pobre ~ ., data = train_fit)
summary(modelo_lineal)

# Cutoff
pred_val <- predict(modelo_lineal, newdata = valid_fit)

cutoff <- seq(0.10, 0.90, by = 0.05)

f1_scores <- sapply(cutoff, function(t) {
  pred_bin <- ifelse(pred_val > t, 1, 0)
  cm <- confusionMatrix(
    data      = as.factor(pred_bin),
    reference = as.factor(valid_fit$Pobre),
    positive  = "1"
  )
  prec <- cm$byClass["Precision"]
  rec  <- cm$byClass["Recall"]
  2 * (prec * rec) / (prec + rec)
})

best_t  <- cutoff[which.max(f1_scores)]
best_f1 <- max(f1_scores)

cat("Cutoff =", best_t, "con F1 =", round(best_f1, 3), "\n")

# Fuera de muestra 
test_mod <- test %>%
  select(all_of(vars_modelo)) %>%
  mutate(across(everything(), ~replace_na(., 0)))

pred_test <- predict(modelo_lineal, newdata = test_mod)
pred_test_class <- ifelse(pred_test > best_t, 1, 0)

# Archivo
prediccion <- tibble(id = test$id, Pobre = pred_test_class)
write.csv(prediccion, paste0(wd_estimations, "/modelo_regresion.csv"), row.names = FALSE)

