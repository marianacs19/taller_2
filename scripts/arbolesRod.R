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

test_def <- train_def %>%
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

pred_basic <- predict(basic_tree, newdata = train_def, type = "class")

# Confusion matrix
cm_basic <- confusionMatrix(pred_basic, train_def$Pobre, positive = "Pobre")
cm_basic

# Extract F1 manually
precision_basic <- cm_basic$byClass["Precision"]
recall_basic    <- cm_basic$byClass["Recall"]
F1_basic <- 2 * ((precision_basic * recall_basic) / (precision_basic + recall_basic))
F1_basic

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
pred_reducted <- predict(reducted_tree, newdata = train_def, type = "class")

# Confusion matrix
cm_reducted <- confusionMatrix(pred_reducted, train_def$Pobre, positive = "Pobre")
cm_reducted

# Extract F1 manually
precision_reducted <- cm_reducted$byClass["Precision"]
recall_reducted    <- cm_reducted$byClass["Recall"]
F1_reducted <- 2 * ((precision_reducted * recall_reducted) / (precision_reducted + recall_reducted))
F1_reducted
