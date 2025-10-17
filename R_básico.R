# =========================================================
# Setup ---------------------------------------------------
# =========================================================
# Paquetes (instala si hace falta: install.packages(c("tidyverse","janitor","labelled","srvyr")))
library(pacman)

p_load(
  "tidyverse",
  "janitor",
  "labelled",
  "srvyr",
  "tidyverse", 
  "glmnet",
  "caret",
  "MLmetrics",
  "Metrics",
  rio,           # import/export data
  tidyverse,     # tidy-data
  glmnet,        # To implement regularization algorithms. 
  rpart,         # To implement decision trees.
  rpart.plot,    # To plot trees.
  caret,         # To estimate predictive models.
  Metrics 
)

# =========================================================
# CONFIG --------------------------------------------------
# =========================================================
# Carpeta donde están los .csv (separados por ;)
setwd("C:/Users/marce/Documents/Andes/Taller2GIT/taller_2")
data_dir <- "Data/Raw"     # <-- cámbialo
pattern  <- "\\.csv$"      # filtra solo .csv

# Locale típico de csv con ';' y coma decimal
# my_locale <- readr::locale(decimal_mark = ",", grouping_mark = ".")
# 
# # Si quieres tratar códigos centinela como NA (opcional)
# sentinels <- c("98","99","999","9999","99999","999999","9999999","99999999","999999999")
# 
# # Nombre de la columna de peso (factor de expansión). Ajústalo a tu base.
# weight_var <- "factor_expansion"  # p.ej. "fex_c", "Factor_Exp", etc.

# =========================================================
# Lectura de archivos ------------------------------------
# =========================================================

test  <- read.csv("Data/test_collpase_hogar.csv",sep=';')
train <- read.csv("Data/train_collpase_hogar.csv",sep=';')

# =========================================================
# Diccionario de etiquetas (labels) -----------------------
# =========================================================
# =========================================================
# Función: aplicar labels -> factors ----------------------
# =========================================================
apply_labels_from_dict <- function(data, dict) {
  # Normaliza tipos a character para matchear códigos
  dict <- dict |>
    mutate(
      var   = as.character(var),
      code  = as.character(code),
      label = as.character(label)
    )
  
  # Variables presentes en data y en el diccionario
  vars_in_both <- intersect(dict$var, names(data))
  
  # Para cada variable, construimos un named vector code->label y lo aplicamos
  for (v in vars_in_both) {
    dv <- dict |> filter(var == v) |> distinct(code, label)
    # Ordena por código si es numérica; si no, deja orden alfabético
    if (all(suppressWarnings(!is.na(as.numeric(dv$code))))) {
      dv <- dv |> arrange(as.numeric(code))
    } else {
      dv <- dv |> arrange(code)
    }
    
    codes  <- dv$code
    labels <- dv$label
    
    # Coerciona columna a character para matchear contra codes
    col_chr <- as.character(data[[v]])
    
    # A veces la base trae números; conservamos NA originales también
    col_chr[!(col_chr %in% codes)] <- col_chr[!(col_chr %in% codes)]  # mantiene valores no etiquetados
    # Crea factor con niveles = codes y etiquetas = labels
    data[[v]] <- factor(col_chr, levels = codes, labels = labels)
  }
  
  data
}

# Definiendo lables
dict_df <- tribble(
  ~var,  ~code, ~label,
  "Urbano","1", "Urbano",
  "Urbano","0", "Rural",
  "Departamento","5","Antioquia",
  "Departamento","8","Atlántico",
  "Departamento","11","Bogotá_DC",
  "Departamento","13","Bolívar",
  "Departamento","15","Boyacá",
  "Departamento","17","Caldas",
  "Departamento","18","Caquetá",
  "Departamento","19","Cauca",
  "Departamento","20","Cesar",
  "Departamento","23","Córdoba",
  "Departamento","25","Cundinamarca",
  "Departamento","27","Chocó",
  "Departamento","41","Huila",
  "Departamento","44","La_guajira",
  "Departamento","47","Magdalena",
  "Departamento","50","Meta",
  "Departamento","52","Nariño",
  "Departamento","54","Norte_de_santander",
  "Departamento","63","Quindío",
  "Departamento","66","Risaralda",
  "Departamento","68","Santander",
  "Departamento","70","Sucre",
  "Departamento","73","Tolima",
  "Departamento","76","Valle_del_Cauca",
  "Propiedad_vivienda","1","Propia_pagada",
  "Propiedad_vivienda","2","Propia_pagando",
  "Propiedad_vivienda","3","Arriendo",
  "Propiedad_vivienda","4","Usufructo",
  "Propiedad_vivienda","5","Ocupante",
  "Propiedad_vivienda","6","Otra",
  "Pobre","1","Pobre",
  "Pobre","0","No_pobre",
  "Ciudad_cat","1",'RESTO_URBANO',
  "Ciudad_cat","2",'RURAL',
  "Ciudad_cat","3",'BOGOTA_MEDELLÍN',
  "Ciudad_cat","4",'BARRANQUILLA',
  "Ciudad_cat","5",'SANTA_MARTA',
  "Ciudad_cat","6",'RIOHACHA',
  "Ciudad_cat","7",'CARTAGENA',
  "Ciudad_cat","8",'SINCELEJO',
  "Ciudad_cat","9",'CALI',
  "Ciudad_cat","10",'MONTERIA',
  "Ciudad_cat","11",'VALLEDUPAR',
  "Ciudad_cat","12",'POPAYAN',
  "Ciudad_cat","13",'MANIZALES',
  "Ciudad_cat","14",'NEIVA',
  "Ciudad_cat","15",'BUCARAMANGA',
  "Ciudad_cat","16",'PEREIRA',
  "Ciudad_cat","17",'CUCUTA',
  "Ciudad_cat","18",'IBAGUE',
  "Ciudad_cat","19",'ARMENIA',
  "Ciudad_cat","20",'VILLAVICENCIO',
  "Ciudad_cat","21",'PASTO',
  "Ciudad_cat","22",'FLORENCIA',
  "Ciudad_cat","23",'QUIBDO' ,
  "Ciudad_cat","24",'TUNJA',
  "Mujer_jefe","0","Hombre_cabeza_hogar",
  "Mujer_jefe","1","Mujer cabeza_hogar",
  "Oficio_C8_jefe","0","Fuerzas_militares",
  "Oficio_C8_jefe","1","Directores_y_gerentes",
  "Oficio_C8_jefe","2","Profesionales,_científicos_e_intelectuales",
  "Oficio_C8_jefe","3","Técnicos_y_profesionales_del_nivel_medio",
  "Oficio_C8_jefe","4","Personal_de_apoyo_administrativo",
  "Oficio_C8_jefe","5","Trabajadores_de_los_servicios_y_vendedores_de_comercios_y_mercados",
  "Oficio_C8_jefe","6","Agricultores_y_trabajadores_calificados_agropecuarios_forestales_y_pesqueros",
  "Oficio_C8_jefe","7","Oficiales_operarios_artesanos_y_oficios_relacionados",
  "Oficio_C8_jefe","8","Operadores_de_instalaciones_y_máquinas_y_ensambladores",
  "Oficio_C8_jefe","9","Ocupaciones_elementales",
  "Oficio_C8_jefe","99","Desocupado_inactivo",
  "Actividad_semana_pasada_jefe","1","Trabajando",
  "Actividad_semana_pasada_jefe","2","Buscando_trabajo",
  "Actividad_semana_pasada_jefe","3","Estudiando",
  "Actividad_semana_pasada_jefe","4","Oficios_del_hogar",
  "Actividad_semana_pasada_jefe","5","Incapacitado",
  "Actividad_semana_pasada_jefe","6","Otra",
  "Regimen_salud_jefe","1","Contributivo",
  "Regimen_salud_jefe","2","Especial",
  "Regimen_salud_jefe","3","Subsidio",
  "Regimen_salud_jefe","0","No_sabe_No_respone_No_tiene",
  "Horas_extras_jefe","1","Si_Horas_extras_jefe",
  "Horas_extras_jefe","0","No_Horas_extras_jefe",
  "Primas_jefe","1","Si_Primas_jefe",
  "Primas_jefe","0","No_Primas_jefe",
  "Bonificaciones_jefe","1","Si_Bonificaciones_jefe",
  "Bonificaciones_jefe","0","No_Bonificaciones_jefe",
  "Subsidio_alimentacion_jefe","1","Si_Subsidio_alimentacion_jefe",
  "Subsidio_alimentacion_jefe","0","No_Subsidio_alimentacion_jefe",
  "Subsidio_transporte_jefe","1","Si_Subsidio_transporte_jefe",
  "Subsidio_transporte_jefe","0","No_Subsidio_transporte_jefe",
  "Subsidio_familiar_jefe","1","Si_Subsidio_familiar_jefe",
  "Subsidio_familiar_jefe","0","No_Subsidio_familiar_jefe",
  "Subsidio_educativo_jefe","1","Si_Subsidio_educativo_jefe",
  "Subsidio_educativo_jefe","0","No_Subsidio_educativo_jefe",
  "Alimentos_en_especie_jefe","1","Si_Alimentos_en_especie_jefe",
  "Alimentos_en_especie_jefe","0","No_Alimentos_en_especie_jefe",
  "Vivienda_en_especie_jefe","1","Si_Vivienda_en_especie_jefe",
  "Vivienda_en_especie_jefe","0","No_Vivienda_en_especie_jefe",
  "Transporte_empresa_jefe","1","Si_Transporte_empresa_jefe",
  "Transporte_empresa_jefe","0","No_Transporte_empresa_jefe",
  "Otros_en_especie_trabajo_jefe","1","Si_Otros_en_especie_trabajo_jefe",
  "Otros_en_especie_trabajo_jefe","0","No_Otros_en_especie_trabajo_jefe",
  "Recibio_prima_servicios_12m_jefe","1","Si_Recibio_prima_servicios_12m_jefe",
  "Recibio_prima_servicios_12m_jefe","0","No_Recibio_prima_servicios_12m_jefe",
  "Recibio_prima_navidad_12m_jefe","1","Si_Recibio_prima_navidad_12m_jefe",
  "Recibio_prima_navidad_12m_jefe","0","No_Recibio_prima_navidad_12m_jefe",
  "Recibio_prima_vacaciones_12m_jefe","1","Si_Recibio_prima_vacaciones_12m_jefe",
  "Recibio_prima_vacaciones_12m_jefe","0","No_Recibio_prima_vacaciones_12m_jefe",
  "Recibio_viaticos_permanentes_12m_jefe","1","Si_Recibio_viaticos_permanentes_12m_jefe",
  "Recibio_viaticos_permanentes_12m_jefe","0","No_Recibio_viaticos_permanentes_12m_jefe",
  "Recibio_bonificaciones_anuales_12m_jefe","1","Si_Recibio_bonificaciones_anuales_12m_jefe",
  "Recibio_bonificaciones_anuales_12m_jefe","0","No_Recibio_bonificaciones_anuales_12m_jefe",
  "Ingreso_trabajo_mes_pasado_desocupado_jefe","1","Si_Ingreso_trabajo_mes_pasado_desocupado_jefe",
  "Ingreso_trabajo_mes_pasado_desocupado_jefe","0","No_Ingreso_trabajo_mes_pasado_desocupado_jefe",
  "Ingreso_pension_mes_pasado_jefe","1","Si_Ingreso_pension_mes_pasado_jefe",
  "Ingreso_pension_mes_pasado_jefe","0","No_Ingreso_pension_mes_pasado_jefe",
  "Recibio_arriendos_o_pensiones_mes_pasado_jefe","1","Si_Recibio_arriendos_o_pensiones_mes_pasado_jefe",
  "Recibio_arriendos_o_pensiones_mes_pasado_jefe","0","No_Recibio_arriendos_o_pensiones_mes_pasado_jefe",
  "Recibio_pensiones_mes_pasado_jefe","1","Si_Recibio_pensiones_mes_pasado_jefe",
  "Recibio_pensiones_mes_pasado_jefe","0","No_Recibio_pensiones_mes_pasado_jefe",
  "Recibio_pension_alimentaria_mes_pasado_jefe","1","Si_Recibio_pension_alimentaria_mes_pasado_jefe",
  "Recibio_pension_alimentaria_mes_pasado_jefe","0","No_Recibio_pension_alimentaria_mes_pasado_jefe",
  "Recibio_transferencias_12m_jefe","1","Si_Recibio_transferencias_12m_jefe",
  "Recibio_transferencias_12m_jefe","0","No_Recibio_transferencias_12m_jefe",
  "Dinero_otroshogares_pais_12m_jefe","1","Si_Dinero_otroshogares_pais_12m_jefe",
  "Dinero_otroshogares_pais_12m_jefe","0","No_Dinero_otroshogares_pais_12m_jefe",
  "Dinero_residentes_exterior_12m_jefe","1","Si_Dinero_residentes_exterior_12m_jefe",
  "Dinero_residentes_exterior_12m_jefe","0","No_Dinero_residentes_exterior_12m_jefe",
  "Ayudas_gobierno_12m_jefe","1","Si_Ayudas_gobierno_12m_jefe",
  "Ayudas_gobierno_12m_jefe","0","No_Ayudas_gobierno_12m_jefe",
  "Intereses_dividendos_utilidades_12m_jefe","1","Si_Intereses_dividendos_utilidades_12m_jefe",
  "Intereses_dividendos_utilidades_12m_jefe","0","No_Intereses_dividendos_utilidades_12m_jefe",
  "Cesantias_o_intereses_12m_jefe","1","Si_Cesantias_o_intereses_12m_jefe",
  "Cesantias_o_intereses_12m_jefe","0","No_Cesantias_o_intereses_12m_jefe",
  "Otras_fuentes_12m_jefe","1","Si_Otras_fuentes_12m_jefe",
  "Otras_fuentes_12m_jefe","0","No_Otras_fuentes_12m_jefe",
  "Quiere_trabajar_mas_horas","1","Si_Quiere_trabajar_mas_horas",
  "Quiere_trabajar_mas_horas","0","No_Quiere_trabajar_mas_horas",
  "Diligencias_trabajar_mas_horas_ult4s","1","Si_Diligencias_trabajar_mas_horas_ult4s",
  "Diligencias_trabajar_mas_horas_ult4s","0","No_Diligencias_trabajar_mas_horas_ult4s",
  "Disponible_trabajar_mas_horas","1","Si_Disponible_trabajar_mas_horas",
  "Disponible_trabajar_mas_horas","0","No_Disponible_trabajar_mas_horas",
  "Diligencias_cambiar_trabajo_ult4s","1","Si_Diligencias_cambiar_trabajo_ult4s",
  "Diligencias_cambiar_trabajo_ult4s","0","No_Diligencias_cambiar_trabajo_ult4s",
  "Disponibilidad_comenzar_en_un_mes","1","Si_Disponibilidad_comenzar_en_un_mes",
  "Disponibilidad_comenzar_en_un_mes","0","No_Disponibilidad_comenzar_en_un_mes",
  "Poblacion_edad_trabajar","1","Si_Poblacion_edad_trabajar",
  "Poblacion_edad_trabajar","0","No_Poblacion_edad_trabajar",
  "Ocupado","1","Si_Ocupado",
  "Ocupado","0","No_Ocupado",
  "Desocupado","1","Si_Desocupado",
  "Desocupado","0","No_Desocupado",
  "Inactivo","1","Si_Inactivo",
  "Inactivo","0","No_Inactivo"
) |> mutate(across(everything(), as.character))

##Aplicar labels

train_def <- apply_labels_from_dict(train, dict_df)
test_def <- apply_labels_from_dict(test, dict_df)

save(test_def, file="Data/test_def.RData")
save(train_def, file="Data/train_def.RData")

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

# Estadísticas descriptivas



# =========================================================
# Fin -----------------------------------------------------
# =========================================================
