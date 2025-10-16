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

#Base de datos train

train_raw <- read_delim("train_raw.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)

summary(train_raw$Urbano) # Casi el 90 % de las observaciones son urbanas

summary(train_raw$Hombre)

summary(train_raw$Edad)

histogram(train_raw$Edad)

summary(train_raw$Pobre) # El 25% de los hogares son pobres

length(which(train_raw$Edad==0)) # Hay 7 mil bebes

#Ponemos las variables que son factores como factores

summary(train_raw)

## Factores:

# Conversión explícita a factor ----

train_raw$Urbano                      <- as.factor(train_raw$Urbano)
train_raw$Hombre                      <- as.factor(train_raw$Hombre)
train_raw$Parentesco_jefe             <- as.factor(train_raw$Parentesco_jefe)
train_raw$Asegurado_salud             <- as.factor(train_raw$Asegurado_salud)
train_raw$Regimen_salud               <- as.factor(train_raw$Regimen_salud)
train_raw$Nivel_educativo             <- as.factor(train_raw$Nivel_educativo)
train_raw$Actividad_semana_pasada     <- as.factor(train_raw$Actividad_semana_pasada)
train_raw$Posicion_ocupacional        <- as.factor(train_raw$Posicion_ocupacional)
train_raw$Oficio                      <- as.factor(train_raw$Oficio)
train_raw$Posicion_ocupacional_segundo<- as.factor(train_raw$Posicion_ocupacional_segundo)
train_raw$Quiere_trabajar_mas_horas   <- as.factor(train_raw$Quiere_trabajar_mas_horas)
train_raw$Diligencias_trabajar_mas_horas_ult4s <- as.factor(train_raw$Diligencias_trabajar_mas_horas_ult4s)
train_raw$Disponible_trabajar_mas_horas        <- as.factor(train_raw$Disponible_trabajar_mas_horas)
train_raw$Diligencias_cambiar_trabajo_ult4s    <- as.factor(train_raw$Diligencias_cambiar_trabajo_ult4s)
train_raw$Disponibilidad_comenzar_en_un_mes    <- as.factor(train_raw$Disponibilidad_comenzar_en_un_mes)
train_raw$Busco_trabajo_primera_vez_o_experiencia <- as.factor(train_raw$Busco_trabajo_primera_vez_o_experiencia)
train_raw$Recibio_prima_servicios_12m <- as.factor(train_raw$Recibio_prima_servicios_12m)
train_raw$Recibio_prima_navidad_12m   <- as.factor(train_raw$Recibio_prima_navidad_12m)
train_raw$Recibio_prima_vacaciones_12m<- as.factor(train_raw$Recibio_prima_vacaciones_12m)
train_raw$Recibio_viaticos_permanentes_12m <- as.factor(train_raw$Recibio_viaticos_permanentes_12m)
train_raw$Recibio_bonificaciones_anuales_12m<- as.factor(train_raw$Recibio_bonificaciones_anuales_12m)
train_raw$Cotiza_pensiones           <- as.factor(train_raw$Cotiza_pensiones)
train_raw$Segundo_trabajo            <- as.factor(train_raw$Segundo_trabajo)
train_raw$Recibio_arriendos_o_pensiones_mes_pasado <- as.factor(train_raw$Recibio_arriendos_o_pensiones_mes_pasado)
train_raw$Recibio_pensiones_mes_pasado <- as.factor(train_raw$Recibio_pensiones_mes_pasado)
train_raw$Recibio_pension_alimentaria_mes_pasado <- as.factor(train_raw$Recibio_pension_alimentaria_mes_pasado)
train_raw$Recibio_transferencias_12m  <- as.factor(train_raw$Recibio_transferencias_12m)
train_raw$Dinero_otroshogares_pais_12m <- as.factor(train_raw$Dinero_otroshogares_pais_12m)
train_raw$Dinero_residentes_exterior_12m <- as.factor(train_raw$Dinero_residentes_exterior_12m)
train_raw$Ayudas_gobierno_12m         <- as.factor(train_raw$Ayudas_gobierno_12m)
train_raw$Intereses_dividendos_utilidades_12m <- as.factor(train_raw$Intereses_dividendos_utilidades_12m)
train_raw$Cesantias_o_intereses_12m   <- as.factor(train_raw$Cesantias_o_intereses_12m)
train_raw$Otras_fuentes_12m           <- as.factor(train_raw$Otras_fuentes_12m)
train_raw$Ocupado                     <- as.factor(train_raw$Ocupado)
train_raw$Desocupado                  <- as.factor(train_raw$Desocupado)
train_raw$Inactivo                    <- as.factor(train_raw$Inactivo)
train_raw$Propiedad_vivienda          <- as.factor(train_raw$Propiedad_vivienda)
train_raw$Departamento                <- as.factor(train_raw$Departamento)
train_raw$Ciudad_cat                  <- as.factor(train_raw$Ciudad_cat)
train_raw$Mujer_jefe                  <- as.factor(train_raw$Mujer_jefe)
train_raw$Oficio_C8                   <- as.factor(train_raw$Oficio_C8)
train_raw$Oficio_nombre               <- as.factor(train_raw$Oficio_nombre)
train_raw$Oficio_skill                <- as.factor(train_raw$Oficio_skill)
train_raw$Ingreso_binario_hogar       <- as.factor(train_raw$Ingreso_binario_hogar)
train_raw$Pobre                       <- as.factor(train_raw$Pobre)

#Ordenamos un tanto la base de datos

train_raw <- train_raw %>% relocate(Pobre, .after = Orden)

# Logit general con regla de 0.5 (criterio de Bayes)
#limpiamos los factores 
train_raw <- droplevels(train_raw)
train_raw <- train_raw %>% select(-Ingreso_binario_hogar)
mylogit <- glm(formula = Pobre ~., data=train_raw,
               family = 'binomial') 
regla <- 0.5

for (v in names(train_raw)) {
  if (is.factor(train_raw[[v]])) {
    lvl <- levels(droplevels(train_raw[[v]]))
    if (length(lvl) < 2) cat(v, "has only one level:", lvl, "\n")
  }
}
bad_vars <- names(which(colMeans(is.na(train_raw)) > 0.4))
train_model <- train_raw[, !(names(train_raw) %in% bad_vars)]

train_raw %>%
  group_by(Edad) %>%
  summarise(missing_income = sum(is.na(Cotiza_pensiones))) %>%
  ggplot(aes(x = Edad, y = missing_income)) +
  geom_col() +
  labs(title = "Missing income values by age")

table(train_raw$Cotiza_pensiones, useNA = "ifany")
