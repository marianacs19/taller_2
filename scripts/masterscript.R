###############################################################################
# PROBLEM SET 2 — MODELOS DE PREDICCIÓN DE POBREZA
# Grupo 6: Juan Esteban Moncada, Marcel Montesdoca, Mariana Correa, Rodrigo Iriarte
# -----------------------------------------------------------------------------
# Script maestro (equivalente a "do file" en Stata).
# Ejecuta los componentes del proyecto en orden lógico:
# 1) Preparación/Limpieza y Descriptivas
# 2) Modelos de referencia
# 3) Modelos predictivos (Logit, Elastic Net, Árboles/Bagging, RF con pesos, XGBoost)
#
# Requisitos: abrir el proyecto "taller_1.Rproj" para rutas relativas correctas.
###############################################################################

# --- LIMPIEZA DE ENTORNO -----------------------------------------------------
rm(list = ls())            # Limpia el entorno
cat("\014")                # Limpia consola (opcional)
graphics.off()             # Cierra dispositivos gráficos

# --- CONFIGURACIÓN INICIAL ---------------------------------------------------
# Cargar/instalar librerías con pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, rvest, writexl, readxl,
  gt, gtsummary, modelsummary, ranger,
  caret, boot, stargazer, rpart, rpart.plot,
  skimr, car, forcats, scales, glmnet, pROC,
  xgboost, Matrix                              # <- añadido para XGBoost
)

# --- RUTA DE TRABAJO ---------------------------------------------------------
# Establece el directorio del archivo actual (requiere RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cat("Directorio de trabajo establecido en:\n", getwd(), "\n\n")

# --- CREACIÓN DE CARPETAS DE SALIDA ------------------------------------------
dirs <- c("scripts", "stores", "Tables-graphs", "estimations")
for (d in dirs) if (!dir.exists(d)) dir.create(d)

###############################################################################
# 1. PREPARACIÓN DE LOS DATOS Y DESCRIPTIVAS
###############################################################################
# - "Proceso_taller.ipynb" (Python) debe haberse ejecutado previamente
# - "R_básico.R": descriptivas y cualquier limpieza adicional en R

cat("Ejecutando: R_básico.R (estadísticas descriptivas y limpieza)\n")
source("scripts/R_básico.R")

###############################################################################
# 2. MODELO BASE (REFERENCIA)
###############################################################################
# - Regresión lineal como benchmark inicial para comparar desempeño de clasificadores

cat("Ejecutando: Regresion_lineal.R (modelo lineal base)\n")
source("scripts/Regresion_lineal.R")

###############################################################################
# 3. MODELOS LOGIT
###############################################################################
# - 7 especificaciones: Modelos 1–3, 4A–4C y Logit Penalizado
# - Incluye "Logit F1 optimizado" y "Logit Penalizado (Elastic Net)" del top 5 F1

cat("Ejecutando: estimacion_modelos_logit.R (modelos logit)\n")
source("scripts/estimacion_modelos_logit.R")

###############################################################################
# 4. ELASTIC NET (CV EN α Y λ)
###############################################################################
# - `elastic_net.R` implementa dos variantes:
#   (a) Logit (binomial) con glmnet + validación cruzada en alpha y lambda
#   (b) Lineal (gaussian) con glmnet + validación cruzada en alpha y lambda
# - Selección de hiperparámetros vía grilla y/o caret + cv.glmnet

cat("Ejecutando: elastic_net.R (glmnet: logit y lineal con CV en alpha y lambda)\n")
source("scripts/elastic_net.R")

###############################################################################
# 5. ÁRBOLES DE DECISIÓN / BAGGING
###############################################################################
# - `arbolesRod.R` incluye un CART básico y un modelo con Bagging

cat("Ejecutando: arbolesRod.R (CART y Bagging)\n")
source("scripts/arbolesRod.R")

###############################################################################
# 6. RANDOM FOREST CON PESOS
###############################################################################
# - `script_RF_Con_Pesos.R` usa ponderación/clase para mitigar desbalance
# - Único modelo que superó F1 > 0.65 en la competencia (según README)

cat("Ejecutando: script_RF_Con_Pesos.R (Random Forest con pesos)\n")
source("scripts/script_RF_Con_Pesos.R")

###############################################################################
# 7. XGBOOST (BÚSQUEDA EN GRILLA)
###############################################################################
# - `xgboost.R` realiza búsqueda en grilla (grid search) de hiperparámetros
#   para maximizar desempeño (por ejemplo, F1/AUC) con validación cruzada.
# - Requiere matrices dispersas (Matrix) o matrices numéricas para xgb.DMatrix.

cat("Ejecutando: xgboost.R (XGBoost con búsqueda en grilla)\n")
source("scripts/xgboost.R")

###############################################################################
# FINALIZACIÓN
###############################################################################
cat("\n✅ Todos los scripts se ejecutaron exitosamente.\n")

###############################################################################
# FIN DEL SCRIPT
###############################################################################
