###############################################################################
# PROBLEM SET 2 — MODELOS DE PREDICCIÓN DE POBREZA
# Grupo 6: Juan Esteban Moncada, Marcel Montesdoca, Mariana Correa, Rodrigo Iriarte
# -----------------------------------------------------------------------------
# Este script actúa como un "master script"
# Su propósito es ejecutar todos los scripts del proyecto en orden lógico:
# 1. Preparación y limpieza de datos
# 2. Estadísticas descriptivas
# 3. Estimación de modelos predictivos
#
# Requisitos: tener la carpeta completa del repositorio y abrir el proyecto
# "taller_1.Rproj" para asegurar rutas relativas correctas.
###############################################################################

# --- LIMPIEZA DE ENTORNO -----------------------------------------------------
rm(list = ls())            # Elimina todos los objetos del entorno actual
cat("\014")                # Limpia la consola (opcional)
graphics.off()             # Cierra todas las ventanas de gráficos abiertas

# --- CONFIGURACIÓN INICIAL ---------------------------------------------------
# Cargar librerías necesarias con pacman (instala si no existen)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, rvest, writexl, readxl,
  gt, gtsummary, modelsummary, ranger,
  caret, boot, stargazer, rpart, rpart.plot,
  skimr, car, forcats, scales, glmnet, pROC
)

# --- RUTA DE TRABAJO ---------------------------------------------------------
# Establecer el directorio de trabajo automáticamente (solo funciona en RStudio)
# seguir instrucciones del readme
# --- CREACIÓN DE CARPETAS DE SALIDA ------------------------------------------
# Crear subcarpetas si no existen
dirs <- c("scripts", "stores", "Tables-graphs", "estimations")
for (d in dirs) if (!dir.exists(d)) dir.create(d)

###############################################################################
# 1. PREPARACIÓN DE LOS DATOS
###############################################################################
# Este paso debe realizarse primero, ya que genera parte de las bases limpias 
#que se usan
# en los demás scripts. 
# Requiere ejecutar previamente el notebook "Proceso_taller.ipynb" en Python.

cat("Ejecutando: R_básico.R (estadísticas descriptivas y limpieza)\n")
source("scripts/R_básico.R")

###############################################################################
# 2. MODELOS LOGIT
###############################################################################
# Contiene 7 modelos logit (Modelos 1–3, 4A–4C y Logit Penalizado).
# Incluye los modelos con mejor desempeño fuera de muestra:
# "Logit F1 optimizado" y "Logit Penalizado (Elastic Net)".

cat("Ejecutando: estimacion_modelos_logit.R (modelos logit)\n")
source("scripts/estimacion_modelos_logit.R")

###############################################################################
# 3. MODELOS DE ÁRBOLES DE DECISIÓN
###############################################################################
# Contiene modelos CART y Bagging implementados.

cat("Ejecutando: arbolesRod.R (CART y Bagging)\n")
source("scripts/arbolesRod.R")

###############################################################################
# 4. MODELOS DE REGRESIÓN LINEAL
###############################################################################
# Script comparativo con un modelo de regresión lineal simple como referencia.

cat("Ejecutando: Regresion_lineal.R (modelo lineal base)\n")
source("scripts/Regresion_lineal.R")

###############################################################################
# 5. RANDOM FOREST CON PESOS
###############################################################################
# Este modelo resolvió el problema de desbalance de clases.
# Fue el único modelo que superó F1-score = 0.65.

cat("Ejecutando: script_RF_Con_Pesos.R (Random Forest con pesos)\n")
source("scripts/script_RF_Con_Pesos.R")

###############################################################################
# FINALIZACIÓN
###############################################################################
cat("\n✅ Todos los scripts se ejecutaron exitosamente.\n")
cat("Revisar las carpetas '/stores' y '/Tables-graphs' para ver los outputs.\n")

###############################################################################
# FIN DEL SCRIPT
###############################################################################
