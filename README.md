Problem Set 2 —  Modelos de predicción de pobreza
======================================================

Autores - Grupo 6
-------
- Juan Esteban Moncada 202021566
- Marcel Montesdoca 202012736
- Mariana Correa 202020924
- Rodrigo Iriarte 202022852
------------------------------------------------------

Propósito del repositorio
-------------------------
Este repositorio contiene la solución al **Problem Set 2**, de la clase: Big Data Y Machine Learning para la Economía Aplicada 
El propósito del trabajo es **desarrollar modelos predictivos a partir de un conjunto reducido de variables socioeconómicas, puede ser una herramienta útil, precisa y más costo-eficiente para el diseño de políticas públicas**.  
------------------------------------------------------

Origen de los datos
-------------------
Los datos provienen de: **Pobreza Monetaria y Multidimensional en Colombia 2018** para Colombia. https://www.dane.gov.co/index.php/estadisticas-por-tema/pobreza-y-condiciones-de-vida/pobreza-y-desigualdad/pobreza-monetaria-y-multidimensional-en-colombia-2018

El dataset original corresponde a la medición de pobreza monetaria y desigualdad del DANE y contiene información sobre empleo, ingresos y características sociodemográficas de los hogares. 
------------------------------------------------------

Estructura del repositorio
--------------------------
- `Proceso_taller.ipynb` - Jupyter Notebook en Python que utiliza las bases de datos descargadas de la competencia y las limpia para posteriormente utilizarla en el análisis. Para reproducibilidad, descargar la carpeta de la competencia e incluirla con el siguiente nombre: `uniandes-bdml-2025-20-ps-2`.
- `/scripts/`
  - `R_básico.R` — Script que ejecuta las estadísticas descriptivas y las gráficas descriptivas.
  - `Script_RF_Con_Pesos.R` - Scrip que ejecuta y estima modelos que fueron subidos a la competencia. El único modelo que superó el F-score 0.65, fue un random forest.
- `/stores/`
  - Base de datos ( `base_geih.xlsx`).
- `/Tables-graphs/`
  - Gráficas y tablas exportadas.
- `taller_1.Rproj`
  - Proyecto de R para ejecutar con rutas relativas.`
  - Archivos de configuración y entorno.
- `README` (este archivo).
------------------------------------------------------

Para ejecución primero correr `Proceso_taller.ipynb`. Luego los ejecutar el script `masterscript.R`. 
Este archivo correrá los scripts asociados a las estimación de los diferentes modelos.

------------------------------------------------------

A continuación se presentan las divisiones del código, en caso de que se quiera estimar algunos modelos en específico (la descripción también se encuentra en
el masterscript).
---------------------

`Random_forest.R` es el script del modelo elegido y con el mejor F1-score. 
`Regresion_lineal.R` es el script donde está contenido el modelo básico de rgresión lineal que se llevó a cabo como un primer insumo para comparar los demás modelos. 
`arbolesRod.R` están contenidos dos modelos, uno de ellos es un CART básico y otro es usando la metodología bagging. `estimacion_modelos_logit.R` contiene 7 modelos logit (incluye Modelos 1–3, 4A–4C y Logit Penalizado). De aquí salen los modelos "Logit f1 optimizado" y "Logit Penalizado (Elastic Net)" que terminaron en el top 5 fuera de muestra por F1. 
`script_RF_Con_Pesos.R` contiene un Random Forest utilizando pesos para solucionar el desbalance de clases. 
`elastic_net.R` contiene un modelo de Elastic Net con dos versiones: una logit y la otra normal. Se utiliza un cross validation con variaciones en los parámetros alpha y lambda para conseguir el mejor resultado.
`xgboost.R` contiene un modelo de XGBoost que usa una grilla para estimar la mejor combinación de parámetros mediante una grilla.

------------------------------------------------------

Requisitos y configuración
--------------------------
**Versión sugerida:** R (≥ 4.3) y RStudio.

**Paquetes R**
El script usa `pacman::p_load()` para instalar/cargar automáticamente:
- `tidyverse`, `rvest`, `writexl`, `readxl`
- `gt`, `gtsummary`, `modelsummary`, `ranger`
- `caret`, `boot`, `stargazer`, `rpart`, `rpart.plot`
- `skimr`, `car`, `forcats`, `scales`, `glmnet`, `pROC`

Instalación rápida si se necesita:

install.packages(c(
  "pacman","tidyverse","rvest","writexl","readxl",
  "gt","gtsummary","modelsummary", "glmnet", "rpart", "rpart.plot"
  "caret","boot","stargazer","skimr","car","forcats","scales", "pROC", "ranger"
))

**Reproducibilidad**
En cada script hay diferentes semillas que se deben dejar igual para que los modelos sean reproducibles

------------------------------------------------------

Instrucciones de uso
--------------------
1. Clonar el repositorio:
   git clone <url-del-repo>

2. Abrir el proyecto en RStudio:
   Abrir el archivo `taller_2.Rproj`. Esto asegura que las rutas relativas a `/scripts`, `/stores` y `/views` funcionen correctamente.

3. Ejecutar el script `Proceso_taller.ipynb` y `masterscript.R` para la parte de los datos y guardar las bases de tados
   
   Nota: el tiempo de ejecución será largo ya que correrá todos los modelos. 

6. Outputs generados:
   - stores/train_def.RData — base de entrenamiento
   - stores/test_def.RData — base de test
   - Gráficas en /views
   - Los resultados de los modelos para subir a Kaggle en /estimations.
