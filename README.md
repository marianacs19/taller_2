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

Para ejecución primero correr `Proceso_taller.ipynb` y `R_básico.R`. Luego los demás scripts de la carpeta `/scripts/`. 

------------------------------------------------------

Divisiones del código
---------------------
En la carpeta de scripts están contenidos los diferentes modelos que se desarrollaron. `Random Forest.R` está dividido en secciones principales:


------------------------------------------------------

Requisitos y configuración
--------------------------
**Versión sugerida:** R (≥ 4.3) y RStudio.

**Paquetes R**
El script usa `pacman::p_load()` para instalar/cargar automáticamente:
- `tidyverse`, `rvest`, `writexl`, `readxl`
- `gt`, `gtsummary`, `modelsummary`
- `caret`, `boot`, `stargazer`
- `skimr`, `car`, `forcats`, `scales`

Instalación rápida (si no se tiene `pacman`):

install.packages(c(
  "pacman","tidyverse","rvest","writexl","readxl",
  "gt","gtsummary","modelsummary",
  "caret","boot","stargazer","skimr","car","forcats","scales"
))

**Reproducibilidad**
- set.seed(07092025) para procesos generales.
- set.seed(10101) para partición train/test. 

------------------------------------------------------

Instrucciones de uso
--------------------
1. Clonar el repositorio:
   git clone <url-del-repo>

2. Abrir el proyecto en RStudio:
   Abrir el archivo `taller_1.Rproj`. Esto asegura que las rutas relativas a `/scripts`, `/stores` y `/views` funcionen correctamente.

3. Ejecutar el script principal:
   source("scripts/taller1.R")

   Nota: la ejecución completa puede tardar un largo tiempo (especialmente scraping y bootstraps).

4. Outputs generados:
   - stores/base_geih.xlsx — base consolidada tras el scraping y limpieza.
   - Gráficas en /views (ej. salario_por_sexo_edad.png).
   - Tablas exportadas en LaTeX (.tex) en /views.

------------------------------------------------------

Resultados esperados / ejemplos
-------------------------------
Ejemplo de outputs clave:

- Gráfica Edad–Salario por Sexo (ceteris paribus)
  Archivo generado: views/salario_por_sexo_edad.png

- Tablas en LaTeX exportadas con stargazer y modelsummary,
  listas para incluir en reportes académicos.

------------------------------------------------------




