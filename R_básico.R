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
  Metrics,
  kableExtra
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
  "Inactivo","0","No_Inactivo",
  "Ocupado_jefe","0","No_ocupado",
  "Ocupado_jefe","1","Ocupado",
  "Inactivo_jefe","0","No_inactivo",
  "Inactivo_jefe","1","Inactivo",
  "Quiere_trabajar_mas_horas_jefe","0","No",
  "Quiere_trabajar_mas_horas_jefe","1","Si",
  "Diligencias_cambiar_trabajo_ult4s_jefe","0","No",
  "Diligencias_cambiar_trabajo_ult4s_jefe","1","Si",
  "Diligencias_trabajar_mas_horas_ult4s_jefe","0","No", 
  "Diligencias_trabajar_mas_horas_ult4s_jefe","1","Si", 
  "Disponibilidad_comenzar_en_un_mes_jefe","0","No",
  "Disponibilidad_comenzar_en_un_mes_jefe","1","Si",
  "Disponible_trabajar_mas_horas_jefe","0","No",
  "Disponible_trabajar_mas_horas_jefe","1","Si",
  "Poblacion_edad_trabajar_jefe","0","No",
  "Poblacion_edad_trabajar_jefe","1","Si",
  "Subsidio_educativo_hogar" ,"0","No",
  "Subsidio_educativo_hogar","1","Si",
  "Subsidio_familiar_hogar","0","No",
  "Subsidio_familiar_hogar","1","Si",
  "Subsidio_transporte_hogar","0","No",
  "Subsidio_transporte_hogar","1","Si",
) |> mutate(across(everything(), as.character))

##Aplicar labels

train_def <- apply_labels_from_dict(train, dict_df)
test_def <- apply_labels_from_dict(test, dict_df)

save(test_def, file="Data/test_def.RData")
save(train_def, file="Data/train_def.RData")

# Estadísticas descriptivas

# ==== (1) Resumen compacto de TODAS las variables ====
overall_summary_table <- function(df, exclude = NULL, digits = 3) {
  if (!is.null(exclude)) df <- df[, setdiff(names(df), exclude), drop = FALSE]
  
  is_cat <- function(x) is.factor(x) || is.character(x) || is.logical(x)
  
  # --- Numéricas ---
  num_vars <- dplyr::select(df, where(is.numeric))
  if (ncol(num_vars) > 0) {
    num_tbl <- num_vars |>
      tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") |>
      dplyr::summarise(
        n     = sum(!is.na(value)),
        media = mean(value, na.rm = TRUE),
        sd    = sd(value, na.rm = TRUE),
        min   = suppressWarnings(min(value, na.rm = TRUE)),
        max   = suppressWarnings(max(value, na.rm = TRUE)),
        .by   = "variable"
      ) |>
      dplyr::mutate(tipo = "numérica")
  } else {
    num_tbl <- tibble::tibble(
      variable = character(), n = integer(), media = double(), sd = double(),
      min = double(), max = double(), tipo = character()
    )
  }
  
  # --- Categóricas (arreglo: calcular p y modo por variable) ---
  cat_vars <- dplyr::select(df, where(is_cat))
  if (ncol(cat_vars) > 0) {
    # 1) Conteos por nivel (excluye NA)
    cat_counts <- cat_vars |>
      dplyr::mutate(dplyr::across(everything(), as.character)) |>
      tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") |>
      dplyr::filter(!is.na(value)) |>
      dplyr::count(variable, value, name = "n_value")
    
    # 2) Totales por variable
    tot <- cat_counts |>
      dplyr::summarise(
        n         = sum(n_value),
        n_niveles = dplyr::n_distinct(value),
        .by = "variable"
      )
    
    # 3) Modo y su porcentaje (agrupado por variable)
    modes <- cat_counts |>
      dplyr::group_by(variable) |>
      dplyr::mutate(p = n_value / sum(n_value)) |>
      dplyr::slice_max(n_value, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        variable,
        modo            = value,
        modo_porcentaje = p
      )
    
    cat_tbl <- dplyr::left_join(tot, modes, by = "variable") |>
      dplyr::mutate(tipo = "categórica")
  } else {
    cat_tbl <- tibble::tibble(
      variable = character(), n = integer(), n_niveles = integer(),
      modo = character(), modo_porcentaje = double(), tipo = character()
    )
  }
  
  # --- Unir y redondear ---
  out <- dplyr::bind_rows(num_tbl, cat_tbl) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, digits))) |>
    dplyr::relocate(variable, tipo)
  
  out
}

# ==== (2) Diferencias de medias por pobreza (Welch t-test)
#      y proporciones para factores binarios ====
diff_effects_by_poverty <- function(df,
                                    poverty_var,
                                    poor_level = NULL,
                                    digits = 3,
                                    success_map = NULL,    # list(var="Sí") o c(var="Sí")
                                    success_ref = c("second","first")) {
  stopifnot(poverty_var %in% names(df))
  success_ref <- match.arg(success_ref)
  pov <- .to_poverty01(df[[poverty_var]], poor_level = poor_level)
  df2 <- df |> dplyr::mutate(`__poverty__` = pov)
  
  # --- NUMÉRICAS -> Welch t-test ---
  num_vars <- names(df2)[sapply(df2, is.numeric)]
  num_vars <- setdiff(num_vars, "__poverty__")
  
  res_num <- purrr::map_dfr(num_vars, function(v) {
    x  <- df2[[v]]
    g1 <- x[df2$`__poverty__` == 1]
    g0 <- x[df2$`__poverty__` == 0]
    tt <- if (sum(!is.na(g1))>1 && sum(!is.na(g0))>1) try(t.test(g1, g0), silent = TRUE) else NULL
    pval <- if (inherits(tt, "htest")) tt$p.value else NA_real_
    sig  <- ifelse(is.na(pval), "",
                   ifelse(pval < .001, "***",
                          ifelse(pval < .01,  "**",
                                 ifelse(pval < .05,  "*",
                                        ifelse(pval < .1,   "•", "")))))
    
    tibble::tibble(
      variable     = v,
      tipo         = "numérica",
      medida       = "media",
      n_pobre      = sum(!is.na(g1)),
      est_pobre    = mean(g1, na.rm = TRUE),
      n_no_pobre   = sum(!is.na(g0)),
      est_no_pobre = mean(g0, na.rm = TRUE),
      diferencia   = mean(g1, na.rm = TRUE) - mean(g0, na.rm = TRUE),
      p_value      = pval,
      Sig.         = sig
    )
  })
  
  # --- FACTOR BINARIO -> prop.test ---
  is_bin_factor <- function(x) is.factor(x) && (nlevels(x) == 2)
  fac_vars <- names(df2)[sapply(df2, is_bin_factor)]
  fac_vars <- setdiff(fac_vars, c(poverty_var))  # excluye la propia pobreza
  
  res_fac <- purrr::map_dfr(fac_vars, function(v) {
    f <- df2[[v]]
    levs <- levels(f)
    target <- if (!is.null(success_map) && !is.null(unname(success_map[[v]]))) {
      as.character(success_map[[v]])
    } else if (success_ref == "second") {
      levs[2]
    } else levs[1]
    
    z  <- as.integer(f == target)
    z1 <- z[df2$`__poverty__` == 1]
    z0 <- z[df2$`__poverty__` == 0]
    a  <- sum(z1 == 1, na.rm = TRUE); n1 <- sum(!is.na(z1))
    b  <- sum(z0 == 1, na.rm = TRUE); n0 <- sum(!is.na(z0))
    
    pt <- if (n1>0 && n0>0) try(prop.test(x = c(a,b), n = c(n1,n0)), silent = TRUE) else NULL
    pval <- if (inherits(pt, "htest")) pt$p.value else NA_real_
    sig  <- ifelse(is.na(pval), "",
                   ifelse(pval < .001, "***",
                          ifelse(pval < .01,  "**",
                                 ifelse(pval < .05,  "*",
                                        ifelse(pval < .1,   "•", "")))))
    
    tibble::tibble(
      variable     = v,
      tipo         = "factor_bin",
      medida       = paste0('proporción de "', target, '"'),
      n_pobre      = n1,
      est_pobre    = ifelse(n1>0, a/n1, NA_real_),
      n_no_pobre   = n0,
      est_no_pobre = ifelse(n0>0, b/n0, NA_real_),
      diferencia   = (ifelse(n1>0, a/n1, NA_real_) - ifelse(n0>0, b/n0, NA_real_)),
      p_value      = pval,
      Sig.         = sig
    )
  })
  
  out <- dplyr::bind_rows(res_num, res_fac) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, digits))) |>
    dplyr::arrange(dplyr::desc(abs(diferencia)))
  out
}

# ==== (A) Helper: exportar a LaTeX con kableExtra ====
to_latex_file <- function(df, caption, label, file, digits = 3, col_names = NULL) {
  latex <- kbl(
    df,
    format      = "latex",
    booktabs    = TRUE,
    longtable   = TRUE,
    linesep     = "",
    caption     = caption,
    label       = label,
    align       = "l",
    escape      = TRUE
  ) |>
    kable_styling(latex_options = c("repeat_header", "hold_position"))
  cat(latex, file = file)
  invisible(file)
}

# ==== (B) Helper: convertir variable de pobreza a 0/1 ====
# poor_level: nombre del nivel que indica "pobre" si la variable no es 0/1.
.to_poverty01 <- function(x, poor_level = NULL) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x))  return(as.integer(x > 0))
  x <- as.factor(x)
  if (!is.null(poor_level)) return(as.integer(x == poor_level))
  levs <- levels(x)
  if (length(levs) != 2) stop("La variable de pobreza debe tener 2 niveles o especifica 'poor_level'.")
  as.integer(x == levs[2])  # convención: segundo nivel = pobre
}

overall <- overall_summary_table(train_def, exclude = c("id"))
diff_mix <- diff_effects_by_poverty(
  train_def,
  poverty_var = "Pobre",
  poor_level  = "Pobre",
  success_ref = "second"
)

                                 
overall_out <- overall |>
  rename(
    `Variable` = variable, `Tipo` = tipo, `N` = n,
    `Media` = media, `Desv. Est.` = sd, `Mín` = min, `Máx` = max,
    `N niveles` = n_niveles, `Modo` = modo, `Modo (%)` = modo_porcentaje
  )
diff_mix_out <- diff_mix |>
  rename(
    Variable = variable, `Tipo` = tipo, `Medida` = medida,
    `N pobre` = n_pobre, `Est. pobre` = est_pobre,
    `N no pobre` = n_no_pobre, `Est. no pobre` = est_no_pobre,
    `Dif.` = diferencia, `p-valor` = p_value
  )

to_latex_file(
  overall_out,
  caption = "Resumen descriptivo de todas las variables.",
  label   = "tab:resumen_todas",
  file    = "tabla_resumen_todas.tex"
)
to_latex_file(
  diff_mix_out,
  caption = "Diferencias por condición de pobreza: medias (numéricas) y proporciones (factores binarios).",
  label   = "tab:diff_mix_pobreza",
  file    = "tabla_diff_mix_pobreza.tex"
)


#Gráficas


p_load(ggplot2,dplyr,tidyr,patchwork)

graficar_categoricas <- function(datos, 
                                 variables, 
                                 variable_grupo, 
                                 titulo_principal = "Comparación de Variables Categóricas",
                                 colores = c("#037AE7", "#020050")) {
  
  # Verificar que las variables existan en los datos
  vars_faltantes <- setdiff(c(variables, variable_grupo), names(datos))
  if (length(vars_faltantes) > 0) {
    stop("Las siguientes variables no existen en los datos: ", 
         paste(vars_faltantes, collapse = ", "))
  }
  
  # Lista para almacenar las gráficas
  lista_graficas <- list()
  
  # Crear una gráfica para cada variable
  for (var in variables) {
    # Calcular proporciones por grupo
    datos_graf <- datos %>%
      filter(!is.na(.data[[var]]) & !is.na(.data[[variable_grupo]])) %>%
      group_by(.data[[variable_grupo]], .data[[var]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(.data[[variable_grupo]]) %>%
      mutate(prop = n / sum(n) * 100)
    
    # Crear la gráfica
    p <- ggplot(datos_graf, aes(x = .data[[var]], y = prop, fill = .data[[variable_grupo]])) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      scale_fill_manual(values = colores) +
      labs(title = var,
           x = NULL,
           y = "Porcentaje (%)",
           fill = variable_grupo) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        panel.grid.minor = element_blank()
      ) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))
    
    lista_graficas[[var]] <- p
  }
  
  # Combinar todas las gráficas
  n_vars <- length(variables)
  n_cols <- min(2, n_vars)  # Máximo 3 columnas
  
  grafica_combinada <- wrap_plots(lista_graficas, ncol = n_cols) +
    plot_annotation(
      # title = titulo_principal,
      theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
    )
  
  return(grafica_combinada)
}

variables_a_graficar <- c( "Ciudad_cat",  
                           "Años_educ_mean_hogar", 
                           "Afiliados_salud_hogar"
)

# Crear la gráfica comparando por sexo
grafica1 <- graficar_categoricas(
  datos = train_def,
  variables = variables_a_graficar,
  variable_grupo = "Pobre"
)

print(grafica1)

ggsave("Estimations/Descriptivas1.pdf", plot = grafica1, width = 15, height = 8)

variables_a_graficar <- c( "Mujer_jefe",
                           "Regimen_salud_jefe", "Oficio_C8_jefe", "Ocupado_jefe"
)

# Crear la gráfica comparando por sexo
grafica1 <- graficar_categoricas(
  datos = train_def,
  variables = variables_a_graficar,
  variable_grupo = "Pobre"
)

print(grafica1)

ggsave("Estimations/Descriptivas2.pdf", plot = grafica1, width = 15, height = 8)

# =========================================================
# Fin -----------------------------------------------------
# =========================================================
