# =============================================================================
# Fragmentación del Congreso Nacional de Chile, 2010–2026
# 00_funciones.R — paquetes, rutas y funciones comunes
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(ggplot2); library(readxl)
})

DIR_FUE  <- file.path("data", "fuentes")
DIR_RAW  <- file.path("data", "raw")
DIR_PROC <- file.path("data", "processed")
DIR_TAB  <- file.path("output", "tablas")
DIR_FIG  <- file.path("output", "figuras")
for (d in c(DIR_PROC, DIR_TAB, DIR_FIG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

leer_csv2 <- function(f, ...) read_delim(f, delim = ";", show_col_types = FALSE,
                                         locale = locale(decimal_mark = ",", grouping_mark = "."), ...)
escribir <- function(x, nombre) write_csv2(x, file.path(DIR_TAB, nombre))

# Número efectivo de partidos (Laakso y Taagepera, 1979): 1 / suma de proporciones al cuadrado
nep <- function(x) { x <- x[!is.na(x) & x > 0]; p <- x / sum(x); 1 / sum(p^2) }

# Desproporcionalidad de mínimos cuadrados (Gallagher, 1991), en puntos porcentuales
gallagher <- function(votos, escanos) {
  v <- 100 * votos / sum(votos); s <- 100 * escanos / sum(escanos)
  sqrt(sum((v - s)^2) / 2)
}

# Asignación D'Hondt: votos por lista y número de escaños -> escaños por lista
dhondt <- function(votos, escanos) {
  if (escanos <= 0 || length(votos) == 0) return(integer(length(votos)))
  cocientes <- outer(votos, seq_len(escanos), "/")
  orden <- order(cocientes, decreasing = TRUE)[seq_len(escanos)]
  tabulate(((orden - 1) %% length(votos)) + 1, nbins = length(votos))
}

# Normaliza texto para cruzar nombres entre fuentes (sin tildes, mayúsculas, espacios simples)
normalizar <- function(x) {
  x |> stringi::stri_trans_general("Latin-ASCII") |> toupper() |>
    str_replace_all("[^A-Z ]", " ") |> str_squish()
}
