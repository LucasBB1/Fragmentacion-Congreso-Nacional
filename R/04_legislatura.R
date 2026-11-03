# =============================================================================
# 04_legislatura.R
# -----------------------------------------------------------------------------
# Fragmentación durante la legislatura: cambios de militancia entre una
# elección y la siguiente, en la Cámara (2010–2026) y el Senado (2010–2026).
#   * Cámara: militancias con fecha de inicio y término del servicio de datos
#     abiertos de la Cámara (data/fuentes/militancias_diputados_2010_2030.csv).
#   * Senado: nómina por período y cambios de militancia reconstruidos con las
#     reseñas parlamentarias de la BCN y prensa (data/fuentes).
# Para cada período se compara la composición al inicio (11 de marzo) y al
# término (10 de marzo, cuatro años después). Los independientes cuentan como
# unidades individuales en el número efectivo de partidos.
#
# Salidas  output/tablas/04_*.csv, data/processed/legislatura.rds
# =============================================================================

source(file.path("R", "00_funciones.R"))

PERIODOS <- tibble(periodo = c("2010-2014", "2014-2018", "2018-2022", "2022-2026"),
                   inicio = as.Date(c("2010-03-11", "2014-03-11", "2018-03-11", "2022-03-11")),
                   fin    = as.Date(c("2014-03-10", "2018-03-10", "2022-03-10", "2026-03-10")),
                   anio_eleccion = c(2009L, 2013L, 2017L, 2021L))

# Códigos de la Cámara -> siglas usadas en los resultados electorales
COD <- c(DC = "PDC", PCCH = "PC", LIBERAL = "PL", AMPL = "AMP", IGUAL = "IGUALDAD", PCS = "CS",
         AMA = "AMARILLOS", COMUNES = "COM")
sigla_camara <- function(x) if_else(x %in% names(COD), COD[x], x)

# Fusiones: RD, CS y Comunes se integran al Frente Amplio en 2024 (no cuentan como cambio)
FUSION <- c(RD = "FA", CS = "FA", COM = "FA")
fusionar <- function(x, periodo) if_else(periodo == "2022-2026" & x %in% names(FUSION), FUSION[x], x)

# Número efectivo de partidos de una composición (independientes como unidades individuales)
nep_comp <- function(etiqueta, id) nep(table(if_else(etiqueta == "IND", paste0("IND-", id), etiqueta)))

# ---------------------------------------------------------------------------------
# Cámara de Diputados
# ---------------------------------------------------------------------------------
mil <- read_delim(file.path(DIR_FUE, "militancias_diputados_2010_2030.csv"), delim = ";",
                  show_col_types = FALSE, col_types = cols(.default = "c")) |>
  mutate(across(c(dp_fecha1, mil_inicio, mil_termino), as.Date),
         partido = sigla_camara(partido_id))

# Correcciones con las reseñas parlamentarias de la BCN: en los registros más antiguos de la
# Cámara, algunas militancias empiezan o terminan en el límite del período legislativo y no en la
# fecha efectiva del cambio. Para esos diputados se reemplaza su historial entre 2010 y 2018.
corr <- read_delim(file.path(DIR_FUE, "correcciones_militancia_camara.csv"), delim = ";",
                   show_col_types = FALSE, col_types = cols(.default = "c")) |>
  mutate(across(c(mil_inicio, mil_termino), as.Date), partido = sigla_camara(partido))
VENTANA <- as.Date(c("2010-03-11", "2018-03-10"))
mil <- bind_rows(
  mil |> filter(!(id %in% corr$id & mil_inicio <= VENTANA[2] & (is.na(mil_termino) | mil_termino >= VENTANA[1]))),
  mil |> filter(id %in% corr$id) |> distinct(periodo, id, nombre, dp_fecha1) |>
    inner_join(select(corr, id, partido, mil_inicio, mil_termino), by = "id", relationship = "many-to-many"))

# Servicio del diputado en el período: el campo FechaInicio de la Cámara registra
# la fecha en que el diputado dejó el cargo (o el término del período).
dip <- mil |> distinct(periodo, id, nombre, salida = dp_fecha1) |>
  inner_join(PERIODOS, by = "periodo")

# ¿Quién fue electo? Cruce de nombres con los electos del SERVEL (tokens en común)
electos <- readRDS(file.path(DIR_PROC, "candidatos.rds")) |>
  filter(camara == "Diputados", electo) |> select(anio, candidato, sigla_eleccion = sigla, condicion)
tok <- function(x) strsplit(normalizar(x), " ")[[1]]
emparejar <- function(nombre, anio) {
  t <- tok(nombre); c <- electos[electos$anio == anio, ]
  s <- vapply(c$candidato, function(k) { u <- tok(k); length(intersect(t, u)) / min(length(t), length(u)) }, 0)
  ap_ok <- vapply(c$candidato, function(k) any(tail(t, 2) %in% tok(k)), TRUE)
  s[!ap_ok] <- 0
  if (max(s) >= 0.66 && sum(s == max(s)) == 1) c$candidato[which.max(s)] else NA_character_
}
dip$candidato <- mapply(emparejar, dip$nombre, dip$anio_eleccion)
dip <- dip |> left_join(electos, by = c("anio_eleccion" = "anio", "candidato"))
cobertura <- dip |> group_by(periodo) |>
  summarise(diputados = n(), electos_identificados = sum(!is.na(candidato)), .groups = "drop")
print(as.data.frame(cobertura))

militancia_en <- function(ids, fecha) {
  m <- mil |> filter(id %in% ids, mil_inicio <= fecha, is.na(mil_termino) | mil_termino >= fecha) |>
    group_by(id) |> arrange(desc(mil_inicio), .by_group = TRUE) |> slice(1) |> ungroup() |>
    select(id, partido)
  tibble(id = ids) |> left_join(m, by = "id") |> mutate(partido = coalesce(partido, "IND"))
}

comp_camara <- PERIODOS |> rowwise() |> group_map(function(p, ...) {
  d <- dip |> filter(periodo == p$periodo)
  ini_ids <- d$id[!is.na(d$candidato)]                        # electos que asumieron
  fin_ids <- d$id[d$salida >= p$fin - 1]                      # en ejercicio al término
  ini <- militancia_en(ini_ids, p$inicio + 1) |> mutate(momento = "inicio")
  fin <- militancia_en(fin_ids, p$fin - 1) |> mutate(momento = "termino")
  bind_rows(ini, fin) |> mutate(periodo = p$periodo, camara = "Diputados",
                                partido = if_else(momento == "termino", fusionar(partido, periodo), partido))
}) |> bind_rows()

# Concordancia entre la militancia de la Cámara al inicio y la etiqueta electoral
concord <- comp_camara |> filter(momento == "inicio") |>
  left_join(select(dip, periodo, id, sigla_eleccion, condicion), by = c("periodo", "id")) |>
  mutate(mil_eleccion = if_else(condicion == "militante", sigla_eleccion, "IND"),
         coincide = partido == mil_eleccion)
print(concord |> group_by(periodo) |> summarise(n = n(), coincide = mean(coincide, na.rm = TRUE)))
escribir(concord |> filter(!coincide), "04_discrepancias_camara.csv")

# Cambios de militancia durante el período (diputados que asumieron como electos).
# Se reconstruye la secuencia de etiquetas de cada diputado: militancia al asumir y
# cada militancia que empieza o termina dentro del período (sin militancia = IND).
# Las fusiones no cuentan como cambio: Revolución Democrática, Convergencia Social y
# Comunes se integraron al Frente Amplio en 2024 (Comunes tras su disolución en mayo).

secuencia <- function(id_, p) {
  m <- mil |> filter(id == id_) |> distinct(mil_inicio, mil_termino, partido) |> arrange(mil_inicio)
  hasta <- min(p$fin - 1, dip$salida[dip$id == id_ & dip$periodo == p$periodo][1], na.rm = TRUE)
  fechas <- sort(unique(c(p$inicio + 1,
                          m$mil_inicio[m$mil_inicio > p$inicio + 1 & m$mil_inicio <= hasta],
                          (m$mil_termino + 1)[!is.na(m$mil_termino) & m$mil_termino >= p$inicio + 1 & m$mil_termino < hasta])))
  et <- vapply(fechas, function(f) {
    x <- m |> filter(mil_inicio <= f, is.na(mil_termino) | mil_termino >= f) |> arrange(desc(mil_inicio))
    if (nrow(x)) x$partido[1] else "IND" }, "")
  et <- fusionar(et, p$periodo)
  et <- et[c(TRUE, et[-1] != et[-length(et)])]          # colapsa repeticiones
  tibble(fecha = fechas[seq_along(et)], etiqueta = et)
}

cambios_camara <- PERIODOS |> rowwise() |> group_map(function(p, ...) {
  ids <- dip$id[dip$periodo == p$periodo & !is.na(dip$candidato)]
  map_dfr(ids, function(i) {
    s <- secuencia(i, p)
    if (nrow(s) < 2) return(NULL)
    tibble(id = i, de = head(s$etiqueta, -1), a = s$etiqueta[-1])
  }) |> mutate(periodo = p$periodo)
}) |> bind_rows() |>
  left_join(distinct(dip, periodo, id, nombre), by = c("periodo", "id"))
escribir(cambios_camara, "04_cambios_camara_detalle.csv")

# ---------------------------------------------------------------------------------
# Senado
# ---------------------------------------------------------------------------------
sen <- leer_csv2(file.path(DIR_FUE, "senadores_periodos.csv")) |>
  mutate(fecha_asume = as.Date(fecha_asume))
cam_sen <- leer_csv2(file.path(DIR_FUE, "cambios_militancia_senado.csv")) |> mutate(fecha = as.Date(fecha))
# Fecha de salida de los senadores reemplazados
salidas <- sen |> filter(!is.na(reemplaza_a)) |> transmute(periodo, senador = reemplaza_a, salida = fecha_asume)
sen <- sen |> left_join(salidas, by = c("periodo", "senador")) |> inner_join(PERIODOS, by = "periodo")

# Etiqueta partidaria: militancia (los independientes en cupo de un partido cuentan como IND)
base_mil <- function(x) if_else(str_starts(x, "IND"), "IND", x)
etiqueta_sen <- function(p, fecha) {
  s <- sen |> filter(periodo == p, (is.na(fecha_asume) | fecha_asume <= fecha), (is.na(salida) | salida > fecha))
  ultimo <- cam_sen |> filter(fecha <= !!fecha) |> arrange(fecha) |>
    group_by(senador) |> summarise(a = last(a), .groups = "drop")
  s |> left_join(ultimo, by = "senador") |>
    transmute(id = senador, partido = base_mil(coalesce(a, partido_inicio)))
}
comp_senado <- PERIODOS |> rowwise() |> group_map(function(p, ...) bind_rows(
  etiqueta_sen(p$periodo, p$inicio + 1) |> mutate(momento = "inicio"),
  etiqueta_sen(p$periodo, p$fin - 1) |> mutate(momento = "termino")) |>
    mutate(periodo = p$periodo, camara = "Senado")) |> bind_rows()

# ---------------------------------------------------------------------------------
# Resumen por período
# ---------------------------------------------------------------------------------
comp <- bind_rows(comp_camara, comp_senado)
resumen <- comp |> group_by(camara, periodo, momento) |>
  summarise(miembros = n(), independientes = sum(partido == "IND"),
            partidos = n_distinct(partido[partido != "IND"]),
            nep = nep_comp(partido, id), .groups = "drop") |>
  pivot_wider(names_from = momento, values_from = c(miembros, independientes, partidos, nep))

eventos <- bind_rows(
  cambios_camara |> mutate(camara = "Diputados",
                            tipo = case_when(a == "IND" ~ "salida", de == "IND" ~ "ingreso", TRUE ~ "cambio")) |>
    count(camara, periodo, tipo),
  cam_sen |> filter(tipo != "fusión de partidos") |>
    mutate(camara = "Senado", tipo = case_when(a == "IND" ~ "salida", de == "IND" ~ "ingreso", TRUE ~ "cambio")) |>
    count(camara, periodo, tipo)) |>
  pivot_wider(names_from = tipo, values_from = n, values_fill = 0)

# Legisladores que cambiaron de militancia al menos una vez (sin fusiones)
cambiaron <- bind_rows(
  cambios_camara |> distinct(periodo, id) |> count(periodo, name = "legisladores_que_cambian") |> mutate(camara = "Diputados"),
  cam_sen |> filter(tipo != "fusión de partidos") |> distinct(periodo, senador) |>
    count(periodo, name = "legisladores_que_cambian") |> mutate(camara = "Senado"))

resumen <- resumen |> left_join(eventos, by = c("camara", "periodo")) |>
  left_join(cambiaron, by = c("camara", "periodo")) |>
  mutate(across(c(salida, ingreso, cambio, legisladores_que_cambian), \(x) coalesce(x, 0L)),
         pct_cambian = legisladores_que_cambian / miembros_inicio,
         delta_nep = nep_termino - nep_inicio)
print(as.data.frame(resumen |> mutate(across(where(is.double), \(x) round(x, 2)))))
escribir(resumen, "04_legislatura_resumen.csv")
escribir(comp, "04_composicion_inicio_termino.csv")
saveRDS(list(comp = comp, resumen = resumen, cambios_camara = cambios_camara, cobertura = cobertura,
             concordancia = concord), file.path(DIR_PROC, "legislatura.rds"))
