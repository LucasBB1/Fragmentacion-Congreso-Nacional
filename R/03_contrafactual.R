# =============================================================================
# 03_contrafactual.R
# -----------------------------------------------------------------------------
# Separa el efecto mecánico de la reforma de 2015 (Ley 20.840) del cambio en
# el comportamiento de los votantes, en la elección de diputados.
#   A. Votos de 2009 y 2013 asignados con la regla nueva: 28 distritos (cada
#      distrito antiguo está contenido en uno nuevo) y las magnitudes de 2017.
#   B. Votos de 2017, 2021 y 2025 asignados con el binominal: 60 distritos
#      antiguos (reconstruidos desde las comunas) y dos escaños por distrito.
# En ambos casos los escaños se reparten entre listas con D'Hondt y, dentro de
# cada lista, a los candidatos más votados. Los votos se mantienen fijos: el
# contrafactual mide solo la conversión de votos en escaños, no las respuestas
# estratégicas de partidos y electores a la nueva regla.
#
# Entradas  data/processed/candidatos.rds, candidatos_comuna.rds
# Salidas   output/tablas/03_*.csv, data/processed/contrafactual.rds
# =============================================================================

source(file.path("R", "00_funciones.R"))
res <- readRDS(file.path(DIR_PROC, "candidatos.rds")) |> filter(camara == "Diputados", tipo == "candidato")
rc  <- readRDS(file.path(DIR_PROC, "candidatos_comuna.rds")) |> filter(camara == "Diputados", tipo == "candidato") |>
  mutate(sigla = case_when(sigla == "IND-LISTA" ~ paste0("IND-", lista),     # mismas reglas que el script 01
                           sigla == "IND" ~ paste0("IND-", candidato), TRUE ~ sigla))

# Identificador de lista: los independientes fuera de pacto compiten cada uno como lista propia
id_lista <- function(d) d |>
  mutate(fuera = str_detect(toupper(coalesce(pacto, "")), "FUERA DE PACTO|^INDEPENDIENTES?$|^CANDIDATURA INDEPENDIENTE") |
           (anio == 2009 & lista == "IND"),
         id_lista = if_else(fuera, paste("IND", candidato), lista))

sigla_cand <- res |> distinct(anio, candidato, sigla)

# Asignación: votos por candidato en una unidad -> candidatos electos.
# D'Hondt entre listas; con la regla nueva (dos_niveles = TRUE), D'Hondt entre los
# partidos de cada lista (Ley 18.700, art. 121) y luego a los más votados de cada
# partido. Con el binominal, a los más votados de la lista. Si un grupo obtiene más
# escaños que candidatos, el excedente pasa al siguiente cociente.
dhondt_tope <- function(votos, m, tope) {
  esc <- integer(length(votos)); if (m <= 0) return(esc)
  for (i in seq_len(m)) {
    q <- votos / (esc + 1); q[esc >= tope] <- -Inf
    if (all(!is.finite(q))) break
    j <- which.max(q); esc[j] <- esc[j] + 1L
  }
  esc
}
asignar <- function(d, m, dos_niveles) {
  d <- d |> mutate(grupo = if (dos_niveles) paste(id_lista, sigla) else id_lista)
  lv <- d |> group_by(id_lista) |> summarise(votos = sum(votos), n = n(), .groups = "drop")
  lv$esc_lista <- dhondt_tope(lv$votos, m, lv$n)
  gv <- d |> group_by(id_lista, grupo) |> summarise(votos = sum(votos), n = n(), .groups = "drop") |>
    left_join(select(lv, id_lista, esc_lista), by = "id_lista") |>
    group_by(id_lista) |> mutate(esc = dhondt_tope(votos, esc_lista[1], n)) |> ungroup()
  d |> left_join(select(gv, grupo, esc), by = "grupo") |>
    group_by(grupo) |> arrange(desc(votos), .by_group = TRUE) |>
    mutate(electo_sim = row_number() <= esc) |> ungroup() |> select(-grupo, -esc)
}
simular <- function(d, magnitudes, dos_niveles) {
  d |> left_join(magnitudes, by = "distrito") |>
    group_by(anio, distrito) |>
    group_modify(~ asignar(.x, .x$m[1], dos_niveles)) |> ungroup()
}

# Correspondencia comuna -> distrito antiguo (2013) y nuevo (2017)
map_old <- rc |> filter(anio == 2013) |> distinct(comuna, d_old = unidad)
map_new <- rc |> filter(anio == 2017) |> distinct(comuna, d_new = unidad)
old_new <- inner_join(map_old, map_new, by = "comuna") |> distinct(d_old, d_new)
stopifnot(!any(duplicated(old_new$d_old)))   # cada distrito antiguo cae en un solo distrito nuevo

mag_new <- res |> filter(anio == 2017) |> group_by(distrito = unidad) |> summarise(m = sum(electo))
mag_old <- tibble(distrito = unique(old_new$d_old), m = 2L)

# Validación: la regla real reproduce los electos reales -------------------------------
val <- res |> id_lista() |> rename(distrito = unidad) |>
  group_by(anio) |> group_modify(function(d, k) {
    mags <- d |> group_by(distrito) |> summarise(m = sum(electo))
    simular(mutate(d, anio = k$anio), mags, dos_niveles = k$anio >= 2017) |> select(-anio)
  }) |> ungroup()
validacion <- val |> group_by(anio) |>
  summarise(electos = sum(electo), coinciden = sum(electo & electo_sim), .groups = "drop")
print(as.data.frame(validacion)); escribir(validacion, "03_validacion.csv")

# A. Votos 2009 y 2013 con la regla nueva --------------------------------------------
a <- res |> filter(anio %in% c(2009L, 2013L)) |> id_lista() |>
  inner_join(old_new, by = c("unidad" = "d_old")) |> rename(distrito = d_new) |>
  simular(mag_new, dos_niveles = TRUE)

# B. Votos 2017–2025 con el binominal ---------------------------------------------------
b <- rc |> filter(anio >= 2017L) |> id_lista() |>
  inner_join(map_old, by = "comuna") |>
  group_by(anio, distrito = d_old, id_lista, candidato, sigla) |>
  summarise(votos = sum(votos), .groups = "drop") |>
  simular(mag_old, dos_niveles = FALSE)

nep_sim <- function(d) d |>
  group_by(anio, sigla) |> summarise(votos = sum(votos), esc = sum(electo_sim), .groups = "drop") |>
  group_by(anio) |> summarise(nep_escanos_cf = nep(esc), escanos_cf = sum(esc),
                              gallagher_cf = gallagher(votos, esc),
                              partidos_con_escanos_cf = n_distinct(sigla[esc > 0 & !str_starts(sigla, "IND-")]),
                              .groups = "drop")
cf <- bind_rows(nep_sim(a) |> mutate(regla_cf = "D'Hondt (28 distritos)"),
                nep_sim(b) |> mutate(regla_cf = "Binominal (60 distritos)"))

frag <- readRDS(file.path(DIR_PROC, "fragmentacion.rds")) |> filter(camara == "Diputados")
comparacion <- frag |> select(anio, regla, nep_votos, nep_escanos, gallagher, partidos_con_escanos) |>
  left_join(cf, by = "anio") |>
  mutate(efecto_mecanico = if_else(regla == "Binominal", nep_escanos_cf - nep_escanos, nep_escanos - nep_escanos_cf))
print(as.data.frame(comparacion |> mutate(across(where(is.double), \(x) round(x, 2)))))
escribir(comparacion, "03_contrafactual.csv")
saveRDS(list(a = a, b = b, comparacion = comparacion), file.path(DIR_PROC, "contrafactual.rds"))

# C. Descomposición de la reforma -----------------------------------------------------
# El binominal es D'Hondt con dos escaños por distrito, de modo que la reforma no cambió
# la fórmula sino tres componentes, que se agregan de a uno con los votos de cada elección:
#   E0  Binominal: 60 distritos, 2 escaños, reparto a los más votados de la lista (120)
#   E1  Magnitud: 28 distritos nuevos con 120 escaños (magnitudes de 2017 a escala), un nivel
#   E2  + Reparto entre partidos de la lista (D'Hondt en dos niveles), 120 escaños
#   E3  + Tamaño: 155 escaños (regla vigente desde 2017)
escala <- function(m, total) {   # restos mayores, con mínimo de 2 escaños por distrito
  q <- m * total / sum(m); e <- pmax(2L, floor(q))
  while (sum(e) < total) { r <- q - e; r[e < 2] <- -Inf; i <- which.max(r); e[i] <- e[i] + 1L }
  while (sum(e) > total) { i <- which.max(e - q); e[i] <- e[i] - 1L }
  e
}
mag_new120 <- mag_new |> mutate(m = escala(m, 120L))
stopifnot(sum(mag_new120$m) == 120)

votos_nuevos <- bind_rows(
  res |> filter(anio <= 2013L) |> id_lista() |> inner_join(old_new, by = c("unidad" = "d_old")) |>
    transmute(anio, distrito = d_new, id_lista, candidato, sigla, votos),
  res |> filter(anio >= 2017L) |> id_lista() |> transmute(anio, distrito = unidad, id_lista, candidato, sigla, votos))
votos_antiguos <- bind_rows(
  res |> filter(anio <= 2013L) |> id_lista() |> transmute(anio, distrito = unidad, id_lista, candidato, sigla, votos),
  rc |> filter(anio >= 2017L) |> id_lista() |> inner_join(map_old, by = "comuna") |>
    group_by(anio, distrito = d_old, id_lista, candidato, sigla) |> summarise(votos = sum(votos), .groups = "drop"))

escenarios <- bind_rows(
  nep_sim(simular(votos_antiguos, mag_old, FALSE)) |> mutate(escenario = "E0 Binominal"),
  nep_sim(simular(votos_nuevos, mag_new120, FALSE)) |> mutate(escenario = "E1 Magnitud"),
  nep_sim(simular(votos_nuevos, mag_new120, TRUE)) |> mutate(escenario = "E2 Reparto entre partidos"),
  nep_sim(simular(votos_nuevos, mag_new, TRUE)) |> mutate(escenario = "E3 Tamaño (regla vigente)"))
descomp <- escenarios |> select(anio, escenario, nep = nep_escanos_cf, partidos = partidos_con_escanos_cf) |>
  arrange(anio, escenario)
print(as.data.frame(descomp |> mutate(nep = round(nep, 2))))
escribir(descomp, "03_descomposicion.csv")
saveRDS(list(a = a, b = b, comparacion = comparacion, descomposicion = descomp), file.path(DIR_PROC, "contrafactual.rds"))
