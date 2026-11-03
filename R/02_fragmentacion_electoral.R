# =============================================================================
# 02_fragmentacion_electoral.R
# -----------------------------------------------------------------------------
# Fragmentación electoral y parlamentaria por elección, 2009–2025.
#   * Número efectivo de partidos en votos (NEPv) y en escaños (NEPe), nacional.
#   * NEPv promedio por distrito o circunscripción (fragmentación local).
#   * Número efectivo de listas en votos (coaliciones).
#   * Desproporcionalidad de Gallagher.
# Unidad partidaria: el partido del candidato o del cupo que ocupa (sigla).
#
# Entradas  data/processed/candidatos.rds
# Salidas   output/tablas/02_*.csv, data/processed/fragmentacion.rds
# =============================================================================

source(file.path("R", "00_funciones.R"))
res <- readRDS(file.path(DIR_PROC, "candidatos.rds"))
cand <- res |> filter(tipo == "candidato")

nacional <- cand |>
  group_by(anio, camara, sigla) |>
  summarise(votos = sum(votos), escanos = sum(electo), .groups = "drop") |>
  group_by(anio, camara) |>
  summarise(partidos_con_votos = n_distinct(sigla[!str_starts(sigla, "IND-")]),
            partidos_con_escanos = n_distinct(sigla[escanos > 0 & !str_starts(sigla, "IND-")]),
            nep_votos = nep(votos), nep_escanos = nep(escanos),
            gallagher = gallagher(votos, escanos),
            escanos = sum(escanos), .groups = "drop")

listas <- cand |>
  group_by(anio, camara, pacto) |>
  summarise(votos = sum(votos), .groups = "drop") |>
  group_by(anio, camara) |>
  summarise(listas = n(), nel_votos = nep(votos), .groups = "drop")

local <- cand |>
  group_by(anio, camara, unidad, sigla) |>
  summarise(votos = sum(votos), .groups = "drop") |>
  group_by(anio, camara, unidad) |>
  summarise(nep_local = nep(votos), .groups = "drop") |>
  group_by(anio, camara) |>
  summarise(unidades = n(), nep_local_media = mean(nep_local), .groups = "drop")

participacion <- res |>
  group_by(anio, camara) |>
  summarise(validos = sum(votos[tipo == "candidato"]),
            blancos_nulos = sum(votos[tipo != "candidato"]), .groups = "drop") |>
  mutate(pct_blancos_nulos = blancos_nulos / (validos + blancos_nulos))

frag <- nacional |> left_join(listas, by = c("anio", "camara")) |>
  left_join(local, by = c("anio", "camara")) |>
  left_join(participacion, by = c("anio", "camara")) |>
  mutate(regla = if_else(anio <= 2013, "Binominal", "D'Hondt"))

print(as.data.frame(frag |> select(anio, camara, nep_votos, nep_escanos, gallagher, nel_votos,
                                   nep_local_media, partidos_con_escanos) |>
                      mutate(across(where(is.double), \(x) round(x, 2)))))
saveRDS(frag, file.path(DIR_PROC, "fragmentacion.rds"))
escribir(frag, "02_fragmentacion_electoral.csv")

# Composición de la Cámara por partido según la elección (para las figuras)
comp <- cand |> filter(electo) |> count(anio, camara, sigla, name = "escanos")
escribir(comp, "02_escanos_por_partido.csv")

# Sensibilidad: clasificación de los independientes ----------------------------------
# A (principal): partido del cupo; sin cupo identificable, agrupados por lista
# B: cada independiente como unidad individual (todas las elecciones)
# C: todos los independientes de una lista asignados a una sola unidad por lista
alt <- cand |> mutate(
  sigla_B = if_else(condicion == "independiente", paste0("IND-", candidato), sigla),
  sigla_C = if_else(condicion == "independiente", paste0("IND-", lista), sigla))
sens <- bind_rows(
  alt |> group_by(anio, camara, s = sigla) |> summarise(v = sum(votos), e = sum(electo), .groups = "drop") |> mutate(criterio = "A"),
  alt |> group_by(anio, camara, s = sigla_B) |> summarise(v = sum(votos), e = sum(electo), .groups = "drop") |> mutate(criterio = "B"),
  alt |> group_by(anio, camara, s = sigla_C) |> summarise(v = sum(votos), e = sum(electo), .groups = "drop") |> mutate(criterio = "C")) |>
  group_by(criterio, anio, camara) |> summarise(nep_votos = nep(v), nep_escanos = nep(e), .groups = "drop") |>
  pivot_wider(names_from = criterio, values_from = c(nep_votos, nep_escanos))
print(as.data.frame(sens |> mutate(across(where(is.double), \(x) round(x, 2)))))
escribir(sens, "02_sensibilidad_independientes.csv")

# Oferta electoral: candidatos y listas por distrito ----------------------------------
oferta <- cand |> group_by(anio, camara, unidad) |>
  summarise(candidatos = n(), escanos = sum(electo), listas = n_distinct(pacto),
            listas_efectivas = nep(tapply(votos, pacto, sum)), .groups = "drop") |>
  group_by(anio, camara) |>
  summarise(candidatos = sum(candidatos), candidatos_por_escano = sum(candidatos) / sum(escanos),
            listas_por_distrito = mean(listas), listas_efectivas_distrito = mean(listas_efectivas),
            .groups = "drop")
print(as.data.frame(oferta |> mutate(across(where(is.double), \(x) round(x, 2)))))
escribir(oferta, "02_oferta.csv")
saveRDS(list(sensibilidad = sens, oferta = oferta), file.path(DIR_PROC, "sensibilidad_oferta.rds"))
