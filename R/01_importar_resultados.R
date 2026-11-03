# =============================================================================
# 01_importar_resultados.R
# -----------------------------------------------------------------------------
# Importa los resultados por candidato de las elecciones de diputados y
# senadores de 2009 a 2025.
#   * 2009: transcritos desde el sitio histórico del SERVEL (data/fuentes).
#   * 2013–2025: hoja "Votación por comuna" de los archivos del SERVEL
#     (data/raw), agregada por candidato y distrito o circunscripción.
# Homologa las etiquetas partidarias con data/fuentes/partidos_homologacion.csv.
#
# Salidas  data/processed/candidatos.rds, output/tablas/01_*.csv
# =============================================================================

source(file.path("R", "00_funciones.R"))

# --- 2009 ---------------------------------------------------------------------
pactos09 <- leer_csv2(file.path(DIR_FUE, "pactos_2009.csv"))
r09 <- bind_rows(
  leer_csv2(file.path(DIR_FUE, "resultados_2009_diputados.csv")) |>
    transmute(camara = "Diputados", unidad = paste("DISTRITO", distrito), pacto, candidato, partido, votos, electo = electo == 1),
  leer_csv2(file.path(DIR_FUE, "resultados_2009_senadores.csv")) |>
    transmute(camara = "Senado", unidad = paste("CIRCUNSCRIPCION SENATORIAL", circunscripcion), pacto, candidato, partido, votos, electo = electo == 1)) |>
  left_join(rename(pactos09, nombre_pacto = nombre), by = "pacto") |>
  transmute(anio = 2009L, camara, unidad, lista = pacto,
            pacto = nombre_pacto, partido_raw = partido, candidato = normalizar(candidato),
            votos = as.numeric(votos), electo, tipo = "candidato")

# --- 2013–2025 ----------------------------------------------------------------
ARCHIVOS <- tribble(
  ~anio, ~camara,     ~patron,
  2013L, "Diputados", "2013_11_Diputados",
  2013L, "Senado",    "2013_11_Senatorial",
  2017L, "Diputados", "2017_11_Diputados",
  2017L, "Senado",    "2017_11_Senatorial",
  2021L, "Diputados", "2021_11_Diputados",
  2021L, "Senado",    "2021_11_Senadores",
  2025L, "Diputados", "2025_11_Diputados",
  2025L, "Senado",    "2025_11_Senadores")

leer <- function(anio, camara, patron) {
  f <- list.files(DIR_RAW, pattern = paste0("^", patron, ".*\\.xlsx$"), full.names = TRUE, recursive = TRUE)[1]
  if (is.na(f)) stop("Falta el archivo ", patron, " en data/raw/")
  message("Leyendo ", basename(f))
  d <- read_excel(f, sheet = "Votación por comuna", skip = 6, col_types = "text")
  d <- d |> filter(!is.na(`Nro.Región`), !is.na(Votos))
  unidad <- if (camara == "Diputados") d$Distrito else d$`Circunscripción senatorial`
  d |>
    mutate(unidad = toupper(str_squish(unidad)),
           candidato = normalizar(paste(Nombres, `Primer apellido`, `Segundo apellido`)),
           tipo = case_when(str_detect(candidato, "^VOTOS EN BLANCO") ~ "blanco",
                            str_detect(candidato, "^VOTOS NULOS") ~ "nulo",
                            TRUE ~ "candidato")) |>
    group_by(unidad, comuna = normalizar(Comuna), lista = Lista, pacto = Pacto,
             partido_raw = str_squish(Partido), candidato, tipo) |>
    summarise(votos = sum(as.numeric(Votos)), electo = any(!is.na(Cargo)), .groups = "drop") |>
    mutate(anio = anio, camara = camara)
}

# Votos por candidato y comuna (2013–2025); se usan para los contrafactuales del script 03
r_comuna <- pmap_dfr(ARCHIVOS, leer) |>
  mutate(partido_raw = toupper(stringi::stri_trans_general(coalesce(partido_raw, ""), "Latin-ASCII")),
         partido_raw = str_squish(partido_raw))

r13 <- r_comuna |>
  group_by(anio, camara, unidad, lista, pacto, partido_raw, candidato, tipo) |>
  summarise(votos = sum(votos), electo = any(electo), .groups = "drop")

res <- bind_rows(r09, r13) |>
  mutate(partido_raw = toupper(stringi::stri_trans_general(coalesce(partido_raw, ""), "Latin-ASCII")),
         partido_raw = str_squish(partido_raw))

# --- Homologación de etiquetas -----------------------------------------------------
# sigla: partido del candidato o del cupo que ocupa; condicion: militante o independiente
hom <- leer_csv2(file.path(DIR_FUE, "partidos_homologacion.csv"))
faltan <- setdiff(unique(res$partido_raw[res$tipo == "candidato"]), hom$partido_raw)
if (length(faltan)) stop("Partidos sin homologar: ", paste(faltan, collapse = " | "))

res <- res |> left_join(hom, by = "partido_raw") |>
  mutate(
    # Independientes sin partido de cupo identificable: una unidad por lista y unidad territorial
    sigla = if_else(tipo == "candidato" & sigla == "IND-LISTA", paste0("IND-", lista), sigla),
    sigla = if_else(tipo == "candidato" & sigla == "IND", paste0("IND-", candidato), sigla))

# Validación: escaños por elección y cámara
escanos <- res |> filter(electo) |> count(anio, camara, name = "electos")
print(as.data.frame(escanos))
esperado <- tribble(~anio, ~camara, ~esperado,
  2009L, "Diputados", 120L, 2009L, "Senado", 18L, 2013L, "Diputados", 120L, 2013L, "Senado", 20L,
  2017L, "Diputados", 155L, 2017L, "Senado", 23L, 2021L, "Diputados", 155L, 2021L, "Senado", 27L,
  2025L, "Diputados", 155L, 2025L, "Senado", 23L)
chk <- left_join(esperado, escanos, by = c("anio", "camara"))
stopifnot(all(chk$electos == chk$esperado))

saveRDS(res, file.path(DIR_PROC, "candidatos.rds"))
saveRDS(r_comuna |> left_join(hom, by = "partido_raw"), file.path(DIR_PROC, "candidatos_comuna.rds"))
escribir(res |> filter(tipo == "candidato") |> count(anio, camara, sigla, wt = votos, name = "votos") |>
           arrange(anio, camara, desc(votos)), "01_votos_por_sigla.csv")
message("Candidatos importados: ", sum(res$tipo == "candidato"))
