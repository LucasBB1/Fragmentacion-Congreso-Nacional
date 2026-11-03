# =============================================================================
# 05_figuras.R — figuras del documento de trabajo
# Entradas  data/processed/fragmentacion.rds, contrafactual.rds, legislatura.rds
# Salidas   output/figuras/fig1–fig4 (.png)
# =============================================================================

source(file.path("R", "00_funciones.R"))
frag <- readRDS(file.path(DIR_PROC, "fragmentacion.rds"))
cf   <- readRDS(file.path(DIR_PROC, "contrafactual.rds"))$comparacion
leg  <- readRDS(file.path(DIR_PROC, "legislatura.rds"))$resumen

AZUL <- "#20587F"; CELESTE <- "#9DB9D3"; ROJO <- "#B5543C"; GRIS <- "#8C8C8C"
tema <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title = element_blank(), plot.caption = element_blank())
guardar <- function(g, nombre, w = 8, h = 5) ggsave(file.path(DIR_FIG, nombre), g, width = w, height = h, dpi = 300, bg = "white")
coma <- function(x) format(round(x, 1), decimal.mark = ",", nsmall = 1)

# Figura 1: NEP en votos y en escaños, Cámara de Diputados ------------------------------
d1 <- frag |> filter(camara == "Diputados") |>
  select(anio, `En votos` = nep_votos, `En escaños` = nep_escanos) |>
  pivot_longer(-anio, names_to = "medida", values_to = "nep")
g1 <- ggplot(d1, aes(anio, nep, color = medida)) +
  annotate("rect", xmin = 2015, xmax = 2025.8, ymin = -Inf, ymax = Inf, fill = "#F2F2F2") +
  annotate("text", x = 2015.3, y = 16.3, label = "Regla D'Hondt (Ley 20.840)", hjust = 0, size = 3.3, color = "grey30") +
  geom_line(linewidth = 1) + geom_point(size = 2.6) +
  geom_text(aes(label = coma(nep)), vjust = -1, size = 3.2, show.legend = FALSE) +
  scale_color_manual(values = c(`En votos` = ROJO, `En escaños` = AZUL), name = NULL) +
  scale_x_continuous(breaks = c(2009, 2013, 2017, 2021, 2025)) +
  scale_y_continuous(limits = c(0, 17), labels = scales::label_number(decimal.mark = ",")) +
  labs(x = NULL, y = "Número efectivo de partidos") + tema
guardar(g1, "fig1_nep_camara.png")

# Figura 2: contrafactual y descomposición de la reforma ------------------------------
desc <- readRDS(file.path(DIR_PROC, "contrafactual.rds"))$descomposicion
d2 <- desc |> mutate(escenario = factor(escenario, levels = sort(unique(escenario)),
                                        labels = c("Binominal (60 distritos, 120 escaños)",
                                                   "+ Distritos nuevos (28, 120 escaños)",
                                                   "+ Reparto entre partidos de la lista",
                                                   "+ 155 escaños (regla vigente)")))
g2 <- ggplot(d2, aes(factor(anio), nep, fill = escenario)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = coma(nep)), position = position_dodge(width = 0.85), vjust = -0.4, size = 2.6) +
  scale_fill_manual(values = c("#D6E2EC", CELESTE, "#5B86A8", AZUL), name = NULL) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "Votos de la elección", y = "Número efectivo de partidos en escaños") + tema
guardar(g2, "fig2_contrafactual.png", h = 5.4)

# Figura 3: legisladores que cambian de militancia -------------------------------------
d3 <- leg |> mutate(camara = recode(camara, Diputados = "Cámara de Diputados"))
g3 <- ggplot(d3, aes(periodo, 100 * pct_cambian, fill = camara)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = round(100 * pct_cambian)), position = position_dodge(width = 0.75), vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c(`Cámara de Diputados` = AZUL, Senado = CELESTE), name = NULL) +
  labs(x = NULL, y = "Porcentaje de legisladores que cambian de militancia") + tema
guardar(g3, "fig3_cambios_militancia.png")

# Figura 4: NEP al inicio y al término de cada período ---------------------------------
d4 <- leg |> select(camara, periodo, Inicio = nep_inicio, `Término` = nep_termino) |>
  pivot_longer(c(Inicio, `Término`), names_to = "momento", values_to = "nep") |>
  mutate(camara = recode(camara, Diputados = "Cámara de Diputados"))
g4 <- ggplot(d4, aes(nep, periodo)) +
  geom_line(aes(group = periodo), color = "#C8C8C8", linewidth = 1) +
  geom_point(aes(color = momento), size = 3) +
  facet_wrap(~ camara, scales = "free_x") +
  scale_color_manual(values = c(Inicio = CELESTE, `Término` = AZUL), name = NULL) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",", accuracy = 0.1)) +
  labs(x = "Número efectivo de partidos (independientes como unidades individuales)", y = NULL) + tema
guardar(g4, "fig4_inicio_termino.png", h = 4.2)
message("Figuras en output/figuras")
