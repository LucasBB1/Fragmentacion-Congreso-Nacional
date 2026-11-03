# Más allá de la regla: fragmentación electoral y parlamentaria en Chile tras el fin del sistema binominal, 2009–2026

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23924439.svg)](https://doi.org/10.5281/zenodo.23924439)

Código y datos del **Documento de trabajo N.° 4** de Lucas Bottinelli Baltra. El estudio pregunta cuánto del aumento de la fragmentación del Congreso chileno se debe a la reforma electoral de 2015 (Ley N.° 20.840) y cuánto a la fragmentación del voto, y si la fragmentación continúa durante la legislatura. Usa los resultados por candidato de las elecciones de diputados y senadores de 2009 a 2025, el registro de militancias de la Cámara y una reconstrucción de la militancia de los senadores.

**Autor:** Lucas Bottinelli Baltra · [lucasbb1.github.io](https://lucasbb1.github.io) · ORCID [0009-0000-4865-7261](https://orcid.org/0009-0000-4865-7261)

El documento está en `documento/` (Word y PDF) y en Zenodo: https://doi.org/10.5281/zenodo.23924456

## Resultados principales

- El número efectivo de partidos en votos de la Cámara pasó de 7,3 en 2009 a 15,4 en 2021 y 14,2 en 2025.
- Con los votos de 2009 y 2013, la regla D'Hondt habría producido casi la misma Cámara que el binominal (+0,2 y −0,1 en el número efectivo de partidos en escaños).
- Con los votos de 2021, el binominal habría reducido el número efectivo de partidos en escaños de 11,6 a 8,5, pero aun así habría llevado a 17 partidos a la Cámara, frente a 8 en 2009 y 2013.
- Casi todo el efecto mecánico de la reforma proviene de la mayor magnitud de los distritos; el aumento a 155 escaños y el reparto entre los partidos de cada lista pesan poco.
- La proporción de diputados que cambió de militancia durante su período pasó de 3% en 2010–2014 a 26% en 2022–2026, y la de senadores, de 5% a 20%.

## Scripts

Correr en orden desde `Fragmentacion-Congreso-Nacional.Rproj`.

| Script | Qué hace |
|---|---|
| `00_funciones.R` | Paquetes, rutas y funciones comunes (número efectivo de partidos, índice de Gallagher, D'Hondt) |
| `01_importar_resultados.R` | Resultados por candidato de 2009 (transcritos) y 2013–2025 (SERVEL); homologación de partidos; validación de electos |
| `02_fragmentacion_electoral.R` | Fragmentación electoral y parlamentaria por elección; sensibilidad a la clasificación de independientes; oferta electoral |
| `03_contrafactual.R` | Validación de la asignación (670 de 670 electos), cada regla aplicada a los votos de la otra época y descomposición de la reforma |
| `04_legislatura.R` | Cambios de militancia y composición al inicio y al término de cada período, Cámara y Senado |
| `05_figuras.R` | Figuras del documento |

Paquetes: `dplyr`, `tidyr`, `readr`, `purrr`, `stringr`, `ggplot2`, `readxl`, `stringi`, `scales`.

## Datos

- `data/raw/`: archivos originales del SERVEL de 2013 a 2025 (no se versionan; ver `data/raw/LEEME.md`).
- `data/fuentes/`: resultados de 2009, registro de militancias de la Cámara y sus correcciones, nómina y cambios de militancia del Senado, y homologación de partidos (ver `data/fuentes/LEEME.md`).

## Cómo citar

Bottinelli Baltra, L. (2026). *Más allá de la regla: fragmentación electoral y parlamentaria en Chile tras el fin del sistema binominal, 2009–2026* (Documento de trabajo N.° 4). https://doi.org/10.5281/zenodo.23924456

Código y datos: Bottinelli Baltra, L. (2026). *Fragmentacion-Congreso-Nacional* (Versión 1.0.0) [Software]. Zenodo. https://doi.org/10.5281/zenodo.23924439

## Licencias

Código: MIT (`LICENSE`). Datos procesados, tablas, figuras y texto: CC BY 4.0 (`LICENSE-DATOS.md`).
