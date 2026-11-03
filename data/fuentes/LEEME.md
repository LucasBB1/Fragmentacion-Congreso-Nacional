# data/fuentes/

| Archivo | Contenido | Fuente |
|---|---|---|
| `resultados_2009_diputados.csv`, `resultados_2009_senadores.csv` | Votos por candidato, pacto, partido y si resultó electo, elecciones de 2009 | Transcritos del sitio histórico del SERVEL (https://historial.servel.cl/SitioHistorico/index2009_dipu.htm y index2009_sena.htm). Reproducen la votación válida nacional (6.615.856 votos), la votación de cada pacto y los 120 diputados y 18 senadores electos |
| `pactos_2009.csv` | Nombre de los pactos de 2009 | SERVEL, sitio histórico |
| `partidos_homologacion.csv` | Correspondencia entre los nombres de partido de los archivos del SERVEL y las siglas usadas en el análisis; condición de militante o independiente | Elaboración propia |
| `militancias_diputados_2010_2030.csv` | Militancias de cada diputado de los períodos 2010–2014 a 2026–2030, con fecha de inicio y de término | Cámara de Diputadas y Diputados, datos abiertos (https://opendata.camara.cl/), servicio `retornarDiputadosXPeriodo`, consultado el 2 de noviembre de 2026. El campo `dp_fecha1` corresponde a la fecha en que el diputado dejó el cargo o al término del período |
| `correcciones_militancia_camara.csv` | Historial de militancia 2010–2018 de seis diputados cuyo registro fija los cambios en los límites del período legislativo | Reseñas parlamentarias de la BCN (https://www.bcn.cl/historiapolitica/resenas_parlamentarias/) |
| `senadores_periodos.csv` | Senadores en ejercicio en cada período 2010–2026, partido o condición al inicio y reemplazos | Reseñas parlamentarias de la BCN y resultados del SERVEL |
| `cambios_militancia_senado.csv` | Cambios de militancia de los senadores durante cada período, con fecha y fuente | Reseñas parlamentarias y datos abiertos de la BCN; prensa (ver la columna `fuente`) |
