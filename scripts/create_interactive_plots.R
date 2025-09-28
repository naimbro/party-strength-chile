# Análisis de Fortaleza Institucional Partidaria - Chile
# Script para generar visualizaciones interactivas
# 
# Este script genera dos gráficos interactivos:
# 1. Índice de Fortaleza Institucional Partidaria (PISI) 2000-2024
# 2. Diagrama de flujos de alcaldes 2021-2024
#
# Autor: Elaboración propia
# Datos: SERVEL (Servicio Electoral de Chile)

# ====================================================================
# CONFIGURACIÓN INICIAL
# ====================================================================

cat("=== ANÁLISIS DE FORTALEZA INSTITUCIONAL PARTIDARIA ===\n")
cat("Generando visualizaciones interactivas...\n\n")

# Cargar paquetes requeridos
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, plotly, htmlwidgets, rio, scales, jsonlite)

# Cargar datos
cat("Cargando datos de análisis...\n")
analysis_data <- readRDS("data/party_strength_analysis_data.rds")

# Extraer objetos de datos
pisi_scores <- analysis_data$pisi_scores
municipal_data <- analysis_data$municipal_data

# Crear directorio de salida
dir.create("outputs", showWarnings = FALSE)

# Paleta de colores
party_colors <- c(
  "Militante" = "#1f77b4",
  "Independiente cupo partido" = "#ff7f0e",
  "Independiente fuera de pacto" = "#2ca02c"
)

# ====================================================================
# GRÁFICO 1: ÍNDICE PISI
# ====================================================================

cat("Creando gráfico PISI...\n")

# Preparar datos PISI
pisi_data <- pisi_scores %>%
  filter(eleccion %in% c("Alcaldes", "Concejales")) %>%
  mutate(
    hover_text = sprintf(
      "Año: %d<br>Elección: %s<br>PISI: %.3f<br>Candidatos militantes: %.1f%%<br>Electos militantes: %.1f%%<br>Penetración territorial: %.1f%%<br>Reelección incumbentes: %.1f%%",
      anio_eleccion, eleccion, pisi_score,
      pct_militante,
      pct_militante_win,
      pct_communes_militante,
      ifelse(is.na(reelection_militante), 0, reelection_militante)
    )
  )

# Crear gráfico PISI interactivo
p_pisi <- plot_ly() %>%
  # Datos Alcaldes
  add_trace(
    data = filter(pisi_data, eleccion == "Alcaldes"),
    x = ~anio_eleccion,
    y = ~pisi_score,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Alcaldes',
    line = list(color = '#d62728', width = 3),
    marker = list(size = 10, color = '#d62728'),
    hovertext = ~hover_text,
    hoverinfo = 'text'
  ) %>%
  # Datos Concejales
  add_trace(
    data = filter(pisi_data, eleccion == "Concejales"),
    x = ~anio_eleccion,
    y = ~pisi_score,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Concejales',
    line = list(color = '#1f77b4', width = 3),
    marker = list(size = 10, color = '#1f77b4'),
    hovertext = ~hover_text,
    hoverinfo = 'text'
  ) %>%
  # Línea de tendencia Alcaldes
  add_trace(
    data = filter(pisi_data, eleccion == "Alcaldes"),
    x = ~anio_eleccion,
    y = ~fitted(lm(pisi_score ~ anio_eleccion)),
    type = 'scatter',
    mode = 'lines',
    name = 'Tendencia Alcaldes',
    line = list(color = '#d62728', dash = 'dash', width = 2),
    showlegend = TRUE,
    hoverinfo = 'skip'
  ) %>%
  # Línea de tendencia Concejales
  add_trace(
    data = filter(pisi_data, eleccion == "Concejales"),
    x = ~anio_eleccion,
    y = ~fitted(lm(pisi_score ~ anio_eleccion)),
    type = 'scatter',
    mode = 'lines',
    name = 'Tendencia Concejales',
    line = list(color = '#1f77b4', dash = 'dash', width = 2),
    showlegend = TRUE,
    hoverinfo = 'skip'
  ) %>%
  layout(
    title = list(
      text = "Índice de Fortaleza Institucional Partidaria (PISI)<br><sub>Evolución 2000-2024 (0 = partidos más débiles, 1 = partidos más fuertes)</sub>",
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Año de elección",
      tickmode = 'linear',
      tick0 = 2000,
      dtick = 5,
      range = c(2000, 2026)
    ),
    yaxis = list(
      title = "Puntaje PISI",
      range = c(0, 1),
      tickformat = '.2f'
    ),
    legend = list(
      x = 0.7,
      y = 0.95,
      bgcolor = 'rgba(255, 255, 255, 0.8)',
      bordercolor = 'rgba(0, 0, 0, 0.2)',
      borderwidth = 1
    ),
    hovermode = 'x unified',
    annotations = list(
      list(
        text = "PISI = promedio estandarizado de: % candidatos militantes, % electos militantes,<br>% comunas con militantes, tasa reelección incumbentes militantes.<br>Fuente: SERVEL. Elaboración propia.",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        x = 0.5, y = -0.15, xanchor = "center", yanchor = "top",
        font = list(size = 10, color = 'gray')
      )
    ),
    margin = list(b = 100)
  )

# Guardar gráfico PISI
htmlwidgets::saveWidget(
  p_pisi, 
  file = file.path(getwd(), "outputs/pisi_interactive.html"),
  selfcontained = TRUE
)

# Exportar datos PISI
pisi_export <- pisi_data %>%
  select(
    año = anio_eleccion,
    elección = eleccion,
    pisi_score,
    pct_candidatos_militantes = pct_militante,
    pct_electos_militantes = pct_militante_win,
    penetración_territorial_militantes = pct_communes_militante,
    tasa_reelección_militantes = reelection_militante
  ) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

rio::export(pisi_export, "outputs/pisi_data.xlsx")

# ====================================================================
# GRÁFICO 2: DIAGRAMA DE FLUJOS
# ====================================================================

cat("Creando diagrama de flujos...\n")

# Preparar datos de flujos
flow_years <- c(2021, 2024)
mayors_flow <- municipal_data %>%
  filter(anio_eleccion %in% flow_years, 
         eleccion == "Alcaldes", won == 1) %>%
  group_by(anio_eleccion, comuna_clean) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(anio_eleccion, comuna_clean, candidate_type) %>%
  pivot_wider(names_from = anio_eleccion, 
              values_from = candidate_type,
              names_prefix = "year_")

# Crear resumen de flujos
flow_summary <- mayors_flow %>%
  filter(!is.na(year_2021), !is.na(year_2024)) %>%
  count(year_2021, year_2024, name = "n_comunas") %>%
  arrange(year_2021, year_2024)

if (nrow(flow_summary) > 0) {
  # Preparar datos para Sankey
  sources <- flow_summary %>%
    distinct(source = year_2021) %>%
    mutate(source_id = row_number() - 1)
  
  targets <- flow_summary %>%
    distinct(target = year_2024) %>%
    mutate(target_id = row_number() - 1 + nrow(sources))
  
  # Crear enlaces
  links <- flow_summary %>%
    left_join(sources, by = c("year_2021" = "source")) %>%
    left_join(targets, by = c("year_2024" = "target")) %>%
    mutate(
      value = n_comunas,
      color = case_when(
        year_2021 == "Militante" ~ "rgba(31, 119, 180, 0.5)",
        year_2021 == "Independiente cupo partido" ~ "rgba(255, 127, 14, 0.5)",
        year_2021 == "Independiente fuera de pacto" ~ "rgba(44, 160, 44, 0.5)",
        TRUE ~ "rgba(128, 128, 128, 0.5)"
      ),
      hover_text = sprintf(
        "%s (2021) → %s (2024)<br>%d comunas",
        year_2021, year_2024, n_comunas
      )
    )
  
  # Etiquetas de nodos
  node_labels <- c(
    paste0(sources$source, " (2021)"),
    paste0(targets$target, " (2024)")
  )
  
  # Colores de nodos
  node_colors <- c(
    sapply(sources$source, function(x) {
      if(x == "Militante") return(party_colors["Militante"])
      if(x == "Independiente cupo partido") return(party_colors["Independiente cupo partido"])
      if(x == "Independiente fuera de pacto") return(party_colors["Independiente fuera de pacto"])
      return("#cccccc")
    }),
    sapply(targets$target, function(x) {
      if(x == "Militante") return(party_colors["Militante"])
      if(x == "Independiente cupo partido") return(party_colors["Independiente cupo partido"])
      if(x == "Independiente fuera de pacto") return(party_colors["Independiente fuera de pacto"])
      return("#cccccc")
    })
  )
  
  # Crear diagrama Sankey
  p_flow <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = node_labels,
      color = node_colors,
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = links$source_id,
      target = links$target_id,
      value = links$value,
      color = links$color,
      customdata = links$hover_text,
      hovertemplate = '%{customdata}<extra></extra>'
    )
  ) %>%
  layout(
    title = list(
      text = "Flujo de Tipos de Alcaldes Entre Elecciones<br><sub>Cambios entre 2021 y 2024</sub>",
      font = list(size = 18)
    ),
    font = list(size = 12),
    margin = list(t = 100, r = 20, b = 100, l = 20),
    annotations = list(
      list(
        text = "Cada flujo representa comunas donde cambió o se mantuvo el tipo de alcalde.<br>Fuente: SERVEL. Elaboración propia.",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        x = 0.5, y = -0.1, xanchor = "center", yanchor = "top",
        font = list(size = 10, color = 'gray')
      )
    )
  )
  
  # Guardar diagrama de flujos
  htmlwidgets::saveWidget(
    p_flow,
    file = file.path(getwd(), "outputs/flow_interactive.html"),
    selfcontained = TRUE
  )
  
  # Exportar datos de flujos
  flow_export <- flow_summary %>%
    select(
      tipo_2021 = year_2021,
      tipo_2024 = year_2024,
      número_comunas = n_comunas
    ) %>%
    arrange(desc(número_comunas))
  
  # Estadísticas resumen
  flow_stats <- mayors_flow %>%
    filter(!is.na(year_2021), !is.na(year_2024)) %>%
    summarise(
      total_comunas = n(),
      militantes_2021 = sum(year_2021 == "Militante"),
      militantes_2024 = sum(year_2024 == "Militante"),
      cambio_neto_militantes = militantes_2024 - militantes_2021
    )
  
  # Crear archivo Excel con múltiples hojas
  export_list <- list(
    "flujos_detallados" = flow_export,
    "estadísticas_resumen" = flow_stats,
    "datos_por_comuna" = mayors_flow %>% 
      select(
        comuna = comuna_clean,
        tipo_alcalde_2021 = year_2021,
        tipo_alcalde_2024 = year_2024
      ) %>%
      arrange(comuna)
  )
  
  rio::export(export_list, "outputs/flow_data.xlsx")
}

# ====================================================================
# PÁGINA COMBINADA
# ====================================================================

cat("Creando página combinada...\n")

# Crear HTML combinado con iframes
html_content <- '<!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Análisis de Fortaleza Institucional Partidaria - Chile</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 20px;
            background-color: #f5f5f5;
        }
        .container {
            max-width: 1400px;
            margin: 0 auto;
            background-color: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 0 10px rgba(0,0,0,0.1);
        }
        h1 {
            color: #333;
            text-align: center;
            margin-bottom: 30px;
        }
        h2 {
            color: #555;
            border-bottom: 2px solid #4CAF50;
            padding-bottom: 10px;
        }
        .graph-container {
            margin-bottom: 50px;
        }
        .download-section {
            text-align: center;
            margin: 20px 0;
        }
        .download-btn {
            background-color: #4CAF50;
            color: white;
            padding: 10px 20px;
            text-decoration: none;
            border-radius: 5px;
            display: inline-block;
            margin: 5px;
            font-size: 14px;
            transition: background-color 0.3s;
        }
        .download-btn:hover {
            background-color: #45a049;
        }
        .description {
            margin: 20px 0;
            padding: 15px;
            background-color: #f9f9f9;
            border-left: 4px solid #4CAF50;
            font-size: 14px;
            line-height: 1.6;
        }
        iframe {
            width: 100%;
            height: 600px;
            border: 1px solid #ddd;
            border-radius: 5px;
        }
        @media (max-width: 768px) {
            .container {
                padding: 15px;
            }
            iframe {
                height: 500px;
            }
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>Análisis de Fortaleza Institucional Partidaria en Chile</h1>
        
        <div class="graph-container">
            <h2>Índice de Fortaleza Institucional Partidaria (PISI)</h2>
            <div class="description">
                <p>El PISI es un índice compuesto que evalúa la capacidad organizacional y el peso electoral de los partidos en las elecciones municipales chilenas entre 2000 y 2024. Se construye a partir de cuatro indicadores estandarizados: porcentaje de candidatos militantes, porcentaje de electos militantes, penetración territorial y continuidad de incumbentes, los cuales se promedian para generar un valor entre 0 (máxima debilidad) y 1 (máxima fortaleza).</p>
                <p><strong>Interactividad:</strong> Haz zoom seleccionando un área, arrastra para moverte, pasa el cursor sobre los puntos para ver detalles, y usa las herramientas en la esquina superior derecha.</p>
            </div>
            <iframe src="pisi_interactive.html" title="PISI Interactive Plot"></iframe>
            <div class="download-section">
                <a href="pisi_data.xlsx" class="download-btn" download>📊 Descargar datos PISI (Excel)</a>
            </div>
            <div class="description">
                <p><strong>Conclusiones:</strong> El análisis del PISI en Chile muestra que los partidos políticos tuvieron su mayor fortaleza institucional en las elecciones municipales de 2004 y 2008, pero desde 2012 se observa un declive sostenido. Aunque la presencia territorial se ha mantenido alta, la efectividad electoral y la capacidad de reelección de incumbentes militantes se han erosionado, reflejando un debilitamiento progresivo de los partidos frente al auge de candidaturas independientes.</p>
            </div>
        </div>
        
        <div class="graph-container">
            <h2>Dinámicas de Cambio (Flujos de Alcaldes 2021-2024)</h2>
            <div class="description">
                <p>Este diagrama de flujo muestra cómo cambiaron los tipos de alcaldes entre las elecciones de 2021 y 2024. Cada flujo representa el número de comunas donde se mantuvo o cambió el tipo de alcalde (militante de partido, independiente en cupo de partido, o independiente fuera de pacto).</p>
                <p><strong>Interactividad:</strong> Pasa el cursor sobre los flujos para ver el número exacto de comunas. Los colores representan el tipo de candidato de origen.</p>
            </div>
            <iframe src="flow_interactive.html" title="Flow Interactive Plot"></iframe>
            <div class="download-section">
                <a href="flow_data.xlsx" class="download-btn" download>📊 Descargar datos de flujos (Excel)</a>
            </div>
            <div class="description">
                <p><strong>Conclusiones:</strong> El diagrama de flujos confirma el debilitamiento partidario observado en el PISI: aunque cerca de dos tercios de las comunas con alcaldes militantes en 2021 mantuvieron esa condición en 2024, más de un tercio migró hacia candidaturas independientes, produciendo una pérdida neta para los partidos. La continuidad dentro de cada categoría es alta, pero la migración desde el mundo militante hacia opciones independientes evidencia la erosión de la efectividad electoral partidaria.</p>
            </div>
        </div>
        
        <div style="margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; text-align: center; color: #666; font-size: 14px;">
            <p>Análisis de elecciones municipales chilenas • Datos: SERVEL • Elaboración propia</p>
        </div>
    </div>
</body>
</html>'

writeLines(html_content, "outputs/analisis_interactivo_completo.html")

# ====================================================================
# FINALIZACIÓN
# ====================================================================

cat("\n=== ANÁLISIS COMPLETADO EXITOSAMENTE ===\n")
cat("Archivos generados en outputs/:\n")
cat("- pisi_interactive.html (Gráfico PISI individual)\n")
cat("- flow_interactive.html (Diagrama de flujos individual)\n")
cat("- analisis_interactivo_completo.html (Página principal)\n")
cat("- pisi_data.xlsx (Datos PISI descargables)\n")
cat("- flow_data.xlsx (Datos de flujos descargables)\n\n")
cat("Para mejores resultados, abra los archivos HTML usando un servidor web local.\n")
cat("En VS Code: instalar 'Live Server' y hacer clic derecho > 'Open with Live Server'\n\n")
cat("¡Análisis listo para compartir! 📊\n")