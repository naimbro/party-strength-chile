# Análisis de Fortaleza Electoral Partidaria - Chile
# Script para generar visualizaciones interactivas con desagregación por comuna
# 
# Este script genera gráficos interactivos del Índice de Fortaleza Partidaria Municipal con selector de comuna

# ====================================================================
# CONFIGURACIÓN INICIAL
# ====================================================================

# Configurar codificación UTF-8
options(encoding = "UTF-8")

cat("=== ANÁLISIS DE FORTALEZA ELECTORAL PARTIDARIA POR COMUNA ===\n")
cat("Generando visualizaciones interactivas con selector de comuna...\n\n")

# Cargar paquetes requeridos
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, plotly, htmlwidgets, rio, scales, jsonlite, htmltools, RColorBrewer, ggplot2, gridExtra)

# Primero, ejecutar el script de cálculo del índice por comuna
cat("Calculando Índice de Fortaleza Partidaria Municipal por comuna...\n")
if (file.exists("scripts/02_party_strength_indicators_comuna.R")) {
  source("scripts/02_party_strength_indicators_comuna.R")
} else {
  stop("No se encuentra el script de cálculo del índice por comuna: scripts/02_party_strength_indicators_comuna.R")
}

# Cargar datos
cat("Cargando datos de análisis por comuna...\n")
comuna_data <- readRDS("data/party_strength_comuna_data.rds")

# Extraer objetos de datos
pisi_scores_all <- comuna_data$pisi_scores_all
municipal_data <- comuna_data$municipal_data

# Crear directorio de salida
dir.create("outputs", showWarnings = FALSE)

# ====================================================================
# GRÁFICOS DEL ÍNDICE CON MULTI-SELECCIÓN (SEPARADOS POR ELECCIÓN)
# ====================================================================

cat("Creando gráficos interactivos del índice con multi-selección de comuna...\n")

# Preparar datos del índice para todas las comunas
index_data_full <- pisi_scores_all %>%
  filter(eleccion %in% c("Alcaldes", "Concejales")) %>%
  mutate(
    hover_text = sprintf(
      "Comuna: %s<br>CUT: %s<br>Año: %d<br>Índice: %.3f<br>Candidatos militantes: %.1f%%<br>Electos militantes: %.1f%%<br>Presencia partidaria: %.1f<br>Reelección militantes: %.1f%%",
      comuna_clean,
      CUT_COM,
      anio_eleccion, 
      pisi_score,
      ifelse(is.na(pct_militante), 0, pct_militante),
      ifelse(is.na(pct_militante_win), 0, pct_militante_win),
      ifelse(is.na(has_militante), 0, has_militante),
      ifelse(is.na(reelection_rate_militante), 0, reelection_rate_militante)
    )
  )

# Obtener lista única de comunas (ordenadas alfabéticamente)
# Filter out any empty, NA, or whitespace-only values
comunas_list <- index_data_full %>%
  filter(comuna_clean != "NIVEL NACIONAL",
         !is.na(comuna_clean),
         comuna_clean != "",
         trimws(comuna_clean) != "") %>%
  pull(comuna_clean) %>%
  unique() %>%
  sort()

# Agregar "NIVEL NACIONAL" al principio
comunas_list <- c("NIVEL NACIONAL", comunas_list)

# Debug: Print number of comunas to verify
cat("Total comunas in list (including NIVEL NACIONAL):", length(comunas_list), "\n")

# Función para generar colores únicos
generate_colors <- function(n) {
  if (n <= 9) {
    return(RColorBrewer::brewer.pal(max(3, n), "Set1")[1:n])
  } else {
    return(grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n))
  }
}

# ====================================================================
# GRÁFICO 1: ALCALDES CON MULTI-SELECCIÓN VIA DROPDOWN LATERAL
# ====================================================================

cat("Creando gráfico para Alcaldes con dropdown lateral...\n")

# Filtrar datos para Alcaldes
alcaldes_data <- index_data_full %>%
  filter(eleccion == "Alcaldes")

# Crear gráfico base
p_alcaldes <- plot_ly(type = 'scatter', mode = 'lines+markers')

# Generar colores para todas las comunas
colors <- generate_colors(length(comunas_list))
names(colors) <- comunas_list

# Agregar una traza por cada comuna
# Debug: Ensure we add traces in the exact same order as comunas_list
cat("Adding traces for alcaldes in this order:\n")
for(i in seq_along(comunas_list)) {
  comuna <- comunas_list[i]
  cat(sprintf("%d: %s\n", i, comuna))
  
  data_comuna <- alcaldes_data %>%
    filter(comuna_clean == comuna) %>%
    arrange(anio_eleccion)
  
  # Determinar visibilidad inicial (solo NIVEL NACIONAL visible)
  visible_state <- ifelse(comuna == "NIVEL NACIONAL", TRUE, FALSE)
  
  if(nrow(data_comuna) > 0) {
    p_alcaldes <- p_alcaldes %>%
      add_trace(
        data = data_comuna,
        x = ~anio_eleccion,
        y = ~pisi_score,
        name = comuna,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = colors[i], width = 3),
        marker = list(color = colors[i], size = 8),
        hovertext = ~hover_text,
        hoverinfo = 'text',
        visible = visible_state,
        showlegend = FALSE
      )
  } else {
    # Add empty trace to maintain 1:1 correspondence with dropdown
    cat(sprintf("Warning: No data found for comuna %s - adding empty trace\n", comuna))
    p_alcaldes <- p_alcaldes %>%
      add_trace(
        x = numeric(0),
        y = numeric(0),
        name = comuna,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = colors[i], width = 3),
        marker = list(color = colors[i], size = 8),
        hovertext = paste("Comuna:", comuna, "<br>Sin datos disponibles"),
        hoverinfo = 'text',
        visible = FALSE,  # Always hidden since no data
        showlegend = FALSE
      )
  }
}

# Layout para Alcaldes
p_alcaldes <- p_alcaldes %>%
  layout(
    title = list(
      text = "Índice de Fortaleza Partidaria Municipal - Alcaldes por Comuna",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Año de elección",
      tickmode = 'linear',
      tick0 = 2005,
      dtick = 5,
      range = c(2003, 2025)
    ),
    yaxis = list(
      title = "Puntaje del Índice",
      range = c(0, 1),
      tickformat = '.2f'
    ),
    margin = list(t = 60, b = 60, l = 60, r = 60),
    hovermode = 'closest'
  )

# ====================================================================
# GRÁFICO 2: CONCEJALES CON MULTI-SELECCIÓN VIA DROPDOWN LATERAL
# ====================================================================

cat("Creando gráfico para Concejales con dropdown lateral...\n")

# Filtrar datos para Concejales
concejales_data <- index_data_full %>%
  filter(eleccion == "Concejales")

# Crear gráfico base
p_concejales <- plot_ly(type = 'scatter', mode = 'lines+markers')

# Agregar una traza por cada comuna
# Debug: Ensure we add traces in the exact same order as comunas_list
cat("Adding traces for concejales in this order:\n")
for(i in seq_along(comunas_list)) {
  comuna <- comunas_list[i]
  cat(sprintf("%d: %s\n", i, comuna))
  
  data_comuna <- concejales_data %>%
    filter(comuna_clean == comuna) %>%
    arrange(anio_eleccion)
  
  # Determinar visibilidad inicial (solo NIVEL NACIONAL visible)
  visible_state <- ifelse(comuna == "NIVEL NACIONAL", TRUE, FALSE)
  
  if(nrow(data_comuna) > 0) {
    p_concejales <- p_concejales %>%
      add_trace(
        data = data_comuna,
        x = ~anio_eleccion,
        y = ~pisi_score,
        name = comuna,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = colors[i], width = 3),
        marker = list(color = colors[i], size = 8),
        hovertext = ~hover_text,
        hoverinfo = 'text',
        visible = visible_state,
        showlegend = FALSE
      )
  } else {
    # Add empty trace to maintain 1:1 correspondence with dropdown
    cat(sprintf("Warning: No data found for comuna %s - adding empty trace\n", comuna))
    p_concejales <- p_concejales %>%
      add_trace(
        x = numeric(0),
        y = numeric(0),
        name = comuna,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = colors[i], width = 3),
        marker = list(color = colors[i], size = 8),
        hovertext = paste("Comuna:", comuna, "<br>Sin datos disponibles"),
        hoverinfo = 'text',
        visible = FALSE,  # Always hidden since no data
        showlegend = FALSE
      )
  }
}

# Layout para Concejales
p_concejales <- p_concejales %>%
  layout(
    title = list(
      text = "Índice de Fortaleza Partidaria Municipal - Concejales por Comuna",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Año de elección",
      tickmode = 'linear',
      tick0 = 2005,
      dtick = 5,
      range = c(2003, 2025)
    ),
    yaxis = list(
      title = "Puntaje del Índice",
      range = c(0, 1),
      tickformat = '.2f'
    ),
    margin = list(t = 60, b = 60, l = 60, r = 60),
    hovermode = 'closest'
  )

# ====================================================================
# CREAR PÁGINA COMBINADA CON DROPDOWN ÚNICO
# ====================================================================

cat("Creando página combinada con dropdown único para ambos gráficos...\n")

# Función auxiliar para extraer HTML de widget plotly
extract_widget_html <- function(plot_obj) {
  temp_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(plot_obj, temp_file, selfcontained = FALSE, libdir = "lib")
  widget_html <- paste(readLines(temp_file, warn = FALSE), collapse = "\n")
  unlink(temp_file)
  # Limpiar referencias a libdir ya que usaremos CDN
  widget_html <- gsub('src="lib/', 'src="https://cdn.plot.ly/', widget_html)
  return(widget_html)
}

# Generar opciones del dropdown
comuna_options <- paste(
  sapply(comunas_list, function(comuna) {
    selected <- if(comuna == "NIVEL NACIONAL") "selected" else ""
    sprintf('<option value="%s" %s>%s</option>', comuna, selected, comuna)
  }),
  collapse = "\n"
)

# Crear JSON con colores para JavaScript
colors_json <- jsonlite::toJSON(colors, auto_unbox = TRUE)
comunas_json <- jsonlite::toJSON(comunas_list, auto_unbox = TRUE)

cat("Extrayendo datos JSON de los gráficos Plotly...\n")

# Extraer datos y layout de cada gráfico como JSON
alcaldes_json <- plotly::plotly_json(p_alcaldes, FALSE)
concejales_json <- plotly::plotly_json(p_concejales, FALSE)

cat("Creando página HTML con dos dropdowns independientes...\n")

# Crear el HTML con dos secciones independientes, cada una con su dropdown
combined_html <- paste0(
'<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>Índice de Fortaleza Partidaria Municipal por Comuna</title>
    <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
    <style>
        * { box-sizing: border-box; }
        html, body {
            margin: 0;
            padding: 0;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            background-color: #f5f5f5;
        }
        .chart-section {
            display: flex;
            align-items: flex-start;
            padding: 20px;
            gap: 20px;
            min-height: 520px;
        }
        .control-panel {
            width: 250px;
            min-width: 250px;
            position: sticky;
            top: 20px;
            align-self: flex-start;
        }
        .chart-area {
            flex: 1;
        }
        .dropdown-card {
            background-color: #ffffff;
            border-radius: 8px;
            padding: 20px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        .dropdown-label {
            display: block;
            margin-bottom: 12px;
            font-weight: 600;
            color: #2c3e50;
            font-size: 16px;
        }
        .comuna-selector {
            width: 100%;
            padding: 12px 15px;
            border: 1px solid #ddd;
            border-radius: 6px;
            font-size: 14px;
            color: #333;
            background-color: #fff;
            cursor: pointer;
            appearance: none;
            -webkit-appearance: none;
            -moz-appearance: none;
            background-image: url("data:image/svg+xml;charset=UTF-8,%3csvg xmlns=%27http://www.w3.org/2000/svg%27 viewBox=%270 0 24 24%27 fill=%27none%27 stroke=%27%23333%27 stroke-width=%272%27 stroke-linecap=%27round%27 stroke-linejoin=%27round%27%3e%3cpolyline points=%276 9 12 15 18 9%27%3e%3c/polyline%3e%3c/svg%3e");
            background-repeat: no-repeat;
            background-position: right 12px center;
            background-size: 16px;
        }
        .comuna-selector:hover {
            border-color: #aaa;
        }
        .comuna-selector:focus {
            outline: none;
            border-color: #3498db;
            box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.15);
        }
        .checkbox-container {
            margin-top: 15px;
            display: flex;
            align-items: center;
            gap: 8px;
        }
        .checkbox-container input[type="checkbox"] {
            width: 18px;
            height: 18px;
            cursor: pointer;
            accent-color: #3498db;
        }
        .checkbox-container label {
            font-size: 13px;
            color: #555;
            cursor: pointer;
        }
        .chart-container {
            background-color: white;
            border-radius: 8px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            padding: 15px;
        }
        .chart-title {
            font-size: 16px;
            font-weight: 600;
            color: #2c3e50;
            margin-bottom: 10px;
            padding-bottom: 10px;
            border-bottom: 2px solid #e0e0e0;
        }
        .chart-title.alcaldes { border-bottom-color: #d62728; }
        .chart-title.concejales { border-bottom-color: #1f77b4; }
        .plot-container {
            width: 100%;
            height: 450px;
        }
        .section-divider {
            height: 1px;
            background-color: #e0e0e0;
            margin: 0 20px;
        }
    </style>
</head>
<body>
    <!-- Sección Alcaldes -->
    <div class="chart-section">
        <div class="control-panel">
            <div class="dropdown-card">
                <label class="dropdown-label" for="alcaldesSelector">Elige una comuna:</label>
                <select id="alcaldesSelector" class="comuna-selector">
', comuna_options, '
                </select>
                <div class="checkbox-container">
                    <input type="checkbox" id="showNacionalAlcaldes" />
                    <label for="showNacionalAlcaldes">Comparar con tendencia nacional</label>
                </div>
            </div>
        </div>
        <div class="chart-area">
            <div class="chart-container">
                <div class="chart-title alcaldes">Índice de Fortaleza Partidaria - Alcaldes</div>
                <div id="alcaldesPlot" class="plot-container"></div>
            </div>
        </div>
    </div>

    <div class="section-divider"></div>

    <!-- Sección Concejales -->
    <div class="chart-section">
        <div class="control-panel">
            <div class="dropdown-card">
                <label class="dropdown-label" for="concejalesSelector">Elige una comuna:</label>
                <select id="concejalesSelector" class="comuna-selector">
', comuna_options, '
                </select>
                <div class="checkbox-container">
                    <input type="checkbox" id="showNacionalConcejales" />
                    <label for="showNacionalConcejales">Comparar con tendencia nacional</label>
                </div>
            </div>
        </div>
        <div class="chart-area">
            <div class="chart-container">
                <div class="chart-title concejales">Índice de Fortaleza Partidaria - Concejales</div>
                <div id="concejalesPlot" class="plot-container"></div>
            </div>
        </div>
    </div>

    <script>
        // Datos de los gráficos
        const alcaldesData = ', alcaldes_json, ';
        const concejalesData = ', concejales_json, ';

        // Función genérica para actualizar un gráfico
        function updateChart(plotId, selectorId, checkboxId, chartData) {
            const selector = document.getElementById(selectorId);
            const checkbox = document.getElementById(checkboxId);
            const selectedComuna = selector.value;
            const showNacional = checkbox.checked;

            const plotDiv = document.getElementById(plotId);
            if (plotDiv && plotDiv.data) {
                const visibilityUpdate = [];
                const lineStyleUpdate = [];

                plotDiv.data.forEach(trace => {
                    if (trace.name === selectedComuna) {
                        visibilityUpdate.push(true);
                        lineStyleUpdate.push({dash: "solid", width: 3});
                    } else if (trace.name === "NIVEL NACIONAL" && showNacional && selectedComuna !== "NIVEL NACIONAL") {
                        visibilityUpdate.push(true);
                        lineStyleUpdate.push({dash: "dot", width: 2});
                    } else {
                        visibilityUpdate.push(false);
                        lineStyleUpdate.push({dash: "solid", width: 3});
                    }
                });

                Plotly.restyle(plotId, {
                    visible: visibilityUpdate,
                    "line.dash": lineStyleUpdate.map(s => s.dash),
                    "line.width": lineStyleUpdate.map(s => s.width)
                });
            }
        }

        // Funciones específicas para cada gráfico
        function updateAlcaldes() {
            updateChart("alcaldesPlot", "alcaldesSelector", "showNacionalAlcaldes", alcaldesData);
        }

        function updateConcejales() {
            updateChart("concejalesPlot", "concejalesSelector", "showNacionalConcejales", concejalesData);
        }

        // Inicializar gráficos
        function initCharts() {
            const config = {responsive: true, displayModeBar: true};

            Plotly.newPlot("alcaldesPlot", alcaldesData.data, alcaldesData.layout, config);
            Plotly.newPlot("concejalesPlot", concejalesData.data, concejalesData.layout, config);

            // Aplicar selección inicial
            updateAlcaldes();
            updateConcejales();
        }

        // Event listeners para Alcaldes
        document.getElementById("alcaldesSelector").addEventListener("change", updateAlcaldes);
        document.getElementById("showNacionalAlcaldes").addEventListener("change", updateAlcaldes);

        // Event listeners para Concejales
        document.getElementById("concejalesSelector").addEventListener("change", updateConcejales);
        document.getElementById("showNacionalConcejales").addEventListener("change", updateConcejales);

        // Inicializar cuando el DOM esté listo
        document.addEventListener("DOMContentLoaded", initCharts);
    </script>
</body>
</html>')

# Guardar el HTML combinado
writeLines(combined_html, "docs/pisi_interactive.html")
cat("✓ HTML combinado guardado en: docs/pisi_interactive.html\n")

# Verificar que el archivo se creó correctamente
if (file.exists("docs/pisi_interactive.html")) {
  cat("✓ Archivo creado exitosamente. Tamaño:", file.size("docs/pisi_interactive.html"), "bytes\n")
} else {
  warning("Error: No se pudo crear el archivo HTML combinado")
}

# ====================================================================
# CREAR GRÁFICOS ESTÁTICOS PARA INDICADORES INDIVIDUALES
# ====================================================================

cat("Creando gráficos estáticos para indicadores del índice...\n")

# Usar los datos ya calculados del nivel nacional desde pisi_scores_all
cat("Extrayendo indicadores nacionales desde datos ya calculados...\n")

# Filtrar solo datos de NIVEL NACIONAL
datos_nacionales <- pisi_scores_all %>%
  filter(comuna_clean == "NIVEL NACIONAL", 
         eleccion %in% c("Alcaldes", "Concejales"))

# Indicador 1: Presencia de candidatos militantes
indicador1_nacional <- datos_nacionales %>%
  select(anio_eleccion, eleccion, valor = pct_militante) %>%
  mutate(indicador = "pct_militante")

# Indicador 2: Éxito electoral de militantes  
indicador2_nacional <- datos_nacionales %>%
  select(anio_eleccion, eleccion, valor = pct_militante_win) %>%
  mutate(indicador = "pct_militante_win")

# Indicador 3: Cobertura territorial
indicador3_nacional <- datos_nacionales %>%
  select(anio_eleccion, eleccion, valor = has_militante) %>%
  mutate(indicador = "has_militante")

# Indicador 4: Reelección incumbentes militantes
indicador4_nacional <- datos_nacionales %>%
  select(anio_eleccion, eleccion, valor = reelection_rate_militante) %>%
  mutate(indicador = "reelection_rate_militante")

# Combinar todos los indicadores
national_data_static <- bind_rows(
  indicador1_nacional,
  indicador2_nacional, 
  indicador3_nacional,
  indicador4_nacional
) %>%
  mutate(
    indicador_label = case_when(
      indicador == "pct_militante" ~ "% Candidatos militantes",
      indicador == "pct_militante_win" ~ "% Electos militantes", 
      indicador == "has_militante" ~ "% Comunas con presencia partidaria",
      indicador == "reelection_rate_militante" ~ "% Reelección incumbentes"
    ),
    valor = ifelse(is.na(valor), 0, valor)
  )

# Tema común para los gráficos
theme_indice <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray50", hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# Colores para los tipos de elección
colores_eleccion <- c("Alcaldes" = "#d62728", "Concejales" = "#1f77b4")

# Función para crear gráfico individual
create_indicator_plot <- function(indicator_name, title, subtitle = NULL, y_label) {
  data_plot <- national_data_static %>%
    filter(indicador == indicator_name)
  
  p <- ggplot(data_plot, aes(x = anio_eleccion, y = valor, color = eleccion)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 3, alpha = 0.9) +
    scale_color_manual(values = colores_eleccion) +
    scale_x_continuous(
      name = "Año",
      breaks = seq(2005, 2025, 5),
      limits = c(2003, 2025)
    ) +
    scale_y_continuous(
      name = y_label,
      labels = function(x) paste0(round(x, 1), "%"),
      limits = c(20, 100)
    ) +
    labs(title = title, subtitle = subtitle) +
    theme_indice +
    theme(legend.position = "none")  # Quitar completamente la leyenda
  
  return(p)
}

# Crear los 4 gráficos estáticos
cat("Creando gráfico 1: Candidatos militantes...\n")
p1_static <- create_indicator_plot(
  "pct_militante",
  "1. Presencia de candidatos militantes",
  NULL,  # Sin subtitle para ahorrar espacio
  "% candidatos militantes"
)

cat("Creando gráfico 2: Electos militantes...\n") 
p2_static <- create_indicator_plot(
  "pct_militante_win",
  "2. Éxito electoral de militantes",
  NULL,
  "% electos militantes"
)

cat("Creando gráfico 3: Cobertura territorial...\n")
p3_static <- create_indicator_plot(
  "has_militante",
  "3. Cobertura territorial partidaria",
  NULL,
  "% comunas con presencia"
)

cat("Creando gráfico 4: Reelección incumbentes...\n")
p4_static <- create_indicator_plot(
  "reelection_rate_militante", 
  "4. Continuidad de incumbentes",
  NULL,
  "% reelección incumbentes"
)

# Crear panel 2x2
cat("Combinando gráficos en panel 2x2...\n")
panel_2x2 <- grid.arrange(p1_static, p2_static, p3_static, p4_static, 
                         ncol = 2, nrow = 2,
                         top = "Evolución de los Indicadores del Índice de Fortaleza Partidaria Municipal (2004-2024)")

# Guardar panel 2x2
ggsave("docs/panel_indicadores_indice.png", panel_2x2, width = 14, height = 10, dpi = 300, bg = "white")

# ====================================================================
# CREAR GRÁFICO ESTÁTICO DEL ÍNDICE AGREGADO NACIONAL
# ====================================================================

cat("Creando gráfico del Índice de Fortaleza Partidaria Municipal agregado nacional...\n")

# Usar directamente los datos ya calculados con el índice
indice_nacional <- datos_nacionales %>%
  select(anio_eleccion, eleccion, pisi_score)

# Gráfico PISI agregado (igual que tu código)
p_indice_nacional <- ggplot(indice_nacional, aes(x = anio_eleccion, y = pisi_score, color = eleccion)) +
  geom_line(linewidth = 1.2, alpha = 1) +
  geom_point(size = 3, alpha = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = colores_eleccion) +
  scale_x_continuous(
    name = "Año de elección",
    breaks = seq(2005, 2025, 5),
    limits = c(2003, 2025)
  ) +
  labs(
    title = "Índice de Fortaleza Partidaria Municipal",
    x = "Año de elección",
    y = "Puntaje del Índice",
    color = "Tipo de elección"
  ) +
  theme_indice +
  theme(legend.position = "bottom")

# Guardar gráfico PISI nacional
ggsave("docs/indice_nacional_agregado.png", p_indice_nacional, width = 12, height = 7, dpi = 300, bg = "white")

cat("✓ Gráficos estáticos creados exitosamente\n")

# ====================================================================
# EXPORTAR DATOS
# ====================================================================

cat("Exportando datos del Índice de Fortaleza Partidaria Municipal por comuna...\n")

# Preparar datos para exportación (incluyendo CUT_COM)
pisi_export_all <- pisi_scores_all %>%
  select(
    CUT_COM,
    comuna = comuna_clean,
    año = anio_eleccion,
    elección = eleccion,
    indice_fortaleza_partidaria = pisi_score,
    pct_candidatos_militantes = pct_militante,
    pct_electos_militantes = pct_militante_win,
    presencia_partidaria = has_militante,
    tasa_reelección_militantes = reelection_rate_militante
  ) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  arrange(CUT_COM, año, elección)

# Exportar datos separados por elección
pisi_export_alcaldes <- pisi_export_all %>%
  filter(elección == "Alcaldes") %>%
  select(-elección)

pisi_export_concejales <- pisi_export_all %>%
  filter(elección == "Concejales") %>%
  select(-elección)

# Crear archivo Excel con múltiples hojas
export_list <- list(
  "datos_completos" = pisi_export_all,
  "alcaldes" = pisi_export_alcaldes,
  "concejales" = pisi_export_concejales
)

rio::export(export_list, "docs/indice_fortaleza_partidaria_comuna_data.xlsx")

# ====================================================================
# CREAR PÁGINA HTML COMBINADA
# ====================================================================

cat("Creando página HTML con visualización del Índice de Fortaleza Partidaria Municipal por comuna...\n")

# Crear el HTML en partes para evitar límite de longitud de string
html_header <- '<!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Fortaleza Electoral Partidaria en Chile - Análisis Municipal</title>
    <style>
        body { font-family: Georgia, serif; margin: 0; padding: 20px; background-color: #f8f9fa; line-height: 1.6; }
        .container { max-width: 900px; margin: 0 auto; background-color: white; padding: 40px; border-radius: 10px; box-shadow: 0 0 20px rgba(0,0,0,0.1); }
        h1 { color: #2c3e50; text-align: center; margin-bottom: 10px; font-size: 2.2em; font-weight: 300; }
        h2 { color: #34495e; border-bottom: 3px solid #3498db; padding-bottom: 10px; margin-top: 40px; font-size: 1.6em; }
        h3 { color: #2c3e50; margin-top: 30px; font-size: 1.3em; }
        .subtitle { text-align: center; color: #7f8c8d; font-size: 1.1em; margin-bottom: 40px; font-style: italic; }
        .intro-section { background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); padding: 30px; border-radius: 10px; margin-bottom: 40px; border-left: 5px solid #3498db; }
        .intro-text { font-size: 1.05em; text-align: justify; line-height: 1.8; color: #2c3e50; }
        .index-explanation { background-color: #ecf0f1; padding: 25px; border-radius: 8px; margin: 30px 0; border-left: 5px solid #e74c3c; }
        .indicators-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin: 30px 0; }
        .indicator-card { background-color: #fff; border: 1px solid #e0e0e0; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .indicator-card h4 { color: #2c3e50; margin: 0 0 10px 0; font-size: 1.1em; }
        .indicator-card p { margin: 0; font-size: 0.95em; color: #555; line-height: 1.5; }
        .static-plot { text-align: center; margin: 30px 0; padding: 20px; background-color: #fafafa; border-radius: 8px; border: 1px solid #e0e0e0; }
        .static-plot img { max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
        .index-nacional { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 10px; margin: 40px 0; text-align: center; }
        .index-nacional h2 { color: white; border-bottom: 3px solid rgba(255,255,255,0.3); margin-top: 0; }
        .interactive-section { background-color: #f8f9fa; padding: 30px; border-radius: 10px; margin: 40px 0; border-left: 5px solid #28a745; }
        .election-section { border: 1px solid #e0e0e0; border-radius: 8px; margin: 30px 0; padding: 20px; background-color: #fafafa; }
        .election-section.alcaldes { border-left: 4px solid #d62728; }
        .election-section.concejales { border-left: 4px solid #1f77b4; }
        iframe { width: 100%; height: 700px; border: 1px solid #ddd; border-radius: 5px; }
        .download-section { text-align: center; margin: 30px 0; padding: 25px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; }
        .download-btn { background-color: #28a745; color: white; padding: 12px 25px; text-decoration: none; border-radius: 25px; display: inline-block; font-size: 1.1em; transition: all 0.3s ease; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
        .download-btn:hover { background-color: #218838; transform: translateY(-2px); box-shadow: 0 6px 12px rgba(0,0,0,0.2); }
        .conclusion { background-color: #f1f3f4; padding: 25px; border-radius: 8px; margin: 30px 0; border-left: 5px solid #6c757d; font-style: italic; }
        .footer { margin-top: 50px; padding-top: 20px; border-top: 2px solid #e9ecef; text-align: center; color: #6c757d; font-size: 0.95em; }
        @media (max-width: 768px) { .container { padding: 20px; } .indicators-grid { grid-template-columns: 1fr; } iframe { height: 500px; } }
    </style>
</head>
<body>
    <div class="container">
        <h1>Fortaleza Electoral Partidaria en Chile</h1>
        <p class="subtitle">Análisis temporal del desempeño electoral de los partidos políticos a nivel municipal</p>
        <p style="text-align: center; color: #7f8c8d; margin-bottom: 40px;">
            <strong>Naim Bro</strong> • Universidad Adolfo Ibáñez • Laboratorio Municipal
        </p>'

html_content_body <- '
        <p style="text-align: justify; font-size: 1.05em; line-height: 1.7; margin: 30px 0;">La fortaleza electoral de los partidos políticos constituye un elemento central para el funcionamiento de la democracia representativa. Los partidos canalizan demandas sociales, reclutan candidatos, y otorgan estabilidad y previsibilidad a la competencia electoral. Sin embargo, en Chile, el desempeño electoral partidario ha mostrado signos de debilitamiento en las últimas décadas, expresado en el aumento de candidaturas independientes y en la pérdida de presencia territorial de las colectividades políticas.</p>
        
        <p style="text-align: justify; font-size: 1.05em; line-height: 1.7; margin: 30px 0;">A nivel municipal, el desempeño electoral de los partidos adquiere una relevancia particular: los gobiernos locales son la base de la representación política y un espacio clave de vinculación entre ciudadanía e instituciones. Evaluar la fuerza electoral de los partidos en este nivel permite comprender mejor el peso que mantienen en la competencia política territorial, así como los desafíos de gobernabilidad, continuidad y coordinación entre niveles de gobierno.</p>
        
        <p style="text-align: justify; font-size: 1.05em; line-height: 1.7; margin: 30px 0;">El Índice de Fortaleza Partidaria Municipal desarrollado por el LabMun mide el desempeño electoral de los partidos en elecciones municipales chilenas desde 2004 hasta 2024. Este índice compuesto (escala 0-1) combina cuatro indicadores complementarios expresados en porcentajes:</p>
        
        <ul style="margin: 20px 0 40px 40px; line-height: 1.8; font-size: 1.05em;">
            <li style="margin-bottom: 8px;"><strong>Presencia de candidatos militantes</strong>: Porcentaje de candidatos afiliados a partidos sobre el total de candidatos por elección.</li>
            <li style="margin-bottom: 8px;"><strong>Éxito electoral de militantes</strong>: Porcentaje de militantes electos sobre el total de personas electas por elección.</li>
            <li style="margin-bottom: 8px;"><strong>Cobertura territorial</strong>: Porcentaje de comunas donde los partidos presentan al menos un candidato militante.</li>
            <li style="margin-bottom: 8px;"><strong>Continuidad de incumbentes</strong>: Tasa de reelección de autoridades militantes que buscan un nuevo mandato.</li>
        </ul>
        
        <p style="text-align: justify; font-size: 1.05em; line-height: 1.7; margin: 40px 0 30px 0;">Los siguientes gráficos muestran la evolución temporal de cada uno de los cuatro indicadores que componen el Índice de Fortaleza Partidaria Municipal a nivel nacional. Todos los valores se expresan en porcentajes, diferenciando entre elecciones de alcaldes (línea roja) y concejales (línea azul):</p>
        
        <div style="text-align: center; margin: 40px 0;">
            <img src="panel_indicadores_indice.png" alt="Panel con los cuatro indicadores del Índice de Fortaleza Partidaria Municipal" style="max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);">
        </div>
        
        <p style="text-align: justify; font-size: 1.05em; line-height: 1.7; margin: 40px 0 30px 0;">Para construir el Índice de Fortaleza Partidaria Municipal, cada uno de los cuatro indicadores se normaliza en una escala de 0 a 1 (donde 0 representa el valor mínimo observado y 1 el máximo para cada tipo de elección). Posteriormente, se calcula el promedio simple de los cuatro indicadores normalizados, generando un índice sintético que varía entre 0 (mínima fortaleza electoral) y 1 (máxima fortaleza electoral). El siguiente gráfico presenta la evolución del índice agregado a nivel nacional:</p>
        
        <div style="text-align: center; margin: 40px 0;">
            <img src="indice_nacional_agregado.png" alt="Índice de Fortaleza Partidaria Municipal Nacional Agregado" style="max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);">
        </div>'

html_interactivo <- '
        <h2 style="margin-top: 60px; margin-bottom: 20px; color: #34495e; border-bottom: 3px solid #3498db; padding-bottom: 10px;">Exploración por Comuna</h2>

        <p style="text-align: justify; font-size: 1.05em; line-height: 1.7; margin: 30px 0 40px 0;">El gráfico interactivo siguiente permite explorar las variaciones del Índice de Fortaleza Partidaria Municipal entre comunas específicas. Use el panel lateral izquierdo para seleccionar las comunas de interés y compare sus trayectorias de fortaleza electoral partidaria para alcaldes y concejales simultáneamente:</p>

        <div style="margin: 20px 0 50px 0;">
            <iframe src="pisi_interactive.html" title="Índice de Fortaleza Partidaria Municipal por Comuna" style="width: 100%; height: 1150px; border: none; overflow: hidden;" scrolling="no"></iframe>
        </div>

        <h2 style="margin-top: 60px; margin-bottom: 20px; color: #34495e;">Hallazgos principales</h2>
        <p style="text-align: justify; font-size: 1.05em; line-height: 1.7; margin: 30px 0;">El análisis del Índice de Fortaleza Partidaria Municipal en Chile revela que, desde 2004 hasta la fecha, los partidos han experimentado un debilitamiento sostenido a nivel municipal. Aunque la presencia territorial se ha mantenido relativamente alta, la efectividad electoral y la capacidad de reelección de incumbentes militantes se han erosionado significativamente, reflejando un debilitamiento progresivo de los partidos frente al auge de candidaturas independientes y personalistas en el ámbito municipal.</p>
        <p style="text-align: justify; font-size: 1.05em; line-height: 1.7; margin: 30px 0;">* Para un reporte metodológico detallado, pincha <a href="https://docs.google.com/document/d/1MAC1RibPgIazMU3c7G1shzhIu92gUu6ougwOOMieX6s/edit?usp=sharing" target="_blank">aquí</a></p>'

html_footer <- '
        <div class="download-section">
            <h3>Datos para descarga</h3>
            <p>Acceda a los datos completos del Índice de Fortaleza Partidaria Municipal por comuna en formato Excel:</p>
            <a href="indice_fortaleza_partidaria_comuna_data.xlsx" class="download-btn" download>📊 Descargar datos del índice (Excel)</a>
        </div>
        <div class="footer">
            <p><strong>Laboratorio Municipal (LabMun)</strong> • Universidad Adolfo Ibáñez</p>
            <p>Análisis de elecciones municipales chilenas • Datos: Servicio Electoral (SERVEL)</p>
        </div>
    </div>
</body>
</html>'

# Combinar todas las partes
html_content <- paste0(html_header, html_content_body, html_interactivo, html_footer)

writeLines(html_content, "docs/analisis_fortaleza_partidaria_comuna.html")

# ====================================================================
# RESUMEN FINAL
# ====================================================================

cat("\n=== ANÁLISIS COMPLETADO EXITOSAMENTE ===\n")
cat("Archivos generados en docs/:\n")
cat("- pisi_interactive.html (Gráfico combinado con dropdown único para ambos tipos de elección)\n")
cat("- analisis_fortaleza_partidaria_comuna.html (Página principal con instrucciones)\n")
cat("- indice_fortaleza_partidaria_comuna_data.xlsx (Datos del índice por comuna)\n\n")

# Estadísticas resumen
n_comunas <- index_data_full %>% 
  filter(comuna_clean != "NIVEL NACIONAL") %>% 
  pull(comuna_clean) %>% 
  n_distinct()

cat(sprintf("Total de comunas incluidas: %d\n", n_comunas))
cat(sprintf("Años analizados: %s\n\n", 
    paste(sort(unique(index_data_full$anio_eleccion)), collapse = ", ")))

cat("Para visualizar los resultados, abra 'docs/analisis_fortaleza_partidaria_comuna.html' en su navegador.\n")
cat("¡Análisis por comuna listo! 📊\n")