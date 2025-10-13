# Análisis de Fortaleza Institucional Partidaria - Chile
# Script para generar visualizaciones interactivas con desagregación por comuna
# 
# Este script genera gráficos interactivos del PISI con selector de comuna

# ====================================================================
# CONFIGURACIÓN INICIAL
# ====================================================================

# Configurar codificación UTF-8
options(encoding = "UTF-8")

cat("=== ANÁLISIS DE FORTALEZA INSTITUCIONAL PARTIDARIA POR COMUNA ===\n")
cat("Generando visualizaciones interactivas con selector de comuna...\n\n")

# Cargar paquetes requeridos
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, plotly, htmlwidgets, rio, scales, jsonlite, htmltools, RColorBrewer)

# Primero, ejecutar el script de cálculo de PISI por comuna
cat("Calculando PISI por comuna...\n")
if (file.exists("scripts/02_party_strength_indicators_comuna.R")) {
  source("scripts/02_party_strength_indicators_comuna.R")
} else {
  stop("No se encuentra el script de cálculo de PISI por comuna: scripts/02_party_strength_indicators_comuna.R")
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
# GRÁFICOS PISI CON MULTI-SELECCIÓN (SEPARADOS POR ELECCIÓN)
# ====================================================================

cat("Creando gráficos PISI interactivos con multi-selección de comuna...\n")

# Preparar datos PISI para todas las comunas
pisi_data_full <- pisi_scores_all %>%
  filter(eleccion %in% c("Alcaldes", "Concejales")) %>%
  mutate(
    hover_text = sprintf(
      "Comuna: %s<br>CUT: %s<br>Año: %d<br>PISI: %.3f<br>Candidatos militantes: %.1f%%<br>Electos militantes: %.1f%%<br>Presencia partidaria: %.1f<br>Reelección militantes: %.1f%%",
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
comunas_list <- pisi_data_full %>%
  filter(comuna_clean != "NIVEL NACIONAL") %>%
  pull(comuna_clean) %>%
  unique() %>%
  sort()

# Agregar "NIVEL NACIONAL" al principio
comunas_list <- c("NIVEL NACIONAL", comunas_list)

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
alcaldes_data <- pisi_data_full %>%
  filter(eleccion == "Alcaldes")

# Crear gráfico base
p_alcaldes <- plot_ly(type = 'scatter', mode = 'lines+markers')

# Generar colores para todas las comunas
colors <- generate_colors(length(comunas_list))
names(colors) <- comunas_list

# Agregar una traza por cada comuna
for(i in seq_along(comunas_list)) {
  comuna <- comunas_list[i]
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
  }
}

# Layout para Alcaldes
p_alcaldes <- p_alcaldes %>%
  layout(
    title = list(
      text = "PISI Alcaldes por Comuna",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Año de elección",
      tickmode = 'linear',
      tick0 = 2000,
      dtick = 5,
      range = c(1999, 2025)
    ),
    yaxis = list(
      title = "Puntaje PISI",
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
concejales_data <- pisi_data_full %>%
  filter(eleccion == "Concejales")

# Crear gráfico base
p_concejales <- plot_ly(type = 'scatter', mode = 'lines+markers')

# Agregar una traza por cada comuna
for(i in seq_along(comunas_list)) {
  comuna <- comunas_list[i]
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
  }
}

# Layout para Concejales
p_concejales <- p_concejales %>%
  layout(
    title = list(
      text = "PISI Concejales por Comuna",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Año de elección",
      tickmode = 'linear',
      tick0 = 2000,
      dtick = 5,
      range = c(1999, 2025)
    ),
    yaxis = list(
      title = "Puntaje PISI",
      range = c(0, 1),
      tickformat = '.2f'
    ),
    margin = list(t = 60, b = 60, l = 60, r = 60),
    hovermode = 'closest'
  )

# ====================================================================
# CREAR GRÁFICOS CON PANEL LATERAL INTEGRADO
# ====================================================================


# Función para crear widget integrado con panel lateral
create_integrated_widget <- function(plot_obj, chart_id, chart_title, comunas_list, colors, output_file) {
  
  # Guardar temporalmente el widget Plotly
  temp_file <- tempfile(fileext = ".html")
  cat("Creando widget temporal en:", temp_file, "\n")
  
  tryCatch({
    htmlwidgets::saveWidget(plot_obj, temp_file, selfcontained = TRUE)
    cat("Widget temporal creado exitosamente\n")
  }, error = function(e) {
    cat("Error al crear widget temporal:", e$message, "\n")
    stop("No se pudo crear el widget temporal")
  })
  
  # Leer el contenido HTML del widget
  cat("Leyendo contenido del widget...\n")
  widget_html <- paste(readLines(temp_file, warn = FALSE), collapse = "\n")
  
  # Limpiar archivo temporal
  unlink(temp_file)
  cat("Widget integrado correctamente\n")
  
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
  
  # Crear el HTML usando paste() para evitar problemas con sprintf
  html_content <- paste0(
'<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>', chart_title, '</title>
    <style>
        body { 
            margin: 0; 
            font-family: Arial, sans-serif; 
            background-color: #f5f5f5;
        }
        .chart-container { 
            display: flex; 
            height: 100vh; 
        }
        .control-panel { 
            width: 300px; 
            background-color: white; 
            border-right: 1px solid #ddd;
            padding: 20px;
            overflow-y: auto;
            box-shadow: 2px 0 5px rgba(0,0,0,0.1);
        }
        .chart-area { 
            flex: 1; 
            background-color: white;
            position: relative;
        }
        .panel-title {
            font-size: 16px;
            font-weight: bold;
            margin-bottom: 15px;
            color: #333;
            border-bottom: 2px solid #4CAF50;
            padding-bottom: 8px;
        }
        .dropdown-container {
            margin-bottom: 20px;
        }
        .dropdown-label {
            display: block;
            margin-bottom: 8px;
            font-weight: bold;
            color: #555;
        }
        #comunaSelector {
            width: 100%;
            height: 200px;
            border: 1px solid #ddd;
            border-radius: 4px;
            padding: 8px;
            font-size: 13px;
        }
        .button-group {
            display: flex;
            gap: 10px;
            margin-top: 10px;
        }
        .control-btn {
            flex: 1;
            padding: 8px 12px;
            border: none;
            border-radius: 4px;
            cursor: pointer;
            font-size: 12px;
            transition: background-color 0.3s;
        }
        .btn-select-all {
            background-color: #4CAF50;
            color: white;
        }
        .btn-select-all:hover {
            background-color: #45a049;
        }
        .btn-clear-all {
            background-color: #f44336;
            color: white;
        }
        .btn-clear-all:hover {
            background-color: #da190b;
        }
        .search-box {
            width: 100%;
            padding: 8px;
            margin-bottom: 10px;
            border: 1px solid #ddd;
            border-radius: 4px;
            font-size: 13px;
        }
        .info-section {
            margin-top: 20px;
            padding: 15px;
            background-color: #f9f9f9;
            border-radius: 4px;
            border-left: 4px solid #4CAF50;
        }
        .info-section h4 {
            margin: 0 0 10px 0;
            color: #333;
            font-size: 14px;
        }
        .info-section p {
            margin: 0;
            font-size: 12px;
            line-height: 1.4;
            color: #666;
        }
        #chartArea {
            width: 100%;
            height: 100%;
        }
    </style>
</head>
<body>
    <div class="chart-container">
        <div class="control-panel">
            <div class="panel-title">Selección de Comunas</div>
            
            <div class="dropdown-container">
                <label class="dropdown-label" for="comunaSelector">Comunas disponibles:</label>
                <input type="text" id="searchBox" class="search-box" placeholder="Buscar comuna..." />
                <select id="comunaSelector" multiple>
', comuna_options, '
                </select>
                
                <div class="button-group">
                    <button id="selectAll" class="control-btn btn-select-all">Seleccionar Todas</button>
                    <button id="clearAll" class="control-btn btn-clear-all">Limpiar</button>
                </div>
            </div>
            
            <div class="info-section">
                <h4>💡 Instrucciones</h4>
                <p><strong>Multi-selección:</strong> Mantenga Ctrl (Cmd en Mac) para seleccionar múltiples comunas.</p>
                <p><strong>Búsqueda:</strong> Use el cuadro de búsqueda para filtrar comunas.</p>
                <p><strong>Botones:</strong> "Seleccionar Todas" muestra todas las comunas, "Limpiar" las oculta todas.</p>
            </div>
        </div>
        
        <div class="chart-area" id="chartArea">
', widget_html, '
        </div>
    </div>

    <script>
        // Datos de configuración
        const COLORS = ', colors_json, ';
        const COMUNAS = ', comunas_json, ';
        
        let chartReady = false;
        
        // Referencias DOM
        const selector = document.getElementById("comunaSelector");
        const searchBox = document.getElementById("searchBox");
        const selectAllBtn = document.getElementById("selectAll");
        const clearAllBtn = document.getElementById("clearAll");
        
        // Función para actualizar visibilidad de trazas
        function updateTraceVisibility() {
            try {
                const selectedComunas = Array.from(selector.selectedOptions).map(option => option.value);
                console.log("Selected comunas:", selectedComunas);
                
                const visibilityUpdate = COMUNAS.map(comuna => selectedComunas.includes(comuna));
                console.log("Visibility update:", visibilityUpdate);
                
                // Buscar el div del gráfico directamente en el documento
                const chartDiv = document.querySelector("div[id^=\'htmlwidget-\']");
                if (chartDiv && window.Plotly) {
                    console.log("Updating chart visibility...");
                    window.Plotly.restyle(chartDiv, { visible: visibilityUpdate });
                } else {
                    console.log("Plotly chart not found yet, retrying...");
                    setTimeout(updateTraceVisibility, 300);
                }
            } catch (error) {
                console.error("Error updating trace visibility:", error);
            }
        }
        
        // Función para verificar si el gráfico está listo
        function checkChartReady() {
            const chartDiv = document.querySelector("div[id^=\'htmlwidget-\']");
            if (chartDiv && window.Plotly) {
                chartReady = true;
                console.log("Chart is ready!");
                // Aplicar la selección inicial
                updateTraceVisibility();
            } else {
                console.log("Waiting for chart to load...");
                setTimeout(checkChartReady, 500);
            }
        }
        
        // Inicializar cuando el DOM esté listo
        document.addEventListener("DOMContentLoaded", function() {
            console.log("DOM loaded, checking for chart...");
            setTimeout(checkChartReady, 1000);
        });
        
        // Función para filtrar opciones del dropdown
        function filterComunas() {
            const searchTerm = searchBox.value.toLowerCase();
            const options = selector.querySelectorAll("option");
            
            options.forEach(option => {
                const comunaName = option.textContent.toLowerCase();
                option.style.display = comunaName.includes(searchTerm) ? "" : "none";
            });
        }
        
        // Event listeners
        selector.addEventListener("change", function() {
            console.log("Selector changed");
            updateTraceVisibility();
        });
        
        searchBox.addEventListener("input", filterComunas);
        
        selectAllBtn.addEventListener("click", () => {
            console.log("Select all clicked");
            Array.from(selector.options).forEach(option => {
                if (option.style.display !== "none") {
                    option.selected = true;
                }
            });
            updateTraceVisibility();
        });
        
        clearAllBtn.addEventListener("click", () => {
            console.log("Clear all clicked");
            Array.from(selector.options).forEach(option => option.selected = false);
            updateTraceVisibility();
        });
        
        // Debug: mostrar información en consola cuando la página cargue
        console.log("Panel script loaded");
        console.log("Available comunas:", COMUNAS.length);
    </script>
</body>
</html>')
  
  # Guardar el HTML principal
  writeLines(html_content, output_file)
  cat("✓ HTML integrado guardado en:", output_file, "\n")
  
  # Verificar que el archivo se creó correctamente
  if (file.exists(output_file)) {
    cat("✓ Archivo creado exitosamente. Tamaño:", file.size(output_file), "bytes\n")
  } else {
    warning("⚠ Error: No se pudo crear el archivo HTML")
  }
}

# Crear y guardar HTML integrado para alcaldes
cat("Generando HTML integrado para alcaldes...\n")
create_integrated_widget(p_alcaldes, "alcaldesChart", "PISI Alcaldes por Comuna", comunas_list, colors, "outputs/pisi_alcaldes_interactive.html")

# Crear y guardar HTML integrado para concejales
cat("Generando HTML integrado para concejales...\n")
create_integrated_widget(p_concejales, "concejalesChart", "PISI Concejales por Comuna", comunas_list, colors, "outputs/pisi_concejales_interactive.html")

# ====================================================================
# EXPORTAR DATOS
# ====================================================================

cat("Exportando datos PISI por comuna...\n")

# Preparar datos para exportación (incluyendo CUT_COM)
pisi_export_all <- pisi_scores_all %>%
  select(
    CUT_COM,
    comuna = comuna_clean,
    año = anio_eleccion,
    elección = eleccion,
    pisi_score,
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

rio::export(export_list, "outputs/pisi_comuna_data.xlsx")

# ====================================================================
# CREAR PÁGINA HTML COMBINADA
# ====================================================================

cat("Creando página HTML con visualización por comuna...\n")

html_content <- '<!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Análisis PISI por Comuna - Chile</title>
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
        h3 {
            color: #666;
            margin-top: 30px;
            border-bottom: 1px solid #ddd;
            padding-bottom: 5px;
        }
        .description {
            margin: 20px 0;
            padding: 15px;
            background-color: #f9f9f9;
            border-left: 4px solid #4CAF50;
            font-size: 14px;
            line-height: 1.6;
        }
        .graph-container {
            margin-bottom: 40px;
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
        .download-btn.alcaldes {
            background-color: #d62728;
        }
        .download-btn.alcaldes:hover {
            background-color: #b91c1c;
        }
        .download-btn.concejales {
            background-color: #1f77b4;
        }
        .download-btn.concejales:hover {
            background-color: #1562a3;
        }
        iframe {
            width: 100%;
            height: 600px;
            border: 1px solid #ddd;
            border-radius: 5px;
        }
        .highlight {
            background-color: #ffffcc;
            padding: 2px 4px;
            border-radius: 3px;
        }
        .election-section {
            border: 1px solid #e0e0e0;
            border-radius: 8px;
            margin: 30px 0;
            padding: 20px;
        }
        .election-section.alcaldes {
            border-left: 4px solid #d62728;
        }
        .election-section.concejales {
            border-left: 4px solid #1f77b4;
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
        <h1>Análisis de Fortaleza Institucional Partidaria por Comuna</h1>
        
        <div class="description">
            <p>El Índice de Fortaleza Institucional Partidaria (PISI) es una medida compuesta que evalúa la capacidad organizacional y el peso electoral de los partidos en las elecciones municipales chilenas entre 2000 y 2024. Se construye a partir de cuatro indicadores estandarizados: porcentaje de candidatos militantes, porcentaje de electos militantes, penetración territorial y continuidad de incumbentes, los cuales se promedian para generar un valor entre 0 (máxima debilidad) y 1 (máxima fortaleza).</p>
            <p><strong>Panel lateral de control:</strong> Cada gráfico incluye un <span class="highlight">panel lateral con selector de comunas</span> que facilita la selección múltiple y búsqueda de territorios específicos. Esta nueva interfaz mejora significativamente la experiencia de análisis comparativo.</p>
            <p><strong>Cómo usar:</strong> Use el <strong>selector múltiple en el panel izquierdo</strong> para elegir las comunas a visualizar. Puede buscar comunas específicas, seleccionar todas, o limpiar la selección. Mantenga Ctrl/Cmd para seleccionar múltiples opciones.</p>
            <p><strong>Interactividad:</strong> Haz zoom seleccionando un área, arrastra para moverte, pasa el cursor sobre los puntos para ver detalles completos de cada comuna y año.</p>
        </div>
        
        <div class="election-section alcaldes">
            <h2>PISI Alcaldes por Comuna</h2>
            <div class="description">
                <p><strong>Análisis de alcaldes:</strong> El PISI para alcaldes muestra patrones específicos del poder ejecutivo municipal. Use el panel lateral para seleccionar y comparar múltiples comunas simultáneamente, analizando cómo varía la fortaleza partidaria entre territorios.</p>
            </div>
            <iframe src="pisi_alcaldes_interactive.html" title="PISI Alcaldes por Comuna" style="height: 800px;"></iframe>
        </div>
        
        <div class="election-section concejales">
            <h2>PISI Concejales por Comuna</h2>
            <div class="description">
                <p><strong>Análisis de concejales:</strong> El PISI para concejales refleja la dinámica del poder legislativo municipal. Utilice el selector de comunas en el panel izquierdo para comparar patrones entre diferentes territorios y contrastar con los resultados de alcaldes para entender diferencias en la competencia electoral.</p>
            </div>
            <iframe src="pisi_concejales_interactive.html" title="PISI Concejales por Comuna" style="height: 800px;"></iframe>
        </div>
        
        <div class="download-section">
            <a href="pisi_comuna_data.xlsx" class="download-btn" download>📊 Descargar datos completos (Excel - 3 hojas)</a>
        </div>
        
        <div class="description">
            <p><strong>Conclusiones:</strong> El análisis del PISI en Chile muestra que los partidos políticos tuvieron su mayor fortaleza institucional en las elecciones municipales de 2004 y 2008, pero desde 2012 se observa un declive sostenido, con niveles más cercanos al umbral de 0.5 y, en algunos casos, bastante por debajo. Aunque la presencia territorial se ha mantenido alta, la efectividad electoral y la capacidad de reelección de incumbentes militantes se han erosionado, reflejando un debilitamiento progresivo de los partidos frente al auge de candidaturas independientes y personalistas en el ámbito municipal.</p>
            <p><strong>Interpretación comunal:</strong> Un PISI más alto indica mayor fortaleza institucional de los partidos en esa comuna. Las diferencias entre comunas pueden reflejar dinámicas políticas locales, tradiciones partidarias regionales, o el impacto de liderazgos independientes locales. La separación por tipo de elección permite identificar si los partidos son más fuertes en el ejecutivo (alcaldes) o legislativo (concejales) municipal en cada territorio.</p>
        </div>
        
        <div style="margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; text-align: center; color: #666; font-size: 14px;">
            <p>Análisis de elecciones municipales chilenas • Datos: SERVEL • Elaboración propia</p>
        </div>
    </div>
</body>
</html>'

writeLines(html_content, "outputs/analisis_pisi_comuna.html")

# ====================================================================
# RESUMEN FINAL
# ====================================================================

cat("\n=== ANÁLISIS COMPLETADO EXITOSAMENTE ===\n")
cat("Archivos generados en outputs/:\n")
cat("- pisi_alcaldes_interactive.html (Gráfico PISI alcaldes con selector de comuna)\n")
cat("- pisi_concejales_interactive.html (Gráfico PISI concejales con selector de comuna)\n")
cat("- analisis_pisi_comuna.html (Página principal con instrucciones)\n")
cat("- pisi_comuna_data.xlsx (Datos PISI por comuna)\n\n")

# Estadísticas resumen
n_comunas <- pisi_data_full %>% 
  filter(comuna_clean != "NIVEL NACIONAL") %>% 
  pull(comuna_clean) %>% 
  n_distinct()

cat(sprintf("Total de comunas incluidas: %d\n", n_comunas))
cat(sprintf("Años analizados: %s\n\n", 
    paste(sort(unique(pisi_data_full$anio_eleccion)), collapse = ", ")))

cat("Para visualizar los resultados, abra 'outputs/analisis_pisi_comuna.html' en su navegador.\n")
cat("¡Análisis por comuna listo! 📊\n")