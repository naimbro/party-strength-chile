# Análisis de Fortaleza Electoral Partidaria por Macro Zona
# Script para generar visualización del PISI agregado por macro zona (Norte, Centro, Sur)

# ====================================================================
# CONFIGURACIÓN INICIAL
# ====================================================================

# Configurar codificación UTF-8
options(encoding = "UTF-8")

cat("=== ANÁLISIS DE FORTALEZA PARTIDARIA POR MACRO ZONA ===\n")
cat("Generando visualizaciones del índice agregado por 6 zonas geográficas...\n\n")

# Cargar paquetes requeridos
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, scales, readr, rio)

# ====================================================================
# CARGAR DATOS
# ====================================================================

cat("Cargando datos del índice por comuna...\n")
comuna_data <- readRDS("data/party_strength_comuna_data.rds")
pisi_scores_all <- comuna_data$pisi_scores_all

# Leer tabla de comunas con códigos de región
cat("Cargando tabla de mapeo comuna-región...\n")
tabla_comunas <- read_delim("../tabla_comunas_labmun.csv", 
                           delim = ";",
                           locale = locale(encoding = "ISO-8859-1"),
                           show_col_types = FALSE) %>%
  select(CUT_COM, CUT_REG, REGION) %>%
  mutate(CUT_COM = as.character(CUT_COM),
         CUT_REG = as.character(str_pad(CUT_REG, 2, pad = "0"))) %>%
  distinct()

# ====================================================================
# CREAR CLASIFICACIÓN POR MACRO ZONA
# ====================================================================

cat("Creando clasificación por macro zona...\n")

# Definir macro zonas (6 categorías)
macro_zonas <- tribble(
  ~CUT_REG, ~macro_zona,
  "15", "Norte",
  "01", "Norte",
  "02", "Norte",
  "03", "Norte Chico",
  "04", "Norte Chico",
  "05", "Centro",
  "13", "Centro",
  "06", "Centro",
  "07", "Centro Sur",
  "16", "Centro Sur",
  "08", "Centro Sur",
  "09", "Sur",
  "14", "Sur",
  "10", "Sur",
  "11", "Austral",
  "12", "Austral"
)

# Unir datos con información de región y macro zona
pisi_con_zona <- pisi_scores_all %>%
  filter(comuna_clean != "NIVEL NACIONAL") %>%  # Excluir nivel nacional
  left_join(tabla_comunas, by = "CUT_COM") %>%
  left_join(macro_zonas, by = "CUT_REG") %>%
  filter(!is.na(macro_zona))  # Filtrar comunas sin match

# Verificar cobertura
n_total <- nrow(pisi_scores_all %>% filter(comuna_clean != "NIVEL NACIONAL"))
n_matched <- nrow(pisi_con_zona)
cat(sprintf("Comunas con código regional: %d de %d (%.1f%%)\n", 
            n_matched, n_total, n_matched/n_total*100))

# ====================================================================
# CALCULAR PISI AGREGADO POR MACRO ZONA
# ====================================================================

cat("Calculando PISI agregado por macro zona...\n")

# Calcular promedio ponderado del PISI por macro zona
# Ponderamos por el número de comunas en cada zona
pisi_macrozona <- pisi_con_zona %>%
  group_by(anio_eleccion, eleccion, macro_zona) %>%
  summarise(
    # Promedio simple del PISI score
    pisi_score = mean(pisi_score, na.rm = TRUE),
    # Número de comunas incluidas
    n_comunas = n(),
    # Indicadores promedio para referencia
    pct_militante_avg = mean(pct_militante, na.rm = TRUE),
    pct_militante_win_avg = mean(pct_militante_win, na.rm = TRUE),
    .groups = "drop"
  )

# Mostrar resumen
cat("\nResumen de comunas por macro zona:\n")
pisi_con_zona %>%
  distinct(macro_zona, CUT_COM) %>%
  count(macro_zona) %>%
  mutate(macro_zona = factor(macro_zona, 
                             levels = c("Norte", "Norte Chico", "Centro", 
                                       "Centro Sur", "Sur", "Austral"))) %>%
  arrange(macro_zona) %>%
  print()

# ====================================================================
# CREAR GRÁFICOS POR MACRO ZONA
# ====================================================================

cat("\nCreando gráficos del PISI por macro zona...\n")

# Tema común para los gráficos
theme_macrozona <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray50", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom"
  )

# Colores para las macro zonas (6 categorías)
colores_zona <- c(
  "Norte" = "#e41a1c",         # Rojo
  "Norte Chico" = "#ff7f00",   # Naranja
  "Centro" = "#377eb8",        # Azul
  "Centro Sur" = "#984ea3",    # Púrpura
  "Sur" = "#4daf4a",           # Verde
  "Austral" = "#a65628"        # Café
)

# Filtrar datos para cada tipo de elección
pisi_alcaldes <- pisi_macrozona %>%
  filter(eleccion == "Alcaldes")

pisi_concejales <- pisi_macrozona %>%
  filter(eleccion == "Concejales")

# GRÁFICO 1: ALCALDES POR MACRO ZONA
cat("Creando gráfico para Alcaldes...\n")

p_alcaldes_zona <- ggplot(pisi_alcaldes, 
                          aes(x = anio_eleccion, y = pisi_score, 
                              color = macro_zona, group = macro_zona)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = colores_zona, name = "Macro zona") +
  scale_x_continuous(
    name = "Año de elección",
    breaks = seq(2005, 2025, 5),
    limits = c(2003, 2025)
  ) +
  scale_y_continuous(
    name = "Índice de Fortaleza Partidaria Municipal",
    limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    title = "Índice de Fortaleza Partidaria Municipal por Macro Zona - Alcaldes",
    subtitle = "Evolución temporal del desempeño electoral de los partidos políticos (2004-2024)"
  ) +
  theme_macrozona

# Guardar gráfico de alcaldes
ggsave("outputs/pisi_alcaldes_macrozona_6zonas.png", p_alcaldes_zona, 
       width = 14, height = 8, dpi = 300, bg = "white")

# GRÁFICO 2: CONCEJALES POR MACRO ZONA
cat("Creando gráfico para Concejales...\n")

p_concejales_zona <- ggplot(pisi_concejales, 
                            aes(x = anio_eleccion, y = pisi_score, 
                                color = macro_zona, group = macro_zona)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = colores_zona, name = "Macro zona") +
  scale_x_continuous(
    name = "Año de elección",
    breaks = seq(2005, 2025, 5),
    limits = c(2003, 2025)
  ) +
  scale_y_continuous(
    name = "Índice de Fortaleza Partidaria Municipal",
    limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    title = "Índice de Fortaleza Partidaria Municipal por Macro Zona - Concejales",
    subtitle = "Evolución temporal del desempeño electoral de los partidos políticos (2004-2024)"
  ) +
  theme_macrozona

# Guardar gráfico de concejales
ggsave("outputs/pisi_concejales_macrozona_6zonas.png", p_concejales_zona, 
       width = 14, height = 8, dpi = 300, bg = "white")

# GRÁFICO 3: PANEL COMBINADO (ALCALDES Y CONCEJALES)
cat("Creando panel combinado...\n")

# Combinar datos y agregar etiqueta de tipo de elección
pisi_combinado <- pisi_macrozona %>%
  mutate(eleccion_tipo = factor(eleccion, 
                                levels = c("Alcaldes", "Concejales")))

p_panel_zona <- ggplot(pisi_combinado, 
                       aes(x = anio_eleccion, y = pisi_score, 
                           color = macro_zona, group = macro_zona)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ eleccion_tipo, ncol = 2) +
  scale_color_manual(values = colores_zona, name = "Macro zona") +
  scale_x_continuous(
    name = "Año de elección",
    breaks = seq(2005, 2025, 5),
    limits = c(2003, 2025)
  ) +
  scale_y_continuous(
    name = "Índice de Fortaleza Partidaria Municipal",
    limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    title = "Índice de Fortaleza Partidaria Municipal por Macro Zona",
    subtitle = "Evolución temporal del desempeño electoral de los partidos políticos (2004-2024)"
  ) +
  theme_macrozona +
  theme(
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    panel.spacing = unit(1.5, "lines")
  )

# Guardar panel combinado
ggsave("outputs/pisi_panel_macrozona_6zonas.png", p_panel_zona, 
       width = 16, height = 8, dpi = 300, bg = "white")

# ====================================================================
# EXPORTAR DATOS AGREGADOS
# ====================================================================

cat("\nExportando datos agregados por macro zona...\n")

# Preparar datos para exportación
export_macrozona <- pisi_macrozona %>%
  select(
    año = anio_eleccion,
    elección = eleccion,
    macro_zona,
    indice_fortaleza = pisi_score,
    n_comunas,
    pct_candidatos_militantes = pct_militante_avg,
    pct_electos_militantes = pct_militante_win_avg
  ) %>%
  mutate(across(where(is.numeric) & !n_comunas, ~round(., 4))) %>%
  arrange(año, elección, macro_zona)

# Guardar en Excel
rio::export(export_macrozona, "outputs/pisi_macrozona_6zonas_data.xlsx")

# ====================================================================
# RESUMEN FINAL
# ====================================================================

cat("\n=== ANÁLISIS POR MACRO ZONA COMPLETADO ===\n")
cat("Archivos generados en outputs/:\n")
cat("- pisi_alcaldes_macrozona_6zonas.png\n")
cat("- pisi_concejales_macrozona_6zonas.png\n")
cat("- pisi_panel_macrozona_6zonas.png\n")
cat("- pisi_macrozona_6zonas_data.xlsx\n\n")

# Mostrar tendencias clave
cat("Tendencias del PISI en 2024:\n")
pisi_macrozona %>%
  filter(anio_eleccion == 2024) %>%
  arrange(eleccion, desc(pisi_score)) %>%
  mutate(pisi_score = round(pisi_score, 3)) %>%
  select(eleccion, macro_zona, pisi_score, n_comunas) %>%
  print()

cat("\n¡Análisis por macro zona completado exitosamente! 📊\n")