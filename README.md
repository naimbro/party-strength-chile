# Análisis de Fortaleza Institucional Partidaria en Chile

## Descripción

Este repositorio contiene un análisis interactivo de la fortaleza institucional de los partidos políticos en las elecciones municipales chilenas (2000-2024). Incluye dos visualizaciones principales:

1. **Índice de Fortaleza Institucional Partidaria (PISI)**: Medida compuesta que evalúa la evolución de la fortaleza partidaria a lo largo del tiempo
2. **Diagrama de Flujos**: Análisis de las transiciones entre tipos de alcaldes entre 2021 y 2024

## Visualizaciones Interactivas

🔗 **[Ver análisis completo](outputs/analisis_interactivo_completo.html)**

### Gráficos Individuales
- [Índice PISI](outputs/pisi_interactive.html)
- [Diagrama de Flujos](outputs/flow_interactive.html)

## Características

- **Gráficos interactivos** con zoom, hover y herramientas de navegación
- **Datos descargables** en formato Excel
- **Análisis temporal** desde 2000 hasta 2024
- **Metodología robusta** basada en datos oficiales de SERVEL

## Uso Rápido

### Ver las Visualizaciones
Simplemente abre cualquiera de los archivos HTML en tu navegador web:
- `outputs/analisis_interactivo_completo.html` - Página principal con ambos gráficos
- Para mejor experiencia, usa un servidor web local (ej: Live Server en VS Code)

### Reproducir el Análisis

1. **Requisitos**:
   ```r
   # Instalar paquetes necesarios
   if (!require(pacman)) install.packages("pacman")
   pacman::p_load(tidyverse, plotly, htmlwidgets, rio, scales, jsonlite)
   ```

2. **Ejecutar**:
   ```r
   # Desde la raíz del repositorio
   source("scripts/create_interactive_plots.R")
   ```

3. **Resultados**: Se generarán los archivos HTML y Excel en la carpeta `outputs/`

## Metodología

### Índice PISI
El PISI combina cuatro indicadores estandarizados:
- **Candidatos militantes**: % de candidatos que son militantes de partidos
- **Electos militantes**: % de ganadores que son militantes
- **Penetración territorial**: % de comunas con candidatos militantes
- **Continuidad incumbente**: Tasa de reelección de alcaldes militantes

Escala: 0 (mínima fortaleza) a 1 (máxima fortaleza)

### Diagrama de Flujos
Muestra las transiciones entre tres tipos de alcaldes:
- **Militante**: Miembro formal de partido político
- **Independiente cupo partido**: Sin militancia, en lista partidaria
- **Independiente fuera de pacto**: Completamente independiente

## Datos

### Fuente Principal
- **SERVEL** (Servicio Electoral de Chile): Resultados oficiales 1992-2024
- **Cobertura**: Todas las comunas de Chile
- **Periodicidad**: Elecciones municipales cada 4 años

### Archivos de Datos
- `data/party_strength_analysis_data.rds`: Datos procesados completos
- `outputs/pisi_data.xlsx`: Datos del índice PISI (descargable)
- `outputs/flow_data.xlsx`: Datos de flujos 2021-2024 (descargable)

## Estructura del Repositorio

```
party-strength-chile/
├── README.md                           # Este archivo
├── data/
│   └── party_strength_analysis_data.rds    # Datos procesados
├── scripts/
│   └── create_interactive_plots.R          # Script principal
├── outputs/
│   ├── analisis_interactivo_completo.html  # Análisis completo
│   ├── pisi_interactive.html               # Gráfico PISI
│   ├── flow_interactive.html               # Diagrama de flujos
│   ├── pisi_data.xlsx                      # Datos PISI
│   └── flow_data.xlsx                      # Datos flujos
```

## Resultados Principales

### Evolución del PISI (2000-2024)
- **2004-2008**: Máxima fortaleza partidaria (PISI > 0.7)
- **2012-2024**: Declive sostenido hacia debilitamiento
- **Diferencias**: Los partidos muestran mayor fortaleza en elecciones de concejales que de alcaldes

### Flujos 2021-2024
- **Continuidad**: Mayoría de comunas mantiene el mismo tipo de alcalde
- **Pérdida neta**: Los partidos perdieron 14 alcaldías frente a independientes
- **Patrón**: Migración desde militantes hacia candidaturas independientes

## Limitaciones

- Análisis limitado al ámbito municipal
- Período de flujos restringido a 2021-2024
- No incluye variables socioeconómicas o contextuales
- Ponderación igual para todos los indicadores del PISI

## Créditos

- **Datos**: SERVEL (Servicio Electoral de Chile)
- **Análisis**: Elaboración propia
- **Tecnología**: R, Plotly, HTML

## Licencia

Los datos electorales son de dominio público. El código y análisis están disponibles para uso académico y de investigación.

---

*Para preguntas o sugerencias sobre el análisis, por favor abre un issue en este repositorio.*