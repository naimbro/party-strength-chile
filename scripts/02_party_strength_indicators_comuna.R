# Party Strength Analysis - Compute Indicators by Comuna
# Chilean Municipal Elections 1992-2024
# Modified version to calculate PISI at comuna level

# Load required packages
pacman::p_load(tidyverse, rio, janitor, readxl)

# Load prepared data
cat("Loading prepared data...\n")
municipal_data <- readRDS("data/party_strength_analysis_data.rds")$municipal_data

# Robust function to standardize comuna names
standardize_comuna_name <- function(name) {
  if(is.na(name)) return(NA_character_)
  
  # Convert to character and trim whitespace
  name <- as.character(name)
  name <- trimws(name)
  
  # Convert to uppercase
  name <- toupper(name)
  
  # Remove accents and special characters
  name <- iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Handle common accent patterns that might not be caught
  name <- gsub("Á|À|Ä|Â", "A", name)
  name <- gsub("É|È|Ë|Ê", "E", name)
  name <- gsub("Í|Ì|Ï|Î", "I", name)
  name <- gsub("Ó|Ò|Ö|Ô", "O", name)
  name <- gsub("Ú|Ù|Ü|Û", "U", name)
  name <- gsub("Ñ", "N", name)
  
  # Remove dots and replace hyphens with spaces
  name <- gsub("\\.", "", name)
  name <- gsub("-", " ", name)
  name <- gsub("'", "", name)  # Remove apostrophes (e.g., O'Higgins -> OHIGGINS)
  
  # Standardize multiple spaces to single space
  name <- gsub("\\s+", " ", name)
  name <- trimws(name)
  
  # Handle specific known variations
  name <- case_when(
    name == "AYSEN" | name == "AISEN" ~ "AYSEN",
    name == "SANTIAGO CENTRO" ~ "SANTIAGO",
    name == "CABO DE HORNOS (EX NAVARINO)" ~ "CABO DE HORNOS",
    name == "CABO DE HORNOS" ~ "CABO DE HORNOS",
    name == "ANTARTICA" ~ "ANTARTICA",
    name == "OHIGGINS" ~ "OHIGGINS",  # From O'Higgins
    name == "RANQUIL" ~ "RANQUIL",
    name == "NIQUEN" ~ "NIQUEN",
    name == "PENALOLEN" ~ "PENALOLEN",
    name == "SANTA BARBARA" ~ "SANTA BARBARA",
    name == "HUALANE" ~ "HUALANE",
    name == "SAN FABIAN" ~ "SAN FABIAN",
    name == "VICHUQUEN" ~ "VICHUQUEN",
    name == "CHAITEN" ~ "CHAITEN",
    name == "HUALAIHUE" ~ "HUALAIHUE",
    name == "FUTALEUFU" ~ "FUTALEUFU",
    name == "COCHAMO" ~ "COCHAMO",
    name == "HUALPEN" ~ "HUALPEN",
    name == "MAULLIN" ~ "MAULLIN",
    name == "QUELLON" ~ "QUELLON",
    name == "QUEILEN" ~ "QUEILEN",
    name == "PUQUELDON" ~ "PUQUELDON",
    name == "CURACAUTIN" ~ "CURACAUTIN",
    name == "PITRUFQUEN" ~ "PITRUFQUEN",
    name == "TOLTEN" ~ "TOLTEN",
    name == "VILCUN" ~ "VILCUN",
    name == "MULCHEN" ~ "MULCHEN",
    name == "PUREN" ~ "PUREN",
    name == "TRAIGUEN" ~ "TRAIGUEN",
    name == "CANETE" ~ "CANETE",
    name == "PICHILEMU" ~ "PICHILEMU",
    name == "COMBARBALA" ~ "COMBARBALA",
    name == "CANELA" ~ "CANELA",
    name == "ILLAPEL" ~ "ILLAPEL",
    name == "OVALLE" ~ "OVALLE",
    name == "PUNITAQUI" ~ "PUNITAQUI",
    name == "VINA DEL MAR" ~ "VINA DEL MAR",
    name == "CALDERA" ~ "CALDERA",
    name == "CHANIARAL" ~ "CHANARAL",
    # Fix the remaining two problematic comunas
    name == "COYHAIQUE" ~ "COIHAIQUE",  # Coyhaique is spelled as Coihaique in CSV
    name == "LA CALERA" ~ "CALERA",     # La Calera is just Calera in CSV
    TRUE ~ name
  )
  
  return(name)
}

# Load comuna codes from CSV file with proper encoding
cat("Loading comuna CUT codes from CSV...\n")

# Try different methods to read the problematic CSV file
comuna_codes <- tryCatch({
  # First try with readr which handles encoding better
  if(require(readr, quietly = TRUE)) {
    readr::read_delim("CUT_comuna.csv", 
                      delim = ";", 
                      locale = readr::locale(encoding = "Latin1"),
                      show_col_types = FALSE)
  } else {
    # Fallback to base R
    read.csv2("CUT_comuna.csv", 
              fileEncoding = "Latin1", 
              stringsAsFactors = FALSE)
  }
}, error = function(e) {
  cat("Primary read failed, trying alternative method...\n")
  # Alternative: read as lines and parse manually
  lines <- readLines("CUT_comuna.csv", encoding = "Latin1")
  # Remove header
  data_lines <- lines[-1]
  # Split by semicolon
  split_data <- strsplit(data_lines, ";")
  # Create data frame
  data.frame(
    CUT_COM = sapply(split_data, function(x) if(length(x) >= 1) x[1] else NA),
    COMUNA = sapply(split_data, function(x) if(length(x) >= 2) x[2] else NA),
    stringsAsFactors = FALSE
  )
})

# Function to repair truncated comuna names
repair_comuna_names <- function(name) {
  if(is.na(name)) return(NA_character_)
  
  # Fix known truncated names based on the CUT codes and context
  name <- case_when(
    name == "Concepci" ~ "Concepción",
    name == "Santa B" ~ "Santa Bárbara",
    name == "Ñiqu" ~ "Ñiquén", 
    name == "Peñalol" ~ "Peñalolén",
    name == "Vichuqu" ~ "Vichuquén",
    name == "Hualañ" ~ "Hualañé",
    name == "San Fabián de Alico" ~ "San Fabián",
    name == "Cañet" ~ "Cañete",
    name == "Chaitén" ~ "Chaitén",
    name == "Hualaihué" ~ "Hualaihué",
    name == "Futaleufú" ~ "Futaleufú",
    name == "Cochamó" ~ "Cochamó", 
    name == "Maullín" ~ "Maullín",
    name == "Quellón" ~ "Quellón",
    name == "Queilén" ~ "Queilén",
    name == "Puqueldón" ~ "Puqueldón",
    name == "Curacautín" ~ "Curacautín",
    name == "Pitrufquén" ~ "Pitrufquén",
    name == "Toltén" ~ "Toltén",
    name == "Vilcún" ~ "Vilcún",
    name == "Mulchén" ~ "Mulchén",
    name == "Purén" ~ "Purén",
    name == "Traiguén" ~ "Traiguén",
    TRUE ~ name
  )
  
  return(name)
}

# Standardize the comuna names
comuna_codes <- comuna_codes %>%
  filter(!is.na(COMUNA), COMUNA != "") %>%
  mutate(
    # Keep original name for reference
    comuna_original = COMUNA,
    # Repair truncated names first
    comuna_repaired = sapply(COMUNA, repair_comuna_names),
    # Create standardized name for joining
    comuna_clean = sapply(comuna_repaired, standardize_comuna_name),
    # Ensure CUT_COM is character
    CUT_COM = as.character(CUT_COM)
  ) %>%
  filter(!is.na(comuna_clean), comuna_clean != "") %>%
  select(comuna_clean, CUT_COM, comuna_original, comuna_repaired) %>%
  # Remove any duplicates, keeping first occurrence
  distinct(comuna_clean, .keep_all = TRUE)

cat("Total comuna codes loaded:", nrow(comuna_codes), "\n")

# Show some examples of name processing
cat("\nExamples of name processing:\n")
if(nrow(comuna_codes) > 0) {
  sample_rows <- head(comuna_codes, 5)
  for(i in 1:nrow(sample_rows)) {
    cat(sprintf("Original: '%s' -> Repaired: '%s' -> Clean: '%s' (CUT: %s)\n",
        sample_rows$comuna_original[i],
        sample_rows$comuna_repaired[i], 
        sample_rows$comuna_clean[i],
        sample_rows$CUT_COM[i]))
  }
}

# Optimize standardization with caching for performance
cat("Optimizing comuna name standardization...\n")

# Create a lookup table for unique comuna names to avoid repeated processing
unique_comunas <- unique(municipal_data$comuna_clean[!is.na(municipal_data$comuna_clean)])
standardized_lookup <- setNames(sapply(unique_comunas, standardize_comuna_name), unique_comunas)

# Apply standardization using lookup table
municipal_data <- municipal_data %>%
  mutate(
    comuna_clean_original = comuna_clean,
    comuna_clean = standardized_lookup[comuna_clean]
  )

cat("Comuna names standardized in both datasets\n")

# Check how many comunas from municipal data will get CUT codes
comunas_in_data <- unique(municipal_data$comuna_clean[!is.na(municipal_data$comuna_clean)])
comunas_with_cut <- intersect(comunas_in_data, comuna_codes$comuna_clean)
cat(sprintf("Comunas in data: %d, will get CUT codes: %d (%.1f%%)\n", 
    length(comunas_in_data), 
    length(comunas_with_cut),
    length(comunas_with_cut) / length(comunas_in_data) * 100))

# Helper function to standardize indicators (0-1 scale) within groups
standardize_indicator_grouped <- function(x) {
  if(all(is.na(x))) return(rep(NA_real_, length(x)))
  if(min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) return(rep(0.5, length(x)))
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# =====================================================
# 1. CANDIDATE SHARES BY COMUNA
# =====================================================

cat("Computing candidate shares by comuna...\n")

candidate_shares_comuna <- municipal_data %>%
  # Filter out rows with empty or invalid comuna names
  filter(!is.na(comuna_clean), 
         comuna_clean != "", 
         trimws(comuna_clean) != "") %>%
  group_by(anio_eleccion, eleccion, comuna_clean) %>%
  summarise(
    total_candidates = n(),
    militante_candidates = sum(candidate_type == "Militante"),
    indep_in_candidates = sum(candidate_type == "Independiente cupo partido"),
    indep_out_candidates = sum(candidate_type == "Independiente fuera de pacto"),
    .groups = "drop"
  ) %>%
  mutate(
    pct_militante = militante_candidates / total_candidates * 100,
    pct_indep_in = indep_in_candidates / total_candidates * 100,
    pct_indep_out = indep_out_candidates / total_candidates * 100
  )

# =====================================================
# 2. WINNER SHARES BY COMUNA
# =====================================================

cat("Computing winner shares by comuna...\n")

winner_shares_comuna <- municipal_data %>%
  # Filter out rows with empty or invalid comuna names
  filter(!is.na(comuna_clean), 
         comuna_clean != "", 
         trimws(comuna_clean) != "",
         won == 1) %>%
  group_by(anio_eleccion, eleccion, comuna_clean) %>%
  summarise(
    total_winners = n(),
    militante_winners = sum(candidate_type == "Militante"),
    indep_in_winners = sum(candidate_type == "Independiente cupo partido"),
    indep_out_winners = sum(candidate_type == "Independiente fuera de pacto"),
    .groups = "drop"
  ) %>%
  mutate(
    pct_militante_win = militante_winners / total_winners * 100,
    pct_indep_in_win = indep_in_winners / total_winners * 100,
    pct_indep_out_win = indep_out_winners / total_winners * 100
  )

# =====================================================
# 3. PARTY PRESENCE (Binary indicator for comuna)
# =====================================================

cat("Computing party presence by comuna...\n")

party_presence_comuna <- municipal_data %>%
  # Filter out rows with empty or invalid comuna names
  filter(!is.na(comuna_clean), 
         comuna_clean != "", 
         trimws(comuna_clean) != "") %>%
  group_by(anio_eleccion, eleccion, comuna_clean) %>%
  summarise(
    has_militante = as.numeric(any(candidate_type == "Militante")),
    has_militante_winner = as.numeric(any(candidate_type == "Militante" & won == 1)),
    .groups = "drop"
  )

# =====================================================
# 4. INCUMBENCY CONTINUITY BY COMUNA
# =====================================================

cat("Computing incumbency continuity by comuna...\n")

incumbency_comuna <- municipal_data %>%
  # Filter out rows with empty or invalid comuna names
  filter(!is.na(comuna_clean), 
         comuna_clean != "", 
         trimws(comuna_clean) != "",
         incumbent == 1) %>%
  group_by(anio_eleccion, eleccion, comuna_clean, candidate_type) %>%
  summarise(
    incumbent_candidates = n(),
    incumbent_reelected = sum(won, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(candidate_type == "Militante") %>%
  mutate(
    reelection_rate_militante = incumbent_reelected / incumbent_candidates * 100
  ) %>%
  select(anio_eleccion, eleccion, comuna_clean, reelection_rate_militante)

# =====================================================
# 5. COMPUTE COMUNA-LEVEL PISI
# =====================================================

cat("Computing comuna-level PISI scores...\n")

# Combine all indicators
pisi_data_comuna <- candidate_shares_comuna %>%
  left_join(winner_shares_comuna, by = c("anio_eleccion", "eleccion", "comuna_clean")) %>%
  left_join(party_presence_comuna, by = c("anio_eleccion", "eleccion", "comuna_clean")) %>%
  left_join(incumbency_comuna, by = c("anio_eleccion", "eleccion", "comuna_clean"))

# Calculate standardized indicators for PISI
pisi_scores_comuna <- pisi_data_comuna %>%
  group_by(eleccion) %>%
  mutate(
    # Standardize indicators across all years for temporal comparability
    std_pct_militante = standardize_indicator_grouped(pct_militante),
    std_pct_militante_win = standardize_indicator_grouped(pct_militante_win),
    std_has_militante = standardize_indicator_grouped(has_militante),
    std_reelection_militante = standardize_indicator_grouped(reelection_rate_militante),
    
    # Create composite PISI score using only available indicators
    pisi_score = ifelse(is.na(std_reelection_militante),
                       # Si falta reelección, promedio de 3 indicadores
                       (std_pct_militante + std_pct_militante_win + std_has_militante) / 3,
                       # Si está disponible, promedio de 4 indicadores
                       (std_pct_militante + std_pct_militante_win + std_has_militante + std_reelection_militante) / 4)
  ) %>%
  ungroup()

# =====================================================
# CALCULATE NATIONAL PISI (for comparison)
# =====================================================

cat("Computing national PISI scores...\n")

pisi_scores_national <- pisi_data_comuna %>%
  group_by(anio_eleccion, eleccion) %>%
  summarise(
    # Weighted averages for national level
    total_candidates_nat = sum(total_candidates),
    militante_candidates_nat = sum(militante_candidates),
    total_winners_nat = sum(total_winners, na.rm = TRUE),
    militante_winners_nat = sum(militante_winners, na.rm = TRUE),
    comunas_with_militante = sum(has_militante, na.rm = TRUE),
    total_comunas = n(),
    avg_reelection_rate = mean(reelection_rate_militante, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_militante = militante_candidates_nat / total_candidates_nat * 100,
    pct_militante_win = militante_winners_nat / total_winners_nat * 100,
    pct_communes_militante = comunas_with_militante / total_comunas * 100,
    reelection_militante = avg_reelection_rate,
    comuna_clean = "NIVEL NACIONAL"  # Special identifier for national level
  ) %>%
  group_by(eleccion) %>%
  mutate(
    std_pct_militante = standardize_indicator_grouped(pct_militante),
    std_pct_militante_win = standardize_indicator_grouped(pct_militante_win),
    std_pct_communes_militante = standardize_indicator_grouped(pct_communes_militante),
    std_reelection_militante = standardize_indicator_grouped(reelection_militante),
    # Para el cálculo del índice, usar solo indicadores disponibles
    available_indicators = 3 + ifelse(is.na(std_reelection_militante), 0, 1),
    
    pisi_score = ifelse(is.na(std_reelection_militante),
                       # Si falta reelección, promedio de 3 indicadores
                       (std_pct_militante + std_pct_militante_win + std_pct_communes_militante) / 3,
                       # Si está disponible, promedio de 4 indicadores  
                       (std_pct_militante + std_pct_militante_win + std_pct_communes_militante + std_reelection_militante) / 4)
  ) %>%
  ungroup()

# Combine comuna and national data
pisi_scores_all <- bind_rows(
  pisi_scores_comuna %>%
    select(anio_eleccion, eleccion, comuna_clean, pisi_score, 
           pct_militante, pct_militante_win, has_militante, reelection_rate_militante),
  pisi_scores_national %>%
    select(anio_eleccion, eleccion, comuna_clean, pisi_score,
           pct_militante, pct_militante_win, pct_communes_militante, reelection_militante) %>%
    rename(has_militante = pct_communes_militante,
           reelection_rate_militante = reelection_militante)
)

# Add CUT_COM codes
pisi_scores_all <- pisi_scores_all %>%
  left_join(comuna_codes, by = "comuna_clean") %>%
  mutate(
    # Add a special code for national level
    CUT_COM = ifelse(comuna_clean == "NIVEL NACIONAL", "00000", CUT_COM)
  ) %>%
  # Reorder columns to have CUT_COM early
  select(anio_eleccion, eleccion, CUT_COM, comuna_clean, everything())

# =====================================================
# EXPORT RESULTS
# =====================================================

cat("Exporting comuna-level PISI results...\n")

# Add CUT_COM to individual datasets as well
pisi_scores_comuna <- pisi_scores_comuna %>%
  left_join(comuna_codes, by = "comuna_clean") %>%
  select(anio_eleccion, eleccion, CUT_COM, comuna_clean, everything())

pisi_scores_national <- pisi_scores_national %>%
  mutate(CUT_COM = "00000") %>%
  select(anio_eleccion, eleccion, CUT_COM, comuna_clean, everything())

# Save the complete dataset
saveRDS(list(
  pisi_scores_comuna = pisi_scores_comuna,
  pisi_scores_national = pisi_scores_national,
  pisi_scores_all = pisi_scores_all,
  municipal_data = municipal_data,
  comuna_codes = comuna_codes
), "data/party_strength_comuna_data.rds")

# Export CSV for inspection
write.csv(pisi_scores_all, "outputs/pisi_scores_by_comuna.csv", row.names = FALSE)

# Summary statistics
cat("\n=== COMUNA-LEVEL PISI ANALYSIS SUMMARY ===\n")
cat(sprintf("Total comunas analyzed: %d\n", n_distinct(pisi_scores_comuna$comuna_clean)))
cat(sprintf("Years covered: %s\n", paste(sort(unique(pisi_scores_comuna$anio_eleccion)), collapse = ", ")))

# Show sample of comunas with highest/lowest PISI in 2024
if(2024 %in% pisi_scores_comuna$anio_eleccion) {
  cat("\nTop 5 comunas by PISI score (Alcaldes 2024):\n")
  pisi_scores_comuna %>%
    filter(anio_eleccion == 2024, eleccion == "Alcaldes") %>%
    arrange(desc(pisi_score)) %>%
    slice_head(n = 5) %>%
    select(comuna_clean, pisi_score) %>%
    print()
  
  cat("\nBottom 5 comunas by PISI score (Alcaldes 2024):\n")
  pisi_scores_comuna %>%
    filter(anio_eleccion == 2024, eleccion == "Alcaldes") %>%
    arrange(pisi_score) %>%
    slice_head(n = 5) %>%
    select(comuna_clean, pisi_score) %>%
    print()
}

cat("\nComuna-level PISI analysis complete!\n")