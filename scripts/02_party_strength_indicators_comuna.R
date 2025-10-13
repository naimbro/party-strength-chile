# Party Strength Analysis - Compute Indicators by Comuna
# Chilean Municipal Elections 1992-2024
# Modified version to calculate PISI at comuna level

# Load required packages
pacman::p_load(tidyverse, rio, janitor, readxl)

# Load prepared data
cat("Loading prepared data...\n")
municipal_data <- readRDS("data/party_strength_analysis_data.rds")$municipal_data

# Load comuna codes
cat("Loading comuna CUT codes...\n")
comuna_codes <- read_excel("tabla_comunas_labmun.xlsx") %>%
  clean_names() %>%
  mutate(
    # Standardize comuna names to match the data
    comuna_clean = toupper(comuna),
    comuna_clean = gsub("\\.", "", comuna_clean),
    comuna_clean = gsub("-", " ", comuna_clean),
    comuna_clean = trimws(comuna_clean)
  ) %>%
  # Rename the column back to uppercase after clean_names
  rename(CUT_COM = cut_com) %>%
  select(comuna_clean, CUT_COM)

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
  filter(won == 1) %>%
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
  filter(incumbent == 1) %>%
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
  group_by(eleccion, anio_eleccion) %>%
  mutate(
    # Standardize indicators within each year (to maintain comparability)
    std_pct_militante = standardize_indicator_grouped(pct_militante),
    std_pct_militante_win = standardize_indicator_grouped(pct_militante_win),
    std_has_militante = standardize_indicator_grouped(has_militante),
    std_reelection_militante = standardize_indicator_grouped(reelection_rate_militante),
    
    # Handle missing reelection data
    std_reelection_militante = ifelse(is.na(std_reelection_militante), 0.5, std_reelection_militante),
    
    # Create composite PISI score
    pisi_score = (std_pct_militante + std_pct_militante_win + 
                  std_has_militante + std_reelection_militante) / 4
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
    std_reelection_militante = ifelse(is.na(std_reelection_militante), 0.5, std_reelection_militante),
    
    pisi_score = (std_pct_militante + std_pct_militante_win + 
                  std_pct_communes_militante + std_reelection_militante) / 4
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