# =======================================================================
# Preprocessing: SUSEP R_AUTO + S_AUTO (2019, first semester)
# Auto Insurance Pricing App — teaching case
# =======================================================================
# Builds auto_tarifacao_2019B.csv from SUSEP's raw public files.
#
# REPRODUCIBILITY
#   Source: SUSEP anonymized auto insurance databases (2019, 1st semester)
#   https://www.gov.br/susep/pt-br/central-de-conteudos/
#     dados-estatisticos/bases-anonimizadas/bases_auto
#   Download the 2019B package and extract R_AUTO_2019B.csv and
#   S_AUTO_2019B.csv into this project's folder (or adjust the paths in
#   Section 0). The package also ships auxiliary code tables (e.g.
#   auto_reg.csv with the region codes reproduced in data_dictionary.qmd).
#   Field layouts and code tables are defined in SUSEP's "Manual de
#   Orientação para Envio de Dados" (Circular SUSEP 522/2015), Section 8.
#
#   The stratified sampling in Section 7 uses set.seed(2019), so rerunning
#   this script on the same raw files reproduces the published CSV exactly.
#   Section 9 runs automatic sanity checks against the published dataset's
#   known figures.
#
# Filters applied:
#   - COD_TARIF = "10"  -> private passenger vehicles, domestic
#   - COBERTURA = "1"   -> comprehensive coverage
#   - TIPO_PROD = "2"   -> profile-rated products (driver variables filled)
#   - COD_END   = "0"   -> original policies (no endorsements)
#   Data cleaning (Section 5b) removes small undocumented artifacts with
#   no pedagogical value (sex = 0, region codes 00/99/blank, bonus classes
#   * and X, negative claim amounts) and drops the effectively unfilled
#   tempo_hab field. The undocumented utilizacao = 0 category (~22%) is
#   intentionally KEPT and documented in data_dictionary.qmd: it carries a
#   distinct risk profile and is the case's real-data lesson.
# -----------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(duckdb)   # install.packages("duckdb") if needed

# -----------------------------------------------------------------------
# 0. Parameters — adjust the paths to where you extracted the SUSEP files
# -----------------------------------------------------------------------
csv_r       <- "R_AUTO_2019B.csv"        # policies file (extracted from ZIP)
csv_s       <- "S_AUTO_2019B.csv"        # claims file (extracted from ZIP)
output_file <- "auto_tarifacao_2019B.csv"

# Semester window (2019, first half)
window_start <- as.Date("2019-01-01")
window_end   <- as.Date("2019-06-30")

# -----------------------------------------------------------------------
# 1. Helper: SUSEP files use comma as decimal separator
# -----------------------------------------------------------------------
br_numeric <- function(x) as.numeric(gsub(",", ".", trimws(x)))

# -----------------------------------------------------------------------
# 2. Read with filters applied directly on the CSV (via DuckDB)
#    — only the necessary rows and columns are loaded into memory
# -----------------------------------------------------------------------
con <- dbConnect(duckdb())

message("Reading and filtering ", csv_r, "...")
r_raw <- dbGetQuery(con, sprintf("
  SELECT
    COD_APO, ITEM, REGIAO,
    INICIO_VIG, FIM_VIG, DATA_NASC,
    ANO_MODELO, VAL_FRANQ, IS_CASCO, PERC_BONUS, PRE_CASCO,
    TIPO_FRANQ, UTILIZACAO, CLAS_BONUS, SEXO
  FROM read_csv('%s',
    delim      = ';',
    quote      = '',
    all_varchar = true)
  WHERE trim(COD_TARIF) = '10'
    AND trim(COBERTURA) = '1'
    AND trim(TIPO_PROD) = '2'
    AND trim(COD_END)   = '0'
", csv_r))
message("  ", nrow(r_raw), " policies after filters.")

message("Reading and filtering ", csv_s, "...")
s_raw <- dbGetQuery(con, sprintf("
  SELECT COD_APO, ITEM, REGIAO, EVENTO, INDENIZ
  FROM read_csv('%s',
    delim      = ';',
    quote      = '',
    all_varchar = true)
  WHERE trim(EVENTO) = '1'
", csv_s))
message("  ", nrow(s_raw), " own-damage claims read.")

dbDisconnect(con, shutdown = TRUE)

# -----------------------------------------------------------------------
# 3. Transformations on the policies file
# -----------------------------------------------------------------------
message("Transforming policies...")

r_filtered <- r_raw |>
  mutate(
    # --- Dates ---
    inicio_vig = as.Date(trimws(INICIO_VIG), format = "%Y%m%d"),
    fim_vig    = as.Date(trimws(FIM_VIG),    format = "%Y%m%d"),

    # --- Exposure in policy-years, clipped to the semester window ---
    exp_start = pmax(inicio_vig, window_start),
    exp_end   = pmin(fim_vig,    window_end),
    exposicao = as.numeric(pmax(exp_end - exp_start + 1, 0)) / 365.25,

    # --- Driver age in years (reference date: start of window) ---
    data_nasc_condutor = as.Date(trimws(DATA_NASC), format = "%Y%m%d"),
    idade_condutor = as.numeric(
      difftime(window_start, data_nasc_condutor, units = "days")
    ) / 365.25,

    # --- Numeric variables (comma decimal -> point) ---
    ano_modelo = as.integer(trimws(ANO_MODELO)),
    val_franq  = br_numeric(VAL_FRANQ),
    is_casco   = br_numeric(IS_CASCO),
    perc_bonus = br_numeric(PERC_BONUS),
    pre_casco  = br_numeric(PRE_CASCO)
  ) |>
  filter(
    exposicao > 0,
    idade_condutor >= 18,
    idade_condutor <= 100,
    !is.na(data_nasc_condutor)
  )

message("  ", nrow(r_filtered), " policies after date and age filters.")

# -----------------------------------------------------------------------
# 4. Own-damage claims (already filtered by DuckDB, EVENTO = 1)
# -----------------------------------------------------------------------
message("Aggregating own-damage claims...")

s_claims <- s_raw |>
  mutate(across(c(COD_APO, ITEM, REGIAO), trimws),
         indeniz = br_numeric(INDENIZ)) |>
  group_by(COD_APO, ITEM, REGIAO) |>
  summarise(
    n_sinistros    = n(),
    valor_sinistro = sum(indeniz, na.rm = TRUE),
    .groups = "drop"
  )

message("  ", nrow(s_claims), " policy groups with at least 1 claim.")

# -----------------------------------------------------------------------
# 5. Join policies with claims
# -----------------------------------------------------------------------
message("Joining...")

base_final <- r_filtered |>
  left_join(s_claims, by = c("COD_APO", "ITEM", "REGIAO")) |>
  mutate(
    n_sinistros    = replace_na(n_sinistros,    0),
    valor_sinistro = replace_na(valor_sinistro, 0)
  ) |>
  select(
    # Identifier
    cod_apo        = COD_APO,

    # Risk / vehicle characteristics
    regiao         = REGIAO,
    ano_modelo,
    tipo_franq     = TIPO_FRANQ,
    val_franq,
    is_casco,
    utilizacao     = UTILIZACAO,

    # Bonus-malus
    clas_bonus     = CLAS_BONUS,
    perc_bonus,

    # Driver characteristics
    sexo           = SEXO,
    idade_condutor,

    # Exposure and observed premium
    exposicao,
    pre_casco,

    # Response variables
    n_sinistros,
    valor_sinistro
  )

# -----------------------------------------------------------------------
# 5b. Data cleaning
#     Removes small undocumented artifacts with no pedagogical value.
#     The utilizacao = 0 category (~22%, "not informed") is deliberately
#     kept — see data_dictionary.qmd.
# -----------------------------------------------------------------------
message("Cleaning...")
n_before <- nrow(base_final)

base_final <- base_final |>
  mutate(across(c(regiao, sexo, clas_bonus, tipo_franq, utilizacao), trimws)) |>
  filter(
    sexo %in% c("M", "F"),                       # 0 = not informed (~84 records)
    regiao %in% sprintf("%02d", 1:41),           # drops blank, 00 and 99 (~0.5%)
    clas_bonus %in% as.character(0:9),           # drops undocumented * and X (~2.8%)
    valor_sinistro >= 0                          # net recoveries break Gamma/Tweedie fits
  )

message("  ", n_before - nrow(base_final), " records removed (",
        round(100 * (n_before - nrow(base_final)) / n_before, 2), "%)")

# -----------------------------------------------------------------------
# 6. Diagnostic summary
# -----------------------------------------------------------------------
message("\n--- Full base (before sampling) ---")
message("Policies:                        ", nrow(base_final))
message("Total own-damage claims:         ", sum(base_final$n_sinistros))
message("Observed frequency:              ", round(
  sum(base_final$n_sinistros) / sum(base_final$exposicao), 4
))
message("Average severity (claims > 0):   BRL ", round(
  mean(base_final$valor_sinistro[base_final$n_sinistros > 0]), 2
))
message("% policies with a claim:         ", round(
  mean(base_final$n_sinistros > 0) * 100, 2
), "%")
message("Size in memory:                  ", round(
  object.size(base_final) / 1e6, 1
), " MB")

# -----------------------------------------------------------------------
# 7. Stratified sampling by region
# -----------------------------------------------------------------------
n_sample <- 200000  # adjust if needed

set.seed(2019)
sample_final <- base_final |>
  group_by(regiao) |>
  slice_sample(prop = n_sample / nrow(base_final)) |>
  ungroup()

message("\n--- Stratified sample (", n_sample, " policies) ---")
message("Policies:                        ", nrow(sample_final))
message("Total own-damage claims:         ", sum(sample_final$n_sinistros))
message("Observed frequency:              ", round(
  sum(sample_final$n_sinistros) / sum(sample_final$exposicao), 4
))
message("Average severity (claims > 0):   BRL ", round(
  mean(sample_final$valor_sinistro[sample_final$n_sinistros > 0]), 2
))
message("% policies with a claim:         ", round(
  mean(sample_final$n_sinistros > 0) * 100, 2
), "%")

# -----------------------------------------------------------------------
# 8. Save
# -----------------------------------------------------------------------
message("\nSaving ", output_file, "...")
write_csv(sample_final, output_file)
message("CSV size: ", round(file.size(output_file) / 1e6, 1), " MB")

# -----------------------------------------------------------------------
# 9. Sanity checks
#    Structural invariants of the cleaned dataset. A failed stopifnot()
#    indicates a pipeline change or a different raw file version.
# -----------------------------------------------------------------------
message("\n--- Sanity checks ---")

stopifnot(
  all(sample_final$exposicao > 0),
  max(sample_final$exposicao) <= as.numeric(window_end - window_start + 1) / 365.25 + 1e-9,
  all(sample_final$idade_condutor >= 18 & sample_final$idade_condutor <= 100),
  all(sample_final$n_sinistros >= 0),
  all(sample_final$valor_sinistro >= 0),
  # claim amounts only occur where claims were matched
  all(sample_final$valor_sinistro[sample_final$n_sinistros == 0] == 0),
  # cleaned categorical fields
  all(sample_final$sexo %in% c("M", "F")),
  all(sample_final$regiao %in% sprintf("%02d", 1:41)),
  all(sample_final$clas_bonus %in% as.character(0:9)),
  all(sample_final$utilizacao %in% as.character(0:3))
)
message("Structural invariants: OK")

# Reference figures (2019B raw data, seed 2019):
#   target sample size  ~200,000 policies
#   overall frequency   ~0.24 claims per policy-year
#   max exposure        ~0.4956 policy-years (181/365.25)
#   utilizacao = 0      ~22% ("not informed", deliberately kept)
message("Policies:           ", nrow(sample_final), "   (target: ~200,000)")
message("Overall frequency:  ",
        round(sum(sample_final$n_sinistros) / sum(sample_final$exposicao), 4),
        "   (reference: ~0.24)")
message("utilizacao = 0:     ",
        round(100 * mean(sample_final$utilizacao == "0"), 1), "%   (reference: ~22%)")
message("Done!")
