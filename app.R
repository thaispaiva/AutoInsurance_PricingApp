# ==============================================================================
# Alterosa Seguros: auto pricing desk
# Example application for the Auto Insurance Pricing Project
# Data: SUSEP anonymized auto database, Brazil, first half of 2019
# ==============================================================================
# This is ONE possible full-marks solution. It follows Option A of the case
# (frequency x severity) and is organized in the three required sections:
#   1. Exploratory analysis   2. Model   3. Premium calculator
#
# How the file is organized:
#   0. Setup and company identity
#   1. Data preparation
#   2. Models (fitted once, at startup)
#   3. Pricing functions used by the calculator
#   4. Plot helpers
#   5. User interface
#   6. Server
#
# Interpretation is left for the presentation: the app shows results only.
#
# Memory note (free hosting tiers have limited RAM): a glm object stores several
# copies of the data. We fit with model = FALSE, y = FALSE, compute every
# diagnostic once at startup, and then strip what prediction does not need
# (see slim_glm). Startup takes about 15 s.
# ==============================================================================


# ── 0. Setup ──────────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)     # hover tooltips on the charts
library(DT)

COMPANY  <- "Alterosa Seguros"      # fictional insurer: rename freely
TAGLINE  <- "Auto pricing desk"
CURRENCY <- "R$ "

# Thresholds that decide when the app warns about thin data
MIN_CLAIMS_REGION <- 30    # regions below this are pooled in the models
MIN_EXPOSURE      <- 100   # policy-years; below this a level is "thin"

# Colours used everywhere (UI and charts)
col_ink    <- "#12393D"
col_main   <- "#1F6F78"
col_soft   <- "#A9CBCB"
col_accent <- "#E9A23B"
col_muted  <- "#64757A"
col_faint  <- "#A9B4B6"   # non-significant coefficients
col_line   <- "#DDE4E2"


# ── 1. Data preparation ───────────────────────────────────────────────────────
# Column names in the CSV are kept in Portuguese, as published by SUSEP.
# The working data frame uses English names:
#   sexo -> sex | idade_condutor -> age_band | regiao -> region_code, region
#   ano_modelo -> veh_year | clas_bonus -> bonus | utilizacao -> use
#   exposicao -> exposure | n_sinistros -> claims
#   valor_sinistro -> claim_amount | pre_casco -> premium_charged

age_breaks <- c(17, 25, 35, 45, 55, 65, 75, Inf)
age_labels <- c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76+")
to_age_band <- function(age) {
  cut(floor(age), breaks = age_breaks, labels = age_labels)
}

raw <- read_csv(
  "auto_tarifacao_2019B.csv",
  col_types = cols(
    .default   = col_double(),
    regiao     = col_character(),
    sexo       = col_character(),
    clas_bonus = col_character(),
    utilizacao = col_character(),
    tipo_franq = col_character()
  )
)

# Defensive cleaning. The published file already comes clean; these filters
# only guarantee that the app also runs on a rawer extract.
dados <- raw |>
  mutate(across(c(regiao, sexo, clas_bonus, utilizacao), trimws)) |>
  filter(
    sexo %in% c("F", "M"),
    clas_bonus %in% as.character(0:9),
    !regiao %in% c("", "00", "99"), !is.na(regiao),
    exposicao > 0,
    valor_sinistro >= 0,
    idade_condutor >= 18, idade_condutor <= 100
  ) |>
  transmute(
    sex         = factor(sexo, levels = c("F", "M"), labels = c("Female", "Male")),
    age_band    = to_age_band(idade_condutor),
    region_code = regiao,
    veh_year    = as.integer(ano_modelo),
    bonus       = factor(clas_bonus, levels = as.character(0:9)),
    # utilizacao = 0 is not documented by SUSEP and covers about 1 in 5
    # policies. We keep it as its own level ("Not informed") instead of
    # dropping the rows or merging it silently with another category.
    use         = factor(utilizacao, levels = c("2", "1", "3", "0"),
                         labels = c("Daily commute", "Leisure",
                                    "Work-related", "Not informed")),
    exposure        = exposicao,
    claims          = as.integer(n_sinistros),
    claim_amount    = valor_sinistro,
    premium_charged = pre_casco
  )
rm(raw)

# Regions with very few claims cannot support their own coefficient.
# They are pooled into a single level, in BOTH models, so the two models
# always share exactly the same set of region levels.
region_claims <- dados |>
  filter(claim_amount > 0) |>
  group_by(region_code) |>
  summarise(n = sum(claims), .groups = "drop")

all_regions    <- sort(unique(dados$region_code))
pooled_regions <- setdiff(
  all_regions,
  region_claims$region_code[region_claims$n >= MIN_CLAIMS_REGION]
)
POOLED <- "Pooled"

# region_map: raw SUSEP code -> level used by the models
region_map <- setNames(ifelse(all_regions %in% pooled_regions, POOLED, all_regions),
                       all_regions)

# Base levels = the most common profile, so the intercept has a meaning
base_region <- names(which.max(table(dados$region_code)))
dados <- dados |>
  mutate(
    region   = relevel(factor(region_map[region_code]), ref = base_region),
    age_band = relevel(age_band, ref = "36-45")
  )
BASE_YEAR <- 2015L
year_min  <- min(dados$veh_year)
year_max  <- max(dados$veh_year)

# Claims subset for the severity model: average amount per claim
sinistros <- dados |>
  filter(claims > 0, claim_amount > 0) |>
  mutate(severity = claim_amount / claims)

# Headline numbers for the first tab
kpi <- list(
  policies   = nrow(dados),
  exposure   = sum(dados$exposure),
  claims     = sum(dados$claims),
  freq_year  = sum(dados$claims) / sum(dados$exposure),
  freq_pol   = sum(dados$claims) / nrow(dados),
  severity   = sum(sinistros$claim_amount) / sum(sinistros$claims),
  pure_prem  = sum(dados$claim_amount) / sum(dados$exposure),
  prem_avg   = mean(dados$premium_charged),
  share_zero = mean(dados$claims == 0)
)

# Rating factors available in the app: label -> column
factors_eda <- c("Driver's sex" = "sex", "Driver's age band" = "age_band",
                 "Risk region" = "region_code", "Vehicle model year" = "veh_year",
                 "Bonus class" = "bonus", "Vehicle use" = "use")

# Exposure observed at each level: used to flag thin data in every chart
support <- lapply(factors_eda, function(v) {
  dados |>
    group_by(level = as.character(.data[[v]])) |>
    summarise(exposure = sum(exposure), .groups = "drop")
})
names(support) <- unname(factors_eda)
is_thin <- function(var, levels) {
  s <- support[[var]]
  e <- s$exposure[match(as.character(levels), s$level)]
  is.na(e) | e < MIN_EXPOSURE
}


# ── 2. Models ─────────────────────────────────────────────────────────────────
# Frequency: Poisson GLM for the claim count, log(exposure) as offset, so the
# model describes claims PER POLICY-YEAR. Bonus class enters here only: the
# bonus scale rewards claim-free years, so it informs how often a driver
# claims, not how expensive the claim is.
mod_freq <- glm(
  claims ~ sex + age_band + region + I(veh_year - BASE_YEAR) + bonus + use +
    offset(log(exposure)),
  family = poisson(link = "log"), data = dados,
  model = FALSE, y = FALSE
)

# Severity: Gamma GLM for the AVERAGE amount per claim, claims subset only,
# weighted by the number of claims behind each average.
mod_sev <- glm(
  severity ~ sex + age_band + region + I(veh_year - BASE_YEAR) + use,
  family = Gamma(link = "log"), data = sinistros, weights = claims,
  model = FALSE, y = FALSE
)

# -- Coefficient tables --------------------------------------------------------
# Pearson dispersion of the Poisson model (1 = no overdispersion). Standard
# errors of the frequency model are inflated by sqrt(dispersion), as in a
# quasi-Poisson fit, so p-values are not too optimistic.
disp_freq <- sum(residuals(mod_freq, type = "pearson")^2) / mod_freq$df.residual

fmt_int0 <- function(x) formatC(round(x), format = "d", big.mark = ",")
nice_term <- function(x) {
  x <- sub("^sex", "Sex: ", x)
  x <- sub("^age_band", "Age ", x)
  x <- sub("^region", "Region ", x)
  x <- sub("^bonus", "Bonus class ", x)
  x <- sub("^use", "Use: ", x)
  x <- sub("^I\\(veh_year - BASE_YEAR\\)", "Model year (per newer year)", x)
  sub("^\\(Intercept\\)", "Base profile (intercept)", x)
}
coef_table <- function(model, se_scale = 1) {
  s  <- summary(model)$coefficients
  b  <- s[, "Estimate"]
  se <- s[, "Std. Error"] * se_scale
  p  <- 2 * pnorm(-abs(b / se))
  data.frame(
    Term         = nice_term(rownames(s)),
    Coefficient  = sprintf("%.3f", b),
    `Std. error` = sprintf("%.3f", se),
    `p-value`    = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    Relativity   = ifelse(exp(b) > 100, fmt_int0(exp(b)), sprintf("%.3f", exp(b))),
    `95% CI`     = ifelse(exp(b) > 100,
                          paste(fmt_int0(exp(b - 1.96 * se)), "to", fmt_int0(exp(b + 1.96 * se))),
                          sprintf("%.2f to %.2f", exp(b - 1.96 * se), exp(b + 1.96 * se))),
    Effect       = c("", sprintf("%+.0f%%", 100 * (exp(b[-1]) - 1))),  # intercept: no effect
    significant  = as.integer(p < 0.05),      # hidden column, drives the row colour
    check.names  = FALSE
  )
}
tab_freq <- coef_table(mod_freq, se_scale = sqrt(disp_freq))
tab_sev  <- coef_table(mod_sev)

# -- Goodness of fit (computed once, before the models are slimmed) ------------
# Observed vs predicted in 10 groups of equal size, sorted by the prediction
by_decile <- function(score, observed, predicted, weight) {
  data.frame(group = ntile(score, 10), observed, predicted, weight) |>
    group_by(group) |>
    summarise(Observed  = sum(observed) / sum(weight),
              Predicted = sum(predicted) / sum(weight), .groups = "drop")
}
dev_explained <- function(m) 1 - m$deviance / m$null.deviance

# Frequency. mu_f = expected number of claims of each policy (exposure included)
mu_f <- fitted(mod_freq)
fit_freq <- list(
  dispersion = disp_freq,
  dev_expl   = dev_explained(mod_freq),
  aic        = AIC(mod_freq),
  # how many policies with 0, 1, 2... claims the Poisson model expects
  counts = data.frame(
    Claims   = c("0", "1", "2", "3", "4+"),
    Observed = as.numeric(table(factor(pmin(dados$claims, 4), levels = 0:4))),
    Expected = c(sapply(0:3, function(k) sum(dpois(k, mu_f))),
                 sum(ppois(3, mu_f, lower.tail = FALSE)))
  ),
  deciles = by_decile(mu_f / dados$exposure, dados$claims, mu_f, dados$exposure)
)

# Severity. Quantile residuals: if the Gamma is adequate they are standard
# normal, so the Q-Q plot should follow the diagonal. They need the Gamma
# shape, estimated here by maximum likelihood with the fitted means fixed:
# average of w claims ~ Gamma(shape = w * alpha, mean = mu).
mu_s <- fitted(mod_sev)
gamma_loglik <- function(alpha) with(sinistros, sum(
  dgamma(severity, shape = claims * alpha, scale = mu_s / (claims * alpha), log = TRUE)))
shape_sev <- optimize(gamma_loglik, c(0.01, 50), maximum = TRUE)$maximum
qres_sev  <- with(sinistros, qnorm(pgamma(severity, shape = claims * shape_sev,
                                          scale = mu_s / (claims * shape_sev))))
pp <- ppoints(300)
fit_sev <- list(
  dispersion = summary(mod_sev)$dispersion,
  shape      = shape_sev,
  dev_expl   = dev_explained(mod_sev),
  aic        = AIC(mod_sev),
  n          = nrow(sinistros),
  qq      = data.frame(theoretical = qnorm(pp),
                       sample = as.numeric(quantile(qres_sev[is.finite(qres_sev)], pp))),
  deciles = by_decile(mu_s, sinistros$claim_amount, mu_s * sinistros$claims,
                      sinistros$claims)
)

# Pure premium = frequency x severity, for every policy in the portfolio
pred_loss <- mu_f * predict(mod_sev, newdata = dados, type = "response")
fit_pp <- list(
  balance = sum(pred_loss) / sum(dados$claim_amount),
  deciles = by_decile(pred_loss / dados$exposure, dados$claim_amount, pred_loss,
                      dados$exposure)
)
rm(mu_f, mu_s, qres_sev, pred_loss)

# Keep only what predict() needs
slim_glm <- function(m) {
  for (part in c("residuals", "fitted.values", "linear.predictors", "weights",
                 "prior.weights", "effects", "data", "offset")) m[[part]] <- NULL
  m$qr$qr <- NULL
  m
}
mod_freq <- slim_glm(mod_freq)
mod_sev  <- slim_glm(mod_sev)
invisible(gc())


# ── 3. Pricing functions ──────────────────────────────────────────────────────
# new_profiles(): builds model-ready rows. Factor levels are taken from the
# fitted models themselves, never retyped by hand: this is what prevents
# level mismatches between the two models at prediction time.
new_profiles <- function(sex, age_band, region_code, veh_year, bonus, use) {
  data.frame(
    sex      = factor(sex,      levels = mod_freq$xlevels$sex),
    age_band = factor(age_band, levels = mod_freq$xlevels$age_band),
    region   = factor(unname(region_map[region_code]),
                      levels = mod_freq$xlevels$region),
    veh_year = as.integer(veh_year),
    bonus    = factor(bonus,    levels = mod_freq$xlevels$bonus),
    use      = factor(use,      levels = mod_freq$xlevels$use),
    exposure = 1      # one full policy-year
  )
}

# price(): frequency, severity and pure premium for each row
price <- function(profiles) {
  freq <- as.numeric(predict(mod_freq, newdata = profiles, type = "response"))
  sev  <- as.numeric(predict(mod_sev,  newdata = profiles, type = "response"))
  data.frame(freq = freq, sev = sev, premium = freq * sev)
}

# check_profile(): decides whether a profile can be priced and which warnings
# to show. Returns list(ok, error, notes).
check_profile <- function(age, region_code, veh_year) {
  notes <- character(0)
  if (is.null(age) || is.na(age) || age < 18 || age > 100)
    return(list(ok = FALSE, error = "Driver's age must be between 18 and 100."))
  if (is.null(veh_year) || is.na(veh_year) || veh_year < year_min || veh_year > year_max)
    return(list(ok = FALSE, error = sprintf(
      "Model year must be between %d and %d, the range covered by the portfolio.",
      year_min, year_max)))
  if (!region_code %in% names(region_map))
    return(list(ok = FALSE, error = "Region not in the portfolio."))
  if (!region_map[[region_code]] %in% mod_sev$xlevels$region)
    return(list(ok = FALSE, error = "No claims observed in this region: severity cannot be estimated."))

  if (region_code %in% pooled_regions)
    notes <- c(notes, sprintf(
      "Region %s has fewer than %d claims in the portfolio. Priced with the pooled rate of low-volume regions.",
      region_code, MIN_CLAIMS_REGION))
  if (is_thin("age_band", as.character(to_age_band(age))) || age > 85)
    notes <- c(notes, sprintf(
      "Few policies at age %d. Priced with the rate of the %s band.",
      as.integer(age), to_age_band(age)))
  if (is_thin("veh_year", veh_year))
    notes <- c(notes, sprintf(
      "Model year %d has under %d policy-years in the portfolio. Priced by extending the fitted trend.",
      as.integer(veh_year), MIN_EXPOSURE))
  list(ok = TRUE, error = NULL, notes = notes)
}

# Prices along the levels of one variable, everything else fixed. Used for
# the relativities (Model tab) and for the response curve (Calculator tab).
base_args <- list(sex = "Female", age_band = "36-45", region_code = base_region,
                  veh_year = BASE_YEAR, bonus = "0", use = "Daily commute")
levels_of <- function(var) {
  switch(var,
         sex = levels(dados$sex), age_band = age_labels, region_code = all_regions,
         veh_year = year_min:year_max, bonus = as.character(0:9),
         use = levels(dados$use))
}
vary_profile <- function(args, var) {
  lv <- levels_of(var)
  args[[var]] <- lv
  out <- price(do.call(new_profiles, args))
  out$level <- factor(as.character(lv), levels = as.character(lv))
  s <- support[[var]]
  out$exposure <- s$exposure[match(as.character(lv), s$level)]
  out$exposure[is.na(out$exposure)] <- 0
  out$thin <- out$exposure < MIN_EXPOSURE
  if (var == "region_code") out$thin <- out$thin | lv %in% pooled_regions
  out
}

fmt_money <- function(x, digits = 0) {
  paste0(CURRENCY, formatC(x, format = "f", digits = digits, big.mark = ","))
}
fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_pct <- function(x) sprintf("%+.0f%%", 100 * (x - 1))


# ── 4. Plot helpers ───────────────────────────────────────────────────────────
theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(
      text               = element_text(colour = col_ink),
      axis.text          = element_text(colour = col_muted),
      axis.title         = element_text(colour = col_muted, size = 11),
      plot.subtitle      = element_text(colour = col_muted, size = 11),
      plot.title.position = "plot",
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = col_line),
      legend.position    = "top", legend.justification = "left",
      legend.title       = element_blank()
    )
}
lab_money <- scales::label_number(prefix = CURRENCY, big.mark = ",")

# ggplot -> plotly. Only the "text" aesthetic is shown on hover.
as_plotly <- function(g, legend = FALSE) {
  p <- ggplotly(g, tooltip = "text")
  # ggplotly creates one legend entry per layer, named like "(Frequency,1)":
  # clean the names and keep a single entry for each
  seen <- character(0)
  for (i in seq_along(p$x$data)) {
    nm <- p$x$data[[i]]$name
    nm <- if (is.null(nm)) "" else gsub("^\\(|(,1)?(,NA)?\\)$", "", nm)
    p$x$data[[i]]$name <- nm
    p$x$data[[i]]$legendgroup <- nm
    p$x$data[[i]]$showlegend <- legend && nm != "" && !nm %in% seen
    seen <- c(seen, nm)
  }
  p |>
    config(displayModeBar = FALSE) |>
    layout(showlegend = legend,
           legend = list(orientation = "h", x = 0, y = 1.15, title = list(text = "")),
           margin = list(t = if (legend) 40 else 10),
           font = list(family = "Public Sans, Segoe UI, Arial, sans-serif"),
           hoverlabel = list(bgcolor = "white", font = list(size = 12)),
           xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
}

# Bars by level of a factor. Levels with little data come in a lighter tone.
# `hover` is the tooltip text of each bar.
bar_by_level <- function(df, y, ylab, hover, money = FALSE) {
  df$hover <- hover
  df <- df[is.finite(df[[y]]), ]           # e.g. severity of a level with no claims
  many <- nrow(df) > 12
  g <- ggplot(df, aes(level, .data[[y]], fill = thin, text = hover)) +
    geom_col(width = 0.72) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(values = c(`FALSE` = col_main, `TRUE` = col_soft)) +
    scale_y_continuous(labels = if (money) lab_money else waiver(),
                       limits = c(0, 1.08 * max(df[[y]])), expand = c(0, 0)) +
    labs(x = NULL, y = ylab) +
    theme_app()
  if (many) g <- g + theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5))
  as_plotly(g)
}

# Observed vs predicted by decile
decile_plot <- function(d, ylab, money = TRUE) {
  fmt  <- if (money) function(x) fmt_money(x) else function(x) sprintf("%.3f", x)
  long <- rbind(data.frame(group = d$group, what = "Observed",  value = d$Observed),
                data.frame(group = d$group, what = "Predicted", value = d$Predicted))
  long$hover <- sprintf("Group %d<br>%s: %s", long$group, long$what, fmt(long$value))
  g <- ggplot(long, aes(group, value, colour = what, group = what, text = hover)) +
    geom_line(linewidth = 0.9) + geom_point(size = 2.6) +
    scale_colour_manual(values = c(Observed = col_accent, Predicted = col_ink)) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(labels = if (money) lab_money else waiver(), limits = c(0, NA)) +
    labs(x = "Group (1 = lowest prediction, 10 = highest)", y = ylab) +
    theme_app()
  as_plotly(g, legend = TRUE)
}

# Axis breaks for relativity charts (log scale): finer when the range is narrow
rel_breaks <- function(lims) {
  b <- if (lims[2] / lims[1] > 4) c(0.1, 0.15, 0.25, 0.5, 1, 2, 4, 8)
       else c(seq(0.3, 1.5, by = 0.1), 1.75, 2, 2.5, 3)
  b[b >= lims[1] & b <= lims[2]]
}


# ── 5. User interface ─────────────────────────────────────────────────────────
css <- sprintf("
  :root { --ink:%s; --main:%s; --accent:%s; --muted:%s; --line:%s;
          --bslib-navbar-default-bg:var(--ink); --bslib-navbar-inverse-bg:var(--ink); }
  body { background:#F5F7F6; color:var(--ink); font-variant-numeric: tabular-nums; }
  nav.navbar.navbar-default, nav.navbar.navbar-inverse, nav.navbar.navbar { background-color:var(--ink) !important; border:0; padding:.7rem 1.25rem; }
  body nav.navbar .navbar-brand, body nav.navbar .nav-link { color:#fff !important; }
  body nav.navbar .nav-link { opacity:.72; margin-left:.6rem; border-bottom:2px solid transparent; }
  body nav.navbar .nav-link.active, body nav.navbar .nav-link:hover { opacity:1; border-bottom-color:var(--accent) !important; }
  body nav.navbar .navbar-toggle, body nav.navbar .navbar-toggler { filter:invert(1); }
  .brand-name { font-weight:700; letter-spacing:-.01em; }
  .brand-tag { font-weight:400; opacity:.65; margin-left:.6rem; font-size:.9rem; }
  .page { max-width:1180px; margin:0 auto; padding:1.75rem 1rem 3rem; }
  .page-title { font-size:1.6rem; font-weight:700; letter-spacing:-.015em; margin:0 0 .3rem; }
  .lede { color:var(--muted); max-width:75ch; margin-bottom:1.5rem; }
  .card { border:1px solid var(--line); border-radius:10px; box-shadow:none; margin-bottom:1.25rem; }
  .card-header { background:#fff; border-bottom:1px solid var(--line); font-weight:600; }
  .hint { color:var(--muted); font-size:.88rem; }
  .plot-title { font-size:.95rem; font-weight:600; margin:0 0 .3rem; }
  .stats { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:1rem; margin-bottom:1.25rem; }
  .stat { background:#fff; border:1px solid var(--line); border-left:4px solid var(--main); border-radius:10px; padding:.9rem 1.1rem; }
  .stat-label { color:var(--muted); font-size:.85rem; }
  .stat-value { font-size:1.55rem; font-weight:700; line-height:1.25; }
  .stat-note { color:var(--muted); font-size:.8rem; }
  .stats-sm .stat { padding:.6rem .9rem; } .stats-sm .stat-value { font-size:1.2rem; }
  table.spec { width:100%%; font-size:.93rem; } table.spec th { color:var(--muted); font-weight:400; width:7.5rem; vertical-align:top; padding:.22rem 0; }
  table.spec td { padding:.22rem 0; }
  table.fit { width:100%%; font-size:.93rem; } table.fit th { color:var(--muted); font-weight:400; border-bottom:1px solid var(--line); }
  table.fit th, table.fit td { padding:.35rem .4rem; text-align:right; } table.fit th:first-child, table.fit td:first-child { text-align:left; }
  .quote { background:var(--ink); color:#fff; border-radius:12px; padding:1.4rem 1.6rem; margin-bottom:1rem; }
  .quote-row { display:flex; flex-wrap:wrap; align-items:flex-end; gap:.5rem 1.4rem; }
  .quote-part .lbl { font-size:.82rem; opacity:.7; display:block; }
  .quote-part .val { font-size:1.35rem; font-weight:600; }
  .quote-op { font-size:1.4rem; opacity:.5; padding-bottom:.1rem; }
  .quote-total .val { font-size:2.4rem; font-weight:700; color:var(--accent); line-height:1.1; }
  .quote-foot { border-top:1px solid rgba(255,255,255,.18); margin-top:1rem; padding-top:.8rem; font-size:.95rem; }
  .quote-foot b { font-size:1.15rem; }
  .note { border-radius:8px; padding:.7rem 1rem; margin-bottom:.6rem; font-size:.93rem; }
  .note-info { background:#FDF4E3; border:1px solid #F1D49B; }
  .note-stop { background:#FBEAE6; border:1px solid #E5B1A6; color:#7A2A1B; font-size:1rem; }
  .bslib-sidebar-layout > .sidebar { background:#fff; }
  table.dataTable { font-size:.88rem; }
  :focus-visible { outline:2px solid var(--accent); outline-offset:2px; }
", col_ink, col_main, col_accent, col_muted, col_line)

stat <- function(label, value, note = NULL) {
  div(class = "stat",
      div(class = "stat-label", label),
      div(class = "stat-value", value),
      if (!is.null(note)) div(class = "stat-note", note))
}
page <- function(title, lede, ...) {
  div(class = "page", h1(class = "page-title", title), p(class = "lede", lede), ...)
}
plot_block <- function(title, output) div(h3(class = "plot-title", title), output)
spec_table <- function(...) {
  rows <- list(...)
  tags$table(class = "spec", lapply(names(rows), function(n) tags$tr(tags$th(n), tags$td(rows[[n]]))))
}
thin_hint <- p(class = "hint", sprintf(
  "Lighter bars: little data (under %d policy-years, or a low-volume region).", MIN_EXPOSURE))

# Tab 1 ------------------------------------------------------------------------
tab_eda <- page(
  "Portfolio",
  "Own-damage coverage, private passenger vehicles. Policies in force from January to June 2019.",

  div(class = "stats",
      stat("Policies", fmt_int(kpi$policies),
           sprintf("%s policy-years of exposure", fmt_int(round(kpi$exposure)))),
      stat("Claim frequency", sprintf("%.3f", kpi$freq_year), "claims per policy-year"),
      stat("Average severity", fmt_money(kpi$severity), "per paid claim"),
      stat("Pure premium", fmt_money(kpi$pure_prem), "claim cost per policy-year"),
      stat("Average premium charged", fmt_money(kpi$prem_avg), "per policy")
  ),

  layout_columns(
    col_widths = c(5, 7),
    card(card_header("Claims per policy"),
         plotOutput("eda_counts", height = "300px")),
    card(card_header("Average amount per claim (log scale)"),
         plotOutput("eda_severity", height = "300px"))
  ),

  card(
    card_header("Frequency, severity and premium by rating factor"),
    card_body(
      selectInput("eda_var", "Rating factor", choices = factors_eda, width = "280px"),
      uiOutput("eda_panels"),
      uiOutput("eda_hint")
    )
  )
)

# Tab 2 ------------------------------------------------------------------------
factor_list <- "sex, age band, region, model year, vehicle use"

fit_table <- tags$table(
  class = "fit",
  tags$tr(tags$th("Claims"), tags$th("Policies observed"), tags$th("Expected (Poisson)"),
          tags$th("Observed / expected")),
  lapply(seq_len(nrow(fit_freq$counts)), function(i) {
    r <- fit_freq$counts[i, ]
    tags$tr(tags$td(r$Claims), tags$td(fmt_int(r$Observed)),
            tags$td(fmt_int(round(r$Expected))),
            tags$td(sprintf("%.2f", r$Observed / r$Expected)))
  })
)

tab_model <- page(
  "Pricing model",
  HTML("Pure premium = expected claim frequency &times; expected claim severity, each estimated with a GLM."),

  layout_columns(
    col_widths = c(6, 6),
    card(card_header("Frequency model"), card_body(spec_table(
      Model    = "Poisson GLM, log link",
      Response = "Number of claims",
      Offset   = "log(exposure), in policy-years",
      Factors  = paste0(factor_list, ", bonus class"),
      Data     = sprintf("%s policies", fmt_int(kpi$policies))
    ))),
    card(card_header("Severity model"), card_body(spec_table(
      Model    = "Gamma GLM, log link",
      Response = "Average amount per claim",
      Weights  = "Number of claims",
      Factors  = factor_list,
      Data     = sprintf("%s policies with a paid claim", fmt_int(fit_sev$n))
    )))
  ),
  p(class = "hint", sprintf(
    "Regions %s (fewer than %d claims) share one pooled level in both models.",
    paste(pooled_regions, collapse = ", "), MIN_CLAIMS_REGION)),

  card(
    card_header("Relativities by rating factor"),
    card_body(
      selectInput("rel_var", "Rating factor", choices = factors_eda,
                  selected = "age_band", width = "280px"),
      plotlyOutput("rel_plot", height = "360px"),
      p(class = "hint", sprintf(
        "Base profile = 1: female driver aged 36-45, region %s, model year %d, bonus class 0, daily commute. Pure premium relativity = frequency relativity x severity relativity.",
        base_region, BASE_YEAR))
    )
  ),

  navset_card_underline(
    title = "Coefficients",
    nav_panel("Frequency (Poisson)", DTOutput("tab_freq"),
              p(class = "hint px-3", sprintf(
                "Grey rows: not significant at 5%%. Relativity = exp(coefficient). Standard errors scaled by the Pearson dispersion (%.2f).",
                fit_freq$dispersion))),
    nav_panel("Severity (Gamma)", DTOutput("tab_sev"),
              p(class = "hint px-3",
                "Grey rows: not significant at 5%. Relativity = exp(coefficient)."))
  ),

  navset_card_underline(
    title = "Goodness of fit",
    nav_panel("Frequency", card_body(
      div(class = "stats stats-sm",
          stat("Pearson dispersion", sprintf("%.2f", fit_freq$dispersion), "1 = Poisson variance"),
          stat("Deviance explained", sprintf("%.1f%%", 100 * fit_freq$dev_expl)),
          stat("AIC", fmt_int(round(fit_freq$aic)))),
      layout_columns(
        col_widths = c(5, 7),
        plot_block("Policies by number of claims", fit_table),
        plot_block("Observed vs predicted frequency, by decile of predicted frequency",
                   plotlyOutput("fit_freq_plot", height = "300px"))
      )
    )),
    nav_panel("Severity", card_body(
      div(class = "stats stats-sm",
          stat("Dispersion", sprintf("%.2f", fit_sev$dispersion), "Pearson estimate"),
          stat("Gamma shape", sprintf("%.2f", fit_sev$shape), "maximum likelihood"),
          stat("Deviance explained", sprintf("%.1f%%", 100 * fit_sev$dev_expl)),
          stat("AIC", fmt_int(round(fit_sev$aic)))),
      layout_columns(
        col_widths = c(5, 7),
        plot_block("Q-Q plot of quantile residuals",
                   plotOutput("fit_sev_qq", height = "300px")),
        plot_block("Observed vs predicted severity, by decile of predicted severity",
                   plotlyOutput("fit_sev_plot", height = "300px"))
      )
    )),
    nav_panel("Pure premium", card_body(
      div(class = "stats stats-sm",
          stat("Predicted / observed claim cost", sprintf("%.3f", fit_pp$balance),
               "whole portfolio, 1 = balanced")),
      plot_block("Observed vs predicted pure premium, by decile of predicted pure premium",
                 plotlyOutput("fit_pp_plot", height = "320px"))
    ))
  )
)

# Tab 3 ------------------------------------------------------------------------
curve_choices <- c("Driver's age band" = "age_band", "Vehicle model year" = "veh_year",
                   "Bonus class" = "bonus", "Risk region" = "region_code",
                   "Vehicle use" = "use", "Driver's sex" = "sex")
region_choices <- setNames(all_regions, ifelse(
  all_regions %in% pooled_regions,
  paste0("Region ", all_regions, " (low volume)"), paste0("Region ", all_regions)))

tab_calc <- div(class = "page", style = "max-width:1280px;",
  layout_sidebar(
    sidebar = sidebar(
      width = 300, open = "always",
      h2("Customer profile", style = "font-size:1.05rem; font-weight:700;"),
      selectInput("in_sex", "Driver's sex", choices = levels(dados$sex)),
      numericInput("in_age", "Driver's age", value = 40, min = 18, max = 100, step = 1),
      selectInput("in_region", "Risk region (SUSEP code)", choices = region_choices,
                  selected = base_region),
      numericInput("in_year", "Vehicle model year", value = BASE_YEAR,
                   min = year_min, max = year_max, step = 1),
      selectInput("in_bonus", "Bonus class", choices = as.character(0:9)),
      div(class = "hint", style = "margin-top:-.6rem;",
          "0 = new customer, 9 = longest claim-free record."),
      # "Not informed" exists in the data and in the model, but a new customer
      # always declares the vehicle use, so it is not offered here.
      selectInput("in_use", "Vehicle use",
                  choices = setdiff(levels(dados$use), "Not informed")),
      hr(),
      sliderInput("in_load", "Loading (expenses and margin)", min = 0, max = 50,
                  value = 30, step = 5, post = "%")
    ),
    h1(class = "page-title", "Premium calculator"),
    p(class = "lede", "Annual premium for own-damage coverage."),
    uiOutput("quote"),
    uiOutput("quote_notes"),
    # Charts only appear when the profile can be priced
    conditionalPanel(
      "output.can_quote",
      card(
        card_header("Premium by rating factor"),
        card_body(
          selectInput("curve_var", "Factor to vary (rest of the profile fixed)",
                      choices = curve_choices, width = "360px"),
          plotlyOutput("curve_plot", height = "360px"),
          uiOutput("curve_hint")
        )
      ),
      card(card_header("Similar policies in the portfolio"),
           card_body(uiOutput("similar")))
    )
  )
)

ui <- page_navbar(
  title = span(span(class = "brand-name", COMPANY), span(class = "brand-tag", TAGLINE)),
  window_title = paste(COMPANY, "|", TAGLINE),
  theme = bs_theme(version = 5, primary = col_main,
                   base_font = font_collection("Public Sans", "Segoe UI", "Helvetica Neue",
                                               "Arial", "sans-serif")),
  header = tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;600;700&display=swap"),
    tags$style(HTML(css))
  ),
  fillable = FALSE,
  nav_panel("Portfolio",  tab_eda),
  nav_panel("Model",      tab_model),
  nav_panel("Calculator", tab_calc),
  nav_spacer(),
  nav_item(span(style = "color:#fff; opacity:.55; font-size:.85rem;",
                "Data: SUSEP, Brazil, Jan to Jun 2019"))
)


# ── 6. Server ─────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # -- Tab 1: distributions ----------------------------------------------------
  output$eda_counts <- renderPlot({
    dados |>
      count(k = ifelse(claims >= 3, "3+", as.character(claims))) |>
      mutate(share = n / sum(n)) |>
      ggplot(aes(k, share)) +
      geom_col(fill = col_main, width = 0.65) +
      geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
                vjust = -0.5, size = 3.8, colour = col_ink) +
      scale_y_continuous(labels = scales::label_percent(),
                         expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Claims in the period", y = "Share of policies") +
      theme_app()
  }, res = 96)

  output$eda_severity <- renderPlot({
    med <- median(sinistros$severity)
    ggplot(sinistros, aes(severity)) +
      geom_histogram(bins = 45, fill = col_main, colour = "white") +
      geom_vline(xintercept = c(med, kpi$severity), colour = col_accent,
                 linetype = c("dashed", "solid"), linewidth = 0.9) +
      scale_x_log10(labels = lab_money) +
      labs(subtitle = sprintf("Policies with a paid claim. Median %s (dashed), mean %s (solid).",
                              fmt_money(med), fmt_money(kpi$severity)),
           x = "Average amount per claim", y = "Policies") +
      theme_app()
  }, res = 96)

  # -- Tab 1: by rating factor -------------------------------------------------
  eda_by <- reactive({
    v <- input$eda_var
    dados |>
      group_by(level = .data[[v]]) |>
      summarise(
        policies  = n(),
        exposure  = sum(exposure),
        n_claims  = sum(claims),
        frequency = sum(claims) / sum(exposure),             # per policy-year
        severity  = sum(claim_amount) / sum(claims[claim_amount > 0]),
        premium   = mean(premium_charged),
        .groups = "drop"
      ) |>
      mutate(thin  = exposure < MIN_EXPOSURE |
               (v == "region_code" & level %in% pooled_regions),
             level = factor(level))
  })
  # Factors with many levels (region, model year) get full-width charts
  output$eda_panels <- renderUI({
    many <- input$eda_var %in% c("region_code", "veh_year")
    h <- if (many) "230px" else "320px"
    layout_columns(
      col_widths = if (many) c(12, 12, 12) else c(4, 4, 4),
      plot_block("Claim frequency",         plotlyOutput("eda_freq", height = h)),
      plot_block("Average severity",        plotlyOutput("eda_sev",  height = h)),
      plot_block("Average premium charged", plotlyOutput("eda_prem", height = h))
    )
  })
  output$eda_hint <- renderUI(if (any(eda_by()$thin)) thin_hint)
  eda_label <- reactive(names(factors_eda)[factors_eda == input$eda_var])
  output$eda_freq <- renderPlotly({
    d <- eda_by()
    bar_by_level(d, "frequency", "Claims per policy-year", hover = sprintf(
      "%s: %s<br>Frequency: %.3f<br>Claims: %s<br>Exposure: %s policy-years",
      eda_label(), d$level, d$frequency, fmt_int(d$n_claims), fmt_int(round(d$exposure))))
  })
  output$eda_sev <- renderPlotly({
    d <- eda_by()
    bar_by_level(d, "severity", "Per claim", money = TRUE, hover = sprintf(
      "%s: %s<br>Severity: %s<br>Claims: %s",
      eda_label(), d$level, fmt_money(d$severity), fmt_int(d$n_claims)))
  })
  output$eda_prem <- renderPlotly({
    d <- eda_by()
    bar_by_level(d, "premium", "Per policy", money = TRUE, hover = sprintf(
      "%s: %s<br>Premium charged: %s<br>Policies: %s",
      eda_label(), d$level, fmt_money(d$premium), fmt_int(d$policies)))
  })

  # -- Tab 2: relativities -----------------------------------------------------
  output$rel_plot <- renderPlotly({
    v    <- input$rel_var
    d    <- vary_profile(base_args, v)
    base <- price(do.call(new_profiles, base_args))
    long <- rbind(
      data.frame(level = d$level, part = "Frequency",    value = d$freq / base$freq),
      data.frame(level = d$level, part = "Severity",     value = d$sev / base$sev),
      data.frame(level = d$level, part = "Pure premium", value = d$premium / base$premium)
    )
    long$part  <- factor(long$part, levels = c("Frequency", "Severity", "Pure premium"))
    long$hover <- sprintf("%s<br>%s relativity: %.2f (%s)", long$level, long$part,
                          long$value, fmt_pct(long$value))
    many    <- nrow(d) > 12
    ordered <- v %in% c("age_band", "veh_year", "bonus")   # lines only if levels have an order
    g <- ggplot(long, aes(level, value, colour = part, group = part, text = hover)) +
      geom_hline(yintercept = 1, colour = col_muted, linetype = "dashed")
    if (ordered) g <- g + geom_line(linewidth = 0.6, alpha = 0.5)
    g <- g + geom_point(aes(size = part)) +
      scale_colour_manual(values = c(Frequency = "#4FA3A5", Severity = col_accent,
                                     `Pure premium` = col_ink)) +
      scale_size_manual(values = c(Frequency = 2.2, Severity = 2.2, `Pure premium` = 3.4)) +
      scale_y_continuous(trans = "log", breaks = rel_breaks) +
      labs(x = NULL, y = "Relativity (log scale)") +
      theme_app() +
      theme(axis.text.x = element_text(size = if (many) 9 else 11,
                                       angle = if (many) 90 else 0, vjust = 0.5))
    as_plotly(g, legend = TRUE)
  })

  # Coefficient tables: rows that are not significant at 5% are greyed out
  coef_dt <- function(tab) {
    hidden <- which(names(tab) == "significant") - 1
    datatable(tab, rownames = FALSE, class = "compact hover",
              options = list(dom = "t", paging = FALSE, scrollY = "380px", ordering = FALSE,
                             columnDefs = list(list(visible = FALSE, targets = hidden)))) |>
      formatStyle(names(tab), valueColumns = "significant",
                  color = styleEqual(c(0, 1), c(col_faint, col_ink)))
  }
  output$tab_freq <- renderDT(coef_dt(tab_freq))
  output$tab_sev  <- renderDT(coef_dt(tab_sev))

  # -- Tab 2: goodness of fit --------------------------------------------------
  output$fit_freq_plot <- renderPlotly(
    decile_plot(fit_freq$deciles, "Claims per policy-year", money = FALSE))
  output$fit_sev_plot <- renderPlotly(
    decile_plot(fit_sev$deciles, "Average amount per claim"))
  output$fit_pp_plot <- renderPlotly(
    decile_plot(fit_pp$deciles, "Pure premium per policy-year"))
  output$fit_sev_qq <- renderPlot({
    ggplot(fit_sev$qq, aes(theoretical, sample)) +
      geom_abline(colour = col_accent, linewidth = 0.9) +
      geom_point(colour = col_main, size = 1.4, alpha = 0.8) +
      labs(x = "Standard normal quantiles", y = "Quantile residuals") +
      theme_app() + theme(panel.grid.major.x = element_line(colour = col_line))
  }, res = 96)

  # -- Tab 3: calculator -------------------------------------------------------
  profile_args <- reactive({
    list(sex = input$in_sex, age_band = as.character(to_age_band(input$in_age)),
         region_code = input$in_region, veh_year = input$in_year,
         bonus = input$in_bonus, use = input$in_use)
  })
  status <- reactive(check_profile(input$in_age, input$in_region, input$in_year))
  output$can_quote <- reactive(status()$ok)
  outputOptions(output, "can_quote", suspendWhenHidden = FALSE)

  quote <- reactive({
    req(status()$ok)
    # tryCatch is the last safety net: the app never shows a raw R error
    tryCatch(price(do.call(new_profiles, profile_args())), error = function(e) NULL)
  })

  output$quote <- renderUI({
    st <- status()
    if (!st$ok) return(div(class = "note note-stop", tags$b("No quote. "), st$error))
    q <- quote()
    if (is.null(q) || !is.finite(q$premium))
      return(div(class = "note note-stop", tags$b("No quote. "),
                 "The model could not price this profile. Review the inputs."))
    commercial <- q$premium / (1 - input$in_load / 100)
    part <- function(lbl, val, cls = "") div(class = paste("quote-part", cls),
                                             span(class = "lbl", lbl), span(class = "val", val))
    div(class = "quote",
        div(class = "quote-row",
            part("Expected claims per year", sprintf("%.3f", q$freq)),
            span(class = "quote-op", HTML("&times;")),
            part("Expected cost per claim", fmt_money(q$sev)),
            span(class = "quote-op", "="),
            part("Pure premium", fmt_money(q$premium, 2), "quote-total")),
        div(class = "quote-foot",
            sprintf("Commercial premium (%d%% loading): ", input$in_load),
            tags$b(fmt_money(commercial, 2)),
            span(style = "opacity:.6;", " = pure premium / (1 - loading)")))
  })

  output$quote_notes <- renderUI({
    st <- status()
    if (!st$ok || length(st$notes) == 0) return(NULL)
    lapply(st$notes, function(n) div(class = "note note-info", tags$b("Low data. "), n))
  })

  output$curve_plot <- renderPlotly({
    req(status()$ok)
    v    <- input$curve_var
    args <- profile_args()
    d    <- vary_profile(args, v)
    lab  <- names(curve_choices)[curve_choices == v]
    d$state <- ifelse(as.character(d$level) == as.character(args[[v]]), "Selected profile",
                      ifelse(d$thin, "Little data", "Model estimate"))
    d$hover <- sprintf("%s: %s<br>Pure premium: %s<br>Frequency: %.3f<br>Severity: %s<br>Exposure in portfolio: %s policy-years",
                       lab, d$level, fmt_money(d$premium), d$freq, fmt_money(d$sev),
                       fmt_int(round(d$exposure)))

    if (v == "veh_year") {
      d$x <- as.integer(as.character(d$level))
      g <- ggplot(d, aes(x, premium, text = hover, group = 1)) +
        geom_line(colour = col_soft, linewidth = 0.9, linetype = "dashed") +
        geom_line(data = d[!d$thin, ], colour = col_main, linewidth = 1.1) +
        geom_point(colour = col_main, size = 1, alpha = 0.01) +        # hover targets
        geom_point(data = d[d$state == "Selected profile", ], colour = col_accent, size = 4)
      legend <- FALSE
    } else {
      fills <- c(`Model estimate` = col_main, `Little data` = col_soft,
                 `Selected profile` = col_accent)
      g <- ggplot(d, aes(level, premium, fill = state, text = hover)) +
        geom_col(width = 0.72) +
        scale_fill_manual(values = fills)
      legend <- TRUE
    }
    g <- g +
      scale_y_continuous(labels = lab_money, limits = c(0, 1.08 * max(d$premium)),
                         expand = c(0, 0)) +
      labs(x = NULL, y = "Pure premium per year") +
      theme_app()
    if (v == "region_code") g <- g + theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5))
    as_plotly(g, legend = legend)
  })

  output$curve_hint <- renderUI({
    txt <- switch(input$curve_var,
      veh_year    = sprintf("Dashed: model years with under %d policy-years in the portfolio (fitted trend extended).", MIN_EXPOSURE),
      age_band    = "Age enters the model in bands.",
      region_code = "Low-volume regions share one pooled rate.",
      NULL)
    if (!is.null(txt)) p(class = "hint", txt)
  })

  output$similar <- renderUI({
    req(status()$ok, quote())
    a <- profile_args()
    sim <- dados |> filter(sex == a$sex, age_band == a$age_band, region_code == a$region_code)
    head <- sprintf("Same sex, age band and region: %s policies.", fmt_int(nrow(sim)))
    if (nrow(sim) < 30)
      return(p(class = "hint", head, "Too few for an average."))
    commercial <- quote()$premium / (1 - input$in_load / 100)
    tagList(
      div(class = "stats stats-sm", style = "margin-bottom:.6rem;",
          stat("Average premium charged", fmt_money(mean(sim$premium_charged))),
          stat("Our commercial premium", fmt_money(commercial)),
          stat("Observed pure premium", fmt_money(sum(sim$claim_amount) / sum(sim$exposure)),
               "claim cost per policy-year")),
      p(class = "hint", head, "Not matched on vehicle, bonus class, use or deductible.")
    )
  })
}

shinyApp(ui, server)
