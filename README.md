# Pricing Auto Insurance from Real Claims Data: Building a Ratemaking Application using Brazilian Regulatory Data

Teaching materials for a hands-on, end-to-end **ratemaking project** using real, anonymized auto insurance data published by SUSEP, the Brazilian insurance regulator. Students explore policy-level claims data, fit actuarial pricing models (frequency-severity GLMs or Tweedie), and build an interactive premium calculator in R Shiny.

Developed for the undergraduate course *Tarifação de Seguros* (Insurance Ratemaking) at the Universidade Federal de Minas Gerais (UFMG), Brazil, and submitted to the **CAS Global Teaching Materials Innovation Challenge (2026)**.

**Rendered documents:** [PAGES URL] · **Live demo of the example app:** [DEMO URL]

## Who this is for

Instructors of ratemaking, non-life insurance pricing, or applied GLM courses. The case is designed as a **capstone project**: it assumes students have already covered GLMs, exposure, and the frequency-severity decomposition. Introducing the case takes about 15 minutes of class time; students then work in teams over 2 to 3 weeks (roughly 12 to 15 hours out of class).

## Contents

| File | Description |
|---|---|
| `case_description.qmd` | Student-facing handout: instructions, data description, the three-part application structure, and assessment criteria |
| `data_dictionary.qmd` | Data provenance, filters, variable dictionary, and data-quality notes |
| `solution_outline.qmd` | Instructor-facing grading guide: what to look for in each part and common pitfalls |
| `CAS_submission.qmd` | The full submission document for the CAS Challenge |
| `index.qmd` | Landing page for the rendered site |
| `auto_tarifacao_2019B.csv` | The dataset: ~200,000 auto policies with claims, first half of 2019 (~17 MB) |
| `preprocessing_susep.R` | Documented script that builds the dataset from SUSEP's raw public files (for transparency; not needed to run the case) |
| `app.R` | Example application: one complete solution (frequency x severity), deployable on shinyapps.io's free tier |

## Working with this project

The repository is an **RStudio project** with documents written in [Quarto](https://quarto.org/).

1. Open `AutoInsurance_PricingApp.Rproj` in RStudio.
2. Edit any `.qmd` file and click **Render**, or render everything from the terminal:

   ```bash
   quarto render
   ```

3. Rendered HTML pages go to `docs/`. To publish them with **GitHub Pages**: on GitHub, go to *Settings > Pages*, set the source to *Deploy from a branch*, branch `main`, folder `/docs`.

To run the example app locally:

```r
shiny::runApp()
```

## Requirements

- R version 4.2 or later
- [Quarto](https://quarto.org/docs/get-started/) (bundled with recent versions of RStudio)
- Packages: `shiny`, `bslib`, `dplyr`, `ggplot2`, `DT`, `readr`, `scales`

```r
install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "DT", "readr", "scales"))
```

No paid services or logins are required. Teams may optionally publish their finished app to [shinyapps.io](https://www.shinyapps.io/) (free tier); `app.R` includes deployment notes on staying within the free tier's 1 GB memory limit.

## Data source and license

The underlying data are published by SUSEP in anonymized form and are freely available at the [official open data portal](https://www.gov.br/susep/pt-br/central-de-conteudos/dados-estatisticos/bases-anonimizadas/bases_auto). See `data_dictionary.qmd` for the exact filters and sampling applied.

## Author

Thaís Paiva Galletti — Departamento de Estatística, Universidade Federal de Minas Gerais (UFMG), Brazil.

Contact: [thaispaiva@est.ufmg.br](mailto:thaispaiva@est.ufmg.br)
