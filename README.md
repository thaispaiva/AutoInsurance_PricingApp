# Pricing Auto Insurance from Real Claims Data: Building a Ratemaking Application using Brazilian Regulatory Data

Teaching materials for a hands-on, end-to-end **ratemaking project** using real, anonymized auto insurance data published by SUSEP, the Brazilian insurance regulator. Students explore policy-level claims data, fit actuarial pricing models (frequency-severity GLMs or Tweedie), and build an interactive premium calculator in R Shiny.

Developed for the undergraduate course *Tarifação de Seguros* (Insurance Ratemaking) at the Universidade Federal de Minas Gerais (UFMG), Brazil, and submitted to the **CAS Global Teaching Materials Innovation Challenge (2026)**.

**Rendered documents:** <https://thaispaiva.github.io/AutoInsurance_PricingApp/>

**Live demo of the example app:** <https://thaispaiva-autoinsurance-pricingapp.share.connect.posit.cloud>

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
| `app.R` | Example application: one complete solution (frequency x severity), with goodness-of-fit checks and a premium calculator |
| `README.txt` | Notes that accompany the example application, in the format students are asked to deliver |
| `manifest.json` | Dependency file used to deploy the example app to Posit Connect Cloud |
| `LICENSE` | CC0 1.0 Universal: free to use, adapt and share, no attribution required |

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
- Packages: `shiny`, `bslib`, `dplyr`, `ggplot2`, `plotly`, `DT`, `readr`, `scales`

```r
install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "plotly", "DT", "readr", "scales"))
```

No paid services are required. Teams may optionally publish their finished app to [Posit Connect Cloud](https://connect.posit.cloud/) (free plan), which deploys from a public GitHub repository; `app.R` includes notes on keeping memory use low on free hosting tiers.

## Data source

The underlying data are published by SUSEP in anonymized form and are freely available at the [official open data portal](https://www.gov.br/susep/pt-br/central-de-conteudos/dados-estatisticos/bases-anonimizadas/bases_auto). See `data_dictionary.qmd` for the exact filters and sampling applied.

## License

The case documents, the example application, the preprocessing script and the prepared dataset are released under [CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/), a public domain dedication: anyone may copy, adapt and redistribute them, for any purpose, without asking permission and without attribution. Instructors are welcome to translate the case, change the data or reuse any part of it in their own courses. The full text is in the `LICENSE` file.

## Use of AI tools

Claude (Anthropic) was used as an assistant in preparing these materials: drafting and refactoring the code of the example Shiny application, reviewing the English text of the documents, and checking the consistency between the case description, the data dictionary, the solution outline and the dataset. The case design, the choice and preparation of the SUSEP data, the modeling decisions and the assessment criteria are the author's own. The case was first run with students in the *Tarifação de Seguros* course at UFMG, and all AI-assisted content was reviewed and tested by the author.

## Author

Thaís Paiva Galletti, Departamento de Estatística, Universidade Federal de Minas Gerais (UFMG), Brazil.

Contact: [thaispaiva@est.ufmg.br](mailto:thaispaiva@est.ufmg.br)
