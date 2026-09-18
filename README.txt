ALTEROSA SEGUROS: AUTO PRICING DESK
Example application for the Auto Insurance Pricing Project

HOW TO RUN
1. Put app.R and auto_tarifacao_2019B.csv in the same folder.
2. Install the packages once:
   install.packages(c("shiny", "bslib", "readr", "dplyr", "ggplot2", "plotly", "DT", "scales"))
3. Open app.R in RStudio and click "Run App", or run shiny::runApp() in the folder.
   The models are fitted at startup, which takes about 15 seconds.

WHAT IS INSIDE
1. Portfolio: exploratory analysis. Frequency per policy-year, severity among
   policies with a paid claim, and premium charged, by rating factor.
2. Model: Option A of the case. Poisson GLM for frequency with log(exposure)
   offset, Gamma GLM for average severity on the claims subset. Relativities,
   coefficient tables (non-significant rows in grey) and goodness of fit:
   observed vs expected claim counts, Q-Q plot of quantile residuals, and
   observed vs predicted by decile for frequency, severity and pure premium.
3. Calculator: pure premium for a customer profile, premium by any rating
   factor, loading for a commercial premium, and a comparison with similar
   policies in the portfolio.
Charts with hover tooltips are built with ggplot2 and converted by plotly.
The app shows results only: interpretation is left for the presentation.

MODELING CHOICES WE SHOULD BE ABLE TO DEFEND
- Age enters in bands, so coefficients are easy to read.
- Regions with fewer than 30 claims are pooled, in both models, so the two
  models always share the same region levels.
- utilizacao = 0 is kept as its own level ("Not informed"), not dropped.
- Bonus class is used in the frequency model only.
- No variable selection: all factors are kept and interpreted.

USE OF AI TOOLS
This example was drafted with Claude (Anthropic): app structure, Shiny and
ggplot2 code, and interface text. All modeling choices and the final code
were reviewed by the instructor.
