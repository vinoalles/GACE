# GACE 1.0.0

* Provides deterministic forecasting for weekly, monthly, quarterly,
  and yearly time series using the Generalized Adaptive Capped Estimator.

* Includes a structured preprocessing pipeline with support for:
  - handling of non-positive or missing values,
  - optional interpolation,
  - optional winsorization of extreme observations.

* Implements multiple growth components:
  - year-over-year,
  - short-term movement,
  - rolling-window behavior,
  - long-run drift.

* Growth components are combined using a trimmed, robust averaging
  framework to ensure stable signal extraction across different series types.

* Includes volatility-aware asymmetric caps that adapt to series
  characteristics and frequency.

* Provides optional seasonal scaling using smoothed, normalized
  seasonal factors derived from the historical pattern.

* Forecast generation uses a recursive formulation incorporating
  growth moderation (`gamma`) and level–growth blending (`beta`).

* The user-facing function `gace_forecast()` returns forecasted
  values in a simple, consistent structure.

* The helper function `plot_gace()` visualizes historical and projected
  values using ggplot2.

* Package includes documentation, examples, vignette, tests, and is
  structured for CRAN compatibility.