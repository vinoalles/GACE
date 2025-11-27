
# GACE <img src="man/figures/logo.png" align="right" width="120" />

> **G**eneralized **A**daptive **C**apped **E**stimator  
> A stable, deterministic forecasting engine for weekly, monthly,
> quarterly, and yearly data.

------------------------------------------------------------------------

## 📘 Overview

**GACE** provides a transparent, tuning-free forecasting method based on
hybrid growth signals and adaptive asymmetric caps.  
It extends deterministic capped-growth forecasting to handle:

- weekly,  
- monthly,  
- quarterly,  
- yearly

time-series with optional seasonal scaling.

GACE is designed for:

- demand forecasting  
- financial & portfolio forecasting  
- operational forecasting  
- budgeting & scenario planning

Its philosophy is simple:

### **Stable + Interpretable + Fast**

No nonlinear optimization. No stochastic fitting. Fully deterministic.

------------------------------------------------------------------------

## ✨ Features

| Capability                                              | Description |
|---------------------------------------------------------|-------------|
| Supports week / month / quarter / year                  | ✔️          |
| Hybrid growth signals (YoY, short-term, rolling, drift) | ✔️          |
| Volatility-aware asymmetric caps                        | ✔️          |
| Seasonal & non-seasonal modes                           | ✔️          |
| Fully deterministic (fast, no model fitting)            | ✔️          |
| Plot helper `plot_gace()`                               | ✔️          |
| CRAN-compatible design                                  | ✔️          |

------------------------------------------------------------------------

## 📦 Installation

### From GitHub (development version)

``` r
install.packages("devtools")
devtools::install_github("vinoalles/GACE")
```
