
# PACE <img src="man/figures/logo.png" align="right" width="120"/>

> **P**roportional **A**djusted **C**apped **E**stimator:  
> A lightweight, transparent forecasting algorithm blending growth-rate
> signals with volatility controls.

The **PACE** R package provides a simple forecasting method inspired by
business planning logic: - Uses **YoY**, **QoQ**, and **Prior-Quarter**
growth - Applies **capping rules** to prevent unrealistic jumps - Works
directly on **date + numeric** time series - No black-box machine
learning — fully transparent

PACE is designed for **business analysts, forecasters, and FP&A teams**
who want a **fast, explainable** alternative to ARIMA/ETS/Prophet for
short-term planning.

------------------------------------------------------------------------

## ✨ Features

| Capability | Description |
|------------|-------------|

✅ Quarterly forecasting  
✅ Growth-rate blending  
✅ Volatility/Outlier caps  
✅ Seasonal version  
✅ Plot helper  
⚙️ Simple & deterministic  
📦 No model fitting required

------------------------------------------------------------------------

## 📦 Installation

\`\`\`r \# when published: \# install.packages(“PACE”)

# until then:

devtools::install(“./PACE”)
