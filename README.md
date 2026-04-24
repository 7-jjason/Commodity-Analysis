# Commodity Market Analytics in R

A collection of R scripts for analyzing commodity markets in three distinct modules: CFTC Commitment of Traders (COT) positioning, Canadian grain basis mapping, and forward curve reconstruction from continuous futures series.

---

## Modules

### 1. CFTC COT Data — Webscraping, Formatting & Visualization

Scrapes the CFTC's publicly available COT reports, parses and formats the data, and produces time series visualizations by commodity category.

**Covered categories:**
- Agricultural products
- Electricity products
- Metals and other products
- Natural gas products
- Petroleum products

**Data source:** [CFTC COT Reports](https://www.cftc.gov/MarketReports/CommitmentsofTraders/index.htm)

**Key features:**
- Pulls both the legacy and disaggregated COT report formats
- Cleans and reshapes position data (commercial, non-commercial, non-reportable) into tidy long format
- Produces time series plots of net positioning by trader class
- Highlights extreme positioning readings relative to historical ranges (e.g., z-score overlays or percentile bands)

**Primary packages:** `rvest`, `httr`, `tidyverse`, `lubridate`, `ggplot2`

---

### 2. Canadian Grain Basis Mapping

Maps commodity basis across Canadian grain-producing regions using publicly available cash price and futures data.

**Covered commodities:** Canola, wheat, barley, oats, and others subject to data availability.

**Data sources:** Publicly available CME/ICE futures and basis benchmarks through https://www.pdqinfo.ca/.

**Methodology and limitations:**

Cash price data is available only at select reported locations and at irregular or infrequent intervals. Because individual elevator locations cannot be binned with sufficient density to produce a clean discrete map, **spatial interpolation** is used to estimate basis values between observed points. The interpolation surfaces are generated using inverse distance weighting (IDW) or kriging depending on the number of available observations.

> **Important caveat:** Interpolated basis values between observed locations are estimates only. They reflect a modeled surface, not observed prices. Accuracy degrades with distance from the nearest reported location and with sparse observation periods. Users should treat interpolated regions with appropriate skepticism.

**Key features:**
- Ingests cash price data and aligns to the nearest active futures contract for basis calculation
- Produces choropleth or continuous surface maps of basis by crop and date
- Allows comparison across crop years where data permits

**Primary packages:** `sf`, `sp`, `gstat`, `tmap` or `ggplot2` + `ggmap`, `tidyverse`

---

### 3. Forward Curve Reconstruction from Continuous Series

Takes continuous commodity futures series — formed by linking successive contract series end-to-end without any roll adjustment — and splices them back into their underlying individual contracts. These contracts are then plotted as forward curves across observation months.

**Data sources:** London Stock Exchange Group.

**Key features:**

**Splicing & reconstruction**
- Parses the continuous series and identifies contract boundaries using expiry calendars
- Assigns each price observation to its originating contract (e.g., CL Mar 2023, CL May 2023, etc.)
- No roll adjustment is applied; prices reflect raw contract levels as originally traded

**Forward curve visualization**
- Plots the term structure (forward curve) for a selected commodity at any given observation date
- Animates or facets the evolution of the forward curve over time so users can observe how the curve shape (contango, backwardation, kinks) changes across a historical window
- Supports one or two commodities simultaneously for side-by-side curve comparison

**Basis between commodities**
- Calculates and plots the basis between two selected commodities along matched contract tenors
- Visualizes how the inter-commodity spread evolves over time by observation month

**Primary packages:** `tidyverse`, `lubridate`, `ggplot2`, `gganimate` (optional, for animated curve evolution)

---

## Repository Structure

```
.
├── R/
│   ├── cot/
│   │   ├── scrape_cot.R          # Fetches COT data from CFTC
│   │   ├── clean_cot.R           # Parses and reshapes raw data
│   │   └── visualize_cot.R       # Time series plots by category
│   ├── basis/
│   │   ├── ingest_cash_prices.R  # Loads and aligns cash price data
│   │   ├── compute_basis.R       # Basis calculation vs. futures benchmark
│   │   └── map_basis.R           # Spatial interpolation and mapping
│   └── forward_curve/
│       ├── parse_continuous.R    # Splices continuous series into contracts
│       ├── build_curves.R        # Assembles forward curves by observation date
│       └── visualize_curves.R    # Plots curves and inter-commodity basis
├── data/
│   ├── raw/                      # Unmodified source data
│   └── processed/                # Cleaned and formatted outputs
├── outputs/
│   └── plots/                    # Generated figures
└── README.md
```

---

## Setup

```r
# Install required packages
install.packages(c(
  "tidyverse", "lubridate", "rvest", "httr",
  "ggplot2", "sf", "sp", "gstat", "tmap", "gganimate"
))
```

---

## Limitations and Notes

- **Canadian basis data:** Publicly available cash prices are sparse and not uniformly reported. Interpolated values are model outputs, not observed prices.
- **Continuous series construction:** Contracts are tagged together without roll adjustment. This means apparent price gaps at roll dates are real and intentional — they reflect the actual spread between expiring and prompt contracts at the time of roll. Users comparing levels across contracts should account for this.
- **Forward curve basis:** Inter-commodity basis plots assume matched tenors where available. Liquidity and data availability may limit reliable comparison at the far end of the curve.