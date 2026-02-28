# data is from the xlsx file on https://www.eia.gov/petroleum/refinerycapacity/

library(readxl)
library(tidyverse)

setwd("/Users/josephjason/Documents/Forecasting/R/projects/refineries")

refinery_dt <- read_xlsx("data/eia_2025_refinery_capacity.xlsx")

refinery_dt <- refinery_dt |>
  mutate(NEW_NAMES = paste0(SUPPLY, " ", PRODUCT)) |>
  select(!c(SUPPLY, PRODUCT)) |>
  pivot_wider(
    names_from = "NEW_NAMES",
    values_from = "QUANTITY"
  ) |>
  relocate(
    # Identification and Metadata
    PADD, STATE_NAME, COMPANY_NAME, CORPORATION, SITE, SURVEY, PERIOD, RDIST_LABEL,
    
    # Atmospheric Crude Oil Distillation Capacity (Screenshot 1)
    `Atmospheric Crude Distillation Capacity (barrels per calendar day) OPERATING CAPACITY`,
    `Atmospheric Crude Distillation Capacity (barrels per calendar day) IDLE CAPACITY`,
    `Atmospheric Crude Distillation Capacity (barrels per calendar day) TOTAL OPERABLE CAPACITY`,
    `Atmospheric Crude Distillation Capacity (barrels per stream day) OPERATING CAPACITY`,
    `Atmospheric Crude Distillation Capacity (barrels per stream day) IDLE CAPACITY`,
    `Atmospheric Crude Distillation Capacity (barrels per stream day) TOTAL OPERABLE CAPACITY`,
    `Atmospheric Crude Distillation Capacity (barrels per stream day) TOTAL OPER CAP (PROJECTED, NEXT YEAR)`,
    
    # Downstream Charge Capacity: Vacuum and Thermal Cracking (Screenshot 1)
    `Downstream Charge Capacity, Current Year (barrels per stream day) VACUUM DISTILLATION`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) VACUUM DISTILLATION`,
    `Downstream Charge Capacity, Current Year (barrels per calendar day) THERM CRACKING, DELAYED COKING`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) THERM CRACKING, DELAYED COKING`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) THERM CRACKING, DELAYED COKING`,
    `Downstream Charge Capacity, Current Year (barrels per calendar day) THERM CRACKING, FLUID COKING`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) THERM CRACKING, FLUID COKING`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) THERM CRACKING, FLUID COKING`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) THERM CRACKING, VISBREAKING`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) THERM CRACKING, VISBREAKING`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) THERM CRACKING, OTHER (INCLDNG GAS OIL)`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) THERM CRACKING, OTHER (INCLDNG GAS OIL)`,
    
    # Downstream Charge Capacity: Catalytic units (Screenshot 2)
    `Downstream Charge Capacity, Current Year (barrels per calendar day) CAT CRACKING: FRESH FEED`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) CAT CRACKING: FRESH FEED`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) CAT CRACKING: FRESH FEED`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) CAT CRACKING: RECYCLED FEED`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) CAT CRACKING: RECYCLED FEED`,
    `Downstream Charge Capacity, Current Year (barrels per calendar day) CAT HYDROCRACKING, DISTILLATE`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) CAT HYDROCRACKING, DISTILLATE`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) CAT HYDROCRACKING, DISTILLATE`,
    `Downstream Charge Capacity, Current Year (barrels per calendar day) CAT HYDROCRACKING, GAS OIL`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) CAT HYDROCRACKING, GAS OIL`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) CAT HYDROCRACKING, GAS OIL`,
    `Downstream Charge Capacity, Current Year (barrels per calendar day) CAT HYDROCRACKING, RESIDUAL`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) CAT HYDROCRACKING, RESIDUAL`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) CAT HYDROCRACKING, RESIDUAL`,
    `Downstream Charge Capacity, Current Year (barrels per calendar day) CAT REFORMING: LOW PRESSURE`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) CAT REFORMING: LOW PRESSURE`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) CAT REFORMING: LOW PRESSURE`,
    `Downstream Charge Capacity, Current Year (barrels per calendar day) CAT REFORMING: HIGH PRESSURE`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) CAT REFORMING: HIGH PRESSURE`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) CAT REFORMING: HIGH PRESSURE`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) FUELS SOLVENT DEASPHALTING`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) FUELS SOLVENT DEASPHALTING`,
    
    # Downstream Charge Capacity: Desulfurization (Screenshot 3)
    `Downstream Charge Capacity, Current Year (barrels per stream day) DESULFURIZATION, NAPHTHA/REFORMER FEED`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) DESULFURIZATION, NAPHTHA/REFORMER FEED`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) DESULFURIZATION, GASOLINE`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) DESULFURIZATION, GASOLINE`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) DESULFURIZATION, KEROSENE AND JET`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) DESULFURIZATION, KEROSENE AND JET`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) DESULFURIZATION, DIESEL FUEL`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) DESULFURIZATION, DIESEL FUEL`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) DESULFURIZATION, OTHER DISTILLATE`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) DESULFURIZATION, OTHER DISTILLATE`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) DESULFURIZATION, RESIDUAL`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) DESULFURIZATION, RESIDUAL`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) DESULFURIZATION, HEAVY GAS OIL`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) DESULFURIZATION, HEAVY GAS OIL`,
    `Downstream Charge Capacity, Current Year (barrels per stream day) DESULFURIZATION, OTHER`,
    `Downstream Charge Capacity, Next Year (barrels per stream day) DESULFURIZATION, OTHER`,
    
    # Production Capacity (Remaining columns)
    `Production Capacity, Current Year (barrels per steam day except sulfur and hydrogen) ALKYLATES`,
    `Production Capacity, Next Year (barrels per stream day except sulfur and hydrogen) ALKYLATES`,
    `Production Capacity, Current Year (barrels per steam day except sulfur and hydrogen) AROMATICS`,
    `Production Capacity, Next Year (barrels per stream day except sulfur and hydrogen) AROMATICS`,
    `Production Capacity, Current Year (barrels per steam day except sulfur and hydrogen) ASPHALT & ROAD OIL`,
    `Production Capacity, Next Year (barrels per stream day except sulfur and hydrogen) ASPHALT & ROAD OIL`,
    `Production Capacity, Current Year (barrels per steam day except sulfur and hydrogen) ISOMERIZATION (ISOBUTANE)`,
    `Production Capacity, Next Year (barrels per stream day except sulfur and hydrogen) ISOMERIZATION (ISOBUTANE)`,
    `Production Capacity, Current Year (barrels per steam day except sulfur and hydrogen) ISOMERIZATION (ISOPENTANE/ISOHEXANE)`,
    `Production Capacity, Next Year (barrels per stream day except sulfur and hydrogen) ISOMERIZATION (ISOPENTANE/ISOHEXANE)`,
    `Production Capacity, Current Year (barrels per steam day except sulfur and hydrogen) LUBRICANTS`,
    `Production Capacity, Next Year (barrels per stream day except sulfur and hydrogen) LUBRICANTS`,
    `Production Capacity, Current Year (barrels per steam day except sulfur and hydrogen) PETCOKE,MARKET`,
    `Production Capacity, Next Year (barrels per stream day except sulfur and hydrogen) PETCOKE,MARKET`,
    `Production Capacity, Current Year (barrels per steam day except sulfur and hydrogen) SULFUR (SHORT TONS/DAY)`,
    `Production Capacity, Next Year (barrels per stream day except sulfur and hydrogen) SULFUR (SHORT TONS/DAY)`,
    `Production Capacity, Current Year (barrels per steam day except sulfur and hydrogen) HYDROGEN (MMCFD)`,
    `Production Capacity, Next Year (barrels per stream day except sulfur and hydrogen) HYDROGEN (MMCFD)`
  ) |>
  arrange(STATE_NAME, COMPANY_NAME)




















