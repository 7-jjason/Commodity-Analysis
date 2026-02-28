rm(list = ls())

# Load Packages
library(tidyverse)
library(tsibble)
library(stringr)
library(jsonlite)
library(rvest)
library(data.table)
library(slider)

# Load Functions
source("functions/extract_split.R")
source("functions/max_multi_join.R")

# For each of these, I need to put the correct contract quantity in