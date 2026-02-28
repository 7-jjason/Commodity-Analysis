rm(list = ls())

# load packages
library(rvest)
library(readtext)
library(webdriver)
library(tidyverse)
library(readtext)
library(flextable)
library(webdriver)
library(remotes)
# install phantonJS headless browser - only needs to be done once
  # webdriver::install_phantomjs()
# install klippy for copy-to-clipboard button in code chunks
  # remotes::install_github("rlesur/klippy")
# activate klippy
klippy::klippy()
