#----------------------------------------------
# load libraries
suppressPackageStartupMessages(
    {
    library(targets)
    library(rlang)
    library(here)
    library(tidyverse)
    library(ggplot2)
    library(sf)
    library(lubridate)
    library(MetBrewer)
    library(tarchetypes)
    library(fs)
    library(fst)
    library(stringi)
    import::from(purrr, map)
    library(data.table)
    library(future)
    library(future.callr)
    library(openxlsx)
    library(janitor)
    library(qs)
    library(docstring)
    library(psych)
    library(zoo)
    library(fixest)
    library(modelsummary)
    library(stringr)
    }
)

#----------------------------------------------
# set up for future package
plan(callr)

#----------------------------------------------
# paths

main_path <- "N:/Just_Nature/noise_exposure"
data_path <- file.path(main_path, "data")
output_path <- file.path(main_path, "output")
code_path <- file.path(main_path, "code")

#----------------------------------------------
# set working directory

setwd(main_path)

#----------------------------------------------
# Read files
lapply(
    list.files(
        code_path,
        pattern = ".R$",
        full.names = TRUE,
        all.files = FALSE
    ),
    source
)

