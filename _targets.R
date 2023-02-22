# NOTE:
    # Data on noise exposure and noise shapes comes from the personal pipeline
    # on my local computer (PT)

#----------------------------------------------
# load libraries
suppressPackageStartupMessages(
    {
    library(targets)
    library(future)
    library(future.callr)
    library(tarchetypes)
    library(rlang)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(sf)
    library(data.table)
    library(readxl)
    library(openxlsx)
    library(qs)
    library(docstring)
    library(stringr)
    library(MetBrewer)
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
# globals

owndpi <- 800

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

#----------------------------------------------
# population by country

target_country_pop <- rlang::list2(
    tar_fst(
        country_pop_prep,
        prepare_country_pop()
    )
)

#----------------------------------------------
# number of people under noise
# as given by EEA

target_noise_exposure <- rlang::list2(
    tar_fst(
        noise_exposure_prep,
        prepare_noise_exposure()
    ),
    tar_target(
        analysis_noise_exposure,
        analyzing_noise_exposure(
            noise_exposure_prep,
            country_pop_prep
        )
    )
    
)

#----------------------------------------------
# all together

rlang::list2(
    target_country_pop,
    target_noise_exposure
)



