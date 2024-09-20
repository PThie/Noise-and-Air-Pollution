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
    library(fst)
    import::from(plyr, round_any)
    import::from(purrr, reduce)
    library(ncdf4)
    library(raster)
    library(stars)
    library(biscale)
    library(cowplot)
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
utmcrs <- 32632

#----------------------------------------------
# coloring noise sources

cols <- MetBrewer::met.brewer(name = "Java", n = 5)

col_streets <- cols[1]
col_rails <- cols[2]
col_air <- cols[3]
col_ind <- cols[5]

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
# population grid

target_grid_pop <- rlang::list2(
    tar_files(
        file_grid_pop,
        "N:/Just_Nature/pop_eu/grid_1km_surf.gpkg"
    ),
    tar_qs(
        grid_pop_prep,
        prepare_grid_pop(file_grid_pop, utmcrs)
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
# noise exposure for project cities

target_noise_cities <- rlang::list2(
    # Clean city shapes
    tar_target(
        city_shapes,
        prepare_city_shapes()
    ),
    #----------------------------------------------
    # Belgium
    tar_target(
        belgium_noise_exposed,
        belgium_noise(
            utmcrs,
            owndpi,
            grid_pop_prep,
            city_shapes
        )
    ),
    #----------------------------------------------
    # Italy
    tar_qs(
        italy_noise_prepared,
        italy_noise_prep()
    ),
    tar_target(
        italy_noise_exposed,
        italy_noise(
            utmcrs,
            owndpi,
            grid_pop_prep,
            italy_noise_prepared,
            city_shapes
        )
    ),
    #----------------------------------------------
    # Germany
    tar_qs(
        germany_noise_prepared,
        germany_noise_prep()
    ),
    tar_target(
        germany_noise_exposed,
        germany_noise(
            utmcrs,
            owndpi,
            grid_pop_prep,
            germany_noise_prepared,
            city_shapes
        )
    ),
    #----------------------------------------------
    # combine all
    tar_target(
        noise_combined,
        combine_noise(
            germany_noise_prepared,
            italy_noise_prepared,
            city_shapes,
            grid_pop_prep,
            utmcrs,
            owndpi
        )
    )
)

#----------------------------------------------
# pollution data

target_pollution <- rlang::list2(
    tar_target(
        org_poll,
        read_pollution()
    ),
    tar_target(
        pollution_prepared,
        prepare_pollution(city_shapes)
    ),
    # make inspire grid
    tar_target(
        pollution_prepared_inspire,
        make_inspire(grid_pop_prep, pollution_prepared)
    ),
    # plot pollution
    tar_target(
        plotting_pollution,
        plot_pollution(pollution_prepared_inspire, city_shapes)
    )
)

#----------------------------------------------
# pollution and noise

target_poll_noise <- rlang::list2(
    tar_qs(
        poll_noise_combined,
        combine_poll_noise(pollution_prepared_inspire, noise_combined)
    ),
    tar_target(
        bivariate_map,
        mapping_bivariate(poll_noise_combined, city_shapes, owndpi)
    )
)

#----------------------------------------------
# all together

rlang::list2(
    target_country_pop,
    target_grid_pop,
    target_noise_exposure,
    target_noise_cities,
    target_pollution,
    target_poll_noise
)