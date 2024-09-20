prepare_grid_pop <- function(file_grid_pop, utmcrs) {
    #' @title Preparing population grid
    #' 
    #' @description This function prepares the population numbers on a grid
    #' level for Europe
    #' 
    #' @param file_grid_pop Path to data
    #' 
    #' @return Returns
    #' @author Patrick Thiel

    #----------------------------------------------
    # read data

    # population grid
    pop_grid <- st_read(
        file_grid_pop,
        quiet = TRUE
    )

    # load country shapes
    read_country_shapes <- function(
        country = c("Belgium", "Italy", "Germany"),
        country_short = c("BEL", "ITA", "DEU")
    ) {
        dta <- st_read(
            file.path(
                data_path,
                "country_shapes",
                paste0(country, "_shapefile"),
                paste0(country_short, "_adm0.shp")
            ),
            quiet = TRUE
        )
        # drop unwanted columns
        # transform CRS to UTM
        dta <- dta |>
            select(ISO, geometry) |>
            st_transform(crs = utmcrs)
        return(dta)
    }

    belgium <- read_country_shapes(country = "Belgium", country_short = "BEL")
    italy <- read_country_shapes(country = "Italy", country_short = "ITA")
    germany <- read_country_shapes(country = "Germany", country_short = "DEU")
    
    #----------------------------------------------
    # subset to countries of interest

    sample_grid <- pop_grid |>
        filter(str_detect(NUTS2021_0, pattern = "DE|IT|BE") == TRUE)

    # transform
    sample_grid <- sample_grid |>
        st_transform(crs = utmcrs)

    #----------------------------------------------
    # select grid cells for each country that have the largest overlap
    overlap <- function(country_shape) {
        dta <- st_join(
            sample_grid,
            country_shape,
            left = TRUE,
            largest = TRUE
        )

        dta <- dta |>
            filter(!is.na(ISO))
        
        return(dta)
    }

    belgium_grids <- overlap(country_shape = belgium)
    italy_grids <- overlap(country_shape = italy)
    germany_grids <- overlap(country_shape = germany)

    #----------------------------------------------
    # cleaning

    cleaning <- function(grid_pop, country) {
        grid_pop <- grid_pop |>
            select(TOT_P_2018, geometry) |>
            rename(
                pop_2018 = TOT_P_2018
            ) |>
            mutate(country = country) |>
            relocate(country)

        return(grid_pop) 
    }

    belgium_clean <- cleaning(grid_pop = belgium_grids, country = "Belgium")
    italy_clean <- cleaning(grid_pop = italy_grids, country = "Italy")
    germany_clean <- cleaning(grid_pop = germany_grids, country = "Germany")    

    #----------------------------------------------
    # combine in one
    all_pop_countries <- rbind(
        belgium_clean,
        italy_clean,
        germany_clean
    )

    #----------------------------------------------
    # export

    qs::qsave(
        all_pop_countries,
        file.path(
            data_path,
            "population_grids/population_grids.qs"
        )
    )

    #----------------------------------------------
    # return
    return(all_pop_countries)
}