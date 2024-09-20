make_inspire <- function(grid_pop_prep, pollution_prepared) {
    #' @title Bring pollution to INSPIRE grid
    #' 
    #' @description This function brings the unique grid of the pollution data
    #' to the unified INSPIRE grid classification. This matches the other data
    #' better.
    #' 
    #' @param grid_pop_prep Grid data set with population information
    #' @param pollution_prepared Cleaned grid data set with pollution values
    #' 
    #' @return
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # generate empty grid
    # use INSPIRE grid from population data set

    clean_grid <- grid_pop_prep |>
        dplyr::select(-pop_2018)

    # make sure that both data sets have the same CRS
    stopifnot(
        (st_crs(clean_grid) == st_crs(pollution_prepared)) == TRUE
    )

    #----------------------------------------------
    # join empty INSPIRE grid and pollution values

    fill_grid <- function(ct, cnt) {
        # subset pollution data for specific city
        city_pollution <- pollution_prepared |>
            dplyr::filter(city == ct)

        # subset empty grid for country
        # make join faster
        country_grid <- clean_grid |>
            dplyr::filter(country == cnt)

        # join both data sets
        joint <- sf::st_join(
            country_grid,
            city_pollution,
            left = TRUE,
            largest = TRUE
        )

        # drop grids that are not within the city
        city_grids <- joint |>
            dplyr::filter(!is.na(city))

        # return output
        return(city_grids)
    }

    # apply function to each city
    munich_grids <- fill_grid(ct = "Munich", cnt = "Germany")
    merano_grids <- fill_grid(ct = "Merano", cnt = "Italy")
    bolzano_grids <- fill_grid(ct = "Bolzano", cnt = "Italy")
    leuven_grids <- fill_grid(ct = "Leuven", cnt = "Belgium")    

    #----------------------------------------------
    # combine all
    city_grids <- rbind(
        munich_grids,
        merano_grids,
        bolzano_grids,
        leuven_grids
    )

    #----------------------------------------------
    # export
    qs::qsave(
        city_grids,
        file.path(
            data_path,
            "air_pollution/1km/city_pollution_inspire.qs"
        )
    )

    #----------------------------------------------
    # return
    return(city_grids)
}