combine_poll_noise <- function(pollution_prepared_inspire, noise_combined) {
    #' @title Combining noise and air pollution
    #' 
    #' @description This function combines noise and air pollution in one data
    #' set on the city level.
    #' 
    #' @param pollution_prepared_inspire Air pollution on city level
    #' @param noise_combined Noise pollution on city level
    #' 
    #' @return Returns data set with noise and air pollution
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # join data sets

    join_data <- function(ct) {
        # subset data
        poll <- pollution_prepared_inspire |>
            dplyr::filter(city == ct)

        noise <- noise_combined |>
            dplyr::filter(city == ct)

        # join data
        joint <- sf::st_join(
            noise,
            poll,
            left = TRUE,
            largest = TRUE
        )

        # return
        return(joint)
    }

    # loop through cities
    cities <- unique(noise_combined$city)
    data_list <- list()

    for(cts in cities) {
        dta <- join_data(ct = cts)
        
        # some cleaning
        dta <- dta |>
            dplyr::mutate(
                country.y = NULL,
                city.y = NULL
            ) |>
            dplyr::rename(
                country = country.x,
                city = city.x
            )
        data_list[[cts]] <- dta
    }

    # row bind data
    dta <- data.table::rbindlist(data_list)

    #----------------------------------------------
    # export
    qs::qsave(
        dta,
        file.path(
            data_path,
            "pollution_noise_combined.qs"
        )
    )

    #----------------------------------------------
    # return
    return(dta)
}