germany_noise_prep <- function() {
    #' @title Prepares the noise data for Germany
    #' 
    #' @description This function prepares the noise data for
    #' Germany.
    #' 
    #' @return Returns shape with all the noise data
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # read data

    # germany noise shapes
    noise_ind <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/germany_industries_prepared_2017.qs"
        )
    )

    noise_airports <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/germany_airports_prepared_2017.qs"
        )
    )

    noise_railroads <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/germany_railroads_prepared_2017.qs"
        )
    )

    noise_streets <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/germany_streets_prepared_2017.qs"
        )
    )

    #----------------------------------------------
    # combine all

    noise_all <- rbind(
        noise_ind,
        noise_airports,
        noise_railroads,
        noise_streets
    )

    #----------------------------------------------
    # set geometry

    noise_all <- st_set_geometry(noise_all, noise_all$geometry)

    #----------------------------------------------
    # return

    return(noise_all)
}