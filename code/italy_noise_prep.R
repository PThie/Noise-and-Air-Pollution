italy_noise_prep <- function() {
    #' @title Prepares the noise data for Italy
    #' 
    #' @description This function prepares the noise data for
    #' Italy.
    #' 
    #' @return Returns shape with all the noise data
    #' @author Patrick Thiel

    #----------------------------------------------
    # read data

    # italy noise shapes
    noise_agglo <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/italy_agglomerations_prepared_2017.qs"
        )
    )
    noise_agglo <- st_set_geometry(noise_agglo, noise_agglo$geometry)

    noise_airports <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/italy_airports_prepared_2017.qs"
        )
    )
    noise_airports <- st_set_geometry(noise_airports, noise_airports$geometry)

    noise_railroads <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/italy_railroads_prepared_2017.qs"
        )
    )
    noise_railroads <- st_set_geometry(noise_railroads, noise_railroads$geometry)

    noise_streets <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/italy_streets_prepared_2017.qs"
        )
    )
    noise_streets <- st_set_geometry(noise_streets, noise_streets$geometry)

    #----------------------------------------------
    # combine all

    noise <- rbind(
        noise_agglo,
        noise_airports,
        noise_railroads,
        noise_streets
    )

    #----------------------------------------------
    # make valid

    noise <- st_make_valid(noise)

    #----------------------------------------------
    # set geometry

    noise <- st_set_geometry(noise, noise$geometry)

    #----------------------------------------------
    # return

    return(noise)
}