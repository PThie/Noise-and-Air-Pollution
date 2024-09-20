plot_pollution <- function(pollution_prepared_inspire, city_shapes) {
    #' @title Plotting PM2.5 pollution
    #' 
    #' @description This function plots PM2.5 pollution on the city level.
    #' 
    #' @param pollution_prepared_inspire Combined pollution data
    #' @param city_shapes Clean city borders 
    #' 
    #' @return Maps of pollution
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # plot function

    plot_function <- function(ct) {
        # subset data
        poll <- pollution_prepared_inspire |>
            dplyr::filter(city == ct)

        shp <- city_shapes |>
            dplyr::filter(city == ct)

        # plot
        plt <- ggplot()+
            geom_sf(
                data = poll,
                aes(geometry = geometry, fill = pollution)
            )+
            scale_fill_viridis_c(
                option = "magma",
                direction = -1,
                name = expression(
                    paste("Pollution in \U003BCg/", m^{3})
                )
            )+
            geom_sf(
                data = shp,
                aes(geometry = geometry),
                fill = NA,
                col = "black"
            )+
            theme_void()

        filename <- paste0(tolower(ct), "_pollution.png")
        ggsave(
            plot = plt,
            file.path(
                output_path,
                "maps",
                filename
            ),
            dpi = 800
        )
    }

    # loop through city
    cities <- unique(city_shapes$city)

    for(cts in cities) {
        plot_function(ct = cts)
    }
}