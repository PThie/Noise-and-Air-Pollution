mapping_bivariate <- function(poll_noise_combined, city_shapes, owndpi) {
    #' @title Bivariate map of noise and air pollution
    #' 
    #' @description This function brings noise and air pollution together by
    #' plotting the variables on a bivariate map.
    #' 
    #' @param poll_noise_combined Data set with noise (adjusted) and pollution values
    #' @param city_shapes Clean city borders
    #' @param owndpi Own specified DPI for plots
    #' 
    #' @return Returns bivariate maps
    #' @author Patrick Thiel

    #----------------------------------------------
    # set geometry
    poll_noise <- sf::st_set_geometry(poll_noise_combined, poll_noise_combined$geometry)

    #----------------------------------------------
    # day noise and pollution

    bivariate_mapping <- function(ct, day_night = c("day", "night")) {
        #' @param ct City for subset
        #' @param day_night Indicator for day or night noise

        #----------------------------------------------
        # preparation

        # subset for city pollution-noise
        city_poll_noise <- poll_noise |>
            dplyr::filter(city == ct)

        # specify noise variables
        if(day_night == "day") {
            noise_var <- "noise_low_adj_Lden"
        } else {
            noise_var <- "noise_low_adj_Lnight"
        }

        # get breaks for noise values
        breaks_noise <- as.numeric(
            quantile(
                city_poll_noise[[noise_var]],
                prob = seq(0, 1, 0.33333),
                na.rm = TRUE
            )
        )

        # get breaks for pollution values
        breaks_poll <- as.numeric(
            quantile(
                city_poll_noise$pollution,
                prob = seq(0, 1, 0.33333),
                na.rm = TRUE
            )
        )

        # define categories according to breaks
        city_poll_noise_prep <- city_poll_noise |>
            dplyr::mutate(
                noise_cat := case_when(
                    .data[[noise_var]] >= 0 & .data[[noise_var]] <= breaks_noise[2] ~ "1",
                    .data[[noise_var]] > breaks_noise[2] & .data[[noise_var]] <= breaks_noise[3] ~ "2",
                    .data[[noise_var]] > breaks_noise[3] ~ "3"
                ),
                poll_cat := case_when(
                    pollution >= 0 & pollution <= breaks_poll[2] ~ "1",
                    pollution > breaks_poll[2] & pollution <= breaks_poll[3] ~ "2",
                    pollution > breaks_poll[3] ~ "3"
                ),
                class_bivariate = paste0(noise_cat, "-", poll_cat)
            )

        #----------------------------------------------
        # map

        # construct map
        map <- ggplot()+
            geom_sf(
                data = city_poll_noise_prep,
                aes(geometry = geometry, fill = class_bivariate),
                size = 0.1,
                show.legend = FALSE,
                col = "black"
            )+
            bi_scale_fill(pal = "DkBlue2", na.value = "white")+
            geom_sf(
                data = city_shapes |> filter(city == ct),
                aes(geometry = geometry),
                fill = NA,
                col = "black"
            )+
            bi_theme()

        # construct legend
        legend <- bi_legend(
            pal = "DkBlue2",
            dim = 3,
            x = "Noise",
            y = "Pollution",
            size = 6
        )

        # put together
        if(ct == "Munich") {
            final <- cowplot::ggdraw()+
                draw_plot(map, 0, 0, 1, 1)+
                draw_plot(legend, 0.75, 0.8, 0.2, 0.2)
        } else {
            final <- cowplot::ggdraw()+
                draw_plot(map, 0, 0, 1, 1)+
                draw_plot(legend, 0.6, 0.8, 0.2, 0.2)
        }

        #----------------------------------------------
        # export
        # NOTE: ggsave does not work here

        filename <- paste0(tolower(ct), "_bivariate_map_", day_night, ".png")
        cowplot::save_plot(
            plot = final,
            file.path(
                output_path,
                "maps",
                filename
            )
        )
    }

    #----------------------------------------------
    # generate maps through loop

    # define options
    cts <- unique(city_shapes$city)
    timings <- c("day", "night")

    for(timing in timings) {
        for(c in cts) {
            bivariate_mapping(ct = c, day_night = timing)
        }
    }


}