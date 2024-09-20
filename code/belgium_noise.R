belgium_noise <- function(utmcrs, owndpi, grid_pop_prep, city_shapes) {
    #' @title Calculate exposed people Belgium
    #' 
    #' @description This function calculates the number of exposed people in
    #' Belgium.
    #' 
    #' @param utmcrs Global UTM CRS (32632)
    #' @param owndpi Global DPI resolution
    #' @param grid_pop_prep Population on grid level
    #' @param city_shapes Clean city borders
    #' 
    #' @return Returns graphs with number of exposed people
    #' @author Patrick Thiel

    #----------------------------------------------
    # read data

    # belgium noise shapes
    noise <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/belgium_all_prepared_2017.qs"
        )
    )

    # population grid
    pop <- tar_read(grid_pop_prep)

    #----------------------------------------------
    # make sf

    noise <- st_set_geometry(noise, noise$geometry)

    #----------------------------------------------
    # subset for city of interest: Leuven

    leuven <- city_shapes |>
        dplyr::filter(city == "Leuven")

    #----------------------------------------------
    # subset noise data for city

    leuven_noise <- st_intersection(
        leuven,
        noise
    )

    leuven_pop <- st_intersection(
        leuven,
        pop
    )

    #----------------------------------------------
    # make map of population

    # create maps
    pop_map <- ggplot()+
        geom_sf(
            data = leuven,
            aes(geometry = geometry),
            fill = NA,
            col = "black",
            linewidth = 0.8
        )+
        geom_sf(
            data = leuven_pop,
            aes(geometry = geometry, fill = pop_2018)
        )+
        scale_fill_viridis_c(
            direction = -1,
            option = "magma",
            name = "Population (2018)",
            breaks = seq(0, 7000, 1000),
            labels = scales::label_comma()
        )+
        theme_void()

    ggsave(
        plot = pop_map,
        file.path(
            output_path,
            "maps/leuven_population.png"
        ),
        dpi = owndpi
    )

    #----------------------------------------------
    # make map of noise

    # day noise levels
    map_source_day <- ggplot()+
        geom_sf(
            data = leuven,
            aes(geometry = geometry),
            fill = NA,
            col = "black",
            linewidth = 0.8
        )+
        geom_sf(
            data = leuven_noise |> filter(day_night == "Lden"),
            aes(geometry = geometry, fill = source),
            col = "black"
        )+
        scale_fill_manual(
            name = "Source",
            values = c(
                "airports" = col_air,
                "streets" = col_streets,
                "railroads" = col_rails
            ),
            labels = c(
                "airports" = "Airports",
                "streets" = "Streets",
                "railroads" = "Railroads"
            )
        )+
        theme_void()

    ggsave(
        plot = map_source_day,
        file.path(
            output_path,
            "maps/leuven_noise_day.png"
        ),
        dpi = owndpi
    )

    # night noise levels
    map_source_night <- ggplot()+
        geom_sf(
            data = leuven,
            aes(geometry = geometry),
            fill = NA,
            col = "black",
            linewidth = 0.8
        )+
        geom_sf(
            data = leuven_noise |> filter(day_night == "Lnight"),
            aes(geometry = geometry, fill = source),
            col = "black"
        )+
        scale_fill_manual(
            name = "Source",
            values = c(
                "airports" = col_air,
                "streets" = col_streets,
                "railroads" = col_rails
            ),
            labels = c(
                "airports" = "Airports",
                "streets" = "Streets",
                "railroads" = "Railroads"
            )
        )+
        theme_void()

    ggsave(
        plot = map_source_night,
        file.path(
            output_path,
            "maps/leuven_noise_night.png"
        ),
        dpi = owndpi
    )

    #----------------------------------------------
    # bring noise and population together

    bring_noise_pop <- function(noise_data, pop_data, noise_meas, src) {
        noise_subset <- noise_data |>
            filter(source == src) |>
            filter(day_night == noise_meas)

        pop_subset <- pop_data |>
            select(pop_2018, geometry)

        joint_data <- st_join(
            pop_subset,
            noise_subset,
            left = TRUE,
            largest = TRUE
        )

        # calculate city population
        overall_pop <- sum(pop_subset$pop_2018, na.rm = TRUE)

        sum_stats <- joint_data |>
            st_drop_geometry() |>
            group_by(source, noise_low) |>
            filter(day_night == noise_meas) |>
            summarise(
                total_pop = sum(pop_2018, na.rm = TRUE),
                perc_pop = round(
                    (total_pop / overall_pop) * 100,
                    digits = 2
                )
            ) |>
            as.data.frame()

        return(sum_stats)
    }

    #----------------------------------------------
    # generate summary statistics

    # define sources and noise measures
    sources <- unique(leuven_noise$source)
    measures <- unique(leuven_noise$day_night)

    # list for storage
    summary_data_list <- list()

    # generate summary statistics for noise measures and sources
    for(msr in measures) {
        for(srcs in sources) {
            dta <- bring_noise_pop(
                noise_data = leuven_noise,
                pop_data = leuven_pop,
                noise_meas = msr,
                src = srcs
            )

            short_name <- paste(srcs, msr, sep = "_")
            summary_data_list[[short_name]] <- dta
        }
    }

    # extract data for day noise
    exposed_lden <- summary_data_list[
        stringr::str_detect(names(summary_data_list), "Lden") == TRUE
    ] |>
    data.table::rbindlist()

    openxlsx::write.xlsx(
        exposed_lden,
        file.path(
            output_path,
            "descriptives/leuven_exposed_day.xlsx"
        ),
        rowNames = FALSE
    )

    # extract data for night noise
    exposed_lnight <- summary_data_list[
        stringr::str_detect(names(summary_data_list), "Lnight") == TRUE
    ] |>
    data.table::rbindlist()

    openxlsx::write.xlsx(
        exposed_lnight,
        file.path(
            output_path,
            "descriptives/leuven_exposed_night.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # generate bar plots

    # define bar plot function
    make_bars <- function(exposed_data, noise_meas = c("lden", "lnight"), plotname) {
        # define labels and breaks
        if(noise_meas == "lden") {
            lbs <- c(
                "55" = "55-59",
                "60" = "60-64",
                "65" = "65-69",
                "70" = "70-74",
                "75" = "75 and above"
            )
            brk <- c(55, 60, 65, 70, 75)
            lvls <- brk
        } else {
            lbs <- c(
                "45" = "45-49",
                "50" = "50-54",
                "55" = "55-59",
                "60" = "60-64",
                "65" = "65-69",
                "70" = "70 and above"
            )
            brk <- c(45, 50, 55, 60, 65, 70)
            lvls <- brk
        }

        plt <- ggplot(
            data = exposed_data,
            aes(fill = source, x = factor(noise_low, levels = lvls), y = perc_pop)
        )+
        geom_bar(
            position = position_dodge2(preserve = "single"),
            stat = "identity"
        )+
        scale_x_discrete(
            labels = lbs,
            breaks = brk
        )+
        scale_fill_manual(
            name = "Source",
            values = c(
                "airports" = col_air,
                "streets" = col_streets,
                "railroads" = col_rails
            ),
            labels = c(
                "airports" = "Airports",
                "streets" = "Streets",
                "railroads" = "Railroads"
            )
        )+
        labs(
            y = "Share of exposed people (in %)",
            x = "Noise levels (in dB)"
        )+
        theme_classic()+
        theme(
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12)
        )

        filename <- paste0(plotname, ".png")
        ggsave(
            plot = plt,
            file.path(
                output_path,
                "graphs",
                filename
            ),
            dpi = owndpi
        )
    }

    # bar plot for day noise
    make_bars(
        exposed_data = exposed_lden,
        noise_meas = "lden",
        plotname = "leuven_exposed_day"
    )

    # bar plot for night noise
    make_bars(
        exposed_data = exposed_lnight,
        noise_meas = "lnight",
        plotname = "leuven_exposed_night"
    )
}
