italy_noise <- function(utmcrs, owndpi, grid_pop_prep, italy_noise_prepared, city_shapes) {
    #' @title Calculate exposed people Italy
    #' 
    #' @description This function calculates the number of exposed people in
    #' Italy.
    #' 
    #' @param utmcrs Global UTM CRS (32632)
    #' @param owndpi Global DPI resolution
    #' @param grid_pop_prep Population on grid level
    #' @param italy_noise_prepared Noise data
    #' @param city_shapes Clean city borders
    #' 
    #' @return Returns graphs with number of exposed people
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # read data

    # italy noise shapes
    noise <- tar_read(italy_noise_prepared)

    # population grid
    pop <- tar_read(grid_pop_prep)

    #----------------------------------------------
    # make sf

    noise <- st_set_geometry(noise, noise$geometry)

    #----------------------------------------------
    # subset for city of interest: Leuven

    cities <- city_shapes |>
        dplyr::filter(city == "Bolzano" | city == "Merano")

    #----------------------------------------------
    # add noise and population data for cities

    city_noise <- st_intersection(
        cities,
        noise
    )

    city_pop <- st_intersection(
        cities,
        pop
    )

    #----------------------------------------------
    # make map of population

    # define plotting function
    pop_map_function <- function(cty) {
        #' @param cty City identifier
        
        # subet population data for city
        pops <- city_pop |> filter(city == cty)

        # get upper limit of city population
        upper_bound <- plyr::round_any(
            max(pops$pop_2018, na.rm = TRUE),
            1000,
            f = ceiling
        )

        pop_map <- ggplot()+
            geom_sf(
                data = cities |> filter(city == cty),
                aes(geometry = geometry),
                fill = NA,
                col = "black",
                linewidth = 0.8
            )+
            geom_sf(
                data = pops,
                aes(geometry = geometry, fill = pop_2018)
            )+
            scale_fill_viridis_c(
                direction = -1,
                option = "magma",
                name = "Population (2018)",
                breaks = seq(0, upper_bound, 2000),
                labels = scales::label_comma()
            )+
            theme_void()

        filename <- paste0(tolower(cty), "_population.png")
        ggsave(
            plot = pop_map,
            file.path(
                output_path,
                "maps",
                filename
            ),
            dpi = owndpi
        )
    }

    # make maps
    pop_map_function(cty = "Bolzano")
    pop_map_function(cty = "Merano")

    #----------------------------------------------
    # re-adjust night noise
    # for some reason: there are two large shapes (one each for each city)
    # that cover the entire city area -> remove them

    aux_merano <- city_noise |>
        filter(city == "Merano") |>
        filter(day_night == "Lnight") |>
        mutate(
            area = round(as.numeric(st_area(geometry)), digits = 0)
        ) |>
        filter(area < max(area)) |>
        select(-area)

    aux_bolzano <- city_noise |>
        filter(city == "Bolzano") |>
        filter(day_night == "Lnight") |>
        mutate(
            area = round(as.numeric(st_area(geometry)), digits = 0)
        ) |>
        filter(area < max(area)) |>
        select(-area)

    aux <- city_noise |>
        filter(day_night == "Lden")
    
    city_noise <- rbind(
        aux,
        aux_merano,
        aux_bolzano
    )

    #----------------------------------------------
    # make map of noise

    # define plot function
    noise_map_function <- function(noise_meas = c("Lden", "Lnight"), cty) {
        
        # subet population data for city
        nois <- city_noise |> filter(city == cty)
        
        # make map
        map_noise <- ggplot()+
            geom_sf(
                data = cities |> filter(city == cty),
                aes(geometry = geometry),
                fill = NA,
                col = "black",
                linewidth = 0.8
            )+
            geom_sf(
                data = nois |> filter(day_night == noise_meas),
                aes(geometry = geometry, fill = source),
                col = "black"
            )+
            scale_fill_manual(
                name = "Source",
                values = c(
                    "airports" = col_air,
                    "streets" = col_streets,
                    "railroads" = col_rails,
                    "industries" = col_ind
                ),
                labels = c(
                    "airports" = "Airports",
                    "streets" = "Streets",
                    "railroads" = "Railroads",
                    "industries" = "Industries"
                )
            )+
            theme_void()

            # export
            if(noise_meas == "Lden") {
                filename <- paste0(tolower(cty), "_noise_day.png")
            } else {
                filename <- paste0(tolower(cty), "_noise_night.png")
            }
            ggsave(
                plot = map_noise,
                file.path(
                    output_path,
                    "maps",
                    filename
                ),
                dpi = owndpi
            )    
    }

    # make maps
    for(cy in c("Bolzano", "Merano")) {
        for(noim in c("Lden", "Lnight")) {
            noise_map_function(cty = cy, noise_meas = noim)
        }
    }

    #----------------------------------------------
    # bring noise and population together

    bring_noise_pop <- function(noise_data, pop_data, noise_meas, src, cty) {
        noise_subset <- noise_data |>
            filter(city == cty) |>
            filter(source == src) |>
            filter(day_night == noise_meas)

        pop_subset <- pop_data |>
            filter(city == cty) |>
            select(pop_2018, geometry)

        joint_data <- st_join(
            pop_subset,
            noise_subset,
            left = TRUE,
            largest = TRUE
        )
        
        # calculate city population
        overall_pop <- sum(pop_subset$pop_2018, na.rm = TRUE)

        # summarise data
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
    sources <- unique(city_noise$source)
    measures <- unique(city_noise$day_night)
    cities <- unique(city_noise$city)

    # list for storage
    summary_data_list <- list()

    # generate summary statistics for noise measures and sources
    for(ct in cities) {
        for(msr in measures) {
            for(srcs in sources) {
                dta <- bring_noise_pop(
                    noise_data = city_noise,
                    pop_data = city_pop,
                    noise_meas = msr,
                    src = srcs,
                    cty = ct
                )

                # add city
                dta <- dta |>
                    mutate(city = ct) |>
                    relocate(city)

                short_name <- paste(ct, srcs, msr, sep = "_")
                summary_data_list[[short_name]] <- dta
            }
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
            "descriptives/bolzano_merano_exposed_day.xlsx"
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
            "descriptives/bolzano_merano_exposed_night.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # generate bar plots

    # define bar plot function
    make_bars <- function(exposed_data, noise_meas = c("lden", "lnight"), cty, plotname) {
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
            data = exposed_data |> filter(city == cty),
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
                "railroads" = col_rails,
                "industries" = col_ind
            ),
            labels = c(
                "airports" = "Airports",
                "streets" = "Streets",
                "railroads" = "Railroads",
                "industries" = "Industries"
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

    # make bar plots for day noise
    for(ct in cities) {
        make_bars(
            exposed_data = exposed_lden,
            noise_meas = "lden",
            cty = ct,
            plotname = paste0(tolower(ct), "_exposed_day")
        )
    }

    # make bar plots for night noise
    for(ct in cities) {
        make_bars(
            exposed_data = exposed_lnight,
            noise_meas = "lnight",
            cty = ct,
            plotname = paste0(tolower(ct), "_exposed_night")
        )
    }
}