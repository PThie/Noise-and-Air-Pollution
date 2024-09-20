combine_noise <- function(
    germany_noise_prepared, italy_noise_prepared, city_shapes, grid_pop_prep, utmcrs, owndpi) {
    #' @title Combining all CiPeL noise
    #' 
    #' @description This function combines all noise sources of all CiPeLs
    #' into one file.
    #' 
    #' @param germany_noise_prepared Cleaned noise data of Germany
    #' @param italy_noise_prepared Cleaned noise data of Italy
    #' @param city_shapes Clean city borders
    #' @param grid_pop_prep Grid population
    #' @param utmcrs UTM CRS
    #' @param owndpi Own specified DPI for plots
    #' 
    #' @return QS file with CiPeL noise
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # read data

    # noise shapes
    de_noise <- tar_read(germany_noise_prepared)
    it_noise <- tar_read(italy_noise_prepared)

    # belgium noise shapes
    be_noise <- qs::qread(
        file.path(
            data_path,
            "noise_shapes/belgium_all_prepared_2017.qs"
        )
    )

    #----------------------------------------------
    # make sf

    de_noise <- st_set_geometry(de_noise, de_noise$geometry)
    it_noise <- st_set_geometry(it_noise, it_noise$geometry)
    be_noise <- st_set_geometry(be_noise, be_noise$geometry)

    #----------------------------------------------
    # subset municipality data

    de_munic_subset <- city_shapes |>
        dplyr::filter(city == "Munich")

    it_munic_subset <- city_shapes |>
        dplyr::filter(city == "Bolzano" | city == "Merano")

    be_munic_subset <- city_shapes |>
        dplyr::filter(city == "Leuven")

    #----------------------------------------------
    # add noise for cities

    de_city_noise <- st_intersection(
        de_munic_subset,
        de_noise
    )

    it_city_noise <- st_intersection(
        it_munic_subset,
        it_noise
    )

    be_city_noise <- st_intersection(
        be_munic_subset,
        be_noise
    )

    #----------------------------------------------
    # generate empty grid

    gen_empty_grid <- function(ct_subset, count) {
        # subset grids
        grid_subset <- grid_pop_prep |>
            dplyr::filter(country == count) |>
            dplyr::select(-pop_2018)

        # add grid ID
        grid_subset <- grid_subset |>
            dplyr::mutate(grid_id = seq(1, nrow(grid_subset), 1))

        # join both data sets
        joint <- sf::st_join(
            grid_subset,
            ct_subset,
            left = TRUE,
            largest = TRUE
        )

        # drop grids that are not within the city
        city_grids <- joint |>
            dplyr::filter(!is.na(city))

        # return
        return(city_grids)
    }

    munich_grids <- gen_empty_grid(
        ct_subset = de_munic_subset, count = "Germany"
    )
    leuven_grids <- gen_empty_grid(
        ct_subset = be_munic_subset, count = "Belgium"
    )
    merano_grids <- gen_empty_grid(
        ct_subset = it_munic_subset |> dplyr::filter(city == "Merano"),
        count = "Italy"
    )
    bolzano_grids <- gen_empty_grid(
        ct_subset = it_munic_subset |> dplyr::filter(city == "Bolzano"),
        count = "Italy"
    )

    #----------------------------------------------
    # bring noise to grids

    # function for joining data with grid
    make_noise_grids <- function(
        city_noise, city_grids, src, noise_meas
        ) {
        #' @param city_noise Noise values on the city level
        #' @param city_grids Empty city grids
        #' @param src Noise source
        #' @param noise_meas Day or night noise

        # subset noise data
        noise_data <- city_noise |>
            dplyr::filter(source == src) |>
            dplyr::filter(day_night == noise_meas)

        # join with grids
        joint <- st_join(
            city_grids,
            noise_data,
            left = TRUE,
            largest = TRUE
        )

        # return
        return(joint)
    }

    # define options
    srcs <- c("industries", "streets", "railroads", "airports")
    nsm <- c("Lden", "Lnight")

    # function to loop through options
    loop_make_grids <- function(ct_noise, ct_grids) {
        storage_list <- list()

        for(n in nsm) {
            for(s in srcs) {
                dta <- make_noise_grids(
                    city_noise = ct_noise,
                    city_grids = ct_grids,
                    src = s,
                    noise_meas = n
                )
                # some cleaning
                dta <- dta |>
                    dplyr::mutate(
                        city.x = NULL,
                        country.x = NULL
                    ) |>
                    dplyr::rename(
                        country = country.y,
                        city = city.y
                    ) |>
                    dplyr::filter(!is.na(city)) |>
                    dplyr::select(-c(city, country, source, day_night)) |>
                    sf::st_drop_geometry()

                # add source to variable names
                nam <- dta |>
                    dplyr::select(contains("noise")) |>
                    names()
                colnames(dta) <- c("grid_id", paste0(nam, "_", s, "_", n))

                name <- paste0(n, s)
                if(nrow(dta) != 0) {
                    storage_list[[name]] <- dta
                }
            }
        }

        return(storage_list)
    }

    # some cleaning for italy shapes
    # for night noise: there is a large shape disrupting everything
    bolzano_city_noise <- it_city_noise |>
        dplyr::filter(city == "Bolzano") |>
        dplyr::mutate(
            area = case_when(
                source == "streets" & day_night == "Lnight" ~ as.numeric(
                    st_area(geometry)
                ),
                TRUE ~ 0
            ),
            area_perc = round((area / as.numeric(st_area(st_union(bolzano_grids)))) * 100, digits = 2)
        ) |>
        dplyr::filter(area_perc < 66) |>
        dplyr::select(-c(area, area_perc))

    merano_city_noise <- it_city_noise |>
        dplyr::filter(city == "Merano") |>
        dplyr::mutate(
            area = case_when(
                source == "streets" & day_night == "Lnight" ~ as.numeric(
                    st_area(geometry)
                ),
                TRUE ~ 0
            ),
            area_perc = round((area / as.numeric(st_area(st_union(merano_grids)))) * 100, digits = 2)
        ) |>
        dplyr::filter(area_perc < 59) |>
        dplyr::select(-c(area, area_perc))

    # apply loop function
    munich_noise_list <- loop_make_grids(ct_noise = de_city_noise, ct_grids = munich_grids)
    leuven_noise_list <- loop_make_grids(ct_noise = be_city_noise, ct_grids = leuven_grids)
    bolzano_noise_list <- loop_make_grids(ct_noise = bolzano_city_noise, ct_grids = bolzano_grids)
    merano_noise_list <- loop_make_grids(ct_noise = merano_city_noise, ct_grids = merano_grids)
    
    #----------------------------------------------
    # row wise join data

    row_join_data <- function(city_noise_list, ct_grids, city) {
        # add empty city grid to list
        dta_list <- c(list(ct_grids), city_noise_list)

        # merge by grid ID
        dta <- dta_list |>
            purrr::reduce(left_join, by = "grid_id")

        # replace NAs in noise columns with zero
        noise_cols <- dta |>
            sf::st_drop_geometry() |>
            dplyr::select(contains("noise")) |>
            names()

        dta <- dta |>
            dplyr::mutate(
                across(
                    .cols = all_of(noise_cols),
                    ~ case_when(
                        is.na(.x) ~ 0,
                        TRUE ~ .x
                    )
                )
            )

        # add noise values accordingly to
        # https://www.hlnug.de/themen/laerm/akustische-grundlagen/rechnen-mit-schallpegeln
        if(city == "Munich" | city == "Bolzano") {
            dta <- dta |>
                dplyr::mutate(
                    noise_low_adj_Lden = 10*log10(
                        (10^(0.1 * noise_low_industries_Lden)) +
                        (10^(0.1 * noise_low_railroads_Lden)) +
                        (10^(0.1 * noise_low_streets_Lden))
                    ),
                    noise_low_adj_Lnight = 10*log10(
                        (10^(0.1 * noise_low_industries_Lnight)) +
                        (10^(0.1 * noise_low_railroads_Lnight)) +
                        (10^(0.1 * noise_low_streets_Lnight))
                    )
                )
        } else if(city == "Leuven") {
            dta <- dta |>
                dplyr::mutate(
                    noise_low_adj_Lden = 10*log10(
                        (10^(0.1 * noise_low_airports_Lden))+
                        (10^(0.1 * noise_low_railroads_Lden))+
                        (10^(0.1 * noise_low_streets_Lden))
                    ),
                    noise_low_adj_Lnight = 10*log10(
                        (10^(0.1 * noise_low_airports_Lnight))+
                        (10^(0.1 * noise_low_railroads_Lnight))+
                        (10^(0.1 * noise_low_streets_Lnight))
                    )
                )
        } else {
            # Merano has only street noise therefore makes no sense to sum data
            dta <- dta |>
                dplyr::mutate(
                    noise_low_adj_Lden = noise_low_streets_Lden,
                    noise_low_adj_Lnight = noise_low_streets_Lnight
                )
        }

        # because of calculation formula, when all values are zero
        # you get a non-zero value
        dta <- dta |>
            mutate(
                across(
                    .cols = all_of(contains("adj")),
                    ~ case_when(
                        .x < 45 ~ NA,
                        TRUE ~ .x
                    )
                )
            ) |>
            # keep only relevant columns
            dplyr::select(country, city, noise_low_adj_Lden, noise_low_adj_Lnight, geometry)

        dta <- dta |>
            as.data.table()

        # return
        return(dta)
    }

    munich_data <- row_join_data(munich_noise_list, ct_grids = munich_grids, city = "Munich")
    bolzano_data <- row_join_data(bolzano_noise_list, ct_grids = bolzano_grids, city = "Bolzano")
    merano_data <- row_join_data(merano_noise_list, ct_grids = merano_grids, city = "Merano")
    leuven_data <- row_join_data(leuven_noise_list, ct_grids = leuven_grids, city = "Leuven")
    
    #----------------------------------------------
    # combine all

    all_city_noise <- rbind(
        munich_data,
        bolzano_data,
        merano_data,
        leuven_data,
        fill = TRUE
    )

    #----------------------------------------------
    # set geometry
    all_city_noise <- st_set_geometry(all_city_noise, all_city_noise$geometry)

    #----------------------------------------------
    # plot adjusted noise values
    # to ensure everything looks right

    plot_adjusted <- function(ct, val) {
        # subset city noise
        city_noise <- all_city_noise |>
            filter(city == ct)

        # generate plot
        map_noise <- ggplot()+
            geom_sf(
                data = city_noise,
                aes(geometry = geometry, fill = .data[[val]])
            )+
            scale_fill_viridis_c(
                option = "magma",
                direction = -1,
                name = "Noise (in dB) \n[adjusted]",
                na.value = "grey60"
            )+
            geom_sf(
                data = city_shapes |> filter(city == ct),
                aes(geometry = geometry),
                fill = NA,
                col = "black"
            )+
            theme_void()

        if(stringr::str_detect(val, "Lden")) {
            varname <- "noise_day_adjusted"
        } else {
            varname <- "noise_night_adjusted"
        }

        filename <- paste0(tolower(ct), "_", varname, ".png")
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

    # define options
    cts <- unique(city_shapes$city)
    vals <- c("noise_low_adj_Lden", "noise_low_adj_Lnight")

    # loop through options
    for(c in cts) {
        for(v in vals) {
            plot_adjusted(ct = c, val = v)
        }
    }

    #----------------------------------------------
    # export
    qs::qsave(
        all_city_noise,
        file.path(
            data_path,
            "noise_shapes/city_noise.qs"
        )
    )

    #----------------------------------------------
    # return
    return(all_city_noise)
}