analyzing_noise_exposure <- function(noise_exposure_prep, country_pop_prep) {
    #' @title Analysis of noise exposure data
    #' 
    #' @description This function analyzes the number of exposed people as
    #' given by the EEA.
    #' 
    #' @param noise_exposure_prep Prepared noise exposure data (subsetted on
    #' the countries of interest)
    #' 
    #' @return Generates mainly output plots
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # prepare population data
    # use 2020 population
    pop <- country_pop_prep |>
        filter(country %in% unique(noise_exposure_prep$country)) |>
        select(country, pop_2020)

    #----------------------------------------------
    # summarise noise exposure by country

    # define function
    sum_by_country <- function(agglo = FALSE) {
        #' @param agglo Defines whether the entire sample or only agglomeration
        #' areas should be considered
        
        if(agglo == TRUE) {
            dta <- noise_exposure_prep |>
                filter(main_agglo == "agglo")
        } else {
            dta <- noise_exposure_prep
        }

        noise_exp_country <- dta |>
            group_by(country) |>
            summarise(
                across(
                    .cols = all_of(c(contains("lden"), contains("lnight"))),
                    ~ sum(.x, na.rm = TRUE)
                )
            ) |>
            as.data.frame()
        
        # add country population
        noise_exp_country_perc <- merge(
            noise_exp_country,
            pop,
            by = "country"
        )

        # calculate share
        noise_exp_country_perc <- noise_exp_country_perc |>
            mutate(
                across(
                    .cols = all_of(c(contains("lden"), contains("lnight"))),
                    ~ round((.x / pop_2020) * 100, digits = 2)
                )
            ) |>
            select(-pop_2020)

        # make one table
        complete_table <- list()

        for(row in seq(1, nrow(noise_exp_country), 1)){
            new_table <- rbind(
                noise_exp_country[row,],
                noise_exp_country_perc[row, ]
            )
            complete_table[[row]] <- new_table
        }

        complete_table <- data.table::rbindlist(complete_table)

        # return output
        return(complete_table)
    }

    # summarise for the entire sample
    sum_all <- sum_by_country(agglo = FALSE)

    # summarise only agglomeration areas
    sum_agglo <- sum_by_country(agglo = TRUE)
    
    # export tables
    openxlsx::write.xlsx(
        sum_all,
        file.path(
            output_path,
            "descriptives/noise_exposure_country.xlsx"
        ),
        rowNames = FALSE
    )

    openxlsx::write.xlsx(
        sum_agglo,
        file.path(
            output_path,
            "descriptives/noise_exposure_country_agglomeration.xlsx"
        ),
        rowNames = FALSE
    )

    #----------------------------------------------
    # make bar plots for those countries that also have noise shapes

    # subset for countries with noise shapes
    # keep only percent values
    sample_countries <- sum_all |>
        filter(country %in% c("Germany", "Belgium", "Italy")) |>
        slice(seq(2, nrow(sample_countries), 2))

    # define colors
    pal <- MetBrewer::met.brewer(name = "Java", n = 5)

    # define plot function
    bar_plots <- function(noise_meas = c("lden", "lnight")) {
        # pivot table
        sample_data <- sample_countries |>
            select(country, contains(noise_meas)) |>
            pivot_longer(
                -country,
                names_to = "noise_cat",
                values_to = "exposed"
            ) |>
            as.data.frame()

        # define labels for categories
        if(noise_meas == "lden") {
            labs <- c(
                "55-59", "60-64", "65-69", "70-74", "75 and above"
            )
        } else {
            labs <- c(
                "50-54", "55-59", "60-64", "65-70", "70 and above"
            )
        }

        # plot
        bars <- ggplot(
                data = sample_data,
                aes(fill = country, x = exposed, y = noise_cat)
            )+
            geom_bar(
                position = "dodge",
                stat = "identity"
            )+
            scale_y_discrete(
                name = "Noise category (in dB)",
                labels = labs
            )+
            scale_x_continuous(
                name = "Exposed population (in %)",
                breaks = seq(0, 10, 1)
            )+
            scale_fill_manual(
                name = "",
                values = c(
                    "Italy" = pal[5],
                    "Germany" = pal[1],
                    "Belgium" = pal[3]
                )
            )+
            theme_classic()+
            theme(
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12)
            )
        
        return(bars)
    }

    # generate plots
    bar_lden <- bar_plots(noise_meas = "lden")
    bar_lnight <- bar_plots(noise_meas = "lnight")

    # export
    ggsave(
        plot = bar_lden,
        file.path(
            output_path,
            "graphs/exposed_pop_countries_lden.png"
        ),
        dpi = owndpi
    )

    ggsave(
        plot = bar_lnight,
        file.path(
            output_path,
            "graphs/exposed_pop_countries_lnight.png"
        ),
        dpi = owndpi
    )
}