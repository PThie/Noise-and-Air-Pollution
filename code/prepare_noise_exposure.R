prepare_noise_exposure <- function() {
    #' @titlte Preparing noise exposure data
    #' 
    #' @description This function prepares the noise exposure data. In
    #' particular, subsets for the countries of interest.
    #' 
    #' @return Returns prepared data
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # read original data

    org_data <- data.table::fread(
        file.path(
            data_path,
            "noise_exposure/exposure_numbers_prep.csv"
        ),
        na.strings = "",
        sep = ";"
    )

    #----------------------------------------------
    # select the countries of interest

    countries_int <- c(
        "Belgium", "Germany", "Hungary", "Greece", "Italy", "Malta"
    )

    subset_data <- org_data |>
        filter(country %in% countries_int)
    
    #----------------------------------------------
    # make numeric
    # replace negative values with NA
    subset_data <- subset_data |>
        mutate(
            across(
                .cols = all_of(c(contains("lden"), contains("lnight"))),
                ~ as.numeric(.x)
            ),
            across(
                .cols = all_of(c(contains("lden"), contains("lnight"))),
                ~ replace(.x, .x < 0, NA)
            )
        )
    
    #----------------------------------------------
    # return
    return(subset_data)
}