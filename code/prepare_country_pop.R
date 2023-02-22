prepare_country_pop <- function() {

    #----------------------------------------------
    # load data

    pop_data <- suppressMessages(
        readxl::read_excel(
            file.path(
                data_path,
                "population_countries/tps00001_page_spreadsheet.xlsx"
            ),
            sheet = "Sheet 1",
            skip = 7,
            na = ":"
        )
    ) |>
    as.data.frame()

    #----------------------------------------------
    # cleaning steps

    # drop unwanted columns
    pop_data_prep <- pop_data |>
        select(-contains(".."))

    # rename columns
    colnames(pop_data_prep) <- c(
        "country", paste0("pop_", seq(2011, 2020, 1))
    )

    # drop unwanted rows
    pop_data_prep <- pop_data_prep |>
        slice(6:55)

    # remove string "E7" in some values
    # adjust the E7 numbers by multiplying with 10 mio
    # make population values numeric
    pop_data_prep <- pop_data_prep |>
        mutate(
            across(
                .cols = all_of(contains("pop")),
                ~ case_when(
                    str_detect(.x, pattern = "E7") == TRUE ~ 
                        as.numeric(
                            str_sub(.x, start = 1L, end = -3L)
                        ) * 10000000,
                    TRUE ~ as.numeric(
                        .x
                    )
                )
            )
        )

    # rename some countries
    # remove France metropolitan
    pop_data_prep <- pop_data_prep |>
        mutate(
            country = case_when(
                str_detect(country, pattern = "Germany") ~ "Germany",
                str_detect(country, pattern = "Kosovo") ~ "Kosovo",
                str_detect(country, pattern = "Türkiye") ~ "Turkey",
                TRUE ~ country
            )
        ) |>
        filter(country != "France (metropolitan)")

    #----------------------------------------------
    # return
    return(pop_data_prep)
}