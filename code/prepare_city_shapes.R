prepare_city_shapes <- function() {
    #' @title Preparing city shapes
    #' 
    #' @description This function prepares the city shapes of the considered
    #' CiPeLs (Munich, Leuven, Bolzano, and Merano).
    #' 
    #' @return Shape data set with city borders
    #' @author Patrick Thiel

    #----------------------------------------------
    # load data

    # germany municipalities
    de_munic <- sf::st_read(
        "M:/_FDZ/interne Daten/Gebietseinheit/Gemeinde/2019/VG250_GEM.shp",
        quiet = TRUE
    )
    de_munic <- st_transform(de_munic, crs = utmcrs)

    # italy municipalities
    it_munic <- sf::st_read(
        file.path(
            data_path,
            "administrative_borders/Italy/Municipal_Boundaries_of_Italy_2019/Com01012019_WGS84.shp"
        ),
        quiet = TRUE
    )
    it_munic <- st_transform(it_munic, utmcrs)

    # belgium municipalities
    be_munic <- sf::st_read(
        file.path(
            data_path,
            "administrative_borders/Belgium/municipalities/communes_L08.shp"
        ),
        quiet = TRUE
    )
    be_munic <- st_transform(be_munic, utmcrs)

    #----------------------------------------------
    # subset municipality data

    de_munic_subset <- de_munic |>
        dplyr::mutate(
            GEN = stringi::stri_trans_general(GEN, "de-ASCII; Latin-ASCII")
        ) |>
        dplyr::filter(GEN == "Muenchen") |>
        dplyr::select(city = GEN, geometry) |>
        dplyr::mutate(
            city = case_when(
                city == "Muenchen" ~ "Munich",
                TRUE ~ city
            )
        )

    it_munic_subset <- it_munic |>
        filter(COMUNE == "Bolzano" | COMUNE == "Merano") |>
        dplyr::select(COMUNE, geometry) |>
        rename(city = COMUNE)

    be_munic_subset <- be_munic |>
        filter(Name == "Leuven") |>
        dplyr::select(city = Name, geometry)

    #----------------------------------------------
    # combine

    munic_subset <- rbind(
        de_munic_subset,
        it_munic_subset,
        be_munic_subset
    )

    #----------------------------------------------
    # return
    return(munic_subset)
}