prepare_pollution <- function(city_shapes) {
    #' @title Prepare air pollution data
    #' 
    #' @description Prepares the air pollution data for 2018
    #' 
    #' @param city_shapes Clean city borders
    #' 
    #' @return QS file with CiPeL grid pollution
    #' @author Patrick Thiel

    #----------------------------------------------
    # read data

    # read raster data
    org_geotiff <- stars::read_stars(
        file.path(
            data_path,
            "air_pollution/1km/air_poll_prep_2018.tif"
        )
    )

    #----------------------------------------------
    # clean pollution data
    
    # transform to sf
    org_sf <- st_as_sf(org_geotiff)
    org_sf <- st_transform(org_sf, utmcrs)

    # rename columns
    colnames(org_sf) <- c("pollution", "geometry")

    #----------------------------------------------
    # subset municipality data

    de_munic_subset <- city_shapes |>
        dplyr::filter(city == "Munich")

    it_munic_subset <- city_shapes |>
        dplyr::filter(city == "Bolzano" | city == "Merano")

    be_munic_subset <- city_shapes |>
        dplyr::filter(city == "Leuven")

    #----------------------------------------------
    # intersect pollution data with city data
    de_poll <- st_intersection(
        de_munic_subset,
        org_sf
    )

    it_poll <- st_intersection(
        it_munic_subset,
        org_sf
    )

    be_poll <- st_intersection(
        be_munic_subset,
        org_sf
    )

    #----------------------------------------------
    # combine all

    city_poll <- rbind(
        de_poll,
        it_poll,
        be_poll
    )

    #----------------------------------------------
    # export

    qs::qsave(
        city_poll,
        file.path(
            data_path,
            "air_pollution/1km/city_pollution.qs"
        )
    )

    #----------------------------------------------
    # return
    return(city_poll)
}