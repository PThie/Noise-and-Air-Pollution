read_pollution <- function() {
    #' @title Read air pollution data
    #' 
    #' @description Reads air pollution data for 2018 and stores it as geotiff
    #' data
    #' 
    #' @return Returns orginal data as geotiff data
    #' @author Patrick Thiel

    #----------------------------------------------
    # read data for 2018

    # path to data
    file <- file.path(
        data_path,
        "air_pollution/1km/Annual/V5GL03.HybridPM25.Europe.201801-201812.nc"
    )

    # transform into raster
    nc2raster <- raster(file, varname = "GWRPM25", band = 1)
    nc2raster <- stack(file, varname = "GWRPM25")

    # export
    writeRaster(
        nc2raster,
        file.path(
            data_path,
            "air_pollution/1km/air_poll_prep_2018.tif"
        ),
        format = "GTiff",
        overwrite = TRUE
    )
}