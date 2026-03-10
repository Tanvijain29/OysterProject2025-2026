library(raster)
setwd("C:/Users/tanvi/OneDrive/Desktop/Comp Bio")
my_raster<- raster("POP.asc")
e<- c (-87.5499999999999972, -29.5416666689870056,-20.2500000000000000,11.0499999987479960)
my_raster_cropped <- crop(my_raster, e)
writeRaster(my_raster_cropped,
            filename = "myfile_cropped.asc",
            format   = "ascii",
            overwrite = TRUE)
