setwd("C:/Users/tanvi/Downloads")
library(terra)
library(sf)
vector_data <- st_read("C:/Users/tanvi/Downloads/lisseddata/lisseddata.shp")
# Define resolution (adjust as needed)
res_value <- 0.0083  # Set the desired resolution in degrees

# Create an empty raster with the same extent as the vector data
raster_template <- rast(ext(vector_data), resolution = res_value, crs = st_crs(vector_data)$wkt,
                        nrows=180,ncols=360)
# Rasterizing based on a specific attribute (e.g., "LITH_CODE")
raster_output <- rasterize(vector_data, raster_template, field = "LITH_CODE")
plot(raster_output)
writeRaster(raster_output, "output_raster.tif", overwrite = TRUE)


vector_data$LITH_CODE
##convert individual categories into separate objects, rasterize them, and layer them. 

##remove na values rows
df_clean <- df[!is.na(df$column_name), ]


vector_data_clean<- vector_data[ !is.na(vector_data$SED_CLASS), ]


##Sand = 18, Gravelly Sand = 11, Clayey Silt= 28, Silty sand = 23

sand<- vector_data_clean[vector_data_clean$LITH_CODE == 18, ]
Gravelsand<- vector_data_clean[vector_data_clean$LITH_CODE == 11, ]
Claysilt<- vector_data_clean[vector_data_clean$LITH_CODE == 28, ]
Siltsand<- vector_data_clean[vector_data_clean$LITH_CODE == 23, ]

sand_raster_template <- rast(ext(sand), resolution = res_value, crs = st_crs(vector_data_clean)$wkt,nrows=180,ncols=360)
sand_raster_output <- rasterize(sand, sand_raster_template, field = "LITH_CODE")


claysilt_raster_template <- rast(ext(Claysilt), resolution = res_value, crs = st_crs(vector_data_clean)$wkt,nrows=180,ncols=360)
claysilt_raster_output <- rasterize(Claysilt, claysilt_raster_template, field = "LITH_CODE")


gravelsand_raster_template <- rast(ext(Gravelsand), resolution = res_value, crs = st_crs(vector_data_clean)$wkt,nrows=180,ncols=360)
gravelsand_raster_output <- rasterize(Gravelsand, gravelsand_raster_template, field = "LITH_CODE")


siltsand_raster_template <- rast(ext(Siltsand), resolution = res_value, crs = st_crs(vector_data_clean)$wkt,nrows=180,ncols=360)
siltsand_raster_output <- rasterize(Siltsand, siltsand_raster_template, field = "LITH_CODE")


plot(claysilt_raster_output)

writeRaster(sand_raster_output, "sand.tif", overwrite = TRUE)
writeRaster(claysilt_raster_output, "claysilt.tif", overwrite = TRUE)
writeRaster(gravelsand_raster_output, "gravelsand.tif", overwrite = TRUE)
writeRaster(siltsand_raster_output, "siltsand.tif", overwrite = TRUE)

ras<-raster("C:/Users/tanvi/OneDrive/Documents/GitHub/Bio585/CompBio/Bathymetry.asc")
plot(ras2)
points(oyster2)

plot(oyster2)

oyster2<- oyster2[, c("decimalLongitude", "decimalLatitude")]

extracted<- extract(ras2, oyster2, method = "bilinear", na.rm = "F")  
extracted<- extract(ras2, oyster2, method = "bilinear", na.rm = "TRUE")


ras2<- raster("C:/Users/tanvi/OneDrive/Documents/GitHub/Bio585/CompBio/bathymetry84.tif")


extracted.clean<- as.data.frame(extracted)

extracted.clean<- as.data.frame(na.omit(extracted))


values(ras)[is.na(values(ras))==FALSE]

length(cells_with_data)<- which(is.na(values(ras))==FALSE)##these cells have numbers in them


subset_of_cells<- sample(cells_with_data, size = 25000, replace = F)


my_cell_values<- values(ras)[subset_of_cells]

rasterToPoints(ras) #gives lat and long for all cells with data

#make a new rater where only the subset cells have data

ras2<- ras

values(ras2)<- NA
values(ras2)[subset_of_cells]<- values(ras)[subset_of_cells]

background<- as.data.frame(rasterToPoints(ras2))

background$species<- "background"

background<- background[, c ("species", "x", "y", "Bathymetry")]
write.table(background, "~/background.csv", sep = ",", row.names = FALSE)



##try to get r or maxent.jar (on command line) to take my bathymetry file and lambdas file and get the function to project my suitability model 
##email dr provost tmr 
#MIAmaxent
#maxentmodelselectr






