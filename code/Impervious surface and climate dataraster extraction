
#load raster and site files
mean_precip <- raster('rain.tif')

sites <- read.csv(sites)

#change site file type
sites.sp <- SpatialPointsDataFrame(sites[,3:2], sites)

#declare starting projection system for sites (lat/long)
points <- SpatialPoints(sites.sp, proj4string =CRS("+proj=longlat +datum=WGS84"))


#transform site points to the same crs as the raster file 
#(only needed if they don't already match!) Run CRS for each to check
pp <- spTransform(points, "EPSG:3035")

plot(pp)
plot(sites.sp, add = TRUE)

#make buffer polygons with 150m radius for each site
buffer1<-buffer(pp, width= 150, dissolve=F) 

#find buffer extent
extent(buffer1)
extent(points)

#change the extent of the raster file to the same as that for the buffer and crop (makes raster file smaller and easier to plot)
e <- as(extent(6.414409, 14.653, 47.86848, 54.2179), 'SpatialPolygons')
crs(e) <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
Germany <- crop(mean_temp, e)


#make sure sites are in the right place on map
plot(Germany)
plot(points, add = TRUE)
plot(buffer1, add = TRUE)

#A - use to extract climate data
sites$mean_temp <- extract(mean_temp, buffer1)

# OR

#B - use to extract landcover data
freqs <- exact_extract(mean_precip, buffer1, function(value, coverage_fraction) {
  data.frame(value = value,
             frac = coverage_fraction / sum(coverage_fraction)) %>%
    group_by(value) %>%
    summarize(freq = sum(frac), .groups = 'drop') %>%
    pivot_wider(names_from = 'value',
                names_prefix = 'freq_',
                values_from = 'freq')
}) %>% 
  mutate(across(starts_with('freq'), replace_na, 0))

#export csv file
write.csv2(sites, file = "climate_data.csv", row.names = FALSE)
