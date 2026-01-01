
if (!require('geodata')) install.packages("geodata")
if (!require('terra')) install.packages("terra")

library(geodata)
library(terra)


# Administrative shapefiles-----------------------------------------------------

zwe0 = gadm(country = 'ZWE', level = 0, path = "Geodata")
plot(zwe0)

zwe1 = gadm(country = 'ZWE', level = 1, path = "Geodata")
plot(zwe1)


# Cropland mask-----------------------------------------------------------------

cropland = geodata::cropland(source = "WorldCover", year = 2019, path = "Geodata")
cropland = crop(cropland, zwe0)
cropland = mask(cropland, zwe0)
cropland = ifel(cropland$cropland > 0, 1, NA)

plot(zwe0)
plot(cropland, col = "forestgreen", add = T, legend = F)
plot(zwe1, add = T)


# Rasters of predictors---------------------------------------------------------

elevation = geodata::elevation_30s(country = 'ZWE', path = "Geodata")
elevation = crop(elevation, cropland)
elevation = mask(elevation, cropland)
names(elevation) = 'elevation'

plot(zwe0)
plot(elevation, add = T, legend = T)
plot(zwe1, add = T)


sand_5 = geodata::soil_af(var = 'sand', depth = 5, path = "Geodata")
sand_15 = geodata::soil_af(var = 'sand', depth = 15, path = "Geodata")
sand_30 = geodata::soil_af(var = 'sand', depth = 30, path = "Geodata")
sand = (sand_5 * 5 + sand_15 * 10 + sand_30 * 15) / (5 + 10 + 15)
sand = terra::crop(sand, cropland)
sand = terra::mask(sand, cropland)
names(sand) = 'sand'

plot(zwe0)
plot(sand, add = T, legend = T)
plot(zwe1, add = T)


tavg = worldclim_country(country = 'ZWE', var = 'tavg', path = "Geodata", res = 0.5)
tavg = mean(tavg)
tavg = terra::crop(tavg, cropland)
tavg = terra::mask(tavg, cropland)
names(tavg) = 'tavg'

plot(zwe0)
plot(tavg, add = T, legend = T)
plot(zwe1, add = T)


prec = worldclim_country(country = 'ZWE', var = 'prec', path = "Geodata", res = 0.5)
prec = sum(prec)
prec = terra::crop(prec, cropland)
prec = terra::mask(prec, cropland)
names(prec) = 'prec'

plot(zwe0)
plot(prec, add = T, legend = T)
plot(zwe1, add = T)


pop = geodata::population(year = 2020, path = "Geodata")
pop = resample(pop, cropland)
pop = crop(pop, cropland)
pop = mask(pop, cropland)
names(pop) = 'pop'

plot(zwe0)
plot(pop, add = T, legend = T)
plot(zwe1, add = T)


travel_time = travel_time(to = "city", size = 6, up = TRUE, path = "Geodata")
travel_time = crop(travel_time, cropland)
travel_time = mask(travel_time, cropland)
names(travel_time) = "travel_time"

plot(zwe0)
plot(travel_time, add = T, legend = T)
plot(zwe1, add = T)

