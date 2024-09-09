library(osmdata)
library(happign)
library(sf)
library(tmap)
library(ggplot2)


# Code Paul ----

# Define the area of interest and query for parking
nancy <- get_apicarto_cadastre("54395", type = "commune") |>
  st_transform(4326) |>
  st_bbox()

# script Loris ----
bbox_foret <- getbb("Forêt d'Amance, France")
foret <- opq(bbox = bbox_foret) %>%
   add_osm_feature(key = "landus", value = "forest") %>%
   osmdata
ggplot() + geom_sf(data = foret$osm_polygons, fill = "darkgreen", color = "black")


# test 1 ----
point_foret <- mapedit::drawFeatures()

surface_foret <- get_wfs(x = point_foret,
                         layer = "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale")
qtm(surface_foret)

chemin_shapefile <- "C:/Collège-Lycée-Etudes/FIF Nancy 3ème année/Semaine R/projet/surface_foret.shp"
st_write(surface_foret,
         chemin_shapefile,
         "surface_foret.shp",
         layer = "surface_foret")

#foret <- read_sf(system.file("extdata/surface_foret.shp", package = "happign"))
foret <- st_read(chemin_shapefile)

foret_carto <- get_apicarto_cadastre(foret)

# test 2 ----
point_foret <- mapedit::drawFeatures()
communes_foret <- get_wfs(x = point_foret,
                             layer = "ADMINEXPRESS-COG.LATEST:commune")
qtm(communes_foret)

communes_insee <- as.list(communes_foret$insee_com)

foret2 <- get_apicarto_cadastre(c("54100","54012"), type = "parcelle")

# test 3 ----

point_foret <- mapedit::drawFeatures()
surface_foret <- get_wfs(x = point_foret,
                         layer = "BDTOPO_V3:foret_publique")

bbox_foret <- st_bbox(surface_foret)


query_parking <- opq(bbox = bbox_foret) |>
  add_osm_feature(key = 'amenity', value = c('parking'))
osm_parking <- osmdata_sf(query_parking)
parking_sf <- osm_parking$osm_points

query_water <- opq(bbox = bbox_foret) |>
  add_osm_feature(key = 'natural', value = c('water'))
osm_water <- osmdata_sf(query_water)
water_sf <- osm_water$osm_polygons

#surface_foret_4326 <- st_transform(surface_foret,
#                                   4326)

qtm(surface_foret)
qtm(parking_sf)
qtm(water_sf)

tmap_mode("view")
#tm_shape(parking_sf) +
#  tm_dots(col = "red", size = 0.1, title = "Parkings") +
#  tm_basemap("OpenStreetMap")

# amenity parking
# boundary forest
# amenity townhall
# place municipality
# highway
# natural wood
# natural water

