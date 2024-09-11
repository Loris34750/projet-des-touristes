library(happign)
library(tmap);ttm()
library(osmdata)
library(sf)

point_foret <- mapedit::drawFeatures()  # sélection point forêt
surface_foret <- get_wfs(x = point_foret,
                         layer = "BDTOPO_V3:foret_publique",
                         spatial_filter = "intersects")

perimetre_foret <- st_boundary(surface_foret)


bbox_foret <- st_bbox(surface_foret)  # création bbox pour la suite


query_sentier <- opq(bbox = bbox_foret) |>
  add_osm_feature(key = 'highway',
                  value = c('track', 'cycleway', 'footway', 'bridleway', 'path'))

osm_sentier <- osmdata_sf(query_sentier)
sentier_sf <- osm_sentier$osm_lines  # extraction points parking

sentier_foret <- st_intersection(sentier_sf["geometry"],
                                 surface_foret["geometry"]) 
