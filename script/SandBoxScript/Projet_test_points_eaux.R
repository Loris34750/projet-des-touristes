#ceci est un test de Tifaine pour récupérer les points d'eaux
library(happign)
library(tmap);ttm()
library(osmdata)
library(sf)

#on regarde ce qui existe dans la bdd IGN
get_apikeys()
layers_env <- get_layers_metadata("wfs","environnement")

#on récupère le périmètre de la forêt
point_foret <- mapedit::drawFeatures()  # sélection point forêt
surface_foret <- get_wfs(x = point_foret,
                         layer = "BDTOPO_V3:foret_publique")  # délimitation surface forêt (polygone)
perimetre_foret <- st_boundary(surface_foret)  # délimitation périmètre forêt (ligne)
qtm(perimetre_foret)

#on récupère les points d'eaux avec OSM

bbox_foret <- st_bbox(surface_rech_parking)  # création bbox pour la suite


query_parking <- opq(bbox = bbox_foret) |>
  add_osm_feature(key = 'amenity', value = c('parking'))
osm_parking <- osmdata_sf(query_parking)
parking_sf <- osm_parking$osm_points  # extraction points parking