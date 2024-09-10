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

bbox_foret <- st_bbox(surface_foret)  # création bbox pour la suite


query_point_eau <- opq(bbox = bbox_foret) |>
  add_osm_feature(key = 'water', 
                  value = c('river', 'oxbox', 'canal', 'ditch', 'lake',
                            'reservoir', 'pond', 'stream_pool', 'river', 'stream'))

query_point_eau <- opq(bbox = bbox_foret) |>
  add_osm_feature(key = 'waterway', 
                  value = c('watefall', 'stream'))

osm_point_eau <- osmdata_sf(query_point_eau)
point_eau_sf <- osm_point_eau$osm_lines  # extraction polygones point d'eau

qtm(point_eau_sf)

