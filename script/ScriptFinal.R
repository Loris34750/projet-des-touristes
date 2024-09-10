# A propos du script ----

# Objectif du script : 

# Auteurs : Cattanéo Tifaine, Gonzalez Loris, Pidoux Ella, Vergnol Marion

# Contacts : tifaine.cattaneo@agroparistech.fr,
# loris.gonzalez@agroparistech.fr,
# ella.pidoux@agroparistech.fr,
# marion.vergnol@agroparistech.fr

# Dernière mise à jour : 


# Installation des packages ----


# Charger les library ----
library(happign)
library(sf)  # manipuler les données vecteurs
library(tmap)
library(osmdata)  # manipuler les données d'openstreetmap
library(osrm)  # manipuler les données d'openstreetmap
library(dplyr)


# Dossier de travail ----
setwd("")


# Fonctions ----


# Partie 1 : Identification de la forêt ----

# sélection de la forêt par un point
point_foret <- mapedit::drawFeatures()

# délimitation de la surface de la forêt (polygone)
surface_foret <- get_wfs(x = point_foret,
                         layer = "BDTOPO_V3:foret_publique")

# délimitation du périmètre de la forêt (ligne)
perimetre_foret <- st_boundary(surface_foret)


# Partie 2 : Identification des points de parking en forêt et à 500m autour ----

# faire une surface de 500m autour du périmètre de la forêt
buffer_perim_foret <- st_buffer(perimetre_foret,
                                500)

# faire nouvelle surface de recherche des parking dans la forêt et à 500m autour
surface_rech_parking <- st_union(buffer_perim_foret["geometry"],
                                 surface_foret["geometry"])

# création d'une bbox
bbox_foret <- st_bbox(surface_rech_parking)

# recherche des points de parking référencés dans openstreetmap
query_parking <- opq(bbox = bbox_foret) |>
  add_osm_feature(key = 'amenity',
                  value = c('parking'))

# création d'une couche vecteur avec les points de parking
osm_parking <- osmdata_sf(query_parking)
parking_sf <- osm_parking$osm_points

# suppression des points au-delà de la zone de recherche (forêt et 500m autour)
parking_foret <- st_intersection(parking_sf["geometry"],
                                 surface_rech_parking["geometry"])

# identification pour chaque point des autres points à moins de 100m
dist_parking <- st_is_within_distance(parking_foret,
                                      dist = 100)

# création d'un identifiant pour les groupes de points à moins de 100m les un
# des autres
parking_foret$cluster_id <- sapply(seq_along(dist_parking),
                                   function(i) min(dist_parking[[i]]))

# fusionner les points avec le même numéro de groupe et créer un unique point
# centroïde pour les nouveaux groupes
groupe_parking <- parking_foret %>%
  group_by(cluster_id) %>%
  summarise(geometry = st_centroid(st_combine(geometry))) %>%
  ungroup()


# Partie 3 : Identification des villes de plus de 5000 habitants à moins ----
# de 30 min en voiture des parking de la forêt

# calcul des isochrones de 30 min en voiture des parking de la forêt
iso_30 <- osrmIsochrone(groupe_parking["geometry"],
                        breaks = 30,
                        res = 20)

# récupération les informations des communes concernées par les isochrones
commune_iso <- get_wfs(x = iso_30,
                       layer = "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune")

# sélection des communes de plus de 5000 habitants
commune_5000 <- commune_iso[commune_iso$population >= 5000, ]
