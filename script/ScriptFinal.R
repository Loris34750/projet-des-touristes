# A propos du script ----

# Objectif du script : 

# Auteurs : Cattanéo Tifaine, Gonzalez Loris, Pidoux Ella, Vergnol Marion

# Contacts : tifaine.cattaneo@agroparistech.fr,
# loris.gonzalez@agroparistech.fr,
# ella.pidoux@agroparistech.fr,
# marion.vergnol@agroparistech.fr

# Dernière mise à jour : 


# Installation des packages et chargement library ----

# Pour installer et mettre à jour les packages
install.packages(librarian)
librarian::shelf(happign, osmdata, sf, dplyr, osrm, spplot, viridis)
library(tmap);ttm()

# library(happign) pour les données Web et IGN
# library(sf) pour manipuler les données vecteurs
# library(tmap) pour la visualisation des cartes
# library(osmdata) pour manipuler les données d'openstreetmap
# library(osrm) pour manipuler les données d'openstreetmap
# library(dplyr)


# Dossier de travail ----
setwd("E:/APT/GF/UE2_R_SIG/ProjetR")


# Fonctions ----

# Fonction pour création de buffer de x mètres et choix couleur buffer
buffer.points <- function(sf,x, color){
  buffer <- st_buffer(sf,x)
  map <- tm_shape(buffer) + tm_polygons(col = color)
}

# Fonction buffer de pression pour les parkings
pression.buffer <- function(sf){
  grde_pression <- st_buffer(sf,250)
  moy_pression <- st_buffer(grde_pression,150)
  ptit_pression <- st_buffer(moy_pression,50)
  map <- tm_shape(surface_foret) + 
    tm_borders(col = 'black')
  map <- map + tm_shape(ptit_pression) + tm_polygons(col = 'green')
  map <- map + tm_shape(moy_pression) + tm_polygons(col = 'orange')
  map <- map + tm_shape(grde_pression) + tm_polygons(col = 'red')
}


is_empty_sf <- function(sf) {
  return(nrow(sf) == 0)
}

# Buffer en fonction de l'importance de la route
buffer.taille.couleur <- function(sf, y, x, color){
  # y = importance ; x = dist ; color = couleur
  routes <- subset(sf, sf$importance == y)
  buffer <- st_buffer(routes, dist = x)
  
  if (!is_empty_sf(buffer)) {
    return(tm_shape(buffer) + tm_polygons(col = color))
  } 
}

# Taille et Couleur différentes des buffers
buffer.diff.routes <- function(sf) {
  # Initialisation de la carte avec les bordures
  map <- tm_shape(surface_foret) + 
    tm_borders(col = 'black')
  
  # Ajouter les buffers de taille et couleur différentes
  map <- map + buffer.taille.couleur(sf, 1, 150, 'inferno')
  map <- map + buffer.taille.couleur(sf, 2, 110, 'red')
  map <- map + buffer.taille.couleur(sf, 3, 100, 'orange')
  map <- map + buffer.taille.couleur(sf, 4, 80, 'yellow')
  map <- map + buffer.taille.couleur(sf, 5, 50, 'cyan')
  map <- map + buffer.taille.couleur(sf, 6, 25, 'green')
  
  # Afficher la carte
  print(map)
}

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

# buffer de pression du grand public autour des parkings 
pression_GP_parking <- pression.buffer(groupe_parking)
print(pression_GP_parking)


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

# pression commune selon nbr d'habitants 
# buffer en fonction de la population # BUG !!
petit_commune <- commune_5000$population <= 7000
moy_commune <- commune_5000$population <= 10000
grde_commune <- commune_5000$population > 10000

pression_commune <- buffer.points(petit_commune, 
                                  x = 100, 
                                  color = "green")
pression_commune <- pression_commune + buffer.points(moy_commune,
                                                     x = 500, 
                                                     color = "yellow")
pression_commune <- pression_commune + buffer.points(grde_commune,
                                                     x = 500, 
                                                     color = "red")

print(pression_commune)

# Partie 4 : pression des routes ----

routes_foret <- get_wfs(surface_foret,
                           "BDTOPO_V3:troncon_de_route",
                           spatial_filter = "intersects") 

pression_routes <- buffer.diff.routes(routes_foret)
tmap_mode("view")  # Passe en mode interactif
print(pression_routes)  # Visualisation de la pression des routes 
