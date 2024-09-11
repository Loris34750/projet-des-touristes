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
librarian::shelf(happign,  # pour les données Web et IGN
                 osmdata,   # pour manipuler les données d'openstreetmap
                 osrm,  # pour manipuler les données d'openstreetmap
                 sf,  # pour manipuler les données vecteurs
                 tmap,  # pour la visualisation des cartes
                 dplyr,  #
                 viridis)  # pour les palettes de couleurs

tmap_mode("view")  # passe en mode interactif pour l'affichage des cartes


# Dossier de travail ----
setwd("E:/APT/GF/UE2_R_SIG/ProjetR")


# Fonctions ----

# Fonction pour la création d'un buffer de x mètres et choix couleur buffer
buffer.points <- function(sf, x, color){
  buffer <- st_buffer(sf, x)
  map <- tm_shape(buffer) + tm_polygons(col = color)
}

# Fonction qui crée les buffers de pression qui se cumulent
pression.buffer <- function(sf){
  grde_pression <- st_buffer(sf, 500)
  moy_pression <- st_buffer(grde_pression, 250)
  ptit_pression <- st_buffer(moy_pression, 250)
  return(list(
    grde_pression = grde_pression,
    moy_pression = moy_pression,
    ptit_pression = ptit_pression
  ))
}

# Fonction pour vérifier que le sf est vide
is_empty_sf <- function(sf) {
  return(nrow(sf) == 0)
}

# Fonction qui crée un buffer en fonction de l'importance de la route
buffer.route.taille <- function(sf, y, x){
  # y = importance ; x = dist
  routes <- subset(sf,
                   sf$importance == y)
  buffer <- st_buffer(routes,
                      dist = x)
  
  if (!is_empty_sf(buffer)) {
    return(buffer)
  } 
}

# Fonction pour visualisation d'un buffer route et attribution d'une couleur
buffer.taille.couleur <- function(sf, y, x, color){
  # y = importance ; x = dist ; color = couleur
  routes <- subset(sf,
                   sf$importance == y)
  buffer <- st_buffer(routes,
                      dist = x)
  
  if (!is_empty_sf(buffer)) {
    return(tm_shape(buffer) + tm_polygons(col = color))
  } 
}

# Fonction de visulation avec couleur des buffers de routes
buffer.diff.routes <- function(sf) {
  # initialisation de la carte avec les bordures
  map <- tm_shape(surface_foret) + 
    tm_borders(col = 'black')
  
  # ajouter les buffers de taille et couleur différentes
  map <- map + buffer.taille.couleur(sf, 1, 150, 'inferno')
  map <- map + buffer.taille.couleur(sf, 2, 110, 'red')
  map <- map + buffer.taille.couleur(sf, 3, 100, 'orange')
  map <- map + buffer.taille.couleur(sf, 4, 80, 'yellow')
  map <- map + buffer.taille.couleur(sf, 5, 50, 'cyan')
  map <- map + buffer.taille.couleur(sf, 6, 25, 'green')
  
  # afficher la carte
  print(map)
}

# Partie 1 : Identification de la forêt ----

# Sélection de la forêt par un point
point_foret <- mapedit::drawFeatures()

# Délimitation de la surface de la forêt (polygone)
surface_foret <- get_wfs(x = point_foret,
                         layer = "BDTOPO_V3:foret_publique")

# Délimitation du périmètre de la forêt (ligne) SUPPRIMER
#perimetre_foret <- st_boundary(surface_foret) SUPPRIMER


# Partie 2 : Identification des points de parking en forêt et à 500m autour ----

# Faire une surface de 500m autour de la surface de la forêt
surface_rech_parking <- st_buffer(surface_foret,
                                500)

# Faire nouvelle surface de recherche des parking dans la forêt et à 500m autour
#surface_rech_parking <- st_union(buffer_perim_foret["geometry"], SUPPRIMER
#                                 surface_foret["geometry"])SUPPRIMER

# Création d'une bbox
bbox_foret <- st_bbox(surface_rech_parking)

# Recherche des points de parking référencés dans openstreetmap
query_parking <- opq(bbox = bbox_foret) |>
  add_osm_feature(key = 'amenity',
                  value = c('parking'))

# Création d'une couche vecteur avec les points de parking
osm_parking <- osmdata_sf(query_parking)
parking_sf <- osm_parking$osm_points

# Suppression des points au-delà de la zone de recherche (forêt et 500m autour)
parking_foret <- st_intersection(parking_sf["geometry"],
                                 surface_rech_parking["geometry"])

# Regrouper les points situés à moins de 200m les uns des autres et création
# d'un unique point centroïde pour les nouveaux groupements
dist_parking <- st_is_within_distance(parking_foret, dist = 200)  # Créer les clusters de points proches avec un algorithme de propagation des distances

clusters <- rep(NA, length(dist_parking))  # Créer un vecteur pour les clusters

cluster_id <- 1  # Fonction pour propager l'identifiant de cluster
for (i in seq_along(dist_parking)) {
  if (is.na(clusters[i])) {
    # Assigner un nouvel identifiant de cluster
    clusters[i] <- cluster_id
    # Propager cet identifiant à tous les voisins connectés
    queue <- dist_parking[[i]]
    while (length(queue) > 0) {
      j <- queue[1]
      queue <- queue[-1]
      if (is.na(clusters[j])) {
        clusters[j] <- cluster_id
        queue <- c(queue, dist_parking[[j]])
      }
    }
    cluster_id <- cluster_id + 1
  }
}

parking_foret$cluster_id <- clusters  # Ajouter les clusters au DataFrame

groupe_parking <- parking_foret %>%  # Calculer le centroïde de chaque groupe de points
  group_by(cluster_id) %>%
  summarise(geometry = st_centroid(st_combine(geometry))) %>%
  ungroup()

qtm(groupe_parking)

# Buffer de pression du grand public autour des parkings 
pression_gp_parking <- pression.buffer(groupe_parking)

# Accéder aux buffers de pression des parking
grde_pression_sf <- pression_gp_parking$grde_pression
moy_pression_sf <- pression_gp_parking$moy_pression
ptit_pression_sf <- pression_gp_parking$ptit_pression

# Visualisation des buffers parking
map <- tm_shape(surface_foret) + 
  tm_borders(col = 'black')
map <- map + tm_shape(ptit_pression_sf) + tm_polygons(col = 'green')
map <- map + tm_shape(moy_pression_sf) + tm_polygons(col = 'orange')
map <- map + tm_shape(grde_pression_sf) + tm_polygons(col = 'red')
print(map)


# Partie 3 : Pression sur les chemins aux abords des parking ----
troncons <- get_wfs( x = surface_foret,
                     layer = "BDTOPO_V3:troncon_de_route",
                     spatial_filter = "intersects")

chemin_foret <- troncons[troncons$nature %in% c("Sentier",
                                                "Chemin",
                                                "Route empierrée"), ]

chemin_freq <- st_intersection(chemin_foret["geometry"],
                                   pression_gp_parking$ptit_pression["geometry"])

# Visualisation des chemins les plus fréquentés
qtm(chemin_foret)

# Partie 4 : Identification des villes de plus de 5000 habitants à moins ----
# de 30 min en voiture des parking de la forêt

# Calcul des isochrones de 30 min en voiture des parking de la forêt
iso_30 <- osrmIsochrone(groupe_parking["geometry"],
                        breaks = 30,
                        res = 20)

# Récupération des informations des communes dans l'isochrone
commune_iso <- get_wfs(x = iso_30,
                       layer = "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune")

# Sélection des communes de plus de 5000 habitants
commune_5000 <- commune_iso[commune_iso$population >= 5000, ]

# Pression des communes selon le nombre d'habitants 
# Classification des communes en fonction de la population
ptit_commune <- commune_5000[commune_5000$population <= 7000, ]
moy_commune <- commune_5000[commune_5000$population > 7000 & commune_5000$population <= 10000, ]
grde_commune <- commune_5000[commune_5000$population > 10000, ]

# Visualisation de la classification des communes
# (Possibilité de former un buffer autour des limites communales)
pression_commune <- buffer.points(ptit_commune, x = 0, color = "green") +
  buffer.points(moy_commune, x = 0, color = "yellow") +
  buffer.points(grde_commune, x = 0, color = "red")

print(pression_commune)


# Partie 4 : Pression des routes ----

# Faire nouvelle surface de recherche des routes dans la forêt et à 50m autour
# (pour inclure les routes longeant la forêt sans la pénétrer)
surface_rech_route <- st_buffer(surface_foret,
                                     50)
#surface_rech_route <- st_union(buffer_ptit_perim_foret["geometry"],
#                               surface_foret["geometry"])

# Sélection des routes traversant et longeant la forêt
routes_foret <- get_wfs(surface_rech_route,
                           "BDTOPO_V3:troncon_de_route",
                           spatial_filter = "intersects") 

# Création de buffer selon la nature des routes
route_imp1 <- buffer.route.taille(routes_foret, 1, 150)
route_imp2 <- buffer.route.taille(routes_foret, 2, 110)
route_imp3 <- buffer.route.taille(routes_foret, 3, 100)
route_imp4 <- buffer.route.taille(routes_foret, 4, 80)
route_imp5 <- buffer.route.taille(routes_foret, 5, 50)
route_imp6 <- buffer.route.taille(routes_foret, 6, 25)

# Visualisation de la pression des routes
pression_routes <- buffer.diff.routes(routes_foret)
print(pression_routes)


# Sauvegarde des données créées dans un géopackage ----

getwd()  # Où le gpkg sera enregistré
# à changer selon les couches qu'on garde 
st_write(routes, 
         "ppltmt_data2.gpkg",
         layer = "routes")

st_write(parca_real_clean, 
         "ppltmt_data2.gpkg",
         layer = "parca_real_clean")

st_write(troncon_hydro, 
         "ppltmt_data2.gpkg",
         layer = "troncon_hydro")  # ici pas append = true car on rajoute pas des données sur une couche mais une entité bien distincte

writeRaster(MNH,
            "ppltmt_data2.gpkg",
            filetype = "GPKG",
            gdal = c("APPEND_SUBDATASET=YES",
                     "RASTER_TABLE=MNH"))

writeRaster(IRC,
            "ppltmt_data2.gpkg",
            filetype = "GPKG",
            gdal = c("APPEND_SUBDATASET=YES",
                     "RASTER_TABLE=IRC"))

getwd()
