#scriptElla
  
# https://hackmd.io/@hOaFaD2DS4WcOzNXU6j7vg/HJThOyWvU

librarian::shelf(happign, tmap, sf, dplyr, ggplot2, readxl, openxlsx2, spplot)

library(tmap);ttm() #switch entre plot et viewer

# Test fonction
?st_buffer
?sp.polygons
?st_union
?st_overlaps
?st_line_merge
?st_intersection
?group_by.sf
?summary

# Buffer de ligne dans un polygone ----

# Choix de la ville de Charmes (88130)
point_foret_polygone <- mapedit::drawFeatures()  # Placer notre polygone

get_apikeys()   # Chercher les données dans IGN

# Recup des routes passant dans ce polygone
routes_polygone <- get_wfs(point_foret_polygone,
                  "BDTOPO_V3:troncon_de_route",
                  spatial_filter = "intersects")

buffer_routes <- st_buffer(routes_polygone,15)  # Buffer des lignes routes

# Visualisation des buffers 
tm_shape(buffer_routes)+
  tm_polygons(col = 'blue') +
  tm_shape(point_foret_polygone)+ 
  tm_borders('white')

# Buffer à partir d'un point ---- 

point_foret_point <- mapedit::drawFeatures()  # Placer notre point

buffer_point <- st_buffer(point_foret_point,1500)  # Buffer des lignes routes

# Visualisation des buffers 
tm_shape(buffer_point)+
  tm_polygons(col = 'blue') 

# Voir quand 2 buffers se superposent ---- 

st_combine(buffer_routes)
combinaison_buffer <- st_union(buffer_point, buffer_routes)

# overlaps_buffers <- st_overlaps(buffer_point, buffer_routes)

tm_shape(combinaison_buffer)+
  tm_polygons(col = 'blue')

class(combinaison_buffer)


# Merging <- group_by(overlaps_buffers,add = TRUE)


# Mes fonctions ----

# Fonctions buffer sur plusieurs points : 

# Création des 2 points (à récup dans code de Marion) :

point1 <- mapedit::drawFeatures() 
point2 <- mapedit::drawFeatures()

liste_points <- list(c(point1,point2))

# Combiner les points en un seul objet sf 
points_de_Marion <- bind_rows(liste_points)

buffer.points <- function(sf){
  st_buffer(sf,1000)
}

testbufferpoint <- buffer.point(points_de_Marion)

tm_shape(testbufferpoint)+
  tm_polygons(col = 'blue')

# point 1 et point 2, création fonction
# Faire un gpkg test pour voir si fusion ou juste superposition
