
# Initialisation de la carte
map <- tm_shape(point_foret_polygone) + 
  tm_borders(col = 'black')

# Ajouter chaque couche si elle n'est pas vide
if (!is_empty_sf(routes_1)) {
  map <- map + tm_shape(routes_1) + tm_polygons(col = 'inferno')
}

if (!is_empty_sf(routes_2)) {
  map <- map + tm_shape(routes_2) + tm_polygons(col = 'red')
}

if (!is_empty_sf(routes_3)) {
  map <- map + tm_shape(routes_3) + tm_polygons(col = 'orange')
}

if (!is_empty_sf(routes_4)) {
  map <- map + tm_shape(routes_4) + tm_polygons(col = 'yellow')
}

if (!is_empty_sf(routes_5)) {
  map <- map + tm_shape(routes_5) + tm_polygons(col = 'cyan')
}

if (!is_empty_sf(routes_6)) {
  map <- map + tm_shape(routes_6) + tm_polygons(col = 'green')
}
