Projet sur la fréquentation des forêts publiques, à destination des gestionnaires

*Contexte : pourquoi ce script*

  Une hausse de la fréquentation des forêts et une sensibilisation accrue des administrés aux enjeux environnementaux implique aujourd'hui, pour le gestionnaire, un changement de gestion. Ce script permet d'adapter les coupes et travaux aux forêts les plus exposées au public, et, à l'échelle de la forêt, d'avoir une gestion différenciée aux abords des routes, parkings et points d'eau. 

*Ce que fait notre script*

 Ce script permet de connaître les parkings à 500 M de la forêt. 
A partir de ces parkings, des isochrones ont été tracées pour connaître toutes les villes de plus de 5000 habitants situées à 30 minutes en voiture. Les habitants de ces différentes villes ont été sommées, pour évaluer le nombre de visiteurs potentiels. 
L'impact des routes et des parkings a aussi été pris en compte. 3 zones ont été tracées en fonction de la distance à ces infrastructures. Cela doit permettre au gestionnaire de connaitre les zones les plus exposées au public, mais aussi des connaître les zones du massif où la biodiversité est soumise à des pressions (sonores ou présence de déchets). 
Des Buffers ont aussi été tracés autour des points d'eau afin de mettre en avant les zones d'intérêt pour le public, devant ainsi faire l'objet d'un traitement particulier. 

*Déroulement du code*

Partie 1 : Choix de la forêt par le gestionnaire. 
Partie 2 : Identification des villes de plus de 5000 habitants à proximité de la forêt, et classification de ces villes. 
Partie 3 : Identification des infrastructures à proximité et à l'intérieur de la forêt. 
Partie 4 : Pression du grand public autour des parkings, grâce à des buffers. 
Partie 5 : Visualisation des sentiers dans la forêt. 
Partie 6 : Visualisation des points d'eau présents dans la forêt. 
Partie 7 : Génération d'un fichier .gpkg pour obtenir une carte sur Qgis. 

