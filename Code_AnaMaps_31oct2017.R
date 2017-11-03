# Malaria in pregnacy maps to verify hotspots

#load some pkgs
library(cism)
library(databrew)
library(sp)
library(readxl)
library(raster)
library(rgeos)
library(dplyr)
library(ggplot2)
library(geosphere)

# open the data-base from imMIPPAD with coordenates
#imMIPPAD_map<- read_excel("DB_MIPPADall_Maps_5Oct2017.xlsx")

# open the data-base from imMIPPAD with coordenates
imMIPPAD_map<- read_excel("/Users/anamariafonseca/Desktop/analise_final/06_Maps/allMIPPAD_2010-2012_pcr/DB_MIPPADall_Maps_PCR_18oct2017.xlsx")
# imMIPPAD_map <- read_excel('DB_MIPPADall_Maps_5Oct2017.xlsx')

#convert UTM variables to numeric
imMIPPAD_map$LongUTM <- as.numeric(as.character(imMIPPAD_map$LongUTM))
imMIPPAD_map$LatUTM <- as.numeric(as.character(imMIPPAD_map$LatUTM))

#run ll_from_utm, to comvert UTM to LL
x <- ll_from_utm (x = imMIPPAD_map$LongUTM, y = imMIPPAD_map$LatUTM, zone = 36)

#Join the new variables LL to the dataframe
imMIPPAD_map <- cbind(imMIPPAD_map, x)

# asign the names
colnames(imMIPPAD_map)[colnames(imMIPPAD_map)=="x"] <- "longitude"
colnames(imMIPPAD_map)[colnames(imMIPPAD_map)=="y"] <- "latitude"

#Get a shapfile for mozambique
library(raster)
mozmap <- getData(name="GADM", country = "MOZ", level = 3)

# Get an object just for manhica district
mandist <- mozmap[mozmap@data$NAME_2 == "Manhiça",]

#plot manhiça district Map
plot (mandist)

# Make longitude and latitude NUMERIC
imMIPPAD_map$longitude <- as.numeric(imMIPPAD_map$longitude)
imMIPPAD_map$latitude <- as.numeric(imMIPPAD_map$latitude)

####################################################
#VISUALIZE THE POSITIVES AND NEGATIVES: PLOT THE POINTS AND MAKE AN INTERACTIVE MAP

#A) pcr_mujer (THE REFERENCE)

##generate a variable to deffine the colors of positive red and negative black
imMIPPAD_map$pcr_mujer_color <-ifelse(imMIPPAD_map$pcr_mujer == 1,
                                      "red",
                                      "blue")

## creat different size of the dots if infected (big) or not (smal)
imMIPPAD_map$pcr_mujer_dotsize <- ifelse(imMIPPAD_map$pcr_mujer_color == "black",
                                         1,
                                         1)
plot(x=imMIPPAD_map$longitude, 
     y=imMIPPAD_map$latitude,
     main="pcr_mujer",
     col=imMIPPAD_map$pcr_mujer_color,
     pch=16,
     cex=imMIPPAD_map$pcr_mujer_dotsize)

# perform a interactive map using the cism_map_interactive pckg
cism_map_interactive(lng = imMIPPAD_map$longitude,
                     lat=imMIPPAD_map$latitude,
                     x = imMIPPAD_map$pcr_mujer,
                     spdf = mandist)

#B) mic_mujer 

##generate a variable to deffine the colors of positive red and negative black
imMIPPAD_map$mic_mujer_color <-ifelse(imMIPPAD_map$mic_mujer == 1,
                                      "red",
                                      "blue")

## creat different size of the dots if infected (big) or not (smal)
imMIPPAD_map$mic_mujer_dotsize <- ifelse(imMIPPAD_map$mic_mujer_color == "black",
                                         1,
                                         1)
plot(x=imMIPPAD_map$longitude, 
     y=imMIPPAD_map$latitude,
     main="mic_mujer",
     col=imMIPPAD_map$mic_mujer_color,
     pch=16,
     cex=imMIPPAD_map$mic_mujer_dotsize)

# perform a interactive map using the cism_map_interactive pckg
cism_map_interactive(lng = imMIPPAD_map$longitude,
                     lat=imMIPPAD_map$latitude,
                     x = imMIPPAD_map$mic_mujer,
                     spdf = mandist)


#C) VAR2CSA Peptide 5

##generate a variable to deffine the colors of positive red and negative black
imMIPPAD_map$p5_color <-ifelse(imMIPPAD_map$p_p5 == 1,
                               "red",
                               "blue")

## creat different size of the dots if infected (big) or not (smal)
imMIPPAD_map$p5_dotsize <- ifelse(imMIPPAD_map$p5_color == "black",
                                  1,
                                  1)

plot(x=imMIPPAD_map$longitude, 
     y=imMIPPAD_map$latitude,
     main="p5",
     col=imMIPPAD_map$p5_color,
     pch=16,
     cex=imMIPPAD_map$p5_dotsize)

# perform a interactive map using the cism_map_interactive pckg
cism_map_interactive(lng = imMIPPAD_map$longitude,
                     lat=imMIPPAD_map$latitude,
                     x = imMIPPAD_map$p_p5,
                     spdf = mandist)

#B) VAR2CSA Peptide 8

##generate a variable to deffine the colors of positive red and negative black
imMIPPAD_map$p8_color <-ifelse(imMIPPAD_map$p_p8 == 1,
                               "red",
                               "blue")

## creat different size of the dots if infected (big) or not (smal)
imMIPPAD_map$p8_dotsize <- ifelse(imMIPPAD_map$p8_color == "black",
                                  1,
                                  1)

plot(x=imMIPPAD_map$longitude, 
     y=imMIPPAD_map$latitude,
     main="p8",
     col=imMIPPAD_map$p8_color,
     pch=16,
     cex=imMIPPAD_map$p8_dotsize)

# perform a interactive map using the cism_map_interactive pckg
cism_map_interactive(lng = imMIPPAD_map$longitude,
                     lat=imMIPPAD_map$latitude,
                     x = imMIPPAD_map$p_p8,
                     spdf = mandist)

#E) VAR2CSA Peptide p5+8

##generate a variable to deffine the colors of positive red and negative black
imMIPPAD_map$p5_8_color <-ifelse(imMIPPAD_map$comp5_8 == 1,
                                 "red",
                                 "blue")

## creat different size of the dots if infected (big) or not (smal)
imMIPPAD_map$p5_8_dotsize <- ifelse(imMIPPAD_map$p5_8_color == "black",
                                    1,
                                    1)

plot(x=imMIPPAD_map$longitude, 
     y=imMIPPAD_map$latitude,
     main="p5+8",
     col=imMIPPAD_map$p5_8_color,
     pch=16,
     cex=imMIPPAD_map$p5_8_dotsize)

# perform a interactive map using the cism_map_interactive pckg
cism_map_interactive(lng = imMIPPAD_map$longitude,
                     lat=imMIPPAD_map$latitude,
                     x = imMIPPAD_map$comp5_8,
                     spdf = mandist)

#F) VAR2CSA DBL5

##generate a variable to deffine the colors of positive red and negative black
imMIPPAD_map$dbl5_color <-ifelse(imMIPPAD_map$p_dbl5e == 1,
                                 "red",
                                 "blue")

## creat different size of the dots if infected (big) or not (smal)
imMIPPAD_map$dbl5_dotsize <- ifelse(imMIPPAD_map$dbl5_color == "black",
                                    1,
                                    1)

plot(x=imMIPPAD_map$longitude, 
     y=imMIPPAD_map$latitude,
     main="dbl5",
     col=imMIPPAD_map$dbl5_color,
     pch=16,
     cex=imMIPPAD_map$dbl5_dotsize)

# perform a interactive map using the cism_map_interactive pckg
cism_map_interactive(lng = imMIPPAD_map$longitude,
                     lat=imMIPPAD_map$latitude,
                     x = imMIPPAD_map$p_dbl5e,
                     spdf = mandist)

#G) MSP1_19

##generate a variable to deffine the colors of positive red and negative black
imMIPPAD_map$msp_color <-ifelse(imMIPPAD_map$p_msp1 == 1,
                                "red",
                                "blue")

## creat different size of the dots if infected (big) or not (smal)
imMIPPAD_map$msp_dotsize <- ifelse(imMIPPAD_map$msp_color == "black",
                                   1,
                                   1)

plot(x=imMIPPAD_map$longitude, 
     y=imMIPPAD_map$latitude,
     main="msp",
     col=imMIPPAD_map$msp_color,
     pch=16,
     cex=imMIPPAD_map$msp_dotsize)

# perform a interactive map using the cism_map_interactive pckg
cism_map_interactive(lng = imMIPPAD_map$longitude,
                     lat=imMIPPAD_map$latitude,
                     x = imMIPPAD_map$p_msp1,
                     spdf = mandist)

##################################################################
#ORGANIZE FOR ANALYSIS

# Make a simpler dataframe name
df <- imMIPPAD_map

# Get a map of manhica
man3 <- cism::man3

# # Lets keep only those sub-districts that we like
# the_map <- man3[man3$NAME_3 %in% c(#"Maluana",
#                                 #"3 De Fevereiro",
#                                 "Manhica - Sede"),] 

# Make longitude and latitude NUMERIC
df$longitude <- as.numeric(df$longitude)
df$latitude <- as.numeric(df$latitude)

# Make a spatial version of df
df_spatial <- df
df_spatial$x <- df_spatial$longitude
df_spatial$y <- df_spatial$latitude

# Remove those observations from df_spatial which have na x or y
df_spatial<- df_spatial[!is.na(df_spatial$latitude) &
                          !is.na(df_spatial$longitude),]

# Remove those that are outside of manhica and maragra filtering by latitude and longitude
#df_spatial <- df_spatial %>%
#filter(latitude <= -25.375,
#       longitude >= 32.76)

# make a plot of df_spatial after remove the individuals that are not from manhiça and maragra
coordinates(df_spatial) <- ~x+y
plot(df_spatial)

# Give df_spatial a projection
proj4string(df_spatial) <- proj4string(the_map)

# To confirm that our projections are good, lets plot the_map with df_spatial
plot(the_map)
points(df_spatial)

# KEEP ONLY THOSE POINTS IN MANHICA SEDE!!!
mansed <- man3[man3@data$NAME_3 == "Manhica - Sede",]
proj4string(df_spatial) <- proj4string(man3)
x <- over(df_spatial, polygons(mansed))
df_spatial <- df_spatial[!is.na(x),]

# Keep only the df equivalents
df_original_before_removals <- df
df <- df[df$permid %in% df_spatial@data$permid]

# Use convex hull to create a border
the_map <- gConvexHull(df_spatial)

plot(man3)
points(df_spatial, pch = ".", col = adjustcolor("blue", alpha.f = 0.7))
plot(the_map, add = TRUE, col = adjustcolor("red", alpha.f = 0.2))

# Use a "buffer" to make that border go out
the_map <- gBuffer(the_map, width = 0.02)
plot(the_map, add = TRUE)

#############################################################
# SPATIAL INTERPOLATION

# First we need to create a uniform grid
# Create a gridded dataframe with values 
# for the entire range (bbox) of magude
df_grid <- expand.grid(lng = seq(bbox(the_map)[1,1],
                                 bbox(the_map)[1,2],
                                 by = 0.002), # make this smaller for better quality
                       lat = seq(bbox(the_map)[2,1],
                                 bbox(the_map)[2,2],
                                 by = 0.002), # make this smaller for better quality
                       pos_score = NA)
df_grid$latitude <- df_grid$lat
df_grid$longitude <- df_grid$lng
coordinates(df_grid) <- ~longitude+latitude
# Visualize the grid
plot(the_map)
points(df_grid$lng, df_grid$lat, pch = ".")

# Loop through each point in our grid
for (i in 1:nrow(df_grid)){
  message(i)
  # Get distance from this grid point to every point in df_spatial
  distances <- spDistsN1(pts = df_spatial,
                         pt = df_grid[i,],
                         longlat = TRUE)
  # Define which are acceptably close (define a radius)
  close_enough <- which(distances <= 50)
  # Get a positivity score ( 1 means yes and 0 means no)
  positivity <- stats::weighted.mean(x = df_spatial$pcr_mujer[close_enough],
                                     w = (1 / distances[close_enough])^2,
                                     na.rm = TRUE)
  # Assign irs to the dataframe
  df_grid$pos_score[i] <- positivity
}

# Convert df_grid to raster
temp <- df_grid@data %>% arrange(lng, lat)
r <- rasterFromXYZ(temp[, c('lng', 'lat', 'pos_score')])
plot(r)
plot(the_map, add = TRUE)

# Make grid smaller and then reconvert to raster
proj4string(df_grid) <- proj4string(the_map)
x <- over(df_grid, polygons(the_map))
df_grid_small <- df_grid[!is.na(x),]
plot(df_grid_small,
     col = adjustcolor('black', alpha.f = 0.2))
temp <- df_grid_small@data %>% arrange(lng, lat)
r <- rasterFromXYZ(temp[, c('lng', 'lat', 'pos_score')])
plot(r)
plot(the_map, add = TRUE)
points(df_spatial,
       col = adjustcolor("black", alpha.f = 0.2),
       cex = 0.2)


anamap <- function(variable, smooth_number = 2,
                   colors = c("red", "white", "blue")){
  # Loop through each point in our grid
  for (i in 1:nrow(df_grid)){
    message(i)
    # Get distance from this grid point to every point in df_spatial
    distances <- spDistsN1(pts = df_spatial,
                           pt = df_grid[i,],
                           longlat = TRUE)
    # Define which are acceptably close (define a radius)
    close_enough <- which(distances <= 50)
    # Get a positivity score ( 1 means yes and 0 means no)
    positivity <- stats::weighted.mean(x = data.frame(df_spatial@data)[,variable][close_enough],
                                       w = (1 / distances[close_enough])^smooth_number,
                                       na.rm = TRUE)
    # Assign irs to the dataframe
    df_grid$pos_score[i] <- positivity
  }
  
  # Convert df_grid to raster
  temp <- df_grid@data %>% arrange(lng, lat)
  r <- rasterFromXYZ(temp[, c('lng', 'lat', 'pos_score')])
  # plot(r)
  # plot(the_map, add = TRUE)
  
  # Make grid smaller and then reconvert to raster
  proj4string(df_grid) <- proj4string(the_map)
  x <- over(df_grid, polygons(the_map))
  df_grid_small <- df_grid[!is.na(x),]
  # plot(df_grid_small,
  #      col = adjustcolor('black', alpha.f = 0.2))
  temp <- df_grid_small@data %>% arrange(lng, lat)
  # r <- rasterFromXYZ(temp[, c('lng', 'lat', 'pos_score')])
  cols <- colorRampPalette(
    # c("red", "white", "blue")
    colors
    # brewer.pal(9, "RdYlBu")
  )(100)
  cols <- rev(cols)
  image(r, 
        col = cols,
        # col = topo.colors(length(seq(0, 1, length = 100))),
        # col=terrain.colors(length(seq(0, 1, length = 100))),
        # breaks =  seq(0, 1, length = 101),
        breaks = seq(min(values(r), na.rm = TRUE),
                     max(values(r), na.rm = TRUE),
                     length = 101),
        axes = FALSE, xlab = NA, ylab = NA)
  # values(r) <- percent_rank(values(r))
  # # r <- scale(r)
  # plot(r)
  # map.p <- rasterToContour(r)
  # plot(r)
  # breakpoints <- seq(0, 1, 0.01)
  # colors <- colorRampPalette(brewer.pal(9, "Spectral"))(101)
  # colors <- rev(colors)
  # plot(r, breaks = breakpoints, col = colors, legend = FALSE)
  
  # plot(map.p, add = TRUE)
  # map.p <- rasterToPoints(r)
  # x <- data.frame(map.p)
  # names(x) <- c("lng", "lat", "MAP")
  # g <- ggplot(data = x,
  #        aes(x = lng, y = lat)) +
  #   geom_raster(aes(fill = MAP),
  #               alpha = 0.6) +
  #   scale_fill_gradient2(#limits = c(0,1),
  #                        #midpoint = 0.5,
  #                        low = "blue",
  #                        high = "red",
  #                        name = "Positivity") +
  #   theme_cism() +
  #   labs(title = variable)
  # 
  # print(g)
  # geom_polygon(data = man3_fortified,
  #        aes(x = long, y = lat, group = group),
  #        fill = NA,
  #        color = "black")
  
  # colr <- colorRampPalette(brewer.pal(9, "Spectral"))(101)
  
  # levelplot(r, 
  #           margin=FALSE,                       # suppress marginal graphics
  #           colorkey=list(
  #             space='bottom',                   # plot legend at bottom
  #             labels=list(at=-5:5, font=4)      # legend ticks and labels 
  #           ),    
  #           par.settings=list(
  #             axis.line=list(col='transparent') # suppress axes and legend outline
  #           ),
  #           scales=list(draw=FALSE),            # suppress axis labels
  #           col.regions=colr,                   # colour ramp
  #           at=seq(0, 1, len=101)) 
  # plot(the_map, add = T)
  points(df_spatial,
         pch = ifelse(data.frame(df_spatial@data)[,variable] == 0,
                      1, 3),
         col = adjustcolor("black", alpha.f = 0.5),
         cex = 0.5)
  plot(the_map, add = TRUE, border = adjustcolor("black", alpha.f = 0.3))
  # contour(r, add= TRUE, col = adjustcolor("black", alpha.f = 0.5))
  plot(man3, add = TRUE)
  # points(df_spatial,
  #        col = adjustcolor("black", alpha.f = 0.2),
  #        cex = 0.2)
  title(main = variable)
}
library(rasterVis)

variables <- c("pcr_mujer",
               "mic_mujer",
               "p_p8",
               "comp5_8",
               "p_p5",
               "p_p1",
               "p_p20",
               "p_p6",
               "p_p12",
               "p_p37",
               "p_pcsp",
               "p_dbl3x",
               "p_dbl6e",
               "p_dbl5e",
               "p_ama1",
               "p_msp1")


# plot the maps_ raster maps with bobles
# the lower the smooth number, the more smooth it is
anamap("pcr_mujer", smooth_number = 0.001, colors = c("red", "white", "blue"))
anamap("mic_mujer", smooth_number = 0.001, colors = c("red", "white", "blue"))
anamap("p_p5", smooth_number = 0.001, colors = c("red","white", "grey", "white", "blue"))
anamap("p_p8", smooth_number = 0.001, colors = c("red","white", "grey", "white", "blue"))
anamap("comp5_8", smooth_number = 0.001,  colors = c("red","white", "grey", "white", "blue"))
anamap("p_dbl5e", smooth_number = 0.001,  colors = c("red","white", "grey", "white", "blue"))
anamap("p_msp1", smooth_number = 0.001,  colors = c("red","white", "grey", "white", "blue"))

#####################################################################

# CLUSTERING AND HOTSPOT IDENTIFICATION

# We need to cluster our data "naturally" - we will use DBSCAN for this
# library(dbscan)
library(class)
library(geosphere) # has functionality for spatial clustering
df_spatial@data <- data.frame(df_spatial@data)
mdist <- distm(df_spatial)
test <- hclust(as.dist(mdist), method = "complete")

# Define a distance threshold
d = 2000 # meters (change this up or down to make bigger or smaller clusters, respectively)
df_spatial$cluster <- cutree(test, h = d)
# df_spatial$cluster <- if_else(df_spatial$latitude>-25.42, 1, 2)


# View our clustering results !!!!ELEPHANT!!!!
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(9, "Spectral"))(length(unique(df_spatial$cluster)))
cols <- sample(cols, length(cols))
df_spatial$color <- cols[df_spatial$cluster]
plot(df_spatial, col = df_spatial$color, pch = 1, axes = TRUE, cex.axis = 0.5, las = 1)

# How many clusters
length(unique(df_spatial$cluster))

# Now that we are satisfied with our clustering results,
# we will create polygons with borders
clusters <- unique(sort(df_spatial$cluster))

# Loop through each cluster, and for each one use "convex hull"
# to create a border
ch <- list()
for (i in 1:length(clusters)){
  this_cluster <- clusters[i]
  sub_df <- df_spatial[df_spatial@data$cluster == this_cluster,]
  x <- rgeos::gConvexHull(sub_df)
  ch[[i]] <- x
}

# Create delaunay triangulation / voronoi tiles for entire surface
voronoi <- function(shp = df_spatial){
  
  shp@data <- data.frame(shp@data)
  
  # Fix row names
  row.names(shp) <- 1:nrow(shp)
  
  # Remove any identical ones
  shp <- shp[!duplicated(shp$longitude,
                         shp$latitude),]
  
  # Helper function to create coronoi polygons (tesselation, not delaunay triangles)
  # http://carsonfarmer.com/2009/09/voronoi-polygons-with-r/
  voronoipolygons = function(layer) {
    require(deldir)
    require(rgeos)
    crds = layer@coords
    z = deldir(crds[,1], crds[,2])
    w = tile.list(z)
    polys = vector(mode='list', length=length(w))
    require(sp)
    for (i in seq(along=polys)) {
      pcrds = cbind(w[[i]]$x, w[[i]]$y)
      pcrds = rbind(pcrds, pcrds[1,])
      polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
    }
    SP = SpatialPolygons(polys)
    voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1], 
                                                           y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                        function(x) slot(x, 'ID'))))
  }
  # http://gis.stackexchange.com/questions/180682/merge-a-list-of-spatial-polygon-objects-in-r
  appendSpatialPolygons <- function(x) {
    ## loop over list of polygons
    for (i in 2:length(x)) {
      # create initial output polygon
      if (i == 2) {
        out <- maptools::spRbind(x[[i-1]], x[[i]])
        # append all following polygons to output polygon  
      } else {
        out <- maptools::spRbind(out, x[[i]])
      }
    }
    return(out)
  }
  
  tile_polys <- voronoipolygons(shp)
  # Add the bairro numbers
  tile_polys@data$cluster_number <- the_clusters <- shp$cluster
  cols <- rainbow(as.numeric(factor(tile_polys@data$cluster_number)))
  
  # Disolve borders
  x = gUnaryUnion(tile_polys, id = tile_polys$cluster_number)
  
  jdata = SpatialPolygonsDataFrame(Sr=x, 
                                   data=data.frame(cluster_number = as.numeric(as.character(names(x)))),FALSE)
  
  return(jdata)
}

# Get voronoi tesselations
dfv <- voronoi(shp = df_spatial)

# For each cluster in df_spatial, we want to get the % positive
get_positivity <- function(var = "pcr_mujer"){
  require(dplyr)
  x <- df_spatial@data
  x$var <- df_spatial@data[,var]
  out <- x %>%
    group_by(cluster) %>%
    summarise(positives = length(which(var == 1)),
              total = n()) %>%
    mutate(percentage = positives / total * 100)
  return(out)
}

# Run our function
get_positivity("mic_mujer")
get_positivity("pcr_mujer")
get_positivity("p_p5")
get_positivity("p_p8")
get_positivity("comp5_8")
get_positivity("p_pcsp")
get_positivity("p_dbl3x")
get_positivity("p_dbl5e")
get_positivity("p_dbl6e")
get_positivity("p_msp1")
get_positivity("p_ama1")

# Define a function for identifying hotspots
ana_hotspot <- function(var = "p_p5",
                        plot_it = FALSE,
                        seed = 1){
  # set.seed(seed)  
  if(var %in% c("mic_mujer", 'pcr_mujer')){set.seed(3)} else {set.seed(seed)}
  # Use the kulldorf method to get hotspots
  library(SpatialEpi)
  
  # Create centroids
  centroids <- as.matrix(coordinates(dfv))
  
  # Get the positivity rate for a variable
  positivity <- get_positivity(var)
  
  # Define some parameters about our results
  cases <- positivity$positives
  population <- positivity$total
  n_strata <- nrow(dfv@data)
  
  expected_cases <- sum(cases, na.rm = TRUE)
  expected_cases <- expected_cases * (population / sum(population, na.rm = TRUE))
  
  # Set paramaters
  pop.upper.bound <- 0.5
  n.simulations <- 9999
  alpha.level <- 0.05
  plot <- FALSE
  
  poisson <- kulldorff(geo = centroids,
                       cases = cases,
                       population = population,
                       expected.cases = expected_cases,
                       pop.upper.bound = pop.upper.bound,
                       n.simulations = n.simulations,
                       alpha.level = 0.05,
                       plot = FALSE)
  
  if(plot_it){
    # get clusters
    cluster <- poisson$most.likely.cluster$location.IDs.included
    # cluster <- df_spatial@data[cluster,"cluster"]
    secondary_cluster <- poisson$secondary.clusters$location.IDs.included
    
    # Plot
    plot(dfv,axes=TRUE, border = adjustcolor("black", alpha.f = 0.5))
    plot(dfv[cluster,],add=TRUE,col="red")
    plot(man3, add = TRUE)
    points(df_spatial, pch= ifelse(df_spatial@data[,var] == 1, "+", "0"),
           cex=0.5,
           col= adjustcolor("black", alpha.f = 0.5))
    if(!is.null(secondary_cluster)){
      plot(dfv[secondary_cluster,], add = TRUE, col = "yellow")
    }
    p_value <- poisson$most.likely.cluster$p.value
    p_value <- ifelse(p_value == 1, "> 0.99", p_value)
    title(main = paste0(var, " p:",p_value ))
  } else {
    poisson
  }
}

# # Run the function in order to get the statistical output
# statistical_output <- ana_hotspot("pcr_mujer")
# statistical_output <- ana_hotspot("mic_mujer")
# statistical_output <- ana_hotspot("p_p5")
# statistical_output <- ana_hotspot("p_p8")
# statistical_output <- ana_hotspot("p_dbl5e")
# statistical_output <- ana_hotspot("p_msp1")

# Run the function just to make a pretty map of hotspots####!!!!LION!!!!!
ana_hotspot("pcr_mujer", plot_it = TRUE)
ana_hotspot("mic_mujer", plot_it = TRUE)  
ana_hotspot("p_p5", plot_it = TRUE)
ana_hotspot("p_p8", plot_it = TRUE)
ana_hotspot("comp5_8", plot_it = TRUE)
ana_hotspot("p_pcsp", plot_it = TRUE)
ana_hotspot("p_dbl3x", plot_it = TRUE)
ana_hotspot("p_dbl5e", plot_it = TRUE)
ana_hotspot("p_dbl6e", plot_it = TRUE)
ana_hotspot(var = "p_msp1", plot_it = TRUE)
ana_hotspot(var = "p_ama1", plot_it = TRUE)

############################################################
# COMPARE MANHIÇA VS MARAGRA POSITIVITY

# Create a maragra vs manhica variable
df_spatial$location <- ifelse(df_spatial$latitude>-25.42, "Man", "Mar")

# Simple chisquared test to compare manhica vs. maragra
ana_test <- function(var = "pcr_mujer"){
  require(dplyr)
  x <- df_spatial@data
  x$var <- df_spatial@data[,var]
  out <- x %>%
    group_by(location) %>%
    summarise(positives = length(which(var == 1)),
              total= n())
  prop.test(x = out$positives,
            n = out$total,
            conf.level = 0.95)
}
ana_test("mic_mujer")
ana_test("pcr_mujer")
ana_test("p_p5")
ana_test("p_p8")
ana_test("comp5_8")
ana_test("p_pcsp")
ana_test("p_dbl3x")
ana_test("p_dbl5e")
ana_test("p_dbl6e")
ana_test("p_msp1")
ana_test("p_ama1")

# Get better visualizations

# first, source some functions for minimum enclosing circle:
# taken from https://raw.githubusercontent.com/dwoll/RExRepos/master/R/diagBounding.R

## ------------------------------------------------------------------------
set.seed(123)
xy      <- matrix(rnorm(24, 100, 15), ncol=2)
hullIdx <- chull(xy)

## ------------------------------------------------------------------------
getBoundingBox <- function(xy) {
  stopifnot(is.matrix(xy), is.numeric(xy), ncol(xy) == 2)
  x   <- range(xy[ , 1])
  y   <- range(xy[ , 2])
  pts <- c(xleft=x[1], ybottom=y[1], xright=x[2], ytop=y[2])
  return(list(pts=pts, width=abs(diff(x)), height=abs(diff(y))))
}

getMinBBox <- function(xy) {
  stopifnot(is.matrix(xy), is.numeric(xy), nrow(xy) >= 2, ncol(xy) == 2)
  
  ## rotating calipers algorithm using the convex hull
  H    <- chull(xy)                    # hull indices, vertices ordered clockwise
  n    <- length(H)                    # number of hull vertices
  hull <- xy[H, ]                      # hull vertices
  
  ## unit basis vectors for all subspaces spanned by the hull edges
  hDir  <- diff(rbind(hull, hull[1,])) # account for circular hull vertices
  hLens <- sqrt(rowSums(hDir^2))       # length of basis vectors
  huDir <- diag(1/hLens) %*% hDir      # scaled to unit length
  
  ## unit basis vectors for the orthogonal subspaces
  ## rotation by 90 deg -> y' = x, x' = -y
  ouDir <- cbind(-huDir[ , 2], huDir[ , 1])
  
  ## project hull vertices on the subspaces spanned by the hull edges, and on
  ## the subspaces spanned by their orthogonal complements - in subspace coords
  projMat <- rbind(huDir, ouDir) %*% t(hull)
  
  ## range of projections and corresponding width/height of bounding rectangle
  rangeH  <- matrix(numeric(n*2), ncol=2)   # hull edge
  rangeO  <- matrix(numeric(n*2), ncol=2)   # orth subspace
  widths  <- numeric(n)
  heights <- numeric(n)
  for(i in seq(along=H)) {
    rangeH[i, ] <- range(projMat[  i, ])
    rangeO[i, ] <- range(projMat[n+i, ])  # orth subspace is in 2nd half
    widths[i]   <- abs(diff(rangeH[i, ]))
    heights[i]  <- abs(diff(rangeO[i, ]))
  }
  
  ## extreme projections for min-area rect in subspace coordinates
  eMin  <- which.min(widths*heights)   # hull edge leading to minimum-area
  hProj <- rbind(   rangeH[eMin, ], 0)
  oProj <- rbind(0, rangeO[eMin, ])
  
  ## move projections to rectangle corners
  hPts <- sweep(hProj, 1, oProj[ , 1], "+")
  oPts <- sweep(hProj, 1, oProj[ , 2], "+")
  
  ## corners in standard coordinates, rows = x,y, columns = corners
  ## in combined (4x2)-matrix: reverse point order to be usable in polygon()
  basis <- cbind(huDir[eMin, ], ouDir[eMin, ])  # basis formed by hull edge and orth
  hCorn <- basis %*% hPts
  oCorn <- basis %*% oPts
  pts   <- t(cbind(hCorn, oCorn[ , c(2, 1)]))
  
  return(list(pts=pts, width=widths[eMin], height=heights[eMin]))
}

## ------------------------------------------------------------------------
getCircleFrom3 <- function(xy) {
  stopifnot(is.matrix(xy), is.numeric(xy), nrow(xy) == 3, ncol(xy) == 2)
  
  aa <- xy[1,  ]
  bb <- xy[2,  ]
  cc <- xy[3,  ]
  y  <- xy[ , 2]
  
  xDeltaA <- bb[1] - aa[1]
  yDeltaA <- bb[2] - aa[2]
  xDeltaB <- cc[1] - bb[1]
  yDeltaB <- cc[2] - bb[2]
  xDeltaC <- cc[1] - aa[1]
  yDeltaC <- cc[2] - aa[2]
  
  ## check if the points are collinear: qr(xy)$rank == 1, or:
  ## determinant of difference matrix = 0, no need to use det()
  dMat <- rbind(c(xDeltaA, yDeltaA), c(xDeltaB, yDeltaB))
  if(isTRUE(all.equal(dMat[1,1]*dMat[2,2] - dMat[1,2]*dMat[2,1], 0, check.attributes=FALSE))) {
    ## define the circle as the one that's centered between the points
    rangeX <- range(c(aa[1], bb[1], cc[1]))
    rangeY <- range(c(aa[2], bb[2], cc[2]))
    ctr    <- c(rangeX[1] + 0.5*diff(rangeX), rangeY[1] + 0.5*diff(rangeY))
    rad    <- sqrt((0.5*diff(rangeX))^2 + (0.5*diff(rangeY))^2)
  } else {
    rad <- prod(dist(xy)) / (2 * abs(det(cbind(xy, 1))))  # circle radius
    v1  <- rowSums(xy^2)                    # first vector in the numerator
    v2x <- c( xDeltaB, -xDeltaC,  xDeltaA)  # 2nd vector numerator for Mx
    v2y <- c(-yDeltaB,  yDeltaC, -yDeltaA)  # 2nd vector numerator for My
    ctr <- c(t(v1) %*% v2y, t(v1) %*% v2x) / (2 * (t(y) %*% v2x))  # center
  }
  
  return(list(ctr=ctr, rad=rad))
}


## ------------------------------------------------------------------------
getMaxRad <- function(xy, S) {
  stopifnot(is.matrix(xy), is.numeric(xy), nrow(xy) >= 2, ncol(xy) == 2)
  stopifnot(is.numeric(S), length(S) >= 2, length(S) <= nrow(xy))
  
  n    <- length(S)                    # number of points
  Sidx <- seq(along=S)                 # index for points
  rads <- numeric(n)                   # radii for all circles
  post <- (Sidx %% n) + 1              # next point in S
  prev <- Sidx[order(post)]            # previous point in S
  for(i in Sidx) {
    pts     <- rbind(xy[S[prev[i]], ], xy[S[i], ], xy[S[post[i]], ])
    rads[i] <- getCircleFrom3(pts)$rad  # circle radius
  }
  
  return(which.max(rads))
}

## ------------------------------------------------------------------------
isBiggerThan90 <- function(xy) {
  stopifnot(is.matrix(xy), is.numeric(xy), nrow(xy) == 3, ncol(xy) == 2)
  d   <- dist(xy)
  dAB <- d[1]
  dAC <- d[2]
  dBC <- d[3]
  return((dAB^2 + dBC^2 - dAC^2) < 0)
}

## ------------------------------------------------------------------------
getMaxPairDist <- function(xy) {
  stopifnot(is.matrix(xy), is.numeric(xy), ncol(xy) == 2, nrow(xy) >= 2)
  
  # 2D -> only convex hull is relevant
  H    <- chull(xy)      # convex hull indices (vertices ordered clockwise)
  pts  <- xy[H, ]        # points that make up the convex hull
  N    <- nrow(pts)                      # number of points on hull
  dMat <- dist(pts, method="euclidean")  # distance matrix
  idx  <- which.max(as.matrix(dMat))     # maximum distance
  i    <- (idx-1) %/% N+1                # column -> point 1
  j    <- (idx-1) %%  N+1                # row    -> point 2
  mPts <- H[c(i, j)]                     # rows with max distance
  dst  <- max(dMat)                      # max distance
  
  return(list(d=dst, idx=mPts))
}

## ------------------------------------------------------------------------
getMinCircle <- function(xy) {
  stopifnot(is.matrix(xy), is.numeric(xy), nrow(xy) >= 2, ncol(xy) == 2)
  
  H    <- chull(xy)      # convex hull indices (vertices ordered clockwise)
  hPts <- xy[H, ]        # points that make up the convex hull
  
  ## min circle may touch convex hull in only two points
  ## if so, it is centered between the hull points with max distance
  maxPD  <- getMaxPairDist(hPts)
  idx    <- maxPD$idx    # index of points with max distance
  rad    <- maxPD$d / 2  # half the distance -> radius
  rangeX <- c(hPts[idx[1], 1], hPts[idx[2], 1])
  rangeY <- c(hPts[idx[1], 2], hPts[idx[2], 2])
  ctr    <- c(rangeX[1] + 0.5*diff(rangeX), rangeY[1] + 0.5*diff(rangeY))
  
  ## check if circle centered between hPts[pt1Idx, ] and hPts[pt2Idx, ]
  ## contains all points (all distances <= rad)
  dst2ctr <- dist(rbind(ctr, hPts[-idx, ]))      # distances to center
  if(all(as.matrix(dst2ctr)[-1, 1] <= rad)) {    # if all <= rad, we're done
    tri <- rbind(hPts[idx, ], ctr)
    return(getCircleFrom3(tri))
  }
  
  ## min circle touches hull in three points - Skyum algorithm
  S <- H                               # copy of hull indices that will be changed
  while(length(S) >= 2) {
    n    <- length(S)                # number of remaining hull vertices
    Sidx <- seq(along=S)             # index for vertices
    post <- (Sidx %% n) + 1          # next vertex in S
    prev <- Sidx[order(post)]        # previous vertex in S
    mIdx <- getMaxRad(xy, S)         # idx for maximum radius
    
    ## triangle where mIdx is vertex B in ABC
    Smax <- rbind(xy[S[prev[mIdx]], ], xy[S[mIdx], ], xy[S[post[mIdx]], ])
    
    ## if there's only two hull vertices, we're done
    if(n <= 2) { break }
    
    ## check if angle(ABC) is > 90
    ## if so, eliminate B - if not, we're done
    if(isBiggerThan90(Smax)) { S <- S[-mIdx] } else { break }
  }
  
  return(getCircleFrom3(Smax))
}

# Now define a function for the plotting of a certain hotspot
joe <- function(var = "p_p5",
                hot = TRUE,
                cold = TRUE,
                circle_outline = TRUE,
                cluster_outline = TRUE,
                use_leaflet = FALSE,
                background = 'OpenStreetMap.Mapnik',
                dfs = df_spatial,
                seed = 1){ # change the background by looking here: https://leaflet-extras.github.io/leaflet-providers/preview/
  
  if(var %in% c("mic_mujer", 'pcr_mujer')){set.seed(3)} else {set.seed(seed)}
  
  
  # Use the kulldorf method to get hotspots
  library(SpatialEpi)
  
  # Leaflet library too
  library(leaflet)
  
  # Create centroids
  centroids <- as.matrix(coordinates(dfv))
  
  # Get the positivity rate for a variable
  positivity <- get_positivity(var)
  
  # Define some parameters about our results
  cases <- positivity$positives
  population <- positivity$total
  n_strata <- nrow(dfv@data)
  
  expected_cases <- sum(cases, na.rm = TRUE)
  expected_cases <- expected_cases * (population / sum(population, na.rm = TRUE))
  
  # Set paramaters
  pop.upper.bound <- 0.5
  n.simulations <- 9999
  alpha.level <- 0.05
  plot <- FALSE
  
  poisson <- kulldorff(geo = centroids,
                       cases = cases,
                       population = population,
                       expected.cases = expected_cases,
                       pop.upper.bound = pop.upper.bound,
                       n.simulations = n.simulations,
                       alpha.level = 0.05,
                       plot = FALSE)
  cold_poisson <- kulldorff(geo = centroids,
                            cases = (population - cases),
                            population = population,
                            expected.cases = expected_cases,
                            pop.upper.bound = pop.upper.bound,
                            n.simulations = n.simulations,
                            alpha.level = 0.05,
                            plot = FALSE)
  p_hot <- poisson$most.likely.cluster$p.value
  if(p_hot > 0.05){
    hot_dash <- "5, 5"
  } else {
    hot_dash <- NULL
  }
  p_cold <- cold_poisson$most.likely.cluster$p.value
  if(p_cold > 0.05){
    cold_dash <- "5, 5"
  } else {
    cold_dash <- NULL
  }
  # get clusters
  cluster <- poisson$most.likely.cluster$location.IDs.included
  cold_cluster <- cold_poisson$most.likely.cluster$location.IDs.included
  
  # Remove any NA obs
  dfs <- dfs[!is.na(dfs@data[,var]),]
  
  # Get prevalence in and out of cluster
  prev_in_vec <- dfs@data[dfs@data$cluster %in% cluster,var]
  prev_out_vec <- dfs@data[!dfs@data$cluster %in% cluster,var]
  
  prev_in_message <- paste0('=========================================\n', 
                            'IN CLUSTER: ', sum(prev_in_vec), ' ',
                            'cases in the cluster; ',
                            length(which(prev_in_vec == 0)),
                            ' non-cases in the cluster. ',
                            '\nPopulation = ', length(prev_in_vec), '. ',
                            '\nPrevalence = ',
                            round(sum(prev_in_vec) / length(prev_in_vec) * 100, digits = 2), '%.\n=========================================')
  prev_out_message <- paste0('=========================================\n',
                             'OUT OF CLUSTER: ', sum(prev_out_vec), ' ',
                             'cases out of the cluster; ',
                             length(which(prev_out_vec == 0)),
                             ' non-cases out of the cluster. ',
                             '\nPopulation = ', length(prev_out_vec), '. ',
                             '\nPrevalence = ',
                             round(sum(prev_out_vec) / length(prev_out_vec) * 100, digits = 2), '%.\n=========================================')
  cat(prev_in_message)
  cat(prev_out_message)
  
  if(use_leaflet){
    l <- leaflet() %>%
      addProviderTiles(background) %>%
      # addPolygons(data = dfv, 
      #             fillOpacity = 0,
      #             opacity = 0) %>%
      # addPolylines(data = man3[man3@data$NAME_3 == 'Manhica - Sede',],
      #              color = 'black',
      #              weight = 1) %>%
      addCircleMarkers(data = dfs,
                       color = ifelse(dfs@data[,var] == 1, "red", "blue"),
                       weight = 1,
                       radius = 3,
                       opacity = 0.6,
                       fill = TRUE) %>%
      addScaleBar(position = 'bottomleft')
    
    
    if(hot){
      only_positives_in_cluster <- dfs[dfs$cluster %in% cluster & dfs@data[,var] == 1,]
      l <- l %>%
        addCircleMarkers(data = only_positives_in_cluster,
                         color = 'red',
                         weight = 2,
                         radius = 4,
                         opacity = 0.8,
                         fill = TRUE)
      mc     <- getMinCircle(coordinates(only_positives_in_cluster))
      angles <- seq(0, 2*pi, length.out=200)
      circ   <- cbind(mc$ctr[1] + mc$rad*cos(angles),
                      mc$ctr[2] + mc$rad*sin(angles))
      circ_hot <- circ
      if(circle_outline){
        l <- l %>%
          addPolylines(data = circ,
                       col = 'red',
                       dashArray = hot_dash)
      }
      
      if(cluster_outline){
        l <- l %>%
          addPolylines(data = dfv[cluster,],
                       color = 'black',
                       weight = 2,
                       fillColor = 'red')
        
      }
    }
    
    if(cold){
      only_negatives_in_cluster <- dfs[dfs$cluster %in% cold_cluster & dfs@data[,var] == 0,]
      l <- l %>%
        addCircleMarkers(data = only_negatives_in_cluster,
                         color = 'blue',
                         weight = 2,
                         radius = 4,
                         opacity = 0.8,
                         fill = TRUE)
      mc     <- getMinCircle(coordinates(only_negatives_in_cluster))
      angles <- seq(0, 2*pi, length.out=200)
      circ   <- cbind(mc$ctr[1] + mc$rad*cos(angles),
                      mc$ctr[2] + mc$rad*sin(angles))
      circ_cold <- circ
      if(circle_outline){
        l <- l %>%
          addPolylines(data = circ,
                       col = 'blue',
                       dashArray = cold_dash)
      }
      if(cluster_outline){
        l <- l %>%
          addPolylines(data = dfv[cold_cluster,],
                       color = 'black',
                       weight = 2,
                       fillColor = 'blue')
        
      }
    }
    # Add legend <-
    l <-  l %>%
      addLegend('topright',
                colors = c('blue', 'red'),
                labels = c('Cold spot', 'Hot spot'))
  } else {
    
    
    
    # Plot
    plot(dfv,axes=TRUE, border = adjustcolor("black", alpha.f = 0))
    
    # plot(man3, add = TRUE)
    points(dfs, pch= ifelse(dfs@data[,var] == 1, "+", "0"),
           cex=0.5,
           col= adjustcolor("black", alpha.f = 0.5))
    
    # Get the minimum circle for hotspot
    if(hot){
      only_positives_in_cluster <- dfs[dfs$cluster %in% cluster & dfs@data[,var] == 1,]
      points(only_positives_in_cluster,
             pch = 16,
             col = adjustcolor('darkred', alpha.f = 0.6),
             cex = 0.5)
      mc     <- getMinCircle(coordinates(only_positives_in_cluster))
      angles <- seq(0, 2*pi, length.out=200)
      circ   <- cbind(mc$ctr[1] + mc$rad*cos(angles),
                      mc$ctr[2] + mc$rad*sin(angles))
      circ_hot <- circ
      if(circle_outline){
        lines(circ,
              col = adjustcolor('darkred', alpha.f = 0.6),
              lwd = 2,
              lty = ifelse(p_hot > 0.05, 2, 1))
      }
      if(cluster_outline){
        plot(dfv[cluster,],add=TRUE,
             col = adjustcolor('darkred', alpha.f = 0.3),
             border = adjustcolor('black', alpha.f = 0.5))
      }
    }
    if(cold){
      only_negatives_in_cluster <- dfs[dfs$cluster %in% cold_cluster & dfs@data[,var] == 0,]
      points(only_negatives_in_cluster,
             pch = 16,
             col = adjustcolor('darkblue', alpha.f = 0.6),
             cex = 0.5)
      mc     <- getMinCircle(coordinates(only_negatives_in_cluster))
      angles <- seq(0, 2*pi, length.out=200)
      circ   <- cbind(mc$ctr[1] + mc$rad*cos(angles),
                      mc$ctr[2] + mc$rad*sin(angles))
      circ_cold <- circ
      if(circle_outline){
        lines(circ,
              col = adjustcolor('darkblue', alpha.f = 0.6),
              lty = ifelse(p_cold > 0.05, 2, 1))
      }
      if(cluster_outline){
        plot(dfv[cold_cluster,],add=TRUE,
             col = adjustcolor('darkblue', alpha.f = 0.3),
             border = adjustcolor('black', alpha.f = 0.5))
      }
    }
    
    
    title(main = paste0('Most likely ',
                        ifelse(hot, 'hot ', ''),
                        ifelse(hot & cold, 'and ', ''),
                        ifelse(cold, 'cold ', ''),
                        ifelse(hot & cold, 'spots ', 'spot '),
                        'for ', var),
          sub =  paste0(ifelse(hot,
                               paste0("p-value for hot: ",poisson$most.likely.cluster$p.value),
                               " "),
                        ' | ',
                        ifelse(cold,
                               paste0("p-value for cold:",cold_poisson$most.likely.cluster$p.value) , "")))
    legend('topright',
           pch = 16,
           col = c('blue', 'red'),
           legend = c('Cold spot', 'Hot spot'),
           bty = 'n')
  }
  # Get the circle size
  if(exists('circ')){
    circ_radius <- function(circ, hot = TRUE){
      circ_sp <- circ
      p = Polygon(circ_sp)
      circ_sp = SpatialPolygons(list(Polygons(list(p), ID = "a")), 
                                proj4string = CRS(proj4string(moz0)))
                                # proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      # convert to utm to get area
      circ_utm <- spTransform(circ_sp,
                              CRS(paste0("+proj=utm +zone=", 36, " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
      the_area <- rgeos::gArea(circ_utm) / 1000000
      # Convert area to radius
      the_radius <- sqrt(the_area / 3.14)
      the_message <- paste0('----------\nRadius of the ',
                            ifelse(hot, 'hot ', 'cold '),
                            'circle is ', 
                            round(the_radius, digits = 3), 
                            ' kilometers.',
                            '\n----------\n')
      cat(the_message)
    }
    if(exists('circ_hot')){
      circ_radius(circ = circ_hot, hot = TRUE)
    }
    if(exists('circ_cold')){
      circ_radius(circ = circ_cold, hot = FALSE)
    }
  }
  if(use_leaflet){
    return(l)
    
  }
}

# Now you have function named "joe", which have some options:
# hot (whether to show hotspot)
# cold (whether to show coldspot)
# circle_outline (whether or not to draw the circles)
# and cluster_outline (whether to draw the outline of our cluster borders)
# leaflet (whether to make interactive / satellite maps)
# background (which background from here to use https://leaflet-extras.github.io/leaflet-providers/preview/)

# You can run it like this:

joe('pcr_mujer', hot = TRUE, cold = TRUE, cluster_outline = TRUE)
joe('p_p5', hot = TRUE, cold = TRUE, cluster_outline = TRUE)

# For a satellite map, like this_ the selected for the paper:

joe('mic_mujer', hot = TRUE, cold = FALSE, cluster_outline = FALSE,circle_outline = TRUE, use_leaflet = TRUE)

# joe('pcr_mujer', hot = TRUE, cold = FALSE, cluster_outline = FALSE,circle_outline = TRUE, use_leaflet = TRUE)

joe('p_p5', hot = TRUE, cold = FALSE, cluster_outline = FALSE,circle_outline = TRUE, use_leaflet = TRUE)

joe('p_p8', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('comp5_8', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_pcsp', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_dbl3x', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_dbl5e', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_dbl6e', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_msp1', hot = FALSE, cold = FALSE, cluster_outline = FALSE, circle_outline = FALSE, use_leaflet = TRUE)

joe('p_ama1', hot = FALSE, cold = FALSE, cluster_outline = FALSE, circle_outline = FALSE, use_leaflet = TRUE)

###################################

joe('p_p1', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_p42', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_p12', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_p6', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_p4', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_p37', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_p20', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

joe('p_p29', hot = TRUE, cold = FALSE, cluster_outline = FALSE, circle_outline = TRUE, use_leaflet = TRUE)

# For a change of background...
joe('pcr_mujer', hot = TRUE, cold = FALSE, cluster_outline = FALSE, use_leaflet = TRUE, background = 'OpenStreetMap.BlackAndWhite')

joe('pcr_mujer', hot = TRUE, cold = FALSE, cluster_outline = FALSE, use_leaflet = TRUE, background = 'OpenTopoMap')

joe('pcr_mujer', hot = TRUE, cold = TRUE, cluster_outline = TRUE, use_leaflet = TRUE, background = 'Stamen.Watercolor', circle_outline = FALSE)

joe('pcr_mujer', hot = TRUE, cold = FALSE, cluster_outline = TRUE, use_leaflet = TRUE, background = 'Esri.WorldImagery', circle_outline = TRUE)

#################################!!!!ZEBRA!!!!!!!######################################
# Define a functiion for identifying which points are
# in hotspots
in_or_out <- function(var = "p_p5",
                hot = TRUE,
                dfs = df_spatial,
                seed = 41){
  set.seed(seed)
  # if(var %in% "mic_mujer"){set.seed(50)} else {set.seed(seed)}
  # Use the kulldorf method to get hotspots
  library(SpatialEpi)

  
  # Create centroids
  centroids <- as.matrix(coordinates(dfv))
  
  # Get the positivity rate for a variable
  positivity <- get_positivity(var)
  
  # Define some parameters about our results
  cases <- positivity$positives
  population <- positivity$total
  n_strata <- nrow(dfv@data)
  
  expected_cases <- sum(cases, na.rm = TRUE)
  expected_cases <- expected_cases * (population / sum(population, na.rm = TRUE))
  
  # Set paramaters
  pop.upper.bound <- 0.5
  n.simulations <- 9999
  alpha.level <- 0.05
  plot <- FALSE
  
  poisson <- kulldorff(geo = centroids,
                       cases = cases,
                       population = population,
                       expected.cases = expected_cases,
                       pop.upper.bound = pop.upper.bound,
                       n.simulations = n.simulations,
                       alpha.level = 0.05,
                       plot = FALSE)
  p_hot <- poisson$most.likely.cluster$p.value


  # get clusters
  cluster <- poisson$most.likely.cluster$location.IDs.included
  in_cluster <- dfs@data$cluster %in% cluster
  
  # Get whether in circles or not
  only_positives_in_cluster <- dfs[dfs$cluster %in% cluster & 
                                     !is.na(dfs@data[,var]) &
                                     dfs@data[,var] == 1,]
  
  mc     <- getMinCircle(coordinates(only_positives_in_cluster))
  angles <- seq(0, 2*pi, length.out=200)
  circ   <- cbind(mc$ctr[1] + mc$rad*cos(angles),
                  mc$ctr[2] + mc$rad*sin(angles))
      circ_sp <- circ
      p = Polygon(circ_sp)
      circ_sp = SpatialPolygons(list(Polygons(list(p), ID = "a")), 
                                # proj4string = proj4string(only_positives_in_cluster))
                                proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      dfs2 <- spTransform(dfs, 
                                               proj4string(circ_sp))
      in_circle <- over(dfs2, polygons(circ_sp))
      in_circle <- !is.na(in_circle)
  # Make a dataframe to return
      out <- data.frame(a = in_cluster,
                        b = in_circle)
      names(out)<- paste0(var, c("_cluster", "_circle"))
      return(out)
}

# The below loop creates columns for whether something is in or out
vars <- c('pcr_mujer',
          'mic_mujer',
          'p_p5',
          'p_p8',
          'comp5_8',
          'p_pcsp',
          'p_dbl3x',
          'p_dbl5e',
          'p_dbl6e')#,
          # 'p_msp1',
          #"p_ama1")
# coordinates(df_spatial) <- ~longitude+latitude
# proj4string(df_spatial) <- proj4string(dfs)
df <- df[df$motid %in% df_spatial$motid,]
x <- df

for (j in 1:length(vars)){
  message(vars[j])
  new_cols <- in_or_out(var = vars[j], dfs = df_spatial)
  x <- cbind(x, new_cols)
}
df <- x
readr::write_csv(df, "~/Desktop/df.csv")

# Define a function to get prevalence for both circle vs. cluster
calculate_prevalence <- function(var, plot = TRUE){
  circle_variable <- df[,paste0(var, "_circle")]
  cluster_variable <- df[,paste0(var, "_cluster")]
  prevalence_variable <- df[,paste0(var)]
  # Calculate prevalence in the circle
  out <- data.frame(location = rep(c("in", "out"), each =2),
                    type = c("circle", "cluster"),
                    positives = NA,
                    negatives = NA,
                    population = NA)
  # Calculate in circle
  positives = sum(prevalence_variable[circle_variable], na.rm = TRUE)
  negatives = length(which(prevalence_variable[circle_variable] == 0))
  population = length(prevalence_variable[circle_variable])
  out$positives[out$location == "in" & out$type == "circle"] <- positives
  out$negatives[out$location == "in" & out$type == "circle"] <- negatives
  out$population[out$location == "in" & out$type == "circle"] <- population
  
  # Calculate in cluster
  positives = sum(prevalence_variable[cluster_variable], na.rm = TRUE)
  negatives = length(which(prevalence_variable[cluster_variable] == 0))
  population = length(prevalence_variable[cluster_variable])
  out$positives[out$location == "in" & out$type == "cluster"] <- positives
  out$negatives[out$location == "in" & out$type == "cluster"] <- negatives
  out$population[out$location == "in" & out$type == "cluster"] <- population
  
  # Out of the cirlce
  out$positives[out$location == "out" & out$type == "circle"] <- sum(prevalence_variable[!circle_variable], na.rm = TRUE)
  out$negatives[out$location == "out" & out$type == "circle"] <- length(which(prevalence_variable[!circle_variable] == 0))
  out$population[out$location == "out" & out$type == "circle"] <- length(prevalence_variable[!circle_variable])
  
  # Out of the cirlce
  out$positives[out$location == "out" & out$type == "cluster"] <- sum(prevalence_variable[!cluster_variable], na.rm = TRUE)
  out$negatives[out$location == "out" & out$type == "cluster"] <- length(which(prevalence_variable[!cluster_variable] == 0))
  out$population[out$location == "out" & out$type == "cluster"] <- length(prevalence_variable[!cluster_variable])

  # Calculate prevalence  
  out$prev<-out$positives/out$population*100
  out <- out %>% arrange(type)
  if(plot){
    g <- ggplot(data = out,
           aes(x = location,
               y = prev)) +
      geom_bar(stat = "identity") +
      facet_wrap(~type) +
      geom_label(aes(label = paste0(round(prev, digits = 2), "%"))) +
      theme_cism() +
      ggtitle(var)
    print(g)
  }
  return(out)
}

calculate_prevalence("mic_mujer")
calculate_prevalence("pcr_mujer")
calculate_prevalence("p_p5")
calculate_prevalence("p_p8")
calculate_prevalence("comp5_8")
calculate_prevalence("p_pcsp")
calculate_prevalence("p_dbl3x")
calculate_prevalence("p_dbl5e")
calculate_prevalence("p_dbl6e")
#calculate_prevalence("p_msp1")
#calculate_prevalence("p_ama1")





# The below is a loop to run all the variables and create a pdf with
# all the chart combinations
# It will save a document called "some_examples.pdf" in the below folder:
#getwd() # run this to get the folder
vars <- c('pcr_mujer',
          'p_p5',
          'p_p8',
          'comp5_8',
          'p_dbl5e',
          'p_msp1')
#pdf('some_examples.pdf', height = 11, width = 8.5)
#for (i in 1:length(vars)){
# par(mfrow = c(3,2))
# this_var <- vars[i]
# joe(this_var, hot = TRUE, cold = FALSE, cluster_outline = FALSE)
# joe(this_var, hot = FALSE, cold = TRUE, cluster_outline = FALSE)
# joe(this_var, hot = TRUE, cold = TRUE, cluster_outline = FALSE)
#joe(this_var, hot = TRUE, cold = FALSE, cluster_outline = TRUE)
#joe(this_var, hot = FALSE, cold = TRUE, cluster_outline = TRUE)
#joe(this_var, hot = TRUE, cold = TRUE, cluster_outline = TRUE)

dev.off()


