# load required libraries 
library(tidyverse)
library(assertthat)
library(dplyr)
library(purrr)
library(stringr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(extrafont)
library(maps)


#Specify species
spp <- "MAHA"


metasitesfile <- paste("Data\\",spp, "_metasites_outsideMiCO_v14_meter_projection.csv", sep="")
metaroutesfile <- paste("Data\\",spp, "_metaroutes_outsideMiCO_v14_meter_projection.csv", sep="")

# load georeferenced sites & routes
metasites <-  read.csv(metasitesfile, header = T, sep = ",", dec = ".", stringsAsFactors = F)
metaroutes <- read.csv(metaroutesfile, header=T, sep = ",", dec = ".", stringsAsFactors = F) 

species <- metasites$CommonName[1] 
activitytypes <- c("SPA", "NES", "BRE", "FEE", "STA", "STO", "MIG", "WIN", "NBR", "OBS", "",
                   "REA", "FLE")
fullactivity <- c("Spawning", "Nesting", "Breeding", "Fledging", "Chick-Rearing", "Feeding", "Staging", "Stopover", "Migrating", 
                  "Wintering", "Nonbreeding", "Observation", "")

metasites <- metasites %>% rowwise() %>% mutate(BehaviorForSymbology = str_split(activity_code,";")[[1]][1])

#Prepare data for input to the igraph graph structure (i.e., the network model)  
nodes <- with(metasites, data.frame(id=metasite_id, lon=cntrd_ln, lat=cntrd_lt, radius=radius, type = factor(BehaviorForSymbology, levels = fullactivity) , activities = activity_code, num = n_ndvdl, SiteLocation = metasite_id))
edges <- with(metaroutes, data.frame(from=MetaUniqueID_from, to=MetaUniqueID_to, weight=num_individuals, category=method)) #Each of these edges defines a connection via the MetaUniqueID in the from and to fields

edges <- edges %>% mutate(category = as.factor(category)) #make sure category is read as a factor

dplyr::setdiff(edges$to, nodes$id)

#Generate a graph structure g with the igraph library
g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
foo <- edges[!(edges$from %in% nodes$id),"from"]


####GENERATE GLOBAL MAP
#Dictate the extent of the map and the create base layer
mapcoords <- coord_fixed(xlim = c(-180, 180), ylim = c(-75, 80))
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#FFFFFF", color = "#DDDDDD", #CECECE
                               size = 0.15)


#Choose colours for the various Activities... associate similar Activities to make the map easier to read
nodecolors <- c(
  "Spawning" = "#FB3C0E",
  "Nesting" = "#FB3C0E",
  "Breeding" = "#FB3C0E",
  "Fledging" = "#FB3C0E",
  "Feeding" = "#73AB0D",
  "Staging" = "#73AB0D",
  "Stopover" = "#73AB0D",
  "Migrating" = "#0984C1",
  "Wintering" = "#490530",
  "Nonbreeding" = "#490530",
  "Observation" = "#033B59")


#Dictate size, location and font for text on the map
maptheme <- theme(text = element_text(size = 12),
                  plot.title = element_text(hjust = 0.025, vjust= -12),
                  plot.subtitle = element_text(size = 9, hjust = 0.028, vjust= -20),
                  plot.caption = element_text(hjust = 0.97, vjust= +11, size = 12),
                  legend.position = c(0.07,0.4), #MAY NEED ADJUSTMENT 
                  legend.background=element_blank(),
                  legend.title = element_text(face = "bold"),
                  plot.margin = unit(c(0,0,0,0),"cm"))

#Internal information used to put the igraph structure in a ggplot via ggraph
node_pos <- nodes %>%
  select(lon, lat) %>%
  rename(x = lon, y = lat)   # node positions must be called x, y
lay <- create_layout(g, layout = node_pos)
assert_that(nrow(lay) == nrow(nodes))

#We pass the layout lay and use ggraph's geoms geom_edge_arc and geom_node_point for plotting:
global_plot <- ggraph(lay) + country_shapes + mapcoords +
  geom_node_circle(aes(r = radius, fill = type), color = "transparent", alpha = 0.25) +
  scale_fill_manual(values = nodecolors, breaks =  fullactivity, labels = fullactivity, name = "Node Type") +
  #geom_node_text(aes(label = SiteLocation), repel = TRUE, size = 3,
  #             color = "black") +
  geom_edge_arc(aes(edge_width = weight), strength = 0.25, alpha = 0.4) +
  scale_edge_width_continuous(range = c(0.1, 1), guide = "none") +
  labs(x = NULL, 
       y = NULL, 
       title = paste(c("Global connectivity for ", species, "s"), collapse=""), 
       subtitle = "Derived from publications utilising telemetry data between 1990 & 2015", 
       caption = "Migratory Connectivity in the Ocean (MiCO) System. 2024.") +
  maptheme

global_plot

plotname <- paste(c("Data//",spp,"_Global_NetworkModel_directional.tif"), collapse = "")

ggsave(global_plot, filename = plotname, width = 30, height = 14, units = "cm", device = "tiff", dpi = 300)









