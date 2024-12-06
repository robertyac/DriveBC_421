# Load necessary libraries
library(igraph)
library(dplyr)

# Load data
bc_roads <- read.csv("bcroads.csv")
bc_towns_coords <- read.csv("bctowns.csv")

# Rename the town column to 'name' for compatibility with igraph
bc_towns_coords <- bc_towns_coords %>%
  rename(name = Town)

# Create the graph
g <- graph_from_data_frame(d = bc_roads, vertices = bc_towns_coords, directed = FALSE)

# Extract latitude and longitude for spatial layout
layout_spatial <- cbind(V(g)$Longitude, V(g)$Latitude)

# Plot the graph with smaller, offset labels
plot(
  g, layout = layout_spatial, vertex.label = V(g)$name, vertex.size = 1,
  vertex.color = "black", edge.width = 0.5, edge.color = "gray",
  vertex.label.cex = 0.6,       # Makes labels smaller
  vertex.label.dist = 0.3,      # Offsets labels slightly from the nodes
  main = "Spatial Network of BC Towns and Roads"
)
