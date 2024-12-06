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

# Compute edge betweenness centrality
edge_betweenness <- edge_betweenness(g)

# Add edge betweenness as an edge attribute
E(g)$betweenness <- edge_betweenness

# Print the top edges by betweenness centrality
top_edges <- head(sort(edge_betweenness, decreasing = TRUE), n = 10)

# Define nodes to label
nodes_to_label <- c("Vancouver", "Kamloops", "Hope", "Kelowna", "Williams Lake", "Prince George", "Fort St. John", "Fort Nelson", "Terrace", "Victoria", "Revelstoke", "Cranbrook", "Castlegar")

# Create a vector of labels, with NA for nodes not in the selection
node_labels <- ifelse(V(g)$name %in% nodes_to_label, V(g)$name, NA)

# Extract latitude and longitude for spatial layout
layout_spatial <- cbind(V(g)$Longitude, V(g)$Latitude)

# Plot the graph with edge widths proportional to edge betweenness centrality
plot(
  g,
  layout = layout_spatial,
  vertex.label = node_labels, # Use the custom labels
  vertex.label.cex = 0.8,     # Adjust label size
  vertex.label.color = "black", # Set label color
  vertex.label.font = 2,             # Make labels bold
  vertex.label.dist = 0.5,             # Distance from node center
  vertex.size = 1,
  vertex.color = "black",
  edge.width = E(g)$betweenness / max(E(g)$betweenness) * 5, # Scale edge width
  edge.color = "red",
  main = "Edge Betweenness Centrality of BC Road Network"
)
