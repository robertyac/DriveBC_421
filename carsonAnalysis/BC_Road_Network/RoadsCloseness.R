# Load necessary libraries
library(igraph)
library(dplyr)
library(ggplot2)

# Load data
bc_roads <- read.csv("bcroads.csv")
bc_towns_coords <- read.csv("bctowns.csv")

# Rename the town column to 'name' for compatibility with igraph
bc_towns_coords <- bc_towns_coords %>%
  rename(name = Town)

# Remove rows with missing latitude or longitude
bc_towns_coords <- bc_towns_coords %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Create the graph
g <- graph_from_data_frame(d = bc_roads, vertices = bc_towns_coords, directed = FALSE)

# Ensure all nodes in the graph have valid coordinates
unmatched_nodes <- setdiff(V(g)$name, bc_towns_coords$name)
if (length(unmatched_nodes) > 0) {
  warning("The following nodes have no matching coordinates and will be excluded: ", paste(unmatched_nodes, collapse = ", "))
  g <- delete_vertices(g, unmatched_nodes) # Remove nodes without coordinates
}

# Compute closeness centrality
closeness_centrality <- closeness(g, normalized = TRUE)

# Add closeness centrality as a vertex attribute
V(g)$closeness <- closeness_centrality

# Create a data frame with node names and closeness centrality values
closeness_df <- data.frame(
  Node = V(g)$name,
  Closeness = V(g)$closeness
)

# Sort the data frame by closeness centrality in descending order
closeness_df <- closeness_df %>%
  arrange(desc(Closeness))

# Print the sorted data frame

# Create a bar chart of closeness centrality
ggplot(closeness_df, aes(x = reorder(Node, Closeness), y = Closeness)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Closeness Centrality of Nodes",
    x = "Node",
    y = "Closeness Centrality"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )