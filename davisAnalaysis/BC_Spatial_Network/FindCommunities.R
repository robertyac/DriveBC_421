library(igraph)
library(geosphere)

# Load the saved RData file
load("DriveBC_421/data/bc_incidents.RData")

# Detect communities using the Louvain method
communities <- cluster_louvain(g)

# Get the size of each community and convert to a numeric vector
community_sizes <- sizes(communities)
community_sizes_vector <- as.numeric(community_sizes)

# Sort and select the top 100 largest communities
top_100_indices <- order(community_sizes_vector, decreasing = TRUE)[1:100]

# Get node memberships for the top 100 communities
top_100_nodes <- which(membership(communities) %in% top_100_indices)

# Scale the community sizes for color mapping (small = light, large = dark)
max_size <- max(community_sizes_vector[top_100_indices])
min_size <- min(community_sizes_vector[top_100_indices])
scaled_sizes <- (community_sizes_vector[top_100_indices] - min_size) / (max_size - min_size)

# Create a color gradient from light blue to dark red to represent community size
color_gradient <- colorRampPalette(c("lightblue", "blue", "green", "yellow", "red"))(100)
size_to_color <- color_gradient[as.numeric(cut(scaled_sizes, breaks = 100))]

# Map the color to each top 100 community's nodes
V(g)$color <- NA  # Set a default color for all nodes
for (i in seq_along(top_100_indices)) {
  community_nodes <- which(membership(communities) == top_100_indices[i])
  V(g)$color[community_nodes] <- size_to_color[i]
}

# Create a subgraph with only the nodes in the top 100 communities
g_top_100 <- induced_subgraph(g, top_100_nodes)

# Set spatial layout for the subgraph using latitude and longitude
layout_spatial_top_100 <- cbind(V(g_top_100)$longitude, V(g_top_100)$latitude)

# Plot only the top 100 communities spatially
print("Plotting the top 100 largest communities spatially...")
flush.console()

# Set up layout to add a color bar beside the plot
layout(matrix(c(1, 2), ncol = 2), widths = c(4, 0.5))  # Adjust width of color bar
par(mar = c(5, 4, 4, 1))  # Smaller margins for the plot panel

# Plot the graph with spatial layout
plot(
  g_top_100, layout = layout_spatial_top_100, vertex.label = NA, vertex.size = 3,
  vertex.color = V(g_top_100)$color, edge.width = 0.5, edge.color = "gray",
  main = "Spatial Distribution of Top 100 Vehicle Incident Communities"
)

# Add a smaller vertical color bar to indicate community sizes
par(mar = c(5, 1, 4, 2))  # Adjust margins for the smaller color bar
image(
  1, seq(0, 1, length.out = 100), t(matrix(1:100)), col = color_gradient,
  axes = FALSE
)

# Add the axis for the color bar
axis(4, at = seq(0, 1, length.out = 5), labels = round(seq(min_size, max_size, length.out = 5)), las = 1, cex.axis = 0.6)

# Add "Community Size" as the title at the top of the color bar legend
mtext("Community Size", side = 3, line = 1, cex = 0.8)  # Position title above the legend

print("Spatial plot of the top 100 largest communities complete.")
flush.console()
