load("DriveBC_421/data/bc_incidents.RData")

# Use Kamada-Kawai layout for initial node distribution
layout_kk <- layout_with_kk(g)

# Normalize and scale layout to spread out nodes better
layout_kk <- norm_coords(layout_kk, xmin = -1, xmax = 1, ymin = -1, ymax = 1)

# Plot the graph with Kamada-Kawai layout and normalized coordinates
if (ecount(g) > 0) {
  print("Plotting the graph with Kamada-Kawai layout and colors by month...")
  flush.console()
  plot(
    g, layout = layout_kk, vertex.label = NA, vertex.size = 3,
    vertex.color = V(g)$color, edge.width = 1, edge.color = "darkgray",
    main = "Incident Network in Cariboo District by Month (Kamada-Kawai Layout)"
  )
  
  # Add a legend for month colors
  legend(
    "topright", legend = month.abb, col = month_colors, pch = 19,
    title = "Month", cex = 0.8, pt.cex = 1.5, box.lwd = 0
  )
  
  print("Graph plotted with Kamada-Kawai layout and colors by month.")
  flush.console()
} else {
  print("Graph has no edges to plot.")
  flush.console()
}

# Detect communities using the Louvain algorithm
communities <- cluster_louvain(g)

# Find the largest community
community_sizes <- sizes(communities)
largest_community <- names(which.max(community_sizes))
largest_community_nodes <- which(membership(communities) == largest_community)

# Retrieve the spatial coordinates of the nodes in the largest community
latitude_largest <- V(g)$latitude[largest_community_nodes]
longitude_largest <- V(g)$longitude[largest_community_nodes]

# Combine coordinates into a dataframe for easier inspection
largest_community_coords <- data.frame(
  node = V(g)$name[largest_community_nodes],
  latitude = latitude_largest,
  longitude = longitude_largest
)

# Print the spatial coordinates of the largest community
print(paste("Largest community (Community", largest_community, ") coordinates:"))
print(largest_community_coords)