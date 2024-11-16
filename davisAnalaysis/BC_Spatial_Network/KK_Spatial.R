library(igraph)
library(geosphere)

# Load the saved RData file
load("DriveBC_421/data/bc_incidents.RData")

# Confirm the data has loaded
print("Data loaded")

# Define colors for each month (assuming month values are 1-12)
month_colors <- rainbow(12, s = 0.5, v = 0.8)

# Assign colors to nodes based on the month (ensure 'month' is numeric or use factor)
V(g)$color <- month_colors[as.numeric(V(g)$month)]

# Use spatial coordinates as the initial layout
layout_spatial <- cbind(V(g)$longitude, V(g)$latitude)

# Generate a Kamada-Kawai layout for better node separation
layout_kk <- layout_with_kk(g)

# Normalize the KK layout to the same scale as spatial coordinates
layout_kk <- norm_coords(layout_kk, xmin = min(layout_spatial[,1]), xmax = max(layout_spatial[,1]),
                         ymin = min(layout_spatial[,2]), ymax = max(layout_spatial[,2]))

# Blend the spatial layout and KK layout by averaging the coordinates (you can adjust the blend ratio)
blend_ratio_spatial <- 0.7
blend_ratio_kk <- 0.3
layout_combined <- blend_ratio_spatial * layout_spatial + blend_ratio_kk * layout_kk

# Plot the graph using the combined layout
if (ecount(g) > 0) {
  print("Plotting the graph with combined spatial and Kamada-Kawai layout...")
  flush.console()
  
  # Plot the graph with combined layout
  plot(
    g, layout = layout_combined, vertex.label = NA, vertex.size = 3,
    vertex.color = V(g)$color, edge.width = 1, edge.color = "darkgray",
    main = "Incident Network by Combined Spatial and Kamada-Kawai Layout"
  )
  
  # Add a legend for month colors
  legend(
    "topright", legend = month.abb, col = month_colors, pch = 19,
    title = "Month", cex = 0.8, pt.cex = 1.5, box.lwd = 0
  )
  
  print("Graph plotted with combined spatial and Kamada-Kawai layout.")
  flush.console()
} else {
  print("Graph has no edges to plot.")
  flush.console()
}
