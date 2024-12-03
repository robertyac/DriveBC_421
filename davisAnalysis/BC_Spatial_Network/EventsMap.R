library(igraph)
library(sf)
library(tmap)
library(RColorBrewer)

# Set tmap to interactive viewing mode
tmap_mode("view")

# Define years and EVENT_TYPEs
years <- 2018:2023
event_types <- c("INCIDENT", "CONSTRUCTION", "ROAD_CONDITION", "WEATHER_CONDITION")
type_colors <- brewer.pal(length(event_types), "Dark2")  # Distinct colors for EVENT_TYPEs

# Step 1: Compute a common bounding box across all years and EVENT_TYPEs
all_data <- list()

for (year in years) {
  for (event_type in event_types) {
    # Load the corresponding RData file
    file_path <- paste0(year, "_", event_type, ".RData")
    load(file_path)  # This loads g and df_filtered
    
    # Collect all longitude and latitude data
    all_data[[paste0(year, "_", event_type)]] <- data.frame(
      longitude = V(g)$longitude,
      latitude = V(g)$latitude
    )
  }
}

# Combine all data across years and event types
all_data_df <- do.call(rbind, all_data)

# Define the common bounding box
bbox <- st_bbox(c(
  xmin = min(all_data_df$longitude, na.rm = TRUE),
  xmax = max(all_data_df$longitude, na.rm = TRUE),
  ymin = min(all_data_df$latitude, na.rm = TRUE),
  ymax = max(all_data_df$latitude, na.rm = TRUE)
), crs = st_crs(4326))

# Step 2: Generate interactive maps for each year with the common extent
for (year in years) {
  print(paste("Processing year:", year))
  flush.console()
  
  # Initialize a combined dataset for all EVENT_TYPEs for the current year
  combined_data <- list()
  
  for (event_type in event_types) {
    # Load the corresponding RData file
    file_path <- paste0(year, "_", event_type, ".RData")
    load(file_path)  # This loads g and df_filtered
    print(paste("Processing EVENT_TYPE:", event_type, "for year", year))
    
    # Perform clustering (Louvain method)
    communities <- cluster_louvain(g)
    community_sizes <- sizes(communities)
    
    # Filter top 25 largest clusters
    top_25_indices <- order(community_sizes, decreasing = TRUE)[1:25]
    top_25_nodes <- which(membership(communities) %in% top_25_indices)
    
    # Extract spatial data for top 25 clusters
    node_data <- data.frame(
      id = V(g)$name[top_25_nodes],
      longitude = V(g)$longitude[top_25_nodes],
      latitude = V(g)$latitude[top_25_nodes],
      community = factor(membership(communities)[top_25_nodes]),
      event_type = event_type  # Add EVENT_TYPE information
    )
    
    # Get one representative point per cluster (mean longitude and latitude)
    cluster_centroids <- aggregate(
      cbind(longitude, latitude) ~ community + event_type,
      data = node_data,
      FUN = mean
    )
    
    # Add to the combined dataset
    combined_data[[event_type]] <- cluster_centroids
  }
  
  # Combine all EVENT_TYPE data into a single data frame for the current year
  combined_data_df <- do.call(rbind, combined_data)
  
  # Add jitter to longitude and latitude to avoid overlapping points
  set.seed(42)  # Set a seed for reproducibility
  combined_data_df$longitude <- jitter(combined_data_df$longitude, amount = 0.08)
  combined_data_df$latitude <- jitter(combined_data_df$latitude, amount = 0.08)
  
  # Convert to an sf object
  nodes_sf <- st_as_sf(combined_data_df, coords = c("longitude", "latitude"), crs = 4326)
  
  # Assign colors by EVENT_TYPE
  event_type_colors <- setNames(type_colors, event_types)
  nodes_sf$color <- event_type_colors[nodes_sf$event_type]
  
  # Generate the interactive map for the current year with the common bounding box
  map <- tm_basemap("OpenStreetMap") +
    tm_shape(nodes_sf, bbox = bbox) +          # Use the common bounding box
    tm_symbols(
      col = "event_type",         # Use distinct colors for each EVENT_TYPE
      size = 0.5,                 # Adjust circle size
      border.col = "black",       # Add black borders for visibility
      palette = type_colors,      # Use the predefined EVENT_TYPE colors
      title.col = "Event Type",   # Legend title
      popup.vars = c("Event Type" = "event_type", "Community" = "community")  # Add pop-ups
    ) +
    tm_layout(
      title = paste(year, "Top 25 Communities in Each Event Type"),
      title.size = 2,
      legend.outside = TRUE
    )
  
  # Save the interactive map as an HTML file
  tmap_save(map, filename = paste0("Interactive_Map_", year, ".html"))
  
  # Optionally, display the map in the RStudio Viewer or default web browser
  print(map)
}