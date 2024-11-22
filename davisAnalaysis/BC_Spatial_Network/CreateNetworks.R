library(geosphere)
library(igraph)

# Define the years and file format
years <- 2018:2023
file_pattern <- "drivebc_events_hist_%d.csv"  # File path pattern

# List of EVENT_TYPE categories to process
event_types <- c("INCIDENT", "CONSTRUCTION", "ROAD_CONDITION", "WEATHER_CONDITION")

# Loop through each year
for (year in years) {
  # Generate the file path for the current year
  file_path <- sprintf(file_pattern, year)
  
  print(paste("Loading dataset for year:", year))
  flush.console()
  
  # Load the dataset for the current year
  df <- read.csv(file_path)
  print(paste("Dataset for year", year, "loaded."))
  flush.console()
  
  # Extract month from the START_DATETIME date column
  df$month <- format(as.POSIXct(df$START_DATETIME, format = "%Y-%m-%d %H:%M:%S"), "%m")
  
  # Process each EVENT_TYPE
  for (event_type in event_types) {
    print(paste("Processing EVENT_TYPE:", event_type, "for year", year))
    flush.console()
    
    # Filter for the current EVENT_TYPE
    df_filtered <- df[df$EVENT_TYPE == event_type, ]
    print(paste("Number of incidents with EVENT_TYPE", event_type, ":", nrow(df_filtered)))
    flush.console()
    
    # Create an empty graph with nodes for each incident
    g <- make_empty_graph(n = nrow(df_filtered), directed = FALSE)
    V(g)$name <- paste0("Incident_", 1:nrow(df_filtered))
    V(g)$latitude <- df_filtered$HEAD_LATITUDE
    V(g)$longitude <- df_filtered$HEAD_LONGITUDE
    V(g)$road_name <- df_filtered$ROAD_NAME
    V(g)$month <- df_filtered$month
    V(g)$area_name <- df_filtered$AREA_NAME
    
    # Color nodes by month
    month_colors <- rainbow(12, s = 0.5, v = 0.8)
    V(g)$color <- month_colors[as.numeric(df_filtered$month)]
    
    # Initialize a list to collect edges
    edge_list <- list()
    print("Checking distances between incidents on the same road, in the same region, in the same month, within 1 km...")
    flush.console()
    
    # Loop over each unique road, region, and month
    unique_roads <- unique(df_filtered$ROAD_NAME)
    
    for (road in unique_roads) {
      road_df <- df_filtered[df_filtered$ROAD_NAME == road, ]
      print(paste("Evaluating road:", road, "- Number of incidents:", nrow(road_df)))
      flush.console()
      
      for (area in unique(road_df$AREA_NAME)) {
        area_df <- road_df[road_df$AREA_NAME == area, ]
        
        for (month in unique(area_df$month)) {
          month_df <- area_df[area_df$month == month, ]
          indices <- which(df_filtered$ROAD_NAME == road & df_filtered$AREA_NAME == area & df_filtered$month == month)
          num_incidents <- nrow(month_df)
          
          if (num_incidents > 1) {
            # Calculate distances only once between unique pairs of nodes
            for (i in 1:(num_incidents - 1)) {
              for (j in (i + 1):num_incidents) {
                coord1 <- c(month_df$HEAD_LONGITUDE[i], month_df$HEAD_LATITUDE[i])
                coord2 <- c(month_df$HEAD_LONGITUDE[j], month_df$HEAD_LATITUDE[j])
                
                # Skip pairs with missing coordinates
                if (any(is.na(coord1)) || any(is.na(coord2))) {
                  next  # Skip this pair
                }
                
                distance <- distHaversine(coord1, coord2)
                
                if (distance <= 1000) {
                  edge_list[[length(edge_list) + 1]] <- c(indices[i], indices[j])
                }
              }
            }
          }
        }
      }
    }
    
    # Flatten edge_list and add edges to the graph if there are any
    if (length(edge_list) > 0) {
      edge_matrix <- do.call(rbind, edge_list)
      g <- add_edges(g, t(edge_matrix))
      print("Edges added based on the same road, region, month, and distance criteria.")
      flush.console()
    } else {
      print("No edges were added.")
      flush.console()
    }
    
    # Define the file path where you want to save the data
    save_file_path <- sprintf("%d_%s.RData", year, event_type)
    
    # Save the graph object and dataframe to an RData file
    save(g, df_filtered, file = save_file_path)
    
    # Confirm the data has been saved
    print(paste("Data saved to", save_file_path))
    flush.console()
  }
}
