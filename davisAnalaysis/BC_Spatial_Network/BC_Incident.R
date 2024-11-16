library(geosphere)
library(igraph)

# Load the dataset
print("Loading dataset...")
flush.console()
df <- read.csv("DriveBC_421/data/drivebc_events_hist_2023.csv")
print("Dataset loaded.")
flush.console()

# Filter for EVENT_TYPE == "INCIDENT"
df <- df[df$EVENT_TYPE == "INCIDENT", ]
print(paste("Number of incidents with EVENT_TYPE 'INCIDENT':", nrow(df)))
flush.console()

# Extract month from the START_DATETIME date column
df$month <- format(as.POSIXct(df$START_DATETIME, format="%Y-%m-%d %H:%M:%S"), "%m")

# Create an empty graph with nodes for each incident
g <- make_empty_graph(n = nrow(df), directed = FALSE)
V(g)$name <- paste0("Incident_", 1:nrow(df))
V(g)$latitude <- df$HEAD_LATITUDE
V(g)$longitude <- df$HEAD_LONGITUDE
V(g)$road_name <- df$ROAD_NAME
V(g)$month <- df$month
V(g)$area_name <- df$AREA_NAME

# Color nodes by month
month_colors <- rainbow(12, s = 0.5, v = 0.8)
V(g)$color <- month_colors[as.numeric(df$month)]

# Initialize a list to collect edges
edge_list <- list()
print("Checking distances between incidents on the same road, in the same region, in the same month, within 50 km...")
flush.console()

# Loop over each unique road, region, and month
unique_roads <- unique(df$ROAD_NAME)

for (road in unique_roads) {
  road_df <- df[df$ROAD_NAME == road, ]
  print(paste("Evaluating road:", road, "- Number of incidents:", nrow(road_df)))
  flush.console()
  
  for (area in unique(road_df$AREA_NAME)) {
    area_df <- road_df[road_df$AREA_NAME == area, ]
    
    for (month in unique(area_df$month)) {
      month_df <- area_df[area_df$month == month, ]
      indices <- which(df$ROAD_NAME == road & df$AREA_NAME == area & df$month == month)
      num_incidents <- nrow(month_df)
      
      if (num_incidents > 1) {
        # Calculate distances only once between unique pairs of nodes
        for (i in 1:(num_incidents - 1)) {
          for (j in (i + 1):num_incidents) {
            coord1 <- c(month_df$HEAD_LONGITUDE[i], month_df$HEAD_LATITUDE[i])
            coord2 <- c(month_df$HEAD_LONGITUDE[j], month_df$HEAD_LATITUDE[j])
            distance <- distHaversine(coord1, coord2)
            
            if (distance <= 25000) {
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
save_file_path <- "DriveBC_421/data/bc_incidents.RData"

# Save the graph object and dataframe to an RData file
save(g, df, file = save_file_path)

# Confirm the data has been saved
print(paste("Data saved to", save_file_path))
