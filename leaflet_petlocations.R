library(rgdal)
library(sp)
library(leaflet)
library(sf)

# Read in shapefile and select only the columns nam and geometry
malaysia_shape <- st_read(dsn = "misc/shape_data/polbnda_mys.shp")[, c("nam", "geometry")]

# Change nam WILAYAH PERSEKUTUAN LABUAN to LABUAN
malaysia_shape$nam[malaysia_shape$nam == "WILAYAH PERSEKUTUAN LABUAN"] <- "LABUAN"

# Change nam WILAYAH PERSEKUTUAN to KUALA LUMPUR
malaysia_shape$nam[malaysia_shape$nam == "WILAYAH PERSEKUTUAN"] <- "KUALA LUMPUR"

# Read in data
pet_frequency <- as.data.frame(table(petdata$State))

# To uppercase
pet_frequency <- apply(pet_frequency, 2, toupper)

states <- unique(malaysia_shape$nam)
number_states <- length(states)

# Data frame with 3 columns
choropleth_data <- data.frame(
  state_name = states,
  geometry = rep(NA, number_states),
  petfreq = rep(0, number_states)
)

for (i in seq_len(nrow(choropleth_data))) {
    current_state <- choropleth_data$state_name[i]

    # Add petfreq to choropleth_data
    for (j in seq_len(nrow(pet_frequency))) {
        if (current_state == pet_frequency[j, "Var1"]) {
            choropleth_data$petfreq[i] <- pet_frequency[j, "Freq"]
        }
    }

    # Collect all geometries for current_state
    current_geoms <- st_geometry(malaysia_shape[malaysia_shape$nam == current_state, ])
    choropleth_data$geometry[i] <- st_union(current_geoms)
}

choropleth_data <- choropleth_data %>% st_as_sf(sf_column_name = "geometry")

# Convert to sf object
choropleth_data <- st_as_sf(choropleth_data[, c("state_name", "petfreq")], coords = choropleth_data$geometry)
choropleth_data$petfreq <- as.numeric(choropleth_data$petfreq)

# Create color palette
pal <- colorNumeric("YlOrRd", domain = log(choropleth_data$petfreq + 1))

# Create leaflet map
leaflet(choropleth_data) %>%
addTiles() %>%
addPolygons(
    fillColor = ~pal(log(petfreq)),
    weight = 2,
    opacity = 1,
    color = "green",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
        weight = 5,
        color = "red",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
    ),
    label = ~paste0("<strong>", state_name, "</strong><br>Number of pets: ", petfreq)
) %>%
addLegend(
    pal = pal,
    values = ~log(petfreq + 1),
    opacity = 0.7,
    title = "Log scale of number of pets",
    position = "topright"
)
