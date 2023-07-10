# Read in shapefile and select only the columns nam and geometry
malaysia_shape <- st_read(dsn = "misc/shape_data/polbnda_mys.shp")[, c("nam", "geometry")]

# Change nam WILAYAH PERSEKUTUAN LABUAN to LABUAN
malaysia_shape$nam[malaysia_shape$nam == "WILAYAH PERSEKUTUAN LABUAN"] <- "LABUAN"

# Change nam WILAYAH PERSEKUTUAN to KUALA LUMPUR
malaysia_shape$nam[malaysia_shape$nam == "WILAYAH PERSEKUTUAN"] <- "KUALA LUMPUR"

# Read in data
pet_frequency <- setNames(data.frame(table(petdata$State)), c("state_name", "pets"))
cat_freq <- setNames(data.frame(table(petdata[petdata$Type == "Cat", ]$State)), c("state_name", "cats"))
dog_freq <- setNames(data.frame(table(petdata[petdata$Type == "Dog", ]$State)), c("state_name", "dogs"))

# Merge cat and dog frequencies
pet_frequency <- merge(pet_frequency, cat_freq, by = "state_name", all = TRUE)
pet_frequency <- merge(pet_frequency, dog_freq, by = "state_name", all = TRUE)
# To uppercase
pet_frequency <- data.frame(apply(pet_frequency, 2, toupper))

# Load population data: http://www.dosm.gov.my/portal-main/release-content/demographic-statistics-fourth-quarter-q4-2018-malaysia
population_2018 <- data.frame(
    state_name = c("JOHOR", "KEDAH", "KELANTAN", "MELAKA", "NEGERI SEMBILAN", "PAHANG", "PERAK", "PERLIS", "PULAU PINANG", "SABAH", "SARAWAK", "SELANGOR", "TERENGGANU", "KUALA LUMPUR", "LABUAN"),
    population = c(3764800, 2173700, 1870700, 926800, 1138200, 1675000, 2510700, 254700, 1778200, 392100, 2804900, 6610400, 1241600, 1796200, 100000)
)

# Load gdp data per capita per state: https://en.wikipedia.org/wiki/List_of_Malaysian_states_by_GDP
gdp_per_capita_2018_rm <- data.frame(
    state_name = c("JOHOR", "KEDAH", "KELANTAN", "MELAKA", "NEGERI SEMBILAN", "PAHANG", "PERAK", "PERLIS", "PULAU PINANG", "SABAH", "SARAWAK", "SELANGOR", "TERENGGANU", "KUALA LUMPUR", "LABUAN"),
    gdp = c(36394, 21410, 13668, 47960, 43047, 35577, 30303, 24442, 52397, 25861, 52301, 51528, 30216, 121293, 74337)
)

states <- unique(malaysia_shape$nam)
number_states <- length(states)

# Data frame with 5 columns
choropleth_data <- data.frame(
    state_name = states,
    geometry = rep(NA, number_states),
    petfreq = rep(0, number_states),
    dogfreq = rep(0, number_states),
    catfreq = rep(0, number_states),
    population = rep(0, number_states),
    gdp = rep(0, number_states)
)
pieplots <- data.frame(
    row.names = states,
    pieplot = rep("<strong>No pieplot available</strong>", number_states))

for (i in seq_len(nrow(choropleth_data))) {
    current_state <- choropleth_data$state_name[i]

    # Add petfreq to choropleth_data
    for (j in seq_len(nrow(pet_frequency))) {
        if (current_state == pet_frequency[j, "state_name"]) {
            choropleth_data$petfreq[i] <- pet_frequency[j, "pets"]
            dog_nums <- as.numeric(pet_frequency[j, "dogs"])
            cat_nums <- as.numeric(pet_frequency[j, "cats"])
            choropleth_data$dogfreq[i] <- dog_nums
            choropleth_data$catfreq[i] <- cat_nums

            # Create pie chart
            plot <- ggplot(data.frame(value = c(cat_nums, dog_nums), Type = c("Cats", "Dogs")),
                        aes(x = "", y = value, fill = Type)) +
                    geom_col(color = "black") +
                    geom_text(aes(label = value),
                        position = position_stack(vjust = 0.5),
                        size = 15,
                        fontface = "bold"
                        ) +
                    coord_polar(theta = "y") +
                    scale_fill_manual(values = c("#3f8ded", "#f76060")) +
                    theme_void()  +
                    theme(legend.key.size = unit(2, 'cm'), legend.text = element_text(size=40), legend.title = element_text(size=40))

            png_file <- tempfile(fileext = ".png")
            ggsave(png_file, plot)

            # Encode png file to base64
            encoded_png <- base64enc::base64encode(png_file)

            pieplots[current_state, "pieplot"] <- paste0("<img src='data:image/png;base64,", encoded_png, "' />")
        }
    }
    
    # Add population to choropleth_data
    for (j in seq_len(nrow(population_2018))) {
        if (current_state == population_2018[j, "state_name"]) {
            choropleth_data$population[i] <- population_2018[j, "population"]
        }
    }

    # Add gdp to choropleth_data
    for (j in seq_len(nrow(gdp_per_capita_2018_rm))) {
        if (current_state == gdp_per_capita_2018_rm[j, "state_name"]) {
            choropleth_data$gdp[i] <- gdp_per_capita_2018_rm[j, "gdp"]
        }
    }

    # Collect all geometries for current_state
    current_geoms <- st_geometry(malaysia_shape[malaysia_shape$nam == current_state, ])
    choropleth_data$geometry[i] <- st_union(current_geoms)
}

choropleth_data <- choropleth_data %>% st_as_sf(sf_column_name = "geometry")

# Convert to sf object
choropleth_data <- st_as_sf(choropleth_data[, c("state_name", "petfreq", "dogfreq", "catfreq", "population", "gdp")], coords = choropleth_data$geometry)
choropleth_data$petfreq <- as.numeric(choropleth_data$petfreq)
choropleth_data$dogfreq <- as.numeric(choropleth_data$dogfreq)
choropleth_data$catfreq <- as.numeric(choropleth_data$catfreq)
choropleth_data$population <- as.numeric(choropleth_data$population)
choropleth_data$gdp <- as.numeric(choropleth_data$gdp)

# Create color palette
max_petpalval <- 9001
petpal <- colorNumeric("YlOrRd", domain = c(0, log(max_petpalval)))
poppal <- colorNumeric("YlOrRd", domain = choropleth_data$population)
gdppal <- colorNumeric("YlOrRd", domain = choropleth_data$gdp)
legend_steps <- seq(0, 1, length.out = 8) * log(max_petpalval)

popup <- ~paste0(
    "<strong>", state_name, "</strong>",
    "<br>Number of pets: ", format(petfreq, big.mark = ","),
    "<br>Population: ", format(population, big.mark = ","),
    "<br>GDP per capita: ", format(gdp, big.mark = ","), " [MYR]",
    "<br><br> <u>Distribution of the pet types:</u>",
    "<br>", pieplots[state_name, "pieplot"])

highlight <- highlightOptions(
        weight = 5,
        color = "red",
        dashArray = "",
        bringToFront = TRUE
    )

petlab <- "Number of pets"
poplab <- "Population"
gdplab <- "GDP per capita"
nonelab <- "None"

# Create leaflet map
leaflet(choropleth_data) %>%
addTiles() %>%
addLayersControl(baseGroups = c(petlab, poplab, gdplab, nonelab)) %>%
addPolygons(
    group = nonelab,
    weight = 2,
    opacity = 1,
    color = "blue",
    dashArray = "3",
    fillOpacity = 0,
    highlight = highlight,
    popup = popup
) %>%
addPolygons(
    group = petlab,
    fillColor = ~petpal(log(petfreq + 1)),
    fillOpacity = 0.7,
    weight = 2,
    opacity = 1,
    color = "blue",
    dashArray = "3",
    highlight = highlight,
    popup = popup
) %>%
addPolygons(
    group = poplab,
    fillColor = ~poppal(population),
    fillOpacity = 0.7,
    weight = 2,
    opacity = 1,
    color = "blue",
    dashArray = "3",
    highlight = highlight,
    popup = popup
) %>%
addPolygons(
    group = gdplab,
    fillColor = ~gdppal(gdp),
    fillOpacity = 0.7,
    weight = 2,
    opacity = 1,
    color = "blue",
    dashArray = "3",
    highlight = highlight,
    popup = popup
) %>%
addLegend(
    pal = petpal,
    values = c(0, log(max_petpalval)),
    bins = legend_steps,
    opacity = 0.7,
    title = petlab,
    position = "topright",
    labFormat = labelFormat(transform = function(x) {
         round_any(exp(x) - 1, 5, f = round)
         })
) %>%
addLegend(
    pal = poppal,
    values = ~population,
    opacity = 0.7,
    title = poplab,
    position = "topright"
) %>%
addLegend(
    pal = gdppal,
    values = ~gdp,
    opacity = 0.7,
    title = gdplab,
    position = "topright",
    labFormat = labelFormat(suffix = " [MYR]")
) %>%
htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }"
)

# Citation for the shape data

# International Steering Committee for Global Mapping. Malaysia.
# Jabatan Ukur dan Pemetaan. (2012). Districts, Malaysia, 2013.
# [Shapefile]. International Steering Committee for Global Mapping.
# Retrieved from https://earthworks.stanford.edu/catalog/stanford-zd362bc5680