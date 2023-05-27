library(ggplot2)
library(sf)

## Import the geo-data (from the materials directory) that is needed to produce the map
load("material/map_files.RData")

## Produce a map of Potsdam (as ggplot object)
map = ggplot() +
  # roads and streets
  geom_sf(data = roads, color = "grey", alpha = 0.5) + 
  # railways (trains, trams, etc.)
  geom_sf(data = rail, color = "#A66A02", alpha = 0.3, linewidth = 1.0) +
  # water
  geom_sf(data = water, fill = "#009EE0", alpha = 0.6) + 
  # waterways
  geom_sf(data = waterways, color = "blue", size = 2.5, alpha = 0.2) +
  theme_bw() + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Nextbike-Stations in Potsdam",
    x = "Longitude", 
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
