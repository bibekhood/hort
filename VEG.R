setwd('D:/OneDrive/Papers/Horticulture in Nepali Economy')
library(ggplot2)
library(tidyverse)
library(readxl)
library(openxlsx)
library(sf)
library(dplyr)
library(RColorBrewer)

#Load Shapes and HEV data
province= st_read ("D:/OneDrive/QGIS/Shape Files/province.shp")
veg = read_excel("data.xlsx", sheet="province_veg")
fruit= read_excel("data.xlsx", sheet="province_fruit")
province_veg = merge(province, veg, by="OBJECTID")
province_fruit = merge(province, fruit, by="OBJECTID")

centroids = st_centroid(province_veg)

# Combine centroids with original data
province_veg = province_veg %>%
  mutate(centroid_x = st_coordinates(centroids)[,1],
         centroid_y = st_coordinates(centroids)[,2])

#Map Veg
#
ggplot(province_veg) +
  geom_sf(aes(fill = as.factor(area), geometry = geometry), color = alpha("black", 0.5)) +
  geom_text(aes(x = centroid_x, y = centroid_y, label = paste0(sprintf("%.0f", area / 1000))), 
            size = 3.5, color = ifelse(province_veg$area>mean(province_veg$area), "white", "black"), check_overlap = TRUE) +
  scale_fill_brewer (palette = "Blues") +
  labs(
    title = "Vegetable Area",
    subtitle = bquote(bold("Total: ") ~ .(format(sum(province_veg$area), big.mark = ",")) ~ "Ha"),
    y = "* 1,000 Ha",
    caption = expression(bold("Year:")~"2021/22"~ bold("")~"")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = "navy", face = "bold", hjust = 0.5, margin = margin(b = 40, unit = "pt")),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0, margin = margin(b = 0)),
    legend.key.height = unit(0.6, "in"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.title.x = element_blank(),
    axis.text = element_text(color = "#e0e0e0"),
    panel.grid = element_line(color = "#e0e0e0", linewidth = 0.01),
    legend.title = element_text(color = "red", size = 14, face = "bold", hjust = -0.1),
    legend.text = element_text(size = 12),
    plot.caption = element_text(color = "black", size = 12, hjust = 1, margin = margin(t = 20)),
    legend.position = "none"
  )

#Veg Production
ggplot(province_veg) +
  geom_sf(aes(fill = as.factor(production), geometry = geometry), color = alpha("black", 0.5)) +
  geom_text(aes(x = centroid_x, y = centroid_y, label = paste0(sprintf("%.0f", production / 10000))), 
            size = 3.5, color = ifelse(province_veg$production>mean(province_veg$production), "white", "black"), check_overlap = TRUE) +
  scale_fill_brewer (palette = "Greens") +
  labs(
    title = "Vegetable Production",
    subtitle = bquote(bold("Total: ") ~ .(format(sum(province_veg$production), big.mark = ",")) ~ "MT"),
    y = "* 10,000 MT",
    caption = expression(bold("")~""~ bold("Source:")~"MOALD 2023")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = "navy", face = "bold", hjust = 0.5, margin = margin(b = 40, unit = "pt")),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0, margin = margin(b = 0)),
    legend.key.height = unit(0.6, "in"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.title.x = element_blank(),
    axis.text = element_text(color = "#e0e0e0"),
    panel.grid = element_line(color = "#e0e0e0", linewidth = 0.01),
    legend.title = element_text(color = "red", size = 14, face = "bold", hjust = -0.1),
    legend.text = element_text(size = 12),
    plot.caption = element_text(color = "black", size = 12, hjust = 0, margin = margin(t = 20)),
    legend.position = "none"
  )

#Map Fruit
centroids = st_centroid(province_fruit)

# Combine centroids with original data
province_fruit = province_fruit %>%
  mutate(centroid_x = st_coordinates(centroids)[,1],
         centroid_y = st_coordinates(centroids)[,2])

#Map Fruit
ggplot(province_fruit) +
  geom_sf(aes(fill = as.factor(area), geometry = geometry), color = alpha("black", 0.5)) +
  geom_text(aes(x = centroid_x, y = centroid_y, label = paste0(sprintf("%.0f", area / 1000))), 
            size = 3.5, color = ifelse(province_fruit$area>mean(province_fruit$area), "white", "black"), check_overlap = TRUE) +
  scale_fill_brewer (palette = "Purples") +
  labs(
    title = "Fruit Area",
    subtitle = bquote(bold("Total: ") ~ .(format(sum(province_fruit$area), big.mark = ",")) ~ "Ha"),
    y = "* 1,000 Ha",
    caption = expression(bold("Year:")~"2021/22"~ bold("")~"")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = "#551A8B", face = "bold", hjust = 0.5, margin = margin(b = 40, unit = "pt")),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0, margin = margin(b = 0)),
    legend.key.height = unit(0.6, "in"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.title.x = element_blank(),
    axis.text = element_text(color = "#e0e0e0"),
    panel.grid = element_line(color = "#e0e0e0", linewidth = 0.01),
    legend.title = element_text(color = "red", size = 14, face = "bold", hjust = -0.1),
    legend.text = element_text(size = 12),
    plot.caption = element_text(color = "black", size = 12, hjust = 1, margin = margin(t = 20)),
    legend.position = "none"
  )

#Fruit Production
ggplot(province_fruit) +
  geom_sf(aes(fill = as.factor(production), geometry = geometry), color = alpha("black", 0.5)) +
  geom_text(aes(x = centroid_x, y = centroid_y, label = paste0(sprintf("%.0f", production / 10000))), 
            size = 3.5, color = ifelse(province_fruit$production>mean(province_fruit$production), "white", "black"), check_overlap = TRUE) +
  scale_fill_brewer (palette = "Oranges") +
  labs(
    title = "Fruit Production",
    subtitle = bquote(bold("Total: ") ~ .(format(sum(province_fruit$production), big.mark = ",")) ~ "MT"),
    y = "* 10,000 MT",
    caption = expression(bold("")~""~ bold("Source:")~"MOALD 2023")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, color = "#551A8B", face = "bold", hjust = 0.5, margin = margin(b = 40, unit = "pt")),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0, margin = margin(b = 0)),
    legend.key.height = unit(0.6, "in"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.title.x = element_blank(),
    axis.text = element_text(color = "#e0e0e0"),
    panel.grid = element_line(color = "#e0e0e0", linewidth = 0.01),
    legend.title = element_text(color = "red", size = 14, face = "bold", hjust = -0.1),
    legend.text = element_text(size = 12),
    plot.caption = element_text(color = "black", size = 12, hjust = 0, margin = margin(t = 20)),
    legend.position = "none"
  )