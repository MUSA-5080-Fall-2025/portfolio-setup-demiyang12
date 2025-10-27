library(tidyverse)
library(dplyr)
library(knitr)
library(tidycensus)
library(sf)
library(tigris)
library(scales)
library(patchwork)
library(here)
library(units)
# data clean

opa <- read_csv("opa_properties_public.csv")
View(opa_properties_public)

opa_clean <- opa %>%
  mutate(sale_date = as.Date(sale_date)) %>%
  filter(sale_date >= "2023-01-01" & sale_date <= "2024-12-31")

names(opa_clean)


opa_selected <- opa_clean %>%
  select(
    sale_date, sale_price, market_value, building_code_description,
    total_livable_area, number_of_bedrooms, number_of_bathrooms, 
    number_stories, garage_spaces, central_air, quality_grade,
    interior_condition, exterior_condition, year_built, 
    zip_code, geographic_ward, census_tract, zoning, category_code_description,
    taxable_building, taxable_land, shape
  )

#view the building_code_description
unique(opa_selected$category_code_description)
opa_selected <- opa_selected %>%
  filter(
    category_code_description %in% c(
      "SINGLE FAMILY"
    )
  )
unique(opa_selected$building_code_description)



opa_selected <- opa_selected %>%
  distinct()


opa_selected <- opa_selected %>%
  filter(
    !is.na(sale_price) & sale_price > 1 & sale_price < 5e6,
    !is.na(total_livable_area) & total_livable_area > 0,
    !is.na(year_built)
  )



opa_selected <- opa_selected %>%
  mutate(price_per_sqft = sale_price / total_livable_area)


barplot(table(opa_clean$market_value),
        main = "Distribution of housing price",
        xlab = "Number of Bedrooms",
        ylab = "Frequency",
        col = "lightblue")

#boxplot
boxplot(opa_selected$price_per_sqft,
        main = "Boxplot of Price per Square Foot",
        ylab = "Price per Square Foot",
        col = "lightgreen")



library(dplyr)
library(sf)
opa_sf <- st_as_sf(opa_selected, 
                   wkt = "shape",     
                   crs = 2272)        

opa_sf <- st_transform(opa_sf, 4326)

print(opa_sf)
plot(st_geometry(opa_sf))

opa_sf$value_gap <- opa_sf$sale_price - opa_sf$market_value
boxplot(opa_sf$value_gap,
        main = "Boxplot of Value Gap",
        ylab = "Value Gap (Sale Price - Market Value)",
        col = "lightcoral")
