# Create employment index
# Max Griswold
# 7/6/2026

rm(list = ls())

library(tidyverse)
library(PMEtools)
library(sf)
library(ggplot2)
library(tmap)
library(htmlwidgets)

source("./code/pme_colors.R")

# Prevent tidyverse from printing garbage with read_csv
options(readr.show_col_types = FALSE)

# Load prepped datasets and shapefiles

df_acs_zip       <- read_csv("./data/census_zip_vars.csv")
df_acs_bg        <- read_csv("./data/census_block_vars.csv")
df_acs_tract     <- read_csv("./data/census_tract_vars.csv")

df_211       <- read_csv("./data/wa211_categories_2025-01-01_to_2025-12-31.csv")
df_providers <- read_csv("F:/ASD/Data Requests/General or Other Requests/provider_locations_2025_ASD_geocoded.csv")

sf_zips <- get_geo_sf("zip5")
sf_bg   <- get_geo_sf("block") |>
                mutate(geoid20 = substr(geoid20, 1, 12)) |>
                group_by(geoid20) |>
                summarise(.groups = "drop")

########################################################################################
#
# *Zip code level analysis*: Predicted 211 calls compared to observed calls
# 
# Zip-level negative binomial regression of observed 211 calls on the employment
# index (population offset) gives expected calls per zip. Applying that fitted
# equation to the block-group index yields predicted calls at finer scale.
# Summing block-group predictions back to the zip lets us compare actual:predicted
# calls, and split each block group's predicted need into:
#
#   allocated   = (predicted_bg / predicted_zip) * actual_zip   # need met by calls
#   unallocated = predicted_bg - allocated                      # need not met
#
# so predicted_bg = allocated + unallocated. Negative unallocated = over-served.
# Note: the under/over sign is a zip-level signal shared by all block groups in a
# zip; this concentrates a zip's unmet need where the index is highest, but cannot
# distinguish under- from well-served block groups within the same zip. Magnitude will 
# be proportional to the index, however.
#
########################################################################################

# Rename variables to permit merges
df_acs_zip <- df_acs_zip |> mutate(zipcode = as.character(geoid))
sf_zips <- left_join(sf_zips, df_acs_zip, by = "zipcode")

# For 211: Restrict to Employment calls only
df_211 <- df_211 |> 
            filter(category == "Employment & Income") |>
            mutate(zipcode = as.character(zip))

sf_zips <- left_join(sf_zips, df_211, by = "zipcode")

# Calculate call rate (calls per 1,000 residents)
sf_zips <- sf_zips |>
            mutate(calls_1k = (count / total_population) * 1000)

# Remove zip codes with no population:
sf_zips <- sf_zips |>
            filter(total_population > 0)

# Remove zip codes that are only used for PO boxes (98050, 98288, 98068, 98224)
# and for the UW campus (98195 - which is pop zero: only group quarters there!): 

df_index <- sf_zips |>
            filter(zip_type != "PO Box" & zipcode != "98195")

# Calculate standardized values for index measures, then take 
# the mean and normalize for index score

norm <- function(x) {
    (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) 
}

z_score <- function(x){
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

scaled_z <- function(x){
    x/max(abs(x))
}

# Note components
index_components <- c("percent_unemployed", "percent_poverty_150", "severe_housing_burden", "median_income")

calc_index <- function(dd, index_vars = index_components){

    # I'm being a little goofy about the implementation here but
    # given difficulty to quickly refactor, I'm calculating z-scores
    # for each variable in real time, then creating index and
    # appending them on.

    # In the future, I would like to create z-score for each variable
    # and return them directly. 

    ind <- dd |>
            mutate(across(!!(index_vars), z_score)) |>
            mutate(median_income = -1 * median_income) |>
            transmute(employment_index_z = rowMeans(across(!!(index_vars))),
                    employment_index = scaled_z(employment_index_z))

    dd <- cbind(dd, ind)

    return(dd)

}

# Calculate z-scores for each index component, invert the interpretation for median income,
# and then take the mean to create an overall employment index score. Normalize the index
# to make accessible for interpretation (0-1; high values = more need).

df_index <- calc_index(df_index)

# Hold onto min and max values used in the normalization for later use in block-group index calculation.

min_zip <- min(df_index$employment_index_z, na.rm = T)
max_zip <- max(df_index$employment_index_z, na.rm = T)

# Where are calls to 211 not met by relative need? Calculate predicted calls based off the 
# employment index and compare to actual calls based on residuals.

model_nb <- MASS::glm.nb(count ~ employment_index + offset(log(total_population)), data = df_index)

# Based on the dispersion statistic, the negative binomial model seems appropriate for this data. 
# Note that including PO boxes leads to a much larger dispersion statistic! These certainly should not
# be included.

dispersion <- sum(residuals(model_nb, type = "pearson")^2) / model_nb$df.residual

###############################################################################################
# Block group index: Main analysis
# 
# One wrinkle here. About 2.5% of block groups are missing ACS data for median income
# In these instances, I impute using tract-level values. If developed further, MICE might
# be reasonable here (or gerbil. . .)
#
###############################################################################################

# Remove block groups without population

df_acs_bg <- df_acs_bg |>
             filter(total_population > 0)

# Where is missingness occuring?

value_vars <- c("total_population", "percent_unemployed", "percent_poverty_150", "severe_housing_burden", "median_income")

missingness <- df_acs_bg |>
                    pivot_longer(!!(value_vars), names_to = "variable") |>
                    summarise(missing_count = sum(is.na(value)), missing_percent = mean(is.na(value)), .by = variable) |>
                    arrange(desc(missing_percent))

# Fill missing values with parent tract.
# Note: I'm being a little tricky to get tract  by using integer division
# to obtain tract geoid, slicing off the last digit from the block-group

df_acs_tract <- df_acs_tract |>
                    pivot_longer(all_of(value_vars), names_to = "variable", values_to = "tract_value") |>
                    select(geoid, variable, tract_value)

df_acs_bg <- df_acs_bg |>
                pivot_longer(!!(value_vars), names_to = "variable") |>
                mutate(geoid_tract = geoid %/% 10) |>
                left_join(df_acs_tract, by = c("geoid_tract" = "geoid", "variable")) |>
                mutate(value = coalesce(value, tract_value)) |>
                select(geoid, location, variable, value) |>
                pivot_wider(names_from = variable, values_from = value)

df_index_block <- calc_index(df_acs_bg)

# Round everything to make it look more interpretable on the map:
round_cols <- c("employment_index", index_components)

df_index_block <- df_index_block |>
                    mutate(across(!!round_cols, \(x) round(x, digits = 2)))

# Merge on shapefile
df_index_block <- df_index_block |> 
                    mutate(geoid20 = as.character(geoid))
                    
sf_index <- left_join(sf_bg, df_index_block) |>
            st_transform(crs = 4326)            

# Plot figures
sf_providers <- st_as_sf(df_providers, 
                         coords = c("lon", "lat"), 
                         crs = 4326) |>
                filter(location_type == "Services" & mode_in_office == T)

static_map <- ggplot() +
                geom_sf(data = sf_index, aes(fill = employment_index), alpha = 0.6) +
                geom_sf(data = sf_providers) +
                coord_sf(xlim = c(-122.6, -121),
                        ylim = c(47.05, 47.8)) +
                theme_void()

tmap_mode("view")

interactive_map <- 
    tm_shape(sf_index) +
    tm_polygons(
        fill = "employment_index", 
        fill.scale = tm_scale_intervals(
            values = load_diverging_colors("blue_pink"),
            breaks = c(-1, -0.5, -0.25, -0.01, 0.01, 0.25, 0.5, 1),
            value.na = load_qual_colors('background')[[1]],
            midpoint = 0),
        fill_alpha = 0.7,
        fill.legend = tm_legend(
            title = "Employment Index",
            label.style = 'discrete'
        ),
        
        id = 'employment_index',
        
        popup = tm_popup(vars = c("employment_index", "percent_unemployed", 
                                  "percent_poverty_150", "severe_housing_burden", 
                                  "median_income"))
     ) +
    tm_shape(sf_providers) +
    tm_dots(
        fill = load_qual_colors('primary')[[5]],
        size = 0.7, 

        id = "provider_name",
            
        popup = tm_popup(vars = c("program_name", "strategy"))
    ) 

leaflet_map <- tmap_leaflet(interactive_map)

saveWidget(
  widget = leaflet_map, 
  file = "./figs/prototype_erp_map.html", 
  selfcontained = TRUE
)
