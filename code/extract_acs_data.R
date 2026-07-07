# Extract ACS data for King County geographies and write to CSV files.
# Max Griswold
# 7/6/2026

# Note: You will need to register your census API key with tidycensus to run this script. 
#See https://walker-data.com/tidycensus/articles/basic-usage.html for instructions.

rm(list = ls())

library(tidycensus)
library(tidyverse)
library(PMEtools)

# Note: Extraction is only really valid for 2020+, given changes in geographies between 2010 and 2020. 
# If earlier data is desired, implement the geographical crosswalks from NHGIS.

extract_census <- function(y, geo) {

  var_list <- tribble(

    ~variable_name,                      ~variable_id,
    "total_population",                  "B01003_001",
    "labor_force",                       "B23025_003",
    "number_unemployed",                 "B23025_005",
    "median_income",                     "B19013_001",

    # Poverty (needed to calculate % below 1.5 FPL)
    "ratio_income_poverty_total",        "C17002_001",
    "number_income_poverty_below_0.49",  "C17002_002",
    "number_income_poverty_0.5_to_0.99", "C17002_003",
    "number_income_poverty_1_to_1.24",   "C17002_004",
    "number_income_poverty_1.25_to_1.49","C17002_005",

    # Severe housing cost burden (>= 50% of household income on housing),
    # among all occupied households regardless of tenure. Owners come from
    # B25091 (with/without mortgage); renters from B25070.
    "households_total",                  "B25003_001",
    "renter_burden_50plus",              "B25070_010",
    "owner_burden_50plus_mortgage",      "B25091_011",
    "owner_burden_50plus_no_mortgage",   "B25091_022"

  )
 
    if (geo %in% c("block group", "tract")){

        raw <- get_acs(geography = geo,
            year      = y,
            variables = var_list$variable_id,
            geometry  = FALSE,
            survey    = "acs5",
            state = "WA",
            county = "King",
            cache_table = TRUE)

    }else{
        raw <- get_acs(geography = geo,
                    year      = y,
                    variables = var_list$variable_id,
                    geometry  = FALSE,
                    survey    = "acs5",
                    cache_table = TRUE)
    }



  # Label variables and normalise columns.
  census_vars <- raw |>

    # For ZCTAs, keep only the trailing 5-digit code.

    mutate(GEOID = if (geo == "zcta") str_sub(GEOID, -5L) else GEOID) |>
    transmute(
      geoid           = GEOID,
      location        = NAME,
      variable_id     = variable,
      mean            = as.numeric(estimate),
      margin_of_error = as.numeric(moe)
    ) |>
    left_join(var_list, by = "variable_id")
 
  # Severe housing cost burden using the HUD CHAS severe threshold (>=50% of
  # income on housing), expressed as a share of ALL occupied households so the
  # indicator reflects the prevalence of housing-cost distress in a place
  # rather than a conditional rate among renters only.

  burden_vars <- census_vars |>
    filter(str_detect(variable_name, "burden|households_total")) |>
    select(geoid, variable_name, mean) |>
    pivot_wider(
      names_from  = variable_name,
      values_from = mean
    ) |>
    mutate(
      severe_housing_burden = (renter_burden_50plus +
                                 owner_burden_50plus_mortgage +
                                 owner_burden_50plus_no_mortgage) / households_total
    ) |>
    select(geoid, severe_housing_burden)
 
  # Everything else -> wide, with derived poverty share and rescaled income.

  census_summary <- census_vars |>
    filter(!str_detect(variable_name, "burden|households_total")) |>
    select(geoid, location, variable_name, mean) |>
    pivot_wider(
      names_from  = variable_name,
      values_from = mean
    ) |>
    mutate(
      percent_poverty_150 = (number_income_poverty_below_0.49 +
                               number_income_poverty_0.5_to_0.99 +
                               number_income_poverty_1_to_1.24 +
                               number_income_poverty_1.25_to_1.49) /
        ratio_income_poverty_total,
      median_income = median_income / 1e4,
      percent_unemployed = number_unemployed / labor_force
    )
 
  # Merge components and stamp the year.
  census_summary |>
    left_join(burden_vars, by = "geoid") |>
    mutate(year = y) |>
    select(geoid, location, total_population, percent_unemployed, percent_poverty_150, severe_housing_burden, median_income)

}

years <- 2024

# Modify the location name for census places to better match the PME Tools configuration
# Make sure to trim whitespace and the state info. Note: This does remove Town and Country CDP
# in Spokane but it doesn't impact KC cities!
census_place_vars  <- map_dfr(years, extract_census, geo = "place") |>
                      filter(grepl(", Washington", location) & !grepl("CDP", location)) |>
                      mutate(location = str_remove_all(location, "(?i)\\s*\\b(city|town|\\,).*"))

census_block_vars  <- map_dfr(years, extract_census, geo = "block group")
census_zip_vars    <- map_dfr(years, extract_census, geo = "zcta")

# Tract estimates support simple imputation of suppressed block-group values:
# a block group's parent tract is the first 11 characters of its geoid.
# The get_acs call is already limited to King County for tracts.

census_tract_vars  <- map_dfr(years, extract_census, geo = "tract")

kc_place <- get_geo_sf("city")

# Get block groups, not blocks
kc_block_groups <- get_geo_sf("block") |>
                   mutate(geoid20 = substr(geoid20, 1, 12)) |>
                   group_by(geoid20) |>
                   summarise(.groups = "drop")

kc_zips  <- get_geo_sf("zip5")

# Subset census variables to the KC-specific geographies:.groups

census_place_vars <- census_place_vars |>
                      filter(location %in% kc_place$cityname)

census_block_vars <- census_block_vars |>
                      filter(geoid %in% kc_block_groups$geoid20)

census_zip_vars <- census_zip_vars |>
                    filter(geoid %in% kc_zips$zipcode)

write.csv(census_place_vars, "./data/census_place_vars.csv", row.names = F)
write.csv(census_block_vars, "./data/census_block_vars.csv", row.names = F)
write.csv(census_zip_vars, "./data/census_zip_vars.csv", row.names = F)
write.csv(census_tract_vars, "./data/census_tract_vars.csv", row.names = F)
