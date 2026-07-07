library(tidyverse)
library(tidygeocoder)

# Get lat-long for providers

if (regen_geo) {
    provider_locs <- read_csv("F:/ASD/Data Requests/General or Other Requests/provider_locations_2025_ASD.csv")
    lat_long <- geocode(provider_locs, street = "street_address_1", city = "city", state = "state", postalcode = "zip",
                    method = 'arcgis', lat = latitude , long = longitude, full_results = TRUE)
    write.csv(lat_long, "F:/ASD/Data Requests/General or Other Requests/provider_locations_2025_ASD_geocoded.csv", row.names = F)

}else{
    lat_long <- read_csv("F:/ASD/Data Requests/General or Other Requests/provider_locations_2025_ASD_geocoded.csv")
}