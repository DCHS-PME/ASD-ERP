# Create a map of VSHSL providers and opportunity index
# Max Griswold
# 6/29/2026

library(tidyverse)
library(sf)
library(ggplot2)
library(tidygeocoder)
library(openxlsx)

coo_index <- read.xlsx("F:/ASD/2025 Annual Report/data files/Mapping our reach/COOComposite_zipcode.xlsx")

regen_geo <- T

# Investigate COO index data

# Hold onto final percs for plotting later
coo_index_perc <- coo_index |>
                    filter(indicator == "sum8") |>
                    select(zipcode, rank)

# Calculate normalized values within each indicator to preserve magnitude,
# then create domain-specific hierarchies to ensure health doesn't dominate the index.
# To do so, take average within each domain, the renormalize the domain scores, then sum.
# Following OECD guidelines on index construction: https://www.oecd.org/content/dam/oecd/en/publications/reports/2008/08/handbook-on-constructing-composite-indicators-methodology-and-user-guide_g1gh9301/9789264043466-en.pdf

normalize <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

coo_index <- coo_index |>
             mutate(norm_score = normalize(result), .by = "indicator")

# Create domain variables
health_dom <- c("csmoking", "diabetes", "fmd", "ui1864", "le0", "obesity")

coo_index <- coo_index |>
             mutate(domain = case_match(indicator,
                "hprob" ~ "housing",
                "pov200" ~ "income",
                c(health_dom) ~ "health"
             ))

# Calculate domain averages and normalize within each domain. Then calculate a 
# a composite score as sum across domains
coo_index <- coo_index |>
             filter(!is.na(domain)) |>
             mutate(domain_avg = mean(norm_score, na.rm = T), .by = c("domain", "zipcode")) |>
             select(zipcode, domain, domain_avg) |>
             distinct() |>
             mutate(norm_domain = normalize(domain_avg), .by = "domain")

# Sum normalized scores across domains to create a composite index
coo_index_sum <- coo_index |>
                summarise(norm_index = 1 - mean(norm_domain, na.rm = TRUE), .by = "zipcode") |>
                left_join(coo_index_perc, by = "zipcode") |>
                mutate(perc_index = normalize(rank)) |>
                mutate(rank_perc = rank(perc_index),
                       rank_norm = rank(norm_index))
                
ggplot(coo_index_sum, aes(y = rank_perc, x = rank_norm, label = zipcode)) +
    geom_text() +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
    theme_minimal()
