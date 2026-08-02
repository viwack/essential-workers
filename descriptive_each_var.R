# 5 figure descriptive characteristics for each variable by EWRKR_PROP quintile
# Created 9/2025

# ── Libraries ─────────────────────────────────────────────────────────────────
library(dplyr)
library(readxl)

# ── Initial data loads ────────────────────────────────────────────────────────

# Load RUCA codes from TSV and export a CSV copy for convenience
ruca_codes <- read.delim("ruca_codes.tsv", sep = "\t")
parks <- read.delim("38586-0003-Data.tsv", sep = "\t", colClasses = c("TRACT_FIPS20" = "character"))
write.csv(ruca_codes, "ruca_codes.csv", row.names = FALSE)

# Filter the already-loaded pollution_sites object to 2020 observations only
pollution_sites
pollution_sites_20 <- pollution_sites %>%
  filter(YEAR == 2020)

# Reload RUCA codes from the official 2020-tract Excel file (supersedes the TSV above)
# colClasses = c("TractFIPS20" = "character"),
ruca_codes <- read_xlsx("RUCA-codes-2020-tract.xlsx", sheet = "RUCA2020 Tract Data")

# Collapse the detailed secondary RUCA codes into 4 urbanicity categories:
#   1 = Metropolitan / urban commuting areas
#   2 = Micropolitan / small-city commuting areas
#   3 = Small town / rural commuting areas
#   4 = Rural / isolated areas
ruca_codes <- ruca_codes %>%
  mutate(RUCA4 = case_when(
    SecondaryRUCA %in% c(1, 1.1, 2, 2.1, 3, 4.1, 5.1, 7.1, 8.1, 10.1) ~ 1,
    SecondaryRUCA %in% c(4, 5, 6, 6.1)                                  ~ 2,
    SecondaryRUCA %in% c(7, 7.2, 8, 8.2, 9)                             ~ 3,
    SecondaryRUCA %in% c(10, 10.2, 10.3)                                 ~ 4
  ))

# Load healthcare services and restrict to 2020
healthcare_services <- read.csv("healthcare_services.csv", colClasses = c("tract_fips20" = "character"))

healthcare_services_20 <- healthcare_services %>%
  filter(year == 2020)

# ── Main dataset: essential-worker quintiles ──────────────────────────────────

# Load the combined tract-level analytic dataset
df <- read.csv("everything.csv", colClasses = c("TRACT_FIPS20" = "character"))

# Assign each tract to one of 5 quintiles based on essential-worker proportion
df_quintiles <- df %>%
  mutate(EWRKR_quintile = ntile(EWRKR_PROP, 5))

df <- df_quintiles

# Drop tracts missing essential-worker proportion (no quintile can be assigned)
df_nona <- df %>%
  filter(!is.na(EWRKR_PROP))

# Quick check: mean EWRKR_PROP in the top quintile
df_nona %>%
  filter(EWRKR_quintile == 5) %>%
  summarize(mean(EWRKR_PROP, na.rm =TRUE))

# Mean median family income by quintile (initial sanity check)
df_nona %>%
  group_by(EWRKR_quintile) %>%
  summarize(median = mean(MEDFAMINC16_20, na.rm = FALSE))

# ── Join supplemental datasets ────────────────────────────────────────────────

# First RUCA join attempt (note: overwritten below with corrected key)
df_plusrucacodes <- df_nona %>%
  left_join(ruca_codes, by = c("TRACT_FIPS20" = "TractFIPS20"))

# Load food-environment datasets, each filtered to 2020
groceries <- read.csv("nanda_grocery_Tract20_1990-2021_01P.csv", colClasses = c("tract_fips20" = "character")) %>%
  filter(year == 2020)

dollarstores <- read.csv("dollar_stores.csv", colClasses = c("tract_fips20" = "character")) %>%
  filter(year == 2020)

cvstores <- read.csv("convenience_stores.csv", colClasses = c("tract_fips20" = "character")) %>%
  filter(year == 2020)

# Join parks data onto the main tract data (overwrites the raw parks object loaded above)
parks <- df_nona %>%
  left_join(parks, by = "TRACT_FIPS20")

# Build domain-specific merged datasets for each supplemental source
df_plusconveniencestores <- df_nona %>%
  left_join(cvstores, by = c("TRACT_FIPS20" = "tract_fips20"))

df_plusdollarstores <- df_nona %>%
  left_join(conveniencestores, by = c("TRACT_FIPS20" = "tract_fips20"))

df_plusgroceries <- df_nona %>%
  left_join(groceries, by = c("TRACT_FIPS20" = "tract_fips20"))

# Add pollution sites onto the groceries-joined dataset
df_pluspollution <- df_plusgroceries %>%
  left_join(pollution_sites_20, by = "TRACT_FIPS20")

df_plushealthservices <- df_nona %>%
  left_join(healthcare_services_20, by = c("TRACT_FIPS20" = "tract_fips20"))

# Re-join RUCA codes with the corrected key column name (overwrites first attempt above)
df_plusrucacodes <- df_nona %>%
  left_join(ruca_codes, by = "TRACT_FIPS20")


# ── Initial statistical tests ─────────────────────────────────────────────────

install.packages("clinfun")
library(clinfun)

# t-test comparing total food store density between the lowest and highest quintiles
t.test(den_totalfoodstores ~ EWRKR_quintile, 
       data = df_plusgroceries %>% filter(EWRKR_quintile %in% c(1, 5)))

# Spearman correlation: urbanicity (RUCA4) vs. essential-worker quintile
cor.test(df_plusrucacodes$RUCA4, df_plusrucacodes$EWRKR_quintile, 
         method = "spearman", 
         exact = FALSE)

# ── Descriptive plots ─────────────────────────────────────────────────────────

# Descriptive statistics (N, mean, SD, median, IQR, min, max) for each outcome variable by essential-worker quintile
descriptive_table <- df_nona %>%
  group_by(EWRKR_quintile) %>%
  summarise(
    across(
      all_of(outcomes),
      list(
        N      = ~sum(!is.na(.)),
        Mean   = ~mean(., na.rm = TRUE),
        SD     = ~sd(., na.rm = TRUE),
        Median = ~median(., na.rm = TRUE),
        IQR    = ~IQR(., na.rm = TRUE),
        Min    = ~min(., na.rm = TRUE),
        Max    = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

descriptive_table

library(ggplot2)

# Mean RUCA4 urbanicity score by essential-worker quintile with 95% CI error bars
df_plusruca %>%
  group_by(EWRKR_quintile) %>%
  summarise(
    mean = mean(RUCA4, na.rm = TRUE),
    se = sd(RUCA4, na.rm = TRUE) / sqrt(n())
  ) %>%
  ggplot(aes(x = factor(EWRKR_quintile), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), 
                width = 0.2) +
  labs(
    x = "Essential Worker Quintile",
    y = "Mean RUCA4 Score by Essential Worker Quintile"
  ) +
  theme_classic()

# Mean total food store density by essential-worker quintile with 95% CI error bars
df_plusgroceries %>%
  group_by(EWRKR_quintile) %>%
  summarise(
    mean = mean(den_totalfoodstores, na.rm = TRUE),
    se = sd(den_totalfoodstores, na.rm = TRUE) / sqrt(n())
  ) %>%
  ggplot(aes(x = factor(EWRKR_quintile), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), 
                width = 0.2) +
  labs(
    x = "Essential Worker Quintile",
    y = "Mean Density of Total Food Stores"
  ) +
  theme_classic()

# Mean median family income (ACS 2016–2020) by essential-worker quintile
df_nona %>%
  group_by(EWRKR_quintile) %>%
  summarise(
    mean = mean(MEDFAMINC16_20, na.rm = TRUE),
    se = sd(MEDFAMINC16_20, na.rm = TRUE) / sqrt(n())
  ) %>%
  ggplot(aes(x = factor(EWRKR_quintile), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), 
                width = 0.2) +
  labs(
    x = "Essential Worker Quintile",
    y = "Mean Median Family Income (2016-2020)"
  ) +
  theme_classic()

# Tabular summary: rounded mean income and formatted 95% CI per quintile
df_nona %>%
  group_by(EWRKR_quintile) %>%
  summarise(
    mean = round(mean(MEDFAMINC16_20, na.rm = TRUE), 0),
    se = round(sd(MEDFAMINC16_20, na.rm = TRUE) / sqrt(n()),0)
  ) %>%
  mutate("95% Confint." = paste0("(", mean - 1.96*se, ", ", mean + 1.96*se, ")"))

# Spearman correlation: TRI toxic release facility count vs. essential-worker quintile
cor.test(df_pluspollution$COUNT_TRI_FACILITIES, df_pluspollution$EWRKR_quintile, 
         method = "spearman", 
         exact = FALSE)

# Mean TRI facility count by essential-worker quintile with 95% CI error bars
df_pluspollution %>%
  group_by(EWRKR_quintile) %>%
  summarise(
    mean = mean(COUNT_TRI_FACILITIES, na.rm = TRUE),
    se = sd(COUNT_TRI_FACILITIES, na.rm = TRUE) / sqrt(n())
  ) %>%
  ggplot(aes(x = factor(EWRKR_quintile), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), 
                width = 0.2) +
  labs(
    x = "Essential Worker Quintile",
    y = "Mean Count TRI Facilities"
  ) +
  theme_classic()

# ── Trend tests ───────────────────────────────────────────────────────────────


library(trend)

# Mann-Kendall trend test for urbanicity (RUCA4) ordered by essential-worker quintile
mk.test(df_plusrucacodes$RUCA4[order(df_plusrucacodes$EWRKR_quintile)], continuity = TRUE)

# t-test comparing RUCA4 between quintiles 4 and 5
df_plusrucacodes %>%
  select(EWRKR_quintile, RUCA4) %>%
  filter(EWRKR_quintile %in% c(4, 5)) %>%
  t.test(RUCA4 ~ EWRKR_quintile, data = .)


# Mann-Kendall trend test for food store density ordered by quintile (two equivalent formulations)
mk.test(df_plusgroceries$den_totalfoodstores[order(df_plusgroceries$EWRKR_quintile)], continuity = TRUE)
df_plusgroceries %>%
  filter(!is.na(den_totalfoodstores)) %>%
  arrange(EWRKR_quintile) %>%
  pull(den_totalfoodstores) %>%
  mk.test(continuity = TRUE)

# t-test comparing food store density between quintiles 4 and 5
df_plusgroceries %>%
  select(EWRKR_quintile, den_totalfoodstores) %>%
  filter(EWRKR_quintile %in% c(4, 5)) %>%
  t.test(den_totalfoodstores ~ EWRKR_quintile, data = .)

# Mann-Kendall trend test for park area proportion ordered by essential-worker quintile
parks %>%
  filter(!is.na(PROP_PARK_AREA_TRACT)) %>%
  arrange(EWRKR_quintile) %>%
  pull(PROP_PARK_AREA_TRACT) %>%
  mk.test(continuity = TRUE)

# Pairwise t-tests comparing park area between each pair of adjacent quintiles
parks %>%
  select(EWRKR_quintile, PROP_PARK_AREA_TRACT) %>%
  filter(EWRKR_quintile %in% c(1, 2)) %>%
  t.test(PROP_PARK_AREA_TRACT ~ EWRKR_quintile, data = .)
parks %>%
  select(EWRKR_quintile, PROP_PARK_AREA_TRACT) %>%
  filter(EWRKR_quintile %in% c(2, 3)) %>%
  t.test(PROP_PARK_AREA_TRACT ~ EWRKR_quintile, data = .)
parks %>%
  select(EWRKR_quintile, PROP_PARK_AREA_TRACT) %>%
  filter(EWRKR_quintile %in% c(3, 4)) %>%
  t.test(PROP_PARK_AREA_TRACT ~ EWRKR_quintile, data = .)
parks %>%
  select(EWRKR_quintile, PROP_PARK_AREA_TRACT) %>%
  filter(EWRKR_quintile %in% c(4, 5)) %>%
  t.test(PROP_PARK_AREA_TRACT ~ EWRKR_quintile, data = .)


# ── Metro vs. non-metro stratified analysis ───────────────────────────────────

# Outcome variables to summarize and test across quintiles
outcomes <- c(
  "COUNT_TRI_FACILITIES",
  "den_totalfoodstores",
  "den_allphysicians",
  "den_mentalhealthphys",
  "den_mentalhealthpractitioners",
  "den_pharmacies",
  "den_allrescarefacilities",
  "den_conveniencestores",
  "den_dollarstores",
  "PROP_PARK_AREA_TRACT"
)

library(purrr)

# Split tracts into metropolitan (RUCA4 == 1) and non-metropolitan subsets
metropolitan_regions <- df_plusrucacodes %>%
  filter(RUCA4 == 1)
nonmetro_df <- df_plusrucacodes %>%
  filter(RUCA4 != 1)

# Join all supplemental datasets onto the metropolitan subset
ruca1 <- metropolitan_regions %>% 
  left_join(cvstores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(dollarstores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(groceries, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(healthcare_services_20, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(parks, by = "TRACT_FIPS20") %>%
  left_join(pollution_sites_20, by = "TRACT_FIPS20")

# Join all supplemental datasets onto the non-metropolitan subset
nonmetro <- nonmetro_df %>% 
  left_join(cvstores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(dollarstores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(groceries, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(healthcare_services_20, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(parks, by = "TRACT_FIPS20") %>%
  left_join(pollution_sites_20, by = "TRACT_FIPS20")

# Summarize physician density by quintile within metropolitan tracts (mean, SD, SE, 95% CI)
metro_summary <- ruca1 %>%
  group_by(EWRKR_quintile.x) %>%
  summarise(
    mean_physician = mean(den_allphysicians, na.rm = TRUE),
    sd_physician = sd(den_allphysicians, na.rm = TRUE),
    n = n(),
    se = sd_physician / sqrt(n),
    ci_lower = mean_physician - 1.96 * se,
    ci_upper = mean_physician + 1.96 * se
  )

metro_summary

# Plot physician density by essential-worker quintile in metropolitan tracts
ggplot(
  metro_summary,
  aes(x = EWRKR_quintile.x, y = mean_physician)
) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    width = 0.2
  ) +
  labs(
    x = "Essential Worker Quintile",
    y = "Mean Physician Density",
    title = "Physician Density by Essential Worker Quintile, RUCA4 = 1",
    caption = "Error bars represent 95% confidence intervals."
  ) +
  theme_minimal()


# t-test comparing park area between quintiles 1 and 5 in metropolitan tracts
t.test(PROP_PARK_AREA_TRACT ~ EWRKR_quintile.x, 
       data = ruca1 %>% filter(EWRKR_quintile.x %in% c(1, 5)))


# Summary table of means and SDs for all outcomes in Q1 and Q5 (metropolitan tracts)
summary_table <- ruca1 %>%
  filter(EWRKR_quintile.x %in% c(1,5)) %>%
  group_by(EWRKR_quintile.x) %>%
  summarise(
    across(
      all_of(outcomes),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE)
      )
    )
  )

summary_table

# Loop over all outcomes, running a Welch t-test (Q1 vs Q5) in metropolitan tracts.
# Returns a tidy table with group means, SDs, mean difference, t-statistic, df, and p-value.
ttest_table <- map_dfr(outcomes, function(var) {
  
  # Means and SDs for Q1 and Q5
  summary_stats <- ruca1 %>%
    filter(EWRKR_quintile.x %in% c(1, 5)) %>%
    group_by(EWRKR_quintile.x) %>%
    summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      sd   = sd(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Welch t-test
  test <- t.test(
    reformulate("EWRKR_quintile.x", response = var),
    data = ruca1 %>% filter(EWRKR_quintile.x %in% c(1, 5))
  )
  
  tibble(
    Outcome = var,
    Q1_Mean = summary_stats$mean[summary_stats$EWRKR_quintile.x == 1],
    Q1_SD   = summary_stats$sd[summary_stats$EWRKR_quintile.x == 1],
    Q5_Mean = summary_stats$mean[summary_stats$EWRKR_quintile.x == 5],
    Q5_SD   = summary_stats$sd[summary_stats$EWRKR_quintile.x == 5],
    Mean_Difference = summary_stats$mean[summary_stats$EWRKR_quintile.x == 1] -
      summary_stats$mean[summary_stats$EWRKR_quintile.x == 5],
    t = unname(test$statistic),
    df = unname(test$parameter),
    p_value = test$p.value
  )
})

ttest_table
