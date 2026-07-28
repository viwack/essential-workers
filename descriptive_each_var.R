# 5 figure descriptive characteristics for each variable by EWRKR_PROP quintile
# Created 9/2025
library(dplyr)
library(readxl)

ruca_codes <- read.delim("ruca_codes.tsv", sep = "\t")
parks <- read.delim("38586-0003-Data.tsv", sep = "\t", colClasses = c("TRACT_FIPS20" = "character"))
write.csv(ruca_codes, "ruca_codes.csv", row.names = FALSE)

pollution_sites
pollution_sites_20 <- pollution_sites %>%
  filter(YEAR == 2020)
# colClasses = c("TractFIPS20" = "character"),
ruca_codes <- read_xlsx("RUCA-codes-2020-tract.xlsx", sheet = "RUCA2020 Tract Data")

ruca_codes <- ruca_codes %>%
  mutate(RUCA4 = case_when(
    SecondaryRUCA %in% c(1, 1.1, 2, 2.1, 3, 4.1, 5.1, 7.1, 8.1, 10.1) ~ 1,
    SecondaryRUCA %in% c(4, 5, 6, 6.1)                                  ~ 2,
    SecondaryRUCA %in% c(7, 7.2, 8, 8.2, 9)                             ~ 3,
    SecondaryRUCA %in% c(10, 10.2, 10.3)                                 ~ 4
  ))

healthcare_services <- read.csv("healthcare_services.csv", colClasses = c("tract_fips20" = "character"))

healthcare_services_20 <- healthcare_services %>%
  filter(year == 2020)

df <- read.csv("everything.csv", colClasses = c("TRACT_FIPS20" = "character"))
df_quintiles <- df %>%
  mutate(EWRKR_quintile = ntile(EWRKR_PROP, 5))

df <- df_quintiles

df_nona <- df %>%
  filter(!is.na(EWRKR_PROP))

df_nona %>%
  filter(EWRKR_quintile == 5) %>%
  summarize(mean(EWRKR_PROP, na.rm =TRUE))

df_nona %>%
  group_by(EWRKR_quintile) %>%
  summarize(median = mean(MEDFAMINC16_20, na.rm = FALSE))

df_plusrucacodes <- df_nona %>%
  left_join(ruca_codes, by = c("TRACT_FIPS20" = "TractFIPS20"))

groceries <- read.csv("nanda_grocery_Tract20_1990-2021_01P.csv", colClasses = c("tract_fips20" = "character")) %>%
  filter(year == 2020)

dollarstores <- read.csv("dollar_stores.csv", colClasses = c("tract_fips20" = "character")) %>%
  filter(year == 2020)

cvstores <- read.csv("convenience_stores.csv", colClasses = c("tract_fips20" = "character")) %>%
  filter(year == 2020)
parks <- df_nona %>%
  left_join(parks, by = "TRACT_FIPS20")
df_plusconveniencestores <- df_nona %>%
  left_join(cvstores, by = c("TRACT_FIPS20" = "tract_fips20"))

df_plusdollarstores <- df_nona %>%
  left_join(conveniencestores, by = c("TRACT_FIPS20" = "tract_fips20"))

df_plusgroceries <- df_nona %>%
  left_join(groceries, by = c("TRACT_FIPS20" = "tract_fips20"))

df_pluspollution <- df_plusgroceries %>%
  left_join(pollution_sites_20, by = "TRACT_FIPS20")

df_plushealthservices <- df_nona %>%
  left_join(healthcare_services_20, by = c("TRACT_FIPS20" = "tract_fips20"))

df_plusrucacodes <- df_nona %>%
  left_join(ruca_codes, by = "TRACT_FIPS20")


install.packages("clinfun")
library(clinfun)
t.test(den_totalfoodstores ~ EWRKR_quintile, 
       data = df_plusgroceries %>% filter(EWRKR_quintile %in% c(1, 5)))

cor.test(df_plusrucacodes$RUCA4, df_plusrucacodes$EWRKR_quintile, 
         method = "spearman", 
         exact = FALSE)

library(ggplot2)
df_plusrucacodes %>%
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

df_nona %>%
  group_by(EWRKR_quintile) %>%
  summarise(
    mean = round(mean(MEDFAMINC16_20, na.rm = TRUE), 0),
    se = round(sd(MEDFAMINC16_20, na.rm = TRUE) / sqrt(n()),0)
  ) %>%
  mutate("95% Confint." = paste0("(", mean - 1.96*se, ", ", mean + 1.96*se, ")"))

cor.test(df_pluspollution$COUNT_TRI_FACILITIES, df_pluspollution$EWRKR_quintile, 
         method = "spearman", 
         exact = FALSE)

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

jt <- jonckheere.test(
  x = df_plusgroceries$den_totalfoodstores,
  g = df_plusgroceries$EWRKR_quintile,
  alternative = "increasing"  # change to "decreasing" if you expect negative trend
)

print(jt)

library(trend)
mk.test(df_plusrucacodes$RUCA4[order(df_plusrucacodes$EWRKR_quintile)], continuity = TRUE)
df_plusrucacodes %>%
  select(EWRKR_quintile, RUCA4) %>%
  filter(EWRKR_quintile %in% c(4, 5)) %>%
  t.test(RUCA4 ~ EWRKR_quintile, data = .)

anova(as.character(df_pluspollution$EWRKR_quintile), df_pluspollution$COUNT_TRI_FACILITIES)



mk.test(df_plusgroceries$den_totalfoodstores[order(df_plusgroceries$EWRKR_quintile)], continuity = TRUE)
df_plusgroceries %>%
  filter(!is.na(den_totalfoodstores)) %>%
  arrange(EWRKR_quintile) %>%
  pull(den_totalfoodstores) %>%
  mk.test(continuity = TRUE)

df_plusgroceries %>%
  select(EWRKR_quintile, den_totalfoodstores) %>%
  filter(EWRKR_quintile %in% c(4, 5)) %>%
  t.test(den_totalfoodstores ~ EWRKR_quintile, data = .)


parks %>%
  filter(!is.na(PROP_PARK_AREA_TRACT)) %>%
  arrange(EWRKR_quintile) %>%
  pull(PROP_PARK_AREA_TRACT) %>%
  mk.test(continuity = TRUE)
  
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


metropolitan_regions <- df_plusrucacodes %>%
  filter(RUCA4 == 1)
nonmetro_df <- df_plusrucacodes %>%
  filter(RUCA4 != 1)

ruca1 <- metropolitan_regions %>% 
  left_join(cvstores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(dollarstores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(groceries, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(healthcare_services_20, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(parks, by = "TRACT_FIPS20") %>%
  left_join(pollution_sites_20, by = "TRACT_FIPS20")

nonmetro <- nonmetro_df %>% 
  left_join(cvstores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(dollarstores, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(groceries, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(healthcare_services_20, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  left_join(parks, by = "TRACT_FIPS20") %>%
  left_join(pollution_sites_20, by = "TRACT_FIPS20")
  
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


t.test(PROP_PARK_AREA_TRACT ~ EWRKR_quintile.x, 
       data = ruca1 %>% filter(EWRKR_quintile.x %in% c(1, 5)))


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
