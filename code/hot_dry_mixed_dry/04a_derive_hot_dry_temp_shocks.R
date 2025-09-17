library(tidyverse)
library(here)
library(weathermetrics)
library(arrow)


# Import helper ----------------------------------------------------------------
prism_files <-
  list.files(
    here("data", "prism_gee", "parquet"),
    pattern = "hot_dry_mixed_dry",
    full.names = TRUE
  )


# Common functions ------------------------------------------------------------

## Calculate heat indices as needed
get_heat_indices <- function(dat, new_var, temp_var, dewpoint_var, temp_type) {
  ## Calculate heat index using tmean / tdmean.
  dat[[new_var]] <- heat.index(
    t = dat[[temp_var]],
    dp = dat[[dewpoint_var]],
    temperature.metric = temp_type
  )

  return(dat)
}

## Derive Heat shocks
derive_heat_shocks <- function(dat, shock_var_name, temp_var, heat_thresholds) {
  heat_shock_var <- sym(shock_var_name)

  dat %>%
    mutate(
      !!heat_shock_var := if_else(
        {{ temp_var }} > heat_thresholds,
        "Spike",
        "No Spike"
      )
    )
}

## Derive Cold shocks
derive_cold_shocks <- function(dat, shock_var_name, temp_var, cold_thresholds) {
  cold_shock_var <- sym(shock_var_name)

  dat %>%
    mutate(
      !!cold_shock_var := if_else(
        {{ temp_var }} < cold_thresholds,
        "Spike",
        "No Spike"
      )
    )
}

## Put Cold & Heat Shock days back into one daily data frame
## This one was probably stupid: It was so I know individual days looked, but something simpler
## with filter ---> count function would be better / simpler for these purposes
rejoin_spike_dat <- function(map1, map2, call, shock_var, glue_string) {
  shocks_container <-
    map2(
      map1,
      map2,
      ~ call(hd_md_co_prism, glue::glue(glue_string), {{ shock_var }}, .y) %>%
        mutate(row_id = row_number()) %>%
        select(row_id, all_of(glue::glue(glue_string)))
    )

  combined_shocks <- reduce(shocks_container, full_join, by = "row_id")

  spike_dat_fin <-
    hd_md_co_prism %>%
    mutate(row_id = row_number()) %>%
    left_join(combined_shocks, by = c("row_id"))

  return(spike_dat_fin)
}


## ----------------------------------------------------------------------------=
# 1. Get Annual 30 yr sznal average w/ arrow ----------------------------------
## ----------------------------------------------------------------------------=

## 1.1 Open dataset w/ arrow -  one file per year --------
hd_md_prism_tbls_all <- arrow::open_dataset(prism_files)

## 1.2 Seasonal 30-yr average query ----------
tictoc::tic()
hd_md_szn_study_per_avg <-
  hd_md_prism_tbls_all %>%
  mutate(
    date_ymd = as.character(date_ymd),
    date = as.Date(date_ymd, "%Y%m%d"),
    year = year(date),
    month = month(date),
    date_coerce = make_date(year = 2023, month = month(date), day = day(date))
  ) %>%
  mutate(
    astro_season = case_when(
      between(date_coerce, as.Date("2023-01-01"), as.Date("2023-03-19")) ~
        "WINTER",
      between(date_coerce, as.Date("2023-12-21"), as.Date("2023-12-31")) ~
        "WINTER",
      between(date_coerce, as.Date("2023-03-20"), as.Date("2023-06-19")) ~
        "SPRING",
      between(date_coerce, as.Date("2023-06-20"), as.Date("2023-09-21")) ~
        "SUMMER",
      between(date_coerce, as.Date("2023-09-22"), as.Date("2023-12-20")) ~
        "FALL",
      TRUE ~ "WHY"
    )
  ) %>%
  group_by(GEOID10, astro_season, .drop = FALSE) %>%
  summarise(
    pptstudy_per_avg = mean(ppt),
    tmeanstudy_per_avg = mean(tmean),
    tmaxstudy_per_avg = mean(tmax),
    tminstudy_per_avg = mean(tmin),
    tdmeanstudy_per_avg = mean(tdmean),
    .groups = "drop"
  ) %>%
  collect()
tictoc::toc()

## Experiment with subqueries as needed for testing - did a fair amount, seems fine.

## 1.3 Add farenheit columns just in case ------------------------------------

## Helps make across call readable
measures <- c("tmax", "tmean", "tmin", "tdmean")

hd_md_szn_study_per_avg <-
  hd_md_szn_study_per_avg %>%
  mutate(across(
    contains(measures),
    ~ (. * 9 / 5) + 32,
    .names = "{.col}_fahrenheit"
  ))

## 1.4 Get 30 yr average heat indices ----------------------------------
hd_md_szn_study_per_avg <-
  get_heat_indices(
    hd_md_szn_study_per_avg,
    "tmeanstudy_per_heat_index_f",
    "tmeanstudy_per_avg_fahrenheit",
    "tdmeanstudy_per_avg_fahrenheit",
    "fahrenheit"
  )

hd_md_szn_study_per_avg <-
  get_heat_indices(
    hd_md_szn_study_per_avg,
    "tmaxstudy_per_heat_index_f",
    "tmaxstudy_per_avg_fahrenheit",
    "tdmeanstudy_per_avg_fahrenheit",
    "fahrenheit"
  )


## Because there are only six years of daily data requested, we're just going to
## pull into memory w/ arrow and go from there so we can use in-memory r tools

## 1.4 Import w/ arrow | Add season, farenheit, date stuff
hd_md_co_prism <-
  hd_md_prism_tbls_all %>%
  mutate(
    date_ymd = as.character(date_ymd),
    date = as.Date(date_ymd, "%Y%m%d"),
    year = year(date),
    month = month(date),
    date_coerce = make_date(year = 2024, month = month(date), day = day(date))
  ) %>%
  # filter(year %in% 2018:2024) %>%
  mutate(
    astro_season = case_when(
      between(date_coerce, "2024-01-01", "2024-03-19") ~ "WINTER",
      between(date_coerce, "2024-12-21", "2024-12-31") ~ "WINTER",
      between(date_coerce, "2024-03-20", "2024-06-19") ~ "SPRING",
      between(date_coerce, "2024-06-20", "2024-09-21") ~ "SUMMER",
      between(date_coerce, "2024-09-22", "2024-12-20") ~ "FALL",
      TRUE ~ "WHY"
    )
  ) %>%
  mutate(across(
    contains(measures),
    ~ (. * 9 / 5) + 32,
    .names = "{.col}_fahrenheit"
  )) %>%
  collect()

## 1.5 Derive Heat Indices from mean & max -------------------
hd_md_co_prism <-
  get_heat_indices(
    hd_md_co_prism,
    "tmean_heat_index_f",
    "tmean_fahrenheit",
    "tdmean_fahrenheit",
    "fahrenheit"
  )

hd_md_co_prism <-
  get_heat_indices(
    hd_md_co_prism,
    "tmax_heat_index_f",
    "tmax_fahrenheit",
    "tdmean_fahrenheit",
    "fahrenheit"
  )

## We get a handful of missings where mean temp > (usually = dewpoint temp) | dropping, but need to know what to do
hd_md_co_prism %>%
  filter(is.na(tmean_heat_index_f)) %>%
  nrow() ## .08% of obs

hd_md_co_prism <-
  hd_md_co_prism %>%
  filter(!is.na(tmean_heat_index_f))


## 1.6 Derive annual seasonal averages ----

hd_md_ann_szn <-
  hd_md_co_prism %>%
  group_by(GEOID10, year, astro_season, .drop = FALSE) %>%
  summarise(
    across(
      contains(measures),
      list(
        mean = \(x) mean(x)
      ),
      .names = "{.fn}_{.col}"
    ),
    .groups = "drop"
  )

## 1.7 Join annual seasonal temps with long-term trend and subtract stuff
hd_md_ann_szn_fin <-
  hd_md_ann_szn %>%
  left_join(hd_md_szn_study_per_avg, by = c("GEOID10", "astro_season")) %>%
  mutate(
    tmax_anomaly = mean_tmax - tmaxstudy_per_avg,
    tmax_f_anomaly = mean_tmax_fahrenheit - tmaxstudy_per_avg_fahrenheit,
    tmax_hi_f_anomaly = mean_tmax_heat_index_f - tmaxstudy_per_heat_index_f,
    tmean_anomaly = mean_tmean - tmeanstudy_per_avg,
    tmean_f_anomaly = mean_tmean_fahrenheit - tmeanstudy_per_avg_fahrenheit,
    tmean_hi_f_anomaly = mean_tmean_heat_index_f - mean_tmean_heat_index_f,
    tmin_anomaly = mean_tmin - tminstudy_per_avg,
    tmin_f_anomaly = mean_tmin_fahrenheit - tminstudy_per_avg_fahrenheit,
    tdmean_anomaly = mean_tdmean - tdmeanstudy_per_avg,
    tdmean_f_anomaly = mean_tdmean_fahrenheit - tdmeanstudy_per_avg_fahrenheit
  )


## ----------------------------------------------------------------------------=
# 2. 2018 - 2024 - Import & Easy Temp shock ----
## ----------------------------------------------------------------------------=

# 2.1 Derive Heat Shock ---------------------------------------------------------

## Lots of inputs we can use for calculation, and I'm not sure what makes the most sense
## so head bash

## From Heat Index
heat_thresholds <- c(80, 90, 95)
heat_var_names <- c("80_hi_spike", "90_hi_spike", "95_hi_spike")

hd_md_co_prism <-
  rejoin_spike_dat(
    heat_var_names,
    heat_thresholds,
    derive_heat_shocks,
    tmean_heat_index_f,
    "mean_{.x}"
  )
hd_md_co_prism <-
  rejoin_spike_dat(
    heat_var_names,
    heat_thresholds,
    derive_heat_shocks,
    tmax_heat_index_f,
    "max_{.x}"
  )


## From Daily Temp

heat_thresholds <- c(80, 90, 95)
heat_var_names <- c("80_spike", "90_spike", "95_spike")

hd_md_co_prism <-
  rejoin_spike_dat(
    heat_var_names,
    heat_thresholds,
    derive_heat_shocks,
    tmean_fahrenheit,
    "mean_{.x}"
  )
hd_md_co_prism <-
  rejoin_spike_dat(
    heat_var_names,
    heat_thresholds,
    derive_heat_shocks,
    tmax_fahrenheit,
    "max_{.x}"
  )


# 2.4 Derive Cold Shocks -------------------------------------------------------

cold_thresholds <- c(20, 32, 40)
cold_var_names <- c("20_spike", "32_spike", "40_spike")

## Didn't use heat index because min makes more sense ??? here I think -- TALK THROUGH W/ LEAH

hd_md_co_prism <-
  rejoin_spike_dat(
    cold_var_names,
    cold_thresholds,
    derive_cold_shocks,
    tmean_fahrenheit,
    "mean_{.x}"
  )
hd_md_co_prism <-
  rejoin_spike_dat(
    cold_var_names,
    cold_thresholds,
    derive_cold_shocks,
    tmin_fahrenheit,
    "min_{.x}"
  )

## 2.5 Count Up Temperature spikes -------------------------------------------

## We're just going to store in a list for the moment until we need to output differently
spike_groups <- select(hd_md_co_prism, contains("_spike")) %>% names()


spike_counts <-
  map(
    spike_groups,
    ~ hd_md_co_prism %>%
      group_by(year, GEOID10, !!sym(.x), .drop = FALSE) %>%
      summarise(n = n(), .groups = "drop")
  ) %>%
  set_names(spike_groups)

## ---------------------------------------------------------------------------=
# 3. Derive Derivation from 30 year average -----------
## ---------------------------------------------------------------------------=

## Derive seasonal measures for year / zips of interest
hd_md_co_szn_yr <-
  hd_md_co_prism %>%
  group_by(year, astro_season, GEOID10, .drop = FALSE) %>%
  summarise(
    ppt_szn_avg = mean(ppt),
    tmean_szn_avg_fahrenheit = mean(tmean_fahrenheit),
    tmax_szn_avg_fahrenheit = mean(tmax_fahrenheit),
    tmin_szn_avg_fahrenheit = mean(tmin_fahrenheit),
    tdmean_szn_avg_fahrenheit = mean(tdmean_fahrenheit),
    tmean_hi_szn_avg_f = mean(tmean_heat_index_f),
    tmax_hi_szn_avg_f = mean(tmax_heat_index_f),
    .groups = "drop"
  )

## Join with long-term trends | Calc derivation
hd_md_co_szn_yr_derivations <-
  hd_md_co_szn_yr %>%
  left_join(hd_md_szn_study_per_avg, by = c("astro_season", "GEOID10")) %>%
  mutate(
    ppt_szn_diff = ppt_szn_avg - pptstudy_per_avg,
    tmean_szn_diff_fahrenheit = tmean_szn_avg_fahrenheit -
      tmeanstudy_per_avg_fahrenheit,
    tmax_szn_diff_fahrenheit = tmax_szn_avg_fahrenheit -
      tmaxstudy_per_avg_fahrenheit,
    tmin_szn_diff_fahrenheit = tmin_szn_avg_fahrenheit -
      tminstudy_per_avg_fahrenheit,
    tdmean_szn_diff_fahrenheit = tdmean_szn_avg_fahrenheit -
      tdmeanstudy_per_avg_fahrenheit,
    tmean_hi_szn_diff_fahrenheit = tmean_hi_szn_avg_f -
      tmeanstudy_per_heat_index_f,
    tmax_hi_diff_fahrenheit = tmax_hi_szn_avg_f - tmaxstudy_per_heat_index_f
  )


## ---------------------------------------------------------------------------=
# 4. Make Temp cut points -----------
## ---------------------------------------------------------------------------=
hd_md_co_prism <-
  hd_md_co_prism %>%
  mutate(
    tmean_f_ranks = percent_rank(tmean_fahrenheit),
    tdmean_f_ranks = percent_rank(tdmean_fahrenheit),
    tmax_f_ranks = percent_rank(tmax_fahrenheit),
    tmin_f_ranks = percent_rank(tmin_fahrenheit),
    ppt_ranks = percent_rank(ppt),
    tmean_heat_index_f_ranks = percent_rank(tmean_heat_index_f),
    tmax_heat_index_f_ranks = percent_rank(tmax_heat_index_f)
  )


write_csv(hd_md_co_prism, "fin_data/hd_md_county10_heatshocks.csv")
