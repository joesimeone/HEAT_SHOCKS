library(tidyverse)
library(here)
library(weathermetrics)
library(arrow)


# Import helper ----------------------------------------------------------------
prism_files <-
  list.files(here("data", "prism_gee", "parquet"),
             full.names = TRUE)


# Common functions ------------------------------------------------------------

## Calculate heat indices as needed
get_heat_indices <- function(dat, new_var, temp_var, 
                             dewpoint_var, temp_type){
  
  
  ## Calculate heat index using tmean / tdmean. 
  dat[[new_var]] <- heat.index(t = dat[[temp_var]], 
                               dp = dat[[dewpoint_var]],
                               temperature.metric = temp_type)
  
  return(dat)}

## Derive Heat shocks 
derive_heat_shocks <- function(dat,shock_var_name,
                               temp_var, 
                               heat_thresholds){
  
  heat_shock_var <- sym(shock_var_name)
  
  dat %>%
  mutate(
      !!heat_shock_var := 
        if_else({{ temp_var }} > heat_thresholds,
                                  "Spike", "No Spike"))
}

## Derive Cold shocks 
derive_cold_shocks <- function(dat, shock_var_name,
                               temp_var, 
                               cold_thresholds){
  
  cold_shock_var <- sym(shock_var_name)
  
  dat %>%
    mutate(
      !!cold_shock_var := 
        if_else({{ temp_var }} < cold_thresholds,
                                  "Spike", "No Spike"))
}

## Put Cold & Heat Shock days back into one daily data frame
  ## This one was probably stupid: It was so I know individual days looked, but something simpler
  ## with filter ---> count function would be better / simpler for these purposes 
rejoin_spike_dat <- function(map1, map2, call, shock_var, glue_string){
  
  shocks_container <- 
    map2(map1, map2,
         ~call(ore_zcta_prism, glue::glue(glue_string), 
                             {{ shock_var }}, .y) %>% 
           mutate(row_id = row_number()) %>% 
           select(row_id, all_of(glue::glue(glue_string))
           )
    )
  
  combined_shocks <- reduce(shocks_container, 
                            full_join, by = "row_id")
  
  spike_dat_fin <- 
    ore_zcta_prism %>% 
    mutate(row_id = row_number()) %>% 
    left_join(combined_shocks, by = c("row_id"))
  
  return(spike_dat_fin)}


## ----------------------------------------------------------------------------=
# 1. Get Annual 30 yr sznal average w/ arrow ----------------------------------
## ----------------------------------------------------------------------------=


## 1.1 Open dataset w/ arrow -  one file per year --------
ore_prism_tbls_all <- arrow::open_dataset(prism_files)

## 1.2 Seasonal 30-yr average query ----------
tictoc::tic()
ore_szn_30yr_avg <- 
  ore_prism_tbls_all %>% 
  mutate(date_ymd = as.character(date_ymd),
         date = as.Date(date_ymd, "%Y%m%d"),
         year = year(date),
         month = month(date),
         date_coerce = make_date(year = 2024, 
                                 month = month(date), 
                                 day = day(date))) %>% 
  filter(year %in% 1991:2020) %>% 
  mutate(astro_season = 
           case_when(
             between(date_coerce, "2024-01-01", "2024-03-19") ~ "WINTER",
             between(date_coerce, "2024-12-21", "2024-12-31") ~ "WINTER",
             between(date_coerce, "2024-03-20", "2024-06-19") ~ "SPRING",
             between(date_coerce, "2024-06-20", "2024-09-21") ~ "SUMMER",
             between(date_coerce, "2024-09-22", "2024-12-20") ~ "FALL",
             TRUE ~ "WHY")) %>%
  group_by(GEOID10, astro_season,
           .drop = FALSE) %>% 
  summarise(ppt_30yr_avg = mean(ppt),
            tmean_30yr_avg = mean(tmean),
            tmax_30yr_avg = mean(tmax),
            tmin_30yr_avg = mean(tmin),
            tdmean_30yr_avg = mean(tdmean),
            .groups = "drop") %>% 
  collect()
tictoc::toc()           

## Experiment with subqueries as needed for testing - did a fair amount, seems fine. 

## 1.3 Add farenheit columns just in case ------------------------------------

## Helps make across call readable 
measures <- c("tmax", "tmean", "tmin", "tdmean")

ore_szn_30yr_avg <-
  ore_szn_30yr_avg %>% 
  mutate(across(contains(measures), 
                ~(. * 9/5) + 32, .names = "{.col}_fahrenheit"))
  
## 1.4 Get 30 yr average heat indices ----------------------------------
ore_szn_30yr_avg <- 
  get_heat_indices(ore_szn_30yr_avg,
                   "tmean_30yr_heat_index_f", 
                   "tmean_30yr_avg_fahrenheit", 
                   "tdmean_30yr_avg_fahrenheit", 
                   "fahrenheit")

ore_szn_30yr_avg <- 
  get_heat_indices(ore_szn_30yr_avg,
                   "tmax_30yr_heat_index_f", 
                   "tmax_30yr_avg_fahrenheit", 
                   "tdmean_30yr_avg_fahrenheit", 
                   "fahrenheit")



## ----------------------------------------------------------------------------=
# 2. 2018 - 2024 - Import & Easy Temp shock ----
## ----------------------------------------------------------------------------=

## Because there are only six years of daily data requested, we're just going to
## pull into memory w/ arrow and go from there so we can use in-memory r tools

## 2.1 Import w/ arrow | Add season, farenheit, date stuff
ore_zcta_prism <-
  ore_prism_tbls_all %>%
  mutate(date_ymd = as.character(date_ymd),
         date = as.Date(date_ymd, "%Y%m%d"),
         year = year(date),
         month = month(date),
         date_coerce = make_date(year = 2024, 
                                 month = month(date), 
                                 day = day(date))) %>% 
  filter(year %in% 2018:2024) %>% 
  mutate(astro_season = 
           case_when(
             between(date_coerce, "2024-01-01", "2024-03-19") ~ "WINTER",
             between(date_coerce, "2024-12-21", "2024-12-31") ~ "WINTER",
             between(date_coerce, "2024-03-20", "2024-06-19") ~ "SPRING",
             between(date_coerce, "2024-06-20", "2024-09-21") ~ "SUMMER",
             between(date_coerce, "2024-09-22", "2024-12-20") ~ "FALL",
             TRUE ~ "WHY")) %>% 
  mutate(across(contains(measures), 
                ~(. * 9/5) + 32, .names = "{.col}_fahrenheit")) %>% 
  collect()
  
  
## 2.2 Derive Heat Indices from mean & max -------------------
ore_zcta_prism <- 
  get_heat_indices(ore_zcta_prism,
                   "tmean_heat_index_f", 
                   "tmean_fahrenheit", 
                   "tdmean_fahrenheit", 
                   "fahrenheit")

ore_zcta_prism <- 
  get_heat_indices(ore_zcta_prism,
                   "tmax_heat_index_f", 
                   "tmax_fahrenheit", 
                   "tdmean_fahrenheit", 
                   "fahrenheit")

## We get a handful of missings where mean temp > (usually = dewpoint temp) | dropping, but need to know what to do
ore_zcta_prism %>% 
  filter(is.na(tmean_heat_index_f)) %>% 
  nrow() ## .5% of obs 

ore_zcta_prism <-
  ore_zcta_prism %>% 
  filter(!is.na(tmean_heat_index_f))

# 2.3 Derive Heat Shock ---------------------------------------------------------

## Lots of inputs we can use for calculation, and I'm not sure what makes the most sense
## so head bash 

heat_thresholds <- c(80, 90, 95)
heat_var_names <- c( "80_hi_spike", "90_hi_spike", "95_hi_spike")

ore_zcta_prism <- 
  rejoin_spike_dat(heat_var_names, heat_thresholds, 
                   derive_heat_shocks, tmean_heat_index_f, 
                   "mean_{.x}")
ore_zcta_prism <- 
  rejoin_spike_dat(heat_var_names, heat_thresholds, 
                   derive_heat_shocks, tmax_heat_index_f, 
                   "max_{.x}")


# 2.4 Derive Cold Shocks -------------------------------------------------------

cold_thresholds <- c(20, 32, 40)
cold_var_names <- c( "20_spike", "32_spike", "40_spike")

## Didn't use heat index because min makes more sense ??? here I think -- TALK THROUGH W/ LEAH

ore_zcta_prism <- 
  rejoin_spike_dat(cold_var_names, cold_thresholds, 
                   derive_cold_shocks, tmean_fahrenheit, 
                   "mean_{.x}")
ore_zcta_prism <- 
  rejoin_spike_dat(cold_var_names, cold_thresholds, 
                   derive_cold_shocks, tmin_fahrenheit, 
                   "min_{.x}")
  
## 2.5 Count Up Temperature spikes -------------------------------------------

## We're just going to store in a list for the moment until we need to output differently
spike_groups <- select(ore_zcta_prism, contains("_spike")) %>% names()


spike_counts <- 
  map(spike_groups,
      ~ore_zcta_prism %>% 
      group_by(year, GEOID10, !!sym(.x), .drop = FALSE) %>% 
      summarise(n = n(), .groups = "drop")
  )

## ---------------------------------------------------------------------------=
# 3. Derive Derivation from 30 year average -----------
## ---------------------------------------------------------------------------=

## Derive seasonal measures for year / zips of interest
ore_zcta_szn_yr <-
  ore_zcta_prism %>% 
  group_by(year, astro_season, GEOID10, 
           .drop = FALSE) %>% 
  summarise(ppt_szn_avg = mean(ppt),
            tmean_szn_avg_fahrenheit = mean(tmean_fahrenheit),
            tmax_szn_avg_fahrenheit = mean(tmax_fahrenheit),
            tmin_szn_avg_fahrenheit = mean(tmin_fahrenheit),
            tdmean_szn_avg_fahrenheit = mean(tdmean_fahrenheit),
            tmean_hi_szn_avg_f = mean(tmean_heat_index_f),
            tmax_hi_szn_avg_f = mean(tmax_heat_index_f),
            .groups = "drop")

## Join with long-term trends | Calc derivation
ore_zcta_szn_yr_derivations <-
  ore_zcta_szn_yr %>% 
  left_join(ore_szn_30yr_avg, by = c("astro_season", "GEOID10")) %>% 
  mutate(ppt_szn_diff = ppt_szn_avg - ppt_30yr_avg,
         tmean_szn_diff_fahrenheit = tmean_szn_avg_fahrenheit - tmean_30yr_avg_fahrenheit,
         tmax_szn_diff_fahrenheit = tmax_szn_avg_fahrenheit - tmax_30yr_avg_fahrenheit,
         tmin_szn_diff_fahrenheit = tmin_szn_avg_fahrenheit - tmin_30yr_avg_fahrenheit,
         tdmean_szn_diff_fahrenheit = tdmean_szn_avg_fahrenheit - tdmean_30yr_avg_fahrenheit,
         tmean_hi_szn_diff_fahrenheit = tmean_hi_szn_avg_f - tmean_30yr_heat_index_f ,
         tmax_hi_diff_fahrenheit =    tmax_hi_szn_avg_f  - tmax_30yr_heat_index_f)

         

  
## ---------------------------------------------------------------------------=
# 4. Make Temp cut points -----------
## ---------------------------------------------------------------------------=
ore_zcta_prism <-
  ore_zcta_prism %>% 
  mutate(tmean_f_ranks = percent_rank(tmean_fahrenheit),
         tdmean_f_ranks = percent_rank(tdmean_fahrenheit),
         tmax_f_ranks = percent_rank(tmax_fahrenheit),
         tmin_f_ranks = percent_rank(tmin_fahrenheit),
         ppt_ranks = percent_rank(ppt),
         tmean_heat_index_f_ranks = percent_rank(tmean_heat_index_f),
         tmax_heat_index_f_ranks = percent_rank(tmax_heat_index_f))
