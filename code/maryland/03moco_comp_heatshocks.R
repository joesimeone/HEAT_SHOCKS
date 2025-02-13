library(tidyverse)
library(here)
library(weathermetrics)
library(arrow)


# Import helper ----------------------------------------------------------------
prism_files <-
  list.files(here("data", "prism_gee", "parquet"),
             pattern = "_MONTCO_",
             full.names = TRUE)


## Get heatshock counts derived in GEE 
mo_heatshocks_js <- 
  read_csv(here("data",
                "prism_gee",
                "MontgomeryCounty_DaysAbove80F_CountyLevel.csv"))


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
         ~call(mo_co_zcta_prism, glue::glue(glue_string), 
               {{ shock_var }}, .y) %>% 
           mutate(row_id = row_number()) %>% 
           select(row_id, all_of(glue::glue(glue_string))
           )
    )
  
  combined_shocks <- reduce(shocks_container, 
                            full_join, by = "row_id")
  
  spike_dat_fin <- 
    mo_co_zcta_prism %>% 
    mutate(row_id = row_number()) %>% 
    left_join(combined_shocks, by = c("row_id"))
  
  return(spike_dat_fin)}



## 1.1 Open dataset w/ arrow -  one file per year --------
mo_co_prism_tbls_all <- arrow::open_dataset(prism_files)



## Experiment with subqueries as needed for testing - did a fair amount, seems fine. 

## 1.3 Add farenheit columns just in case ------------------------------------

## Helps make across call readable 
measures <- c("tmax", "tmean", "tmin", "tdmean")

## Because there are only six years of daily data requested, we're just going to
## pull into memory w/ arrow and go from there so we can use in-memory r tools

## 1.4 Import w/ arrow | Add season, farenheit, date stuff
mo_co_zcta_prism <-
  mo_co_prism_tbls_all %>%
  mutate(date_ymd = as.character(date_ymd),
         date = as.Date(date_ymd, "%Y%m%d"),
         year = year(date),
         month = month(date),
         date_coerce = make_date(year = 2024, 
                                 month = month(date), 
                                 day = day(date))) %>% 
  # filter(year %in% 2018:2024) %>% 
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

## 1.5 Derive Heat Indices from mean & max -------------------
mo_co_zcta_prism <- 
  get_heat_indices(mo_co_zcta_prism,
                   "tmean_heat_index_f", 
                   "tmean_fahrenheit", 
                   "tdmean_fahrenheit", 
                   "fahrenheit")

mo_co_zcta_prism <- 
  get_heat_indices(mo_co_zcta_prism,
                   "tmax_heat_index_f", 
                   "tmax_fahrenheit", 
                   "tdmean_fahrenheit", 
                   "fahrenheit")

## We get a handful of missings where mean temp > (usually = dewpoint temp) | dropping, but need to know what to do
mo_co_zcta_prism %>% 
  filter(is.na(tmean_heat_index_f)) %>% 
  nrow() ## .08% of obs 

mo_co_zcta_prism <-
  mo_co_zcta_prism %>% 
  filter(!is.na(tmean_heat_index_f))


glimpse(mo_co_zcta_prism)


# 2 Derive Heat Shock ---------------------------------------------------------

## Lots of inputs we can use for calculation, and I'm not sure what makes the most sense
## so head bash 

heat_thresholds <- c(80, 90, 95)
heat_var_names <- c( "80_max_spike", "90_max_spike", "95_max_spike")


mo_co_zcta_prism <- 
  rejoin_spike_dat(heat_var_names, heat_thresholds, 
                   derive_heat_shocks, tmax_fahrenheit, 
                   "max_{.x}")




## 2.1 Count Up Temperature spikes -------------------------------------------

## We're just going to store in a list for the moment until we need to output differently
spike_groups <- select(mo_co_zcta_prism, contains("_spike")) %>% names()


spike_counts <- 
  map(spike_groups,
      ~mo_co_zcta_prism %>% 
        group_by(year, GEOID, !!sym(.x), .drop = FALSE) %>% 
        summarise(n = n(), .groups = "drop")
  )

names(spike_counts) <- spike_groups

## ---------------------------------------------------------------------------=
# 3. Join my heat spikes with JS / GEE heat spikes  -----------
## ---------------------------------------------------------------------------=


## Looks good here!
mo_heatshocks_tst <- 
  spike_counts$max_80_max_spike %>% 
  filter(max_80_max_spike == "Spike") %>% 
  left_join(mo_heatshocks_js, by = c("year")) %>% 
  mutate(check = n - daysAbove80F)

# Let's also just create a df with temps and counts from 80 max -----------
max_80_shocks <- 
  mo_co_zcta_prism %>% 
  filter(max_80_max_spike == "Spike") %>% 
  select(GEOID, date_ymd, year, tmax_fahrenheit, max_80_max_spike) %>% 
  group_by(GEOID, year) %>% 
  mutate(spike_count = n()) %>% 
  ungroup()





