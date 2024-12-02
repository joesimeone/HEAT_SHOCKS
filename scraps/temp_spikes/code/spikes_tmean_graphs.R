library(here)
library(tidyverse)
library(MetBrewer)
library(gghighlight)
library(ggtext)


# Imports -----------------------------------------------------------------


prism_files <- list.files(here("temp_spikes", "data"), pattern = "PRISM_",
                          full.names = TRUE) 

## Used code meant to get multiple geographies id'd by geoid - Cbsa doesn't have that, but 
## the resulting data should still just be daily temps for the boundaries I supplied - Removing field
phl_prism <- map(prism_files, 
                 ~read_csv(.x) %>% 
                   select(-GEOID) %>%
                   mutate(date_ymd = as.character(date_ymd))
)

## Bind years of data together 
phl_prism_raw <- list_rbind(phl_prism)


# Cleaning ----------------------------------------------------------------



## vector to help w/ some iteration
measures <- c("tmax", "tmean", "tmin", "tdmean", "ppt")


## Derive basics to help w/ plotting
phl_prism_ann <- phl_prism_raw %>% 
  mutate(date = as.Date(date_ymd, "%Y%m%d"),
         month = month(date),
         year = year(date)) %>% 
  mutate(across(all_of(measures), ~(. * 9/5) + 32, .names = "{.col}_fahrenheit"),
         max_temp_spike = if_else(tmax_fahrenheit > 80,
                                  "Spike", "No Spike"),
         mean_temp_spike = if_else(tmean_fahrenheit > 80,
                                   "Spike", "No Spike")) 

## Use derived values to help w/ some additional plot elements 
## pivot spike counts wider to faciliate paste
phl_prism_ann <- phl_prism_ann %>% 
  group_by(year, max_temp_spike) %>% 
  mutate(max_n_spikes = n()) %>% 
  ungroup() %>% 
  group_by(year, mean_temp_spike) %>% 
  mutate(mean_n_spikes = n()) %>% 
  ungroup()


## I want variable with year & number of days next to it parentheses for days > 80. Doing that w/ filter and merging back
## Better way???
max_year_annotate <- phl_prism_ann %>% 
  filter(max_temp_spike == "Spike") %>% 
  mutate(year_fac = as.factor(year),
         year_max_spike = glue::glue("{year_fac} (days reaching > 80°F = {max_n_spikes})")) %>% 
  select(year, year_fac, year_max_spike) %>% 
  distinct(year_max_spike, .keep_all = TRUE)

mean_year_annotate <- phl_prism_ann %>% 
  filter(mean_temp_spike == "Spike") %>% 
  mutate(year_fac = as.factor(year),
         year_mean_spike = glue::glue("{year_fac} (days averaging over > 80°F = {mean_n_spikes})")) %>% 
  select(year, year_mean_spike) %>% 
  distinct(year_mean_spike, .keep_all = TRUE)

## Join annotation variables back to original data

phl_prism_fin <- phl_prism_ann %>% 
  left_join(max_year_annotate, by = c("year")) %>% 
  left_join(mean_year_annotate, by = c("year"))


phl_means <- phl_prism_ann %>% 
  group_by(year, mean_temp_spike) %>% 
  summarise(mean_n_spikes = n()) %>% 
  ungroup() %>% filter(mean_temp_spike == "Spike")

phl_for_series <-  phl_means %>% 
  mutate(year_coerce = as.Date(ISOdate(year, 1, 1)))


# Let's do some plotting  -------------------------------------------------

## Add degree symbol to axis
degree_label <- function(x) {
  paste0(x, "°F")
}

## Use these binwidths depending on which measure we want
max_bins <- 20
max_binwidth <- 3
mean_bins <- 30
mean_binwidth <- NULL

mean_fac_bins <- 40

## Colors to denote spikes 
fill_colors <- c("#b9b9b8", "#591c19")

max_labels <- labs(title = "Philadelphia Core Based Statistical Area: Days over 80°F, 2010 - 2020",
                   subtitle = "*Estimates taken from University of Oregon's PRISM Daily Spatial Climate Dataset*",
                   x = "Maximum Temperature (°F)", 
                   y = "Daily Count",
                   caption = "1. Data pulled using Google Earth Enginge")

mean_labels <- labs(title = "Philadelphia Core Based Statistical Area: Days over 80°F, 2010 - 2020",
                    subtitle = "*Estimates taken from University of Oregon's PRISM Daily Spatial Climate Dataset*",
                    x = "Mean Temperature (°F)", 
                    y = "Daily Count",
                    caption = "1. Data pulled using Google Earth Enginge")



joes_dumb_theme <- theme(panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major.y = element_line(color = "darkgray", 
                                                           linetype = "dashed"),
                         panel.grid.minor.y = element_blank(),
                         panel.background =   element_rect(fill =  "white"),
                         plot.background = element_rect("white"),
                         axis.line.x = element_line(color = "black"),
                         axis.line.y = element_line(color = "black"),
                         plot.subtitle = element_markdown(hjust = .5),
                         plot.title = element_text(hjust = .5),
                         legend.position="none")


#E8E3DC
## To see if I can get an idea of bindwidths and the like, let's see what everything does 

mean_or_max_graph <- function(measure, fill, bin, binwidth){
  spikes_graph <- 
    ggplot(phl_prism_fin, aes({{ measure }}, fill = {{ fill }})) +
    geom_vline(xintercept = 80, color = "black", alpha = .95, linetype = "dashed") +
    geom_histogram(bins = bin, binwidth = binwidth, 
                   color = "white", position = "identity") +
    scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), 
                       labels = degree_label) +
    scale_fill_manual(values = fill_colors) + joes_dumb_theme
  
  return(spikes_graph)}

## Maximum temp graphs
mean_total_spikes_10to20 <- mean_or_max_graph(tmean_fahrenheit, mean_temp_spike, 
                                             mean_bins, mean_binwidth) + mean_labels

mean_faceted_spikes <- mean_total_spikes_10to20 + 
  facet_wrap(~year_mean_spike, scales = "free",
             nrow = 3)


bar_chart_labels <- labs(title = "Philadelphia Core Based Statistical Area: Days Average over 80°F, 2010 - 2020",
                         subtitle = "*Estimates taken from University of Oregon's PRISM Daily Spatial Climate Dataset*",
                         x = "Year", 
                         y = "Days averaging over 80°F",
                         caption = "1. Data pulled using Google Earth Enginge")



# Bar Chart with years on x axis, counts on Y axis ------------------------



temp_spikes_col <- ggplot(phl_means, aes(as.factor(year), mean_n_spikes)) +
  geom_col(color =  "white", 
           fill =   "#D95F02", width = .8) + 
  bar_chart_labels + 
  joes_dumb_theme

temp_spikes_line <- ggplot(phl_for_series, aes(year_coerce, mean_n_spikes)) +
  geom_line(color = "#D95F02", linewidth = .7) + 
  geom_point() +
  bar_chart_labels + 
  joes_dumb_theme
temp_spikes_col

temp_spikes_col

ggsave("phl_avg_temp_spikes_80deg_10to20.png", 
       plot = mean_total_spikes_10to20, 
       device = "png",
       path = (here("temp_spikes")),
       height = 8,
       width = 10,
       dpi = 350)


ggsave("phl_facet_avg_temp_spikes_80deg_10to20.png", 
       plot = mean_faceted_spikes, 
       device = "png",
       path = (here("temp_spikes")),
       height = 8,
       width = 10,
       dpi = 350)


ggsave("phl_avg_temp_spikes_bar_80deg_10to20.png", 
       plot = temp_spikes_col, 
       device = "png",
       path = (here("temp_spikes")),
       height = 8,
       width = 10,
       dpi = 350)

ggsave("phl_avg_temp_spikes_line_80deg_10to20.png", 
       plot = temp_spikes_line , 
       device = "png",
       path = (here("temp_spikes")),
       height = 8,
       width = 10,
       dpi = 350)

# ## Average Temp graphs
# mean_total_spikes_10to20 <- mean_or_max_graph(tmean_fahrenheit, mean_temp_spike, 
#                                              mean_bins, mean_binwidth) + mean_labels
# 
# 
# 
# mean_faceted_spikes <- mean_total_spikes_10to20 + 
#   facet_wrap(~year_mean_spike, scales = "free",
#              nrow = 3)
# mean_faceted_spikes
# mean_total_spikes_10to20
# 
# 
# 
# 
# spikes_all_years <- 
#   ggplot(phl_prism_fin, aes(tmax_fahrenheit, fill = max_temp_spike)) +
#   geom_vline(xintercept = 80, color = "black", alpha = .95, linetype = "dashed") +
#   geom_histogram(bins = max_bins, binwidth = max_binwidth, 
#                  color = "white", position = "identity") +
#   scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), labels = degree_label) +
#   scale_fill_manual(values = fill_colors) + joes_dumb_theme
# 
# spikes_all_years + max_labels + facet_wrap(~year_max_spike, scales = "free", nrow = 3)
# 
# 
