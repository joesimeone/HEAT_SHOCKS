library("arrow")
library("tidyverse")
library("here")
library("cli")


## Create place for parquet to go ----------------
#fs::dir_create(here("data", "prism_gee", "parquet"))

## Assigned a place to write parquets - new relative path -----------------
dest_dir <- "data/prism_gee/parquet"

## This is maybe overkill, but since I pulled down a bunch of daily measures, I'm
## going to convert to parquet, and deal with using arrow.


# Get file pahts 
prism_files <- list.files(here("data", "prism_gee"),
                          pattern = "_MONTCO_", 
                          full.names = TRUE)


## Get table names for export 
table_names <- 
  map(prism_files, basename) %>% 
  str_remove(".csv")


## Function to read and write data to parquet
walk2(prism_files, table_names, ~ {
  
  cli_alert("Processing {.y}")
  
  
  data <- read_csv(.x) %>%
    mutate(table_name = .y)
  
  
  write_parquet(data, 
                glue::glue("{dest_dir}/{.y}.parquet"))
  
  cli_alert("Finshed Processing: {.y} :)")
})

sum(file.size(dest_dir)) / (1024^3)
