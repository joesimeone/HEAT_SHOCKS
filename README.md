# Overview

Here, you'll find script use to derive a range of temperature shock definitions dervied from the [University of Oregon's 4 Kilometer (??) Prism Dataset available on Google Earth Engine](https://developers.google.com/earth-engine/datasets/catalog/OREGONSTATE_PRISM_AN81d#description) for the following States & Geographies:

1. **California:** Counties (2010 vintage)
2. **Oregon:** Counties and zcta (2010 vintage)

Temperature shocks were coded for both extreme heat and extreme cold under the following definitions:

## 1. Days above and below temperature thesholds 

For heat shocks, I counted days above 80, 90, and 95 degrees Farenheit respectively, using heat index temps derived from both maximum and average daily temperatures. Each threshold - temperature combination represents a column in the resulting dataset.

For cold shocks, I counted days below 20, 32, and 40 degrees Farenheit respectively, using minimum and average dry bule temperatures. Calculations using heat index temperatures have been exlcuded for the moment while I work through some conceptual issues with [{weathermetrics}](https://github.com/geanders/weathermetrics). Each threshold - temperature combination represents a column in the resulting dataset.

See **ADD LINK FOR DATA DICTIONARY FOR DETAILS**

## 2. Anomaly: Deviations from 30-year summer/winter average temperature

Subtract the 2018 - 2024 season-specific average daily temperature for each year from the season-specific 30-year average (1991-2020).

See **ADD SEASON TABLE DATA DICTONARY FOR DETAILS**

## 3. Heat shock: Days across temperature distribution categories

For each fahrenheit prism measure (ppt, tmean, tdmean etc.), hit 2018 - 2024 daily temperature values with R {percent_rank} functions. Will use this to establish extreme cutpoints as needed. 

See **ADD LINK FOR DATA DICTIONARY FOR DETAILS**

# Workflow 

Workflows for both states can be found at code/**state_name**, and are organized as follows: 

1. **get_{geography}_files.R:** Download shapefile for relevant geography
2. **get_prism_{geography}.js:** Loop to get daily calculate daily prism measures on Google Earth Engine
3. **convert_{geography}_to_parq.R:** Converts csvs pulled down from google earth engine to Parquet for more efficient storage and run times
4. **derive_{geography}_temp_shocks.R:** Derive the metrics described above.

Annotation within each script should hopefully fill in additional details. 

# Data

Scripts reproduce

# R Package Citations
1. Müller K (2020). _here: A Simpler Way to Find Your Files_. R package version 1.0.1, <https://CRAN.R-project.org/package=here>.
2. Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J,
  Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686.
  doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
3. Anderson GB, Bell ML, Peng RD. 2013.  Methods to calculate the heat index as an exposure metric in environmental health research.  Environmental Health Perspectives
  121(10):1111-1119.
4. Richardson N, Cook I, Crane N, Dunnington D, François R, Keane J, Moldovan-Grünfeld D, Ooms J, Wujciak-Jens J, Apache Arrow (2024). _arrow: Integration to 'Apache'
  'Arrow'_. R package version 15.0.1, <https://CRAN.R-project.org/package=arrow>.
5. Walker K (2024). _tigris: Load Census TIGER/Line Shapefiles_. R package version 2.1, <https://CRAN.R-project.org/package=tigris>.
6.   Pebesma, E., & Bivand, R. (2023). Spatial Data Science: With Applications in R. Chapman and Hall/CRC. https://doi.org/10.1201/9780429459016
     Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal 10 (1), 439-446, https://doi.org/10.32614/RJ-2018-009


Any additional questions or confusion, feel free to contact me at js5466@drexel.edu

