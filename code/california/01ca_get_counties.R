pacman::p_load("tigris", "sf", "here")



# Get Oregon zip code shape files ----------------------------------------------

## I'm going to use 2010 zips for now... run everything, then see if I need to change.
ca_counties <- 
  counties(state = "California", year = "2010")



st_write(ca_counties, here("data", "shapefiles", "ca_2010_counties.shp"))