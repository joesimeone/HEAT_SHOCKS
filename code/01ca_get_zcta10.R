
pacman::p_load("tigris", "sf", "here")



# Get Oregon zip code shape files ----------------------------------------------

## I'm going to use 2010 zips for now... run everything, then see if I need to change.
oregon_zips <- 
  zctas(state = "California", year = "2010")



st_write(oregon_zips, here("data", "shapefiles", "ca_2010_zcta.shp"))



mapview::mapview(oregon_zips)


glimpse(oregon_zips
)