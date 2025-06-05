# libs --------------------------------------------------------------------

pacman::p_load("tigris", "sf", "here")


# Get Oregon zip code shape files ----------------------------------------------

## I'm going to use 2010 zips for now... run everything, then see if I need to change.
oregon_co <-
  counties(state = "Oregon", year = "2010")


st_write(oregon_co, here("data", "shapefiles", "oregon_co_tigris_2010.shp"))


mapview::mapview(oregon_co)
