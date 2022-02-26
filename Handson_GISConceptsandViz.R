library(tidyverse)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)
library(maps)
library(geosphere)
library(htmltools)
library(janitor)
library(rmapshaper)
# library(here)
options(tigris_class = "sf")


#### HOW DO I GET GEOSPATIAL DATA INTO R? #### -----------------------------------------------------------

# There are two primary ways to get geodata into R: through a saved file you import, 
# or though a package that will help download it directly from the web.

# R can handle almost any type of GIS data, including shapefiles, geodatabases, geoJson, etc.
# We'll look at the SF package, one of the best packages for doing importing and processing data.

# What are "Simple Features" (SF)?
# The simple feature geodata format creates an object that behaves much like a dataframe in R, yet has spatial
# fields that contain the geographic information. This represents a big improvement over previous ways of 
# handling geospatial data in R.
# 
# You may see older references online to the SP package, which is the former way of doing geospatial work. 
# If you're just starting out, you're much better off focusing your efforts on the SF package from the get-go.



#### HOW DO I WORK WITH GEOSPATIAL DATA INTO R? #### ------------------------------------------------------

# There are a bunch of different R packages designed to work with geospatial data.
# We'll touch on a few of them here, primarily the tmap package, but there are many more.
# Even ggplot2 itself now has functions to help handle sf objects!

# Let's look as some actual code and examples to get started...



#### AN EXAMPLE: PLOTTING POINTS #### ---------------------------------------------------------------------

# We'll use the tigris package to pull census boundary geo data into our session, for a state map of the US.

# Note that at the end we'll discuss strategies for handling Alaska, Hawaii and Puerto Rico - for now we'll
# take them out for expediency's sake in the example below.

# The tigris package is a wonderful resource for all kinds of boundary files
# several options for resolution - when using whole nation, 20m is usually better
# for individual states 5m may be preferable

# by setting options(tigris_class = "sf") at the top, we've told tigris we want simple feature objects returned
states_geo <- tigris::states(resolution = "20m", cb = TRUE)

# let's take a look as what we have
states_geo

# looks a lot like a dataframe right?
# note the "geometry" field 
# also take note of the CRS, which stands for coordinate reference system; we'll come back to that shortly

# Ok, it's nice I have this GIS data, how do I actually see anything? How do I map it out?
# This is where you have many different options. But we're going to start by using the powerful tmap package.
# Keep in mind tmap using the + sign not the pipe, similar ggplot2.  


# watch out simple it is to get something initial up to see
tm_shape(states_geo) + 
  tm_polygons()


# I said for now we'll focus on the lower 48, how can we do that? 
# Well you can filter sf objects much like you can a regular dataframe/tibble

# First, let's start with just getting rid of U.S. territories, and just keep states, since this
# is something you'll find yourself doing quite frequently.

# tigris also comes with a handle fips code table built in
head(fips_codes)

# this can be a great tool to help get down to just U.S. states and DC
vector_50anddc <- unique(fips_codes$state)[1:51] 

# now we'll filter using our vector, must like we would a normal dataframe
states_geo <- states_geo %>% 
  filter(STUSPS %in% vector_50anddc)

# did it work?
states_geo %>%
  nrow()

# great, now let's take out AK and HI too for now
states_geo <- states_geo %>% 
  filter(!STUSPS %in% c("AK", "HI"))

# let's map now to see what we have
tm_shape(states_geo) + 
  tm_polygons()

# bingo

# tm_polygons also takes some other arguments, including assigning an ID.
# one of the powerful is to symbolize the data based on a certain column
tm_shape(states_geo) +
  tm_polygons("ALAND", id = "GEOID") #here we feed in the land area, ALAND

#generate it again but this time adding labels
tm_shape(states_geo) +
  tm_polygons("ALAND", id = "GEOID") +
  tm_text("STUSPS", size = .5) #this line adds the labels

# there are numerous parameters and customizations you can do



# At this point, let's touch on two geospatial concepts:

# 1) What are "cartographic boundaries" and why do we almost always want to use them?
#      - you may have seen the parameter "cb = TRUE" above
#      - when to use them? when not to use them?

# with cb
states_geo <- tigris::states(resolution = "20m", cb = TRUE) %>% 
              filter(STUSPS %in% vector_50anddc,
                     !STUSPS %in% c("AK", "HI"))

tm_shape(states_geo) +
  tm_polygons()

# without cb
states_geo_nocb <- tigris::states(resolution = "20m") %>% 
  filter(STUSPS %in% vector_50anddc,
         !STUSPS %in% c("AK", "HI"))

tm_shape(states_geo_nocb) +
  tm_polygons()

# Notice what's different? (Hint: look at MI and VA)
# Also it took a lot longer to plot the non-cb version, even with 20m resolution. Why is that? 

# For large sf objects where you're more interested in visualizing, as opposed to spatial analysis based on distances etc,
# you can "simplify" the map object to decrease the size and boost rendering speed 
states_geo_nocb_SIMPLIFIED <- rmapshaper::ms_simplify(states_geo_nocb, keep = 0.1)

# let's see the difference
tm_shape(states_geo_nocb_SIMPLIFIED) +
  tm_polygons()



# 2) What is a Coordinate Reference System (CRS) and why is it important for mapping more than one element?
#      - how can I check the CRS? How can I change it?
#      - what's the difference between planar and geodesic?
#      - https://ihatecoordinatesystems.com/
  
# the sf package's st_crs() function returns the CRS of a simple feature object
st_crs(states_geo)

# This becomes very important when you want to layer different geo datasets, and when doing processing
# work such as spatial joins, or measuring distances and such.
# You want all the data to be using the same CRS, or you could wind up with distorted or incorrect results.



### LET'S ADD SOME CITIES AS POINTS #### ------------------------------------------------------

# load a sample of US cities
cities <- read_csv(here("data/table_data/cities_with_coordinates.csv"))

cities

# now we can create a geospatial object using the coordinates
# can can specify a crs
cities_geo <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4269)

st_crs(cities_geo)
st_crs(states_geo)

# looks like they're the same crs - but are they exactly the same?
# sometimes helps to be extra sure that the text is identical
# we can change a CRS by using st_transform()
cities_geo <- st_transform(cities_geo, st_crs(states_geo))

# now let's look again
st_crs(cities_geo)
st_crs(states_geo)

# great, now let's map our new point layer on top of the base map
tm_shape(states_geo) + tm_polygons() +
  tm_shape(cities_geo) + tm_dots()

# They're on there! 
# Little hard to see though, let's fiddle with the size and color
tm_shape(states_geo) + tm_polygons() +
  tm_shape(cities_geo) + tm_dots(col = "red", size = 1)


# we can actually save our tmap as its own object as well
mymap <- tm_shape(states_geo) + tm_polygons() +
  tm_shape(cities_geo) + tm_dots(col = "red", size = 0.1)

mymap


# We can either use the "export" button directly from the viewer to save as pdf...
# ...or do it using the following code:
tmap_save(mymap, "mymap.pdf")


# We can also save it as an RDS file - the entire map becomes the saved object
saveRDS(mymap, "mymap.rds")

# why might we want to do this?
# one use case: if displaying in a complex map on an rmarkdown document / website, 
# you don't have to compute it on the fly, can use the pre-processed result instead
map_to_include <- readRDS("mymap.rds")

map_to_include


### Wish your tmap was interactive instead of static? ####

# While it doesn't have the same level of specific customization as using the leaflet package
# directly (example of that later on), you can actually turn your map object in tmap into a 
# leaflet map by running a single line of code: setting the tmap_mode()

# let's take a look
tmap_mode(mode = "view")

# what's what happens
mymap

# you can also use the tmap_leaflet() function to convert to a leaflet object and 
# further customize using the leaflet's own methods

# want to go back to static?
tmap_mode(mode = "plot")

mymap

# nice...






### AN EXAMPLE: CHOROPLETH MAPS OF HOUSE DISTRICT DEMOGRAPHICS AND VOTING #### --------------------

# We've done something with points, let's now look at a real-world choropleth use case

# load dataset of district characteristics for pre-2018 election U.S. House districts
alldistricts <- readRDS("data/table_data/alldistricts.rds")

alldistricts

# Since above we used the tigris package to get our base map, this time let's see what's
# involved in loading a geospatial file you already have yourself and want to bring into R.
cd_geo <- st_read(here("data/geospatial_data/cb_2018_us_cd116_20m"))

head(cd_geo)

# join our district dataset to its geography 
# note that we can use dplyr's inner join here to join on the FIPS code (named geoid in the tables)
districtmap <- inner_join(cd_geo, alldistricts, by = c("GEOID" = "geoid"))

# did it work?
glimpse(districtmap)
# woohoo

# remove AK and HI for expediency here again
districtmap <- districtmap %>% 
  filter(state_name != "Alaska",
         state_name != "Hawaii")


# Use TMAP to map it out

tmap_mode(mode = "plot")

tm_shape(districtmap) +
  tm_polygons(id = "house_dist")

# now let's actually do some analysis...
# which districts did Trump or Clinton carry in 2016?
# again you can customize color schemes, labels, etc... we'll just keep defaults for now.
tm_shape(districtmap) +
  tm_polygons("prez_winner_2016", id = "house_dist")

# let's look at whether a district is above/below the national average for pct with a college degree?
tm_shape(districtmap) +
  tm_polygons("pct_ed_college_all_abovebelow_natl", id = "house_dist")


# you can also filter our geospatial dataset to create subsets
# let's look at just GOP-held seats where the race was favoring the Dems or a tossup
rheld_demadvantage <- districtmap %>% 
  filter(incumbent_2018 == "R",
         race_rating_2018 %in% c("likely democratic", "lean democratic", "tossup"))

# and then once again use tmap to display that
tm_shape(rheld_demadvantage) +
  tm_polygons(id = "house_dist")

# Of course what happens here - we don't have the rest of the CD map shown.
# So we can simply layer the base CD map underneath the filtered districts
tm_shape(districtmap) +
  tm_polygons(id = "house_dist") +
  tm_shape(rheld_demadvantage) + 
  tm_polygons(col = "red", id = "house_dist") 


# we can also symbolize the filtered districts by a variable if we want
tm_shape(districtmap) +
  tm_polygons(id = "house_dist") +
  tm_shape(rheld_demadvantage) +
  tm_polygons("pct_ed_college_all_abovebelow_natl", id = "house_dist")


# once we have something we want can save it as its own object
# here we'll add a title for the map, as well as a title for the legend 
map_rheld_demadvantage_byeducation <- tm_shape(districtmap) +
  tm_polygons() +
  tm_shape(rheld_demadvantage) +
  tm_polygons(col = "red", id = "house_dist") +
  tm_layout(main.title = "GOP-Held Seats in 2018 Where Democrats Were Most Competitive",
            main.title.position = "center",
            main.title.color = "darkred",
            main.title.size = 1.2) 

map_rheld_demadvantage_byeducation

# now we can export it
tmap_save(map_rheld_demadvantage_byeducation, "map_rheld_demadvantage_byeducation.pdf")







### SPATIAL JOINING ##### -------------------------------------------------------------------

# A spatial join is where instead of joining to tables based on matching a key field, you join two
# datasets based on their geospatial position in the world so to speak.

# Spatial joining is a mainstay of GIS work, so let's show a very quick example of one way to do it
# in R, using the data we've already seen.
# In this case, we had cities above mapped out. What if we wanted to know which congressional districts
# each city was in?

# We have our cities:
mymap

# And we have our district boundaries:
tm_shape(districtmap) +
  tm_polygons()

# Before we join, it's always good practice to visually look at what you have. 
# This will tell you if anything weird is happening you need to deal with first.
tm_shape(districtmap) + tm_polygons() +
  tm_shape(cities_geo) + tm_dots(col = "red", size = 0.1)

# Even though cities within districts may turn out ok as we are, let's apply a planar CRS just to be sure.  
# More information on the differences between coordinate systems here: https://tinyurl.com/t97h947v
# We'll create two new R objects here to keep them distinct from the previous ones
districtmap_forjoin <- st_transform(districtmap, 2163)
# now we'll assign cities to match what we just assigned for districts
cities_geo_forjoin <- st_transform(cities_geo, st_crs(districtmap_forjoin))

st_crs(cities_geo_forjoin)
st_crs(districtmap_forjoin)


# Now let's do the join using sf's st_join() function
joined <- st_join(cities_geo_forjoin, districtmap_forjoin)

# Did it work?
joined # %>% View()

# We can select just some relevant columns to simplify
joined %>% 
  select(city, state, house_dist) 

# Cool, that's better. Though notice the geometry always travels with it.
# What if you wanted to just have a table as the result, without the geometry?
# The sf package to the rescue again...
matched_table <- joined %>% 
  select(city, state, house_dist) %>% 
  st_set_geometry(NULL)

matched_table





### AUTOMATING REPETITIVE GIS WORK ##### ----------------------------------------------------

# You may be asking yourself, I'm already a QGIS or ArcGIS user and can do what you've shown.
# Is there a benefit to doing it in R?
# The truth is it's always dependent on your situation and what you're trying to accomplish.
# One area where R can be especially helpful is when you need to do a bunch of variations of the
# same thing. Let's take a quick look at an example.

# We'll use a measure from our congressional district data from above to examine pct with college degrees
tm_shape(districtmap) +
  tm_polygons("pct_ed_college_all", id = "GEOID")

# Cool, so for the the whole country, that was easy enough.
# But what about if our need was to have maps for EVERY STATE.
# A separate map for each showing the same. Perhaps to go with state-specific pages on our website etc.


# First, let's solve for one state. How could we do that...

# Since the state abbreviation isn't in our current districtmap object, let's get it in there
fips_statelookup <- fips_codes %>% 
  as_tibble() %>% 
  select(state, state_code) %>% 
  distinct()

fips_statelookup

districtmap <- inner_join(districtmap, fips_statelookup, by = c("STATEFP" = "state_code"))

# create slice of just one state
cd_onestate <- districtmap %>% 
  filter(state == "GA")

#let's see what we've got
tm_shape(cd_onestate) +
  tm_polygons("pct_ed_college_all", id = "GEOID") 


# Let's make a FUNCTION now to do this for a state
# we'll use the state abbreviation to feed into it

make_state_map <- function(stateabbr){
  #choose state
  cd_onestate <- districtmap %>% 
    filter(state == stateabbr)
  # create cd map for the state
  mymap_test <-  tm_shape(cd_onestate) +
    tm_polygons("pct_ed_college_all", id = "GEOID") +
    tm_text("CD116FP", size = .5)
  #export file to pdf
  filename <- paste0("stateoutputs/districtmap_", stateabbr, ".pdf")
  print(filename)
  tmap_save(mymap_test, filename)
}

# try for just one state
make_state_map("GA")

# let's take a look at the generated pdf file. Did it work?


# Now that we know it works for one, we can do it for ALL states in a list we determine.
# Let's create a vector of all the states in our original map
vector_targetstates <- districtmap %>% 
  st_set_geometry(NULL) %>% 
  count(state) %>% 
  pull(state)
  
# Then we'll use that to feed into our new function to loop through everything at once.
# We'll iterate through them all using purrr's walk() function
walk(vector_targetstates, make_state_map)






### ROUTE TRACING ##### -------------------------------------------------------------------

# Will show how to achieve paths-to-a-destination similar to this project:
# https://www.bloomberg.com/graphics/2021-citylab-how-americans-moved/

# Here's we'll use the maps and geosphere packages to demonstrate


# First, let's load a dataset with outbound moves from New York City 
zips_nyc_out <- readRDS("data/zips_nyc_out_beyondcity.rds")

zips_nyc_out
# View(zips_nyc_out)

## split into permanent and temporary tables 
zips_nyc_out_TEMP <- zips_nyc_out %>% 
  filter(temp_moves > 0)

zips_nyc_out_PERM <- zips_nyc_out %>% 
  filter(perm_moves > 0)


# first we have to set point for center of the paths
# for now we'll hard code this in as NYC's lat/lon
centerpoint_coord <- tibble(location = c("center point nyc"),
                            latitude = c(40.7128),
                            longitude = c(-74.006)
                            )


# create basemap using the maps package
# note the maps package operates a little differently in how we add elements sequentially
maps::map("state", 
          fill = TRUE, 
          col = "grey8", 
          bg = "grey15")

# overlay location points - TEMPORARY moves
points(zips_nyc_out_TEMP$new_longitude, zips_nyc_out_TEMP$new_latitude, 
       pch = 3, cex = 0.1, col = "green")

# overlay location points - PERMANENT moves (some of these may overlap with temp as well)
points(zips_nyc_out_PERM$new_longitude, zips_nyc_out_PERM$new_latitude, 
       pch = 3, cex = 0.1, col = "orange")

# Now we'll calculate the paths using the geosphere package

# The gcIntermediate() function takes two points and creates a connection between them 
# that accounts for the curvature and therefore creates a circular path.
# The lines() function then adds the generated path to the map. 
# We'll use a for loop here to cycle through all the points at once.

for (i in (1:dim(zips_nyc_out)[1])) { 
  inter <- gcIntermediate(c(centerpoint_coord$longitude[1], 
                            centerpoint_coord$latitude[1]),
                          c(zips_nyc_out$new_longitude[i], 
                            zips_nyc_out$new_latitude[i]), 
                          n=200)
  lines(inter, lwd = 0.1, col = "purple")    
}  


# We can do something similar focusing on only certain states
# create basemap of just states around New York using regions parameter
maps::map("state", 
          regions = c("new york", "new jersey", "connecticut", "rhode island",
                      "pennsylvania", "massachusetts", "new hampshire", "vermont"),
          fill = TRUE, 
          col = "grey8", 
          bg = "grey15")

# overlay location points - TEMPORARY
points(zips_nyc_out_TEMP$new_longitude, zips_nyc_out_TEMP$new_latitude, 
       pch = 3, cex = 0.1, col = "green")

# overlay location points - PERMANENT (some of these may overlap with temp as well)
points(zips_nyc_out_PERM$new_longitude, zips_nyc_out_PERM$new_latitude, 
       pch = 3, cex = 0.1, col = "orange")

# now we'll use geosphere again as above
for (i in (1:dim(zips_nyc_out)[1])) { 
  inter <- gcIntermediate(c(centerpoint_coord$longitude[1], 
                            centerpoint_coord$latitude[1]),
                          c(zips_nyc_out$new_longitude[i], 
                            zips_nyc_out$new_latitude[i]), 
                          n=200)
  lines(inter, lwd = 0.1, col = "purple")    
}  

# Note that in this case the paths extend beyond our chosen states
# since the dataset itself still includes them. 
# So you'd want to similarly filter out those states from the table
# if doing this for real.








## STRATEGIES FOR DEALING WITH ALASKA, HAWAII AND PUERTO RICO #### -----------------

# Note: keep in mind that these solutions work for displaying state/county shaded mapping,
# but they won't be something you'd use for layering points, or doing spatial joining etc
# because the points won't know where to go anymore in the shifted maps

#new feature in latest version of tigris package
#to help with repositioning AK, HI and PR

#can apply to existing sf geospatial objects

test <- tigris::states(resolution = "20m", cb = TRUE)

test <- shift_geometry(test, preserve_area = FALSE, position = "below")

tm_shape(test) + 
  tm_polygons()

#or you can apply this from the get-go when pulling down geospatial data using tigris
#can pipe it in on the fly like this:
usstates_geo_shifted <- tigris::states(resolution = "20m", cb = TRUE) %>% 
  shift_geometry(preserve_area = FALSE, position = "below")

tm_shape(usstates_geo_shifted) + 
  tm_polygons()


#the ALBERSUSA package also provides functions for dealing with albers projections
#as well as splitting off AK and HI

# To install from github:
# remotes::install_github("hrbrmstr/albersusa")
library(albersusa)

states_albers <- usa_sf("laea")

tm_shape(states_albers) + 
  tm_polygons()

counties_albers <- counties_sf("aeqd")

tm_shape(counties_albers) + 
  tm_polygons()


