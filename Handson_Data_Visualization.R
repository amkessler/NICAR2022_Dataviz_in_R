###########################
# Data Visualization in R #
###########################

library(tidyverse)
library(sf)
library(scales)
library(viridis)
library(tmap)

# data and maps stored locally; instructions on how to import from census
# will be included in Github repository

# import data and maps ----------------------------------------------------

# data tables from csv files
# the col_types choice below makes the sure the GEOID is read as text not a number
ga_races <- read_csv("data/table_data/ga_races.csv", 
                                 col_types = cols(GEOID = col_character())) 

metro_co_races <- read_csv("data/table_data/metro_co_races.csv", 
                           col_types = cols(GEOID = col_character()))

metro_tract_races <- read_csv("data/table_data/metro_tract_race.csv", 
                                                  col_types = cols(GEOID = col_character()))

fulton_races <- read_csv("data/table_data/metro_tract_race.csv", 
                         col_types = cols(GEOID = col_character()))

metro_co_income <- read_csv("data/table_data/metro_county_inc.csv", 
                            col_types = cols(GEOID = col_character()))

metro_tract_income <- read_csv("data/table_data/metro_tract_inc.csv", 
                               col_types = cols(GEOID = col_character()))

# map/geospatial boundary files
# we use st_read() from the sf pacakge to bring in the shapefiles
ga_counties_geo <- st_read("data/geospatial_data/ga_counties")

metro_tracts20_geo <- st_read("data/geospatial_data/metro_tracts20")

fulton_tracts20_geo <- st_read("data/geospatial_data/fulton_tracts20")



# explore the data --------------------------------------------------------

# use histogram to analyze racial patterns in Georgia counties
ggplot(ga_races, aes(White_per)) +
  geom_histogram()

# add horizontal, vertical scales so we can chart all races consistently
ggplot(ga_races, aes(White_per)) +
  geom_histogram() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 20))

# add outline and fill, change background
ggplot(ga_races, aes(White_per)) +
  geom_histogram(color = "navy", fill = "steelblue") +
  theme_classic() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 20))

# use the same code for black residents
ggplot(ga_races, aes(Black_per)) +
  geom_histogram(color = "navy", fill = "steelblue") +
  theme_classic() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 20))



# make maps ---------------------------------------------------------------

# create a map of Georgia's races
ga_race_map <- left_join(ga_counties_geo, ga_races,
                         by = "GEOID")

# we can create maps of geospatial data right in ggplot2 like this:
ggplot(ga_race_map) +
  geom_sf()

# to map the Black percentage by county, we'll use the aesthetic of fill
ggplot(ga_race_map) +
  geom_sf(aes(fill = Black_per))

# add a title and change background
ggplot(ga_race_map) +
  geom_sf(aes(fill = Black_per)) +
  labs(title = "Black percentage in Georgia counties",
       caption = "Source: 2020 Census") +
  theme_bw()

# introduce a new color scheme with the viridis package
ggplot(ga_race_map) +
  geom_sf(aes(fill = Black_per)) +
  labs(title = "Black percentage in Georgia counties",
       caption = "Source: 2020 Census") +
  scale_fill_viridis_c() +
  theme_bw()

# highest percentages are lightest colors, which is counterintuitive;
# that's easy to reverse
ggplot(ga_race_map) +
  geom_sf(aes(fill = Black_per)) +
  labs(title = "Black percentage in Georgia counties",
       caption = "Source: 2020 Census") +
  scale_fill_viridis_c(direction = -1) +
  theme_bw()

# create an interactive map using tmap package
# tmap has two modes -- view (interactive) and plot (static)

# set tmap to interactive mode
tmap_mode("view")

# create map displaying percentage of Blacks in Georgia counties
tm_shape(ga_race_map) +
  tm_fill(col = "Black_per", palette = "viridis", alpha = 0.5)

# improve the pop-up
tm_shape(ga_race_map) +
  tm_fill(col = "Black_per", palette = "viridis", alpha = 0.5,
          popup.vars = c("County" = "NAME.x", 
                         "Black (%)" = "Black_per"))

# create map of Fulton county race data
fulton_tract_race_map <- left_join(fulton_tracts20_geo, 
                                   fulton_races,
                                   by = "GEOID")

# "bubble" map of Black population in Fulton tracts (2020)
tm_shape(fulton_tract_race_map) +
  tm_polygons() +
  tm_bubbles(size = "Black_per", alpha = 0.2, col = "green")

# change the content of the pop-up
tm_shape(fulton_tract_race_map) +
  tm_polygons() +
  tm_bubbles(size = "Black_per", alpha = 0.2, col = "green",
             popup.vars = c("County", "Tract", "Black (%)" = "Black_per"))

# map racial groups by tract 

# first convert tracts (polygons) to centroids
metro_centroids20 <- st_centroid(metro_tracts20_geo)

# convert metro tract race table to long format using tidyr 
metro_tract_race_long <- metro_tract_races %>% 
  select(GEOID, White = White_per, Black = Black_per, 
         Hispanic = Hispanic_per) %>% 
  pivot_longer(!GEOID, names_to = "Race", 
               values_to = "Percent")

# join tract race data with 2020 tract map
metro_tract_race_long_map <- inner_join(metro_tracts20_geo,
                                        metro_tract_race_long,
                                        by = "GEOID")

# shift tmap to static mode
tmap_mode("plot")

# use facets to display size of each race in Atlanta metro side by side
tm_shape(metro_tract_race_long_map) +
  tm_facets(by = "Race", scale.factor = 4) +
  tm_fill(col = "Percent",
          style = "quantile",
          n= 5,
          palette = "Greens") 

# reposition the legend
tm_shape(metro_tract_race_long_map) +
  tm_facets(by = "Race", scale.factor = 4) +
  tm_fill(col = "Percent",
          style = "quantile",
          n= 5,
          palette = "Greens") +
  tm_layout(bg.color = "grey", 
            legend.position = c(-0.7, 0.2),
            panel.label.bg.color = "white")

# other visualizations ----------------------------------------------------

# convert metro county race data to long format using tidyr
metro_co_long <- metro_co_races %>% 
  select(County = NAME, White_per, Black_per, Hispanic_per, Asian_per) %>% 
  pivot_longer(!County, names_to = "Race", values_to = "Percent")

metro_co_long

# remove ", Georgia" from County field -- it will clog the chart
metro_co_long <- metro_co_long %>% 
  mutate(County = str_remove(County, ",.*$"))

metro_co_long

# let's look at individual counties within the metro with a bar chart
ggplot(metro_co_long, aes(x = County, y = Percent, fill = Race)) +
  geom_col(position = "stack") 

# clean up the county labels, add title 
ggplot(metro_co_long, aes(x = County, y = Percent, fill = Race)) +
  geom_col(position = "stack") +
  labs(title = 'Racial makeup of Atlanta metro counties',
       caption = 'Source: 2020 census') +
  theme_classic() +
  xlab('Counties') +
  ylab('Percent') +
  theme(axis.text.x = 
          element_text(angle = 90, hjust = 1, vjust = 0.5))

# graph median incomes in metro counties with scatterplot
ggplot(metro_co_income, aes(x = NAME, y = MedianHHInc)) +
  geom_point()

# rearrange to display counties by descending income order
ggplot(metro_co_income, aes(x = MedianHHInc, 
                             y = reorder(NAME, MedianHHInc))) +
  geom_point(size = 3, color = "forestgreen")

# add error bars to account for margin of error, clean up chart
ggplot(metro_co_income, aes(x = MedianHHInc, 
                            y = reorder(NAME, MedianHHInc))) +
  geom_point(size = 3, color = "forestgreen") +
  geom_errorbar(aes(xmin = MedianHHInc - moe,
                    xmax = MedianHHInc + moe)) +
  labs(title = "Median household income in Atlanta metro",
       caption = "Source: American Community Survey, 2019") +
  xlab("Median Household Income") +
  ylab("") +
  theme_classic() +
  scale_x_continuous(labels = scales::dollar_format())

# analyze range of incomes in metro tracts with box plots
ggplot(metro_tract_income, aes(x = County, y = MedHHInc)) +
  geom_boxplot() 

# clean up the boxplot
ggplot(metro_tract_income, aes(x = County, y = MedHHInc)) +
  geom_boxplot() +
  labs(title = 'Median household income in the Atlanta Metro',
       caption = "Source: American Community Survey, 2019") +
  ylab("Median Household Income") +
  xlab("") +
  theme_classic() +
  theme(axis.text.x = 
          element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(labels=scales::dollar_format())


