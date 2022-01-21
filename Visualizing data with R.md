# Visualizing data with R

It's easy to get overwhelmed by numbers. Sometimes there are just so many of them that you hardly know where to start. This can be a problem for data journalists. But R is here to rescue you. In addition to its tools for slicing, dicing and crunching numbers, it has wonderful packages for charting and mapping data - no artistic talent required.

We'll be using data and maps from the 2020 Census and 2019 American Community Survey for Georgia and the Atlanta metro region. In a separate tipsheet, we describe how to import and clean that data. You should find that tipsheet useful for dealing with all kinds of Census data in R.

If you're taking this class at NICAR in Atlanta, the data and maps are pre-loaded on your laptop. If you're reading this later, the data and maps are here. Click on the word "master" near the top of the screen, just to the left of the word "NICAR2022" in blue; in the drop-down menu, click on "data" to display the list of data files and map folders.

Let's load libraries and get busy. If you don't already have these packages, then in the R console type <code>install.packages("xxxxx")</code> where "xxxxx" is the package name. 

<code>library(tidyverse)</code>

<code>library(sf)</code>

<code>library(scales)</code>

<code>library(viridis)</code>

<code>library(tmap)</code>

Next step: Import the data and maps. Important note: I won't specify file locations; just remember that if you import a file from someplace other than your working directory, you must tell R where to look.

<code>ga_races <- read_csv("ga_races.csv", col_types = "cciiiiiiiiidddd")</code>

<code>ga_counties <- st_read("ga_counties")</code>
  
<code>metro_co_races <- read_csv("metro_co_races.csv", col_types = "cciiiiiiiiidddd")</code>
  
<code>metro_tract_races <- read_csv("metro_tract_race.csv", col_types = "ccciiiiiiiiidddd")</code>
    
<code>metro_tracts20 <- st_read("metro_tracts20")</code>
    
<code>fulton_races <- read_csv("fulton_tract_race.csv", col_types = "ccciiiiiiiiidddd")</code>
  
<code>fulton_tracts20 <- st_read("fulton_tracts20")</code>

<code>metro_co_income <- read_csv("metro_county_inc.csv", col_types = "ccii")</code>
  
<code>metro_tract_income <- read_csv("metro_tract_inc.csv", col_types = "cccii")</code>    

One of the quickest ways to explore data is with a histogram. Let's see how many of Georgia's 159 counties have a high percentage of White residents. We'll start with a simple histogram and gradually make it a bit more elaborate.
  
<code>ggplot(ga_races, aes(White_per)) +
  geom_histogram()</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/histogram1.png)
  
Let's add a scale, using the scales package. That will make it easier to compare Whites with other racial groups. Since only a handful of the 159 counties are in any one category ("bin" in histogram-speak), we can set the vertical (y) axis low.
  
<code>ggplot(ga_races, aes(White_per)) +
  geom_histogram() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 20))</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/histogram2.png)
  
Let's brighten it up by changing the background, or theme, and adding an outline ("color") and fill to the bars.
  
<code>ggplot(ga_races, aes(White_per)) +
  geom_histogram(color = "navy", fill = "steelblue") +
  theme_classic() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 20))</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/histogram3.png)
  
One of the great things about R is that we can reuse the code. Let's recycle this code for Black residents. We just have to change a single word, substituting "Black_per" for "White_per".
  
<code>ggplot(ga_races, aes(Black_per)) +
  geom_histogram(color = "navy", fill = "steelblue") +
  theme_classic() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 20))</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/histogram4.png)  

Next, we'll map the percentage of Black residents in Georgia counties using a combination of ggplot and the sf package. We'll need to merge ga_counties (the shapefile or digital map of Georgia counties) and ga_races, which contains racial data by county. We'll use GEOID, a unique identifier for every county in the US, which is present in both tables.
  
<code>ga_race_map <- left_join(ga_counties, ga_races,
                     by = "GEOID")</code>
  
Now we'll build a simple map:
  
<code>ggplot(ga_race_map) +
  geom_sf(aes(fill = Black_per))</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/BlackCountyMap1.png)
  
Our map could do with a title and a neater background. Last time we used theme_classic as our background; this time, we'll use a different theme.
  
<code>ggplot(ga_race_map) +
  geom_sf(aes(fill = Black_per)) +
  labs(title = "Black percentage in Georgia counties",
       caption = "Source: 2020 Census") +
  theme_bw()</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/BlackCountyMap2.png)
  
The viridis package offers several eye-pleasing color palettes for charts and maps. Let's use that.
  
<code>ggplot(ga_race_map) +
  geom_sf(aes(fill = Black_per)) +
  labs(title = "Black percentage in Georgia counties",
       caption = "Source: 2020 Census") +
  scale_fill_viridis_c() +
  theme_bw()</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/BlackCountyMap3.png)
  
The current map shades from dark (low values) to bright (high values). It might make more sense to reverse the scale. We can do this by specifying "direction = -1".
  
 <code>ggplot(ga_race_map) +
  geom_sf(aes(fill = Black_per)) +
  labs(title = "Black percentage in Georgia counties",
       caption = "Source: 2020 Census") +
  scale_fill_viridis_c(direction = -1) +
   theme_bw()</code>
  
 ![](https://github.com/roncampbell/NICAR2022/blob/images/BlackCountyMap4.png)

R has several packages for interactive graphics. One of my favorities is tmap. It has two modes -- "plot" for static maps and "view" for interactive. We will use both in this class.

Let's take another look at the percentage of Black residents in Georgia counties, this time using tmap in interactive mode.
    
<code>tmap_mode("view")</code>
    
<code>tm_shape(ga_race_map) +
  tm_fill(col = "Black_per", palette = "viridis", alpha = 0.5)</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/tmap1.png)
  
When we click on a county, we get a semi-informative popup. But we can customize the popup to provide more information:
  
<code>tm_shape(ga_race_map) +
  tm_fill(col = "Black_per", palette = "viridis", alpha = 0.5,
          popup.vars = c("County" = "NAME.x", 
  "Black (%)" = "Black_per"))</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/tmap2.png)
  
We've been making choropleth maps, in which shades represent numeric values. But there are other tools for visualizing data on maps. One method: bubbles, with larger bubbles representing greater values. We'll build a bubble map for Fulton County.
  
First, let's join Fulton data with a Fulton County shapefile.
  
<code>fulton_tract_race_map <- left_join(fulton_tracts20, 
                                   fulton_races,
                               by = "GEOID")</code>
  
Now we'll create the map.
  
<code>tm_shape(fulton_tract_race_map) +
  tm_polygons() +
  tm_bubbles(size = "Black_per", alpha = 0.2, col = "green")</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/Fulton1.png)
  
As usual with tmap, the popups could use some improvements.
  
<code>tm_shape(fulton_tract_race_map) +
  tm_polygons() +
  tm_bubbles(size = "Black_per", alpha = 0.2, col = "green",
  popup.vars = c("County", "Tract", "Black (%)" = "Black_per"))</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/Fulton2.png) 
  
Another way to visualize data on a map is to simplify. The Census Bureau reports a lot of information for census tracts, but on a map tracts are almost impossibly hard to visualize. Every tract has boundary lines. In Fulton County there are 327 tracts, and in the Atlanta metro there are 728 tracts. No wonder the maps seem a little cluttered.
  
One alternative is to represent each tract by a single point or centroid. Another alternative is to use facets, displaying different pieces of the data side by side. We'll use centroids and facets together to show where the major racial groups live in the Atlanta metro.
  
First, we'll convert the Atlanta metro tract map from polygons with boundaries into centroids. 
  
<code>metro_centroids20 <- st_centroid(metro_tracts20)</code>
    
Next, we'll convert the metro race table into long format using tidyr; this will make it easier to map each race with a single block of code.
  
<code>metro_tract_race_long <- metro_tract_races %>% 
  select(GEOID, White = White_per, Black = Black_per, 
         Hispanic = Hispanic_per) %>% 
  pivot_longer(!GEOID, names_to = "Race", 
  values_to = "Percent")</code>
  
Now that the data and the shapefile are both ready, we'll join them using their common field, GEOID.
  
<code>metro_tract_race_long_map <- inner_join(metro_tracts20,
                                        metro_tract_race_long,
                                   by = "GEOID")</code>
  
We're going to build this map in tmap in static mode. Unfortunately, it does not work in interactive mode. Believe me, I tried.
  
<code>tmap_mode("plot")</code>
  
Now we'll build a map showing side-by-side the distribution of the primary racial groups in the Atlanta metro area. 
  
<code>tm_shape(metro_tract_race_long_map) +
  tm_facets(by = "Race", scale.factor = 4) +
  tm_fill(col = "Percent",
          style = "quantile",
          n= 5,
  palette = "Greens") </code>
  
 ![](https://github.com/roncampbell/NICAR2022/blob/images/MetroTractRaces1.png) 
  
"Facets" are like tabs in a spreadsheet or pages in a book. The key thing to look for in the code above is the term (by = "Race"). Remember that when we changed metro_tract_race into long format, we created a new column called "Race". That column has values like "White", "Black" and "Hispanic", and the code is using the column to create small maps, or facets, based on what it finds in that column. It then assigns a color to the value in the next column, "Percent", broken down into one of five quantiles, and the colors are shades of, you guessed it, green.
  
But the legend is off by itself. We can do a little better.
  
<code>tm_shape(metro_tract_race_long_map) +
  tm_facets(by = "Race", scale.factor = 4) +
  tm_fill(col = "Percent",
          style = "quantile",
          n= 5,
          palette = "Greens") +
  tm_layout(bg.color = "grey", 
            legend.position = c(-0.7, 0.2),
  panel.label.bg.color = "white")</code>
  
![](https://github.com/roncampbell/NICAR2022/blob/images/MetroTractRaces2.png)
  

  