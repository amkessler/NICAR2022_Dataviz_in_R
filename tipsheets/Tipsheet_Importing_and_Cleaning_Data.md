# Importing and cleaning data in R

All the data and maps you need for this class are already in this Github repo. But learning how to find, import and, most importantly, clean Census data are valuable skills. We'll take a little time to demonstrate, using the files for this class as examples.

First, as always with R, we'll grab a few packages to make life easy - the tidyverse and two Census-centric libraries, tidycensus and tigris. The only thing you need, aside from the packages themselves, is a free Census API key; you can get that by signing up here: <https://api.census.gov/data/key_signup.html> You can install it as a default in tidycensus.

Remember that you must install packages before you can use them. To do so, make sure you have a working web connection and then type this command at the console prompt: <code>install.packages("xxx")</code> where xxx (in quote marks) is the name of the package.

<code>library(tidyverse)</code>

<code>library(tidycensus)</code>

<code>library(tigris)</code>

First, we'll import maps (shapefiles) using the tigris package. By default, Census Bureau maps extend offshore; we want maps trimmed to the coastline, known as cartographic boundary (CB) maps. We'll begin by importing a shapefile of Georgia counties.

<code>ga_counties <- counties("GA", cb = TRUE)</code>

Next, we'll filter that map to get just the eight counties in the Atlanta metro area, using their FIPS codes to identify them.

<code>metro_counties <- ga_counties %>% 
  filter(COUNTYFP %in% c("121","135","067","089","063","057",
  "117","151"))</code>

We also want 2020 census tracts for the metro counties. We have to specify the year because the default is 2019.

<code>metro_tracts20 <- tracts("GA", county = c("121","135","067","089",
                                          "063","057","117","151"), 
                        cb = TRUE, year = 2020)</code>

Finally, we'll filter the metro area tract maps to get just the tracts in Fulton County. Again, we'll use the FIPS code.

<code>fulton_tracts20 <- metro_tracts20 %>% 
  filter(COUNTYFP == '121')</code>

Data journalists do not live by maps alone. We also hunger for data to hang on those maps. And for this, we have tidycensus to lead us through the Census Bureau maze. We're going to grab a slice of the 2020 redistricting file for Georgia - specifically the Hispanic and NonHispanic by Race file (Table P2). That table has 73 columns. We're going to drastically pare it down to just a handful. If you want to know the number of people of six races by census tract, you're on your own.

Use tidycensus to grab redistricting file by county.
  
<code>ga_race20 <- tidycensus::get_decennial(geography = "county", 
                                       year = 2020,
                                       table = 'P2',
                                       cache_table = TRUE,
                                       state = '13',
                   geometry = FALSE)</code> 

Simplify the categories from 73 to 9; the ninth category, multiracial, summarizes all the data in the 10th through 73rd categories.
  
<code>ga_races <- ga_race20 %>% 
  filter(variable %in% c("P2_001N", "P2_002N", "P2_005N", "P2_006N",
                         "P2_007N", "P2_008N", "P2_009N",
                          "P2_010N", "P2_011N")) %>% 
  pivot_wider(names_from = variable, values_from = value)</code>

Replace census variables with labels. Use `load_variables(2020, 'pl', cache=TRUE)` for reference.

<code>colnames(ga_races)[2] <- 'County'
colnames(ga_races)[3] <- 'Total'
colnames(ga_races)[4] <- 'Hispanic'
colnames(ga_races)[5] <- 'White'
colnames(ga_races)[6] <- 'Black'
colnames(ga_races)[7] <- 'AmerInd'
colnames(ga_races)[8] <- 'Asian'
colnames(ga_races)[9] <- 'PacIsl'
colnames(ga_races)[10] <- 'Other' 
                          colnames(ga_races)[11] <- 'Multiracial'</code>

Calculate percentages for largest racial groups
  
<code>ga_races <- ga_races %>% 
  mutate(White_per = 100 * (White / Total),
         Black_per = 100 * (Black / Total),
         Hispanic_per = 100 * (Hispanic / Total),
  Asian_per = 100 * (Asian / Total))</code>
         
Getting a filtered table for the eight Atlanta metro counties is easy now that we've built the statewide table. All we need to do is check the FIPS codes for the metro counties.

<code>metro_co_races <- ga_races %>% 
  filter(GEOID %in% c('13057', '13063', '13067', '13089', '13117',
  '13121', '13135', '13151'))</code>
                      
Now let's get race data at the tract level for the metro counties. We'll use tidycensus again. We specify the FIPS codes for the state ('13') and the counties separately this time.

<code>metro_tract_race <- get_decennial(geography = "tract", 
                                year = 2020,
                                table = 'P2',
                                cache_table = TRUE,
                                state = '13',
                                county = c('057', '063', 
                                           '067', '089', 
                                           '117',
                                           '121', '135', 
                          '151'))</code>
                                           
The data frame is a bit messy. It looks like this.

![](https://github.com/roncampbell/NICAR2022/blob/images/metro_tract_race.png)

We can clean it up with just a few lines of code. The first "mutate" line removes ", Georgia" from the name field because all the counties are in Georgia. The second line creates a new column, County, and fills it with one or more words followed by "County" from the Name column. The third line removes everything from the Name column after the ",", leaving just the tract name. 

<code>metro_tract_race <- metro_tract_race %>% 
  mutate(NAME = str_remove(NAME, ", Georgia"),
         County = str_extract(NAME, "[A-Z,a-z]+ County$"),
  NAME = str_remove(NAME, ", .*$"))</code>

Rearrange the columns and rename the Name column to Tract.
  
<code>metro_tract_race <- metro_tract_race[,c(1,5,2:4)]</code>
<code>colnames(metro_tract_race)[3] <- 'Tract'</code>

![](https://github.com/roncampbell/NICAR2022/blob/images/metro_tract_race2.png)

The next steps with the metro_tract_race table are exactly the same as what we did with the ga_races table:
  
  1) Reduce the number of racial categories from 73 to 9. 
  2) Replace census variable names with plain-English variable names.
  3) Calculate racial percentages.
  
After you have taken those three steps, create a table of Fulton County tracts by filtering metro_tract_race.
  
<code>fulton_tract_race <- metro_tract_race %>% 
  filter(County == 'Fulton County')</code>
  
Now we're going to switch gears from the 2020 Census to the American Community Survey 2019 5-Year Estimates. (The 2020 ACS is due for release in mid to late March, shortly after NICAR.) Once again, tidycensus is our one-stop shopping center for census data. We'll begin by getting median household income for the Atlanta metro counties.
  
<code>metro_county_inc <- get_acs(geography = "county",
                            variables = "B19013_001",
                            year = 2019,
                            state = "GA",
                            county = c('057', '063', '067', '089',
                                       '117', '121', '135', '151'),
                          survey = "acs5")</code>
  
A couple of footnotes: The code above calls for a specific variable in a specific table, B19013_001, median household income in the past 12 months, from "acs5", meaning the American Community Survey 5-Year Estimates, for the year 2019 for eight specified Georgia counties. But how did I know what to ask for among the thousands of tables the Census Bureau offers? The answer is that tidycensus has an insanely useful tool called "load_variables". You can get a list of every variable in every table in the ACS and the decennial census by specifying the year, the dataset and whether you want to cache it. For details see <https://walker-data.com/tidycensus/reference/load_variables.html>. (Not mentioned in the website, if you want the variables for the redistricting file, the dataset is "pl" as in "public law.")

The file requires a little cleaning. We already did something similar with metro_tract_race. 
  
<code>metro_county_inc <- metro_county_inc %>% 
  mutate(NAME = str_remove(NAME, ", .*$"))</code>
  
Rearrange the columns and rename them.
  
<code>metro_county_inc <- metro_county_inc[, c(1:2,4:5)]</code>
  
<code>colnames(metro_county_inc)[2] <- 'County'</code>
  
<code>colnames(metro_county_inc)[3] <- 'MedianHHInc'</code>
    
Now we'll get median household income for census tracts in the Atlanta metro. Our script is almost the same as before, except that the geography is "tract" instead of "county".
  
<code>metro_tract_inc <- get_acs(geography = "tract",
                           variables = "B19013_001",
                           year = 2019,
                           state = "GA",
                           county = c('057', '063', '067', '089',
                                      '117', '121', '135', '151'),
                         survey = "acs5")</code>
  
And of course it requires a little minor cleaning, but nothing we haven't seen before.
  
<code>metro_tract_inc <- metro_tract_inc %>% 
  mutate(NAME = str_remove(NAME, ", Georgia"),
         County = str_extract(NAME, "[A-Z,a-z]+ County$"),
  NAME = str_remove(NAME, ", .*$"))</code>
  
Now we rearrange and rename the columns, and we're done with importing and cleaning!
  
<code>metro_tract_inc <- metro_tract_inc[,c(1,6,2,4:5)]</code>
    
<code>colnames(metro_tract_inc)[3] <- 'Tract'</code>
  
<code>colnames(metro_tract_inc)[4] <- 'MedHHInc'</code>
