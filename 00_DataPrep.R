library(dplyr)
library(RSQLite)
library(tidycensus)
library(tidyverse)
library(stringr)
library(sf)

# census_api_key("YOUR_API_KEY_HERE", install = TRUE)

# Connect to GTFS database
db <- dbConnect(SQLite(), "GTFS.sql")

## Retrieve route-level information from the GTFS database ---------------------

# Retrieve all route IDs and route types from the GTFS database
routes <- dbGetQuery(db, "SELECT route_id, id FROM routes")

# Aggregate walkscore to routes 
# Walkscore was previously collected via their API
routes$ws <- 0

for(i in routes$route_id) {
	# Select all stops that are part of this route for any trip during the week
	routes$ws[routes$route_id == i] <-   
		dbGetQuery(db, paste0(
      "SELECT avg(ws) AS ws_mean FROM stops_wlkscr WHERE 
      stop_id IN (
    	SELECT DISTINCT stop_id FROM stop_times WHERE trip_id IN (
    	SELECT trip_id FROM trips WHERE route_id = \"", i, "\"))"))
}

routes$ws <- as.numeric(unlist(routes$ws))
model.data <- read.table(
  "data/RidershipCensusComparison_ToModel.csv", 
  sep = ",", header = TRUE, row.names = NULL, stringsAsFactors = FALSE)

model.data <- left_join(model.data, routes, by = "route_id")
model.data$ws <- unlist(model.data$ws)

# Read in LEHD 2011 jobs and resident data
lehd.2011 <- read.table(
  "data/ServiceAreas_JobsWithin_LEHD_Join.csv", sep = ",", 
  header = TRUE, row.names = NULL, stringsAsFactors = FALSE)

model.data <- left_join(model.data, lehd.2011, by = "route_id")

## Gather census data ----------------------------------------------------------

# Use areal weighting to generate a median income at the route-level
# using Census 2010 SF1 and the 2008-2012 five-year ACS estimtes

# hhincvbles <- c("B19013_001", # median
#                "B19013H_001", # white alone, not hispanic/latino
#                "B19013B_001", # black alone
#                "B19013I_001") # Hispanic or latino

med.hh.income <- get_acs(
  geography = "block group", variables = "B19013_001", state = "AZ", 
  county = "Maricopa", geometry = TRUE)

# Looks like very little race-specific information at the block group level 

maricopa.blocks <- get_decennial(
  geography = "block", variables = "P0010001", state = "AZ", 
  county = "Maricopa", geometry = TRUE)

maricopa.blocks$bgid <- str_sub(maricopa.blocks$GEOID, 1, 12)

merged <- left_join(
  maricopa.blocks, 
  select(st_set_geometry(med.hh.income, NULL), GEOID, medinc = estimate), 
  by = c("bgid" = "GEOID"))

merged <- st_transform(merged, "+init=epsg:26912")
merged$orig_area <- st_area(merged)

# households -- P0220001

# Read in route buffers
route.buffers <- st_read("output/ServiceAreas_Stops.shp")
route.buffers <- st_transform(route.buffers, "+init=epsg:26912")

# Calculate the geometric intersection of the blocks and the routes
# The dataframe contains the population in the block (value), the median income 
# of the block group (medinc), and the original area of the block (orig_area)
intersect <- st_intersection(route.buffers, merged)

route.stats <- 
  intersect %>%
  mutate(new_area = st_area(intersect), hhs = new_area / orig_area * value) %>%
  group_by(route_id) %>%
  dplyr::summarize(
    wtdinc = sum(hhs * medinc, na.rm = TRUE) / sum(hhs, na.rm = TRUE))

route.stats$wtdinc <- as.numeric(route.stats$wtdinc)

ggplot(route.stats, aes(y = route_id, x = wtdinc)) + geom_point()                    

route.stats <- st_set_geometry(route.stats, NULL)

write.table(route.stats, "output/RouteStatsIncome.csv", sep = ",", 
            row.names = FALSE)
# route.stats <- read.table("output/RouteStatsIncome.csv", sep = ",", 

# Join route-level population-weighted median income data to the model data
model.data <- left_join(model.data, route.stats)

# Create required covariates ---------------------------------------------------

model.data <- mutate(
   model.data,
   mode_collapse = 
     ifelse(route_type %in% c("Express", "Rapid", "BRT"), 3, 
            ifelse(route_type == "Light rail", 2, 1)),
   mode_collapse_f = as.factor(mode_collapse),
   pct_midinc_c = (totalhhs_c - above50k_c - below25k_c) / totalhhs_c,
   midinc_prop_r = (total_r - above50_r - below20_r) / total_r,
   totrid100 = total_r / 1000,
   totjobs = (CE01_w + CE02_w + CE03_w) / 1000,
   cd01_1000 = CD01_w / 1000,
   hiwagejobs = (CNS09_w + CNS10_w + CNS12_w + CNS13_w) / 1000,
   lowagejobs = (CNS07_w + CNS14_w + CNS17_w + CNS18_w + CNS19_w) / 1000,
   ce01 = CE01_w / 1000,
   ce12 = (CE01_w + CE02_w) / 1000,
   ce03 = CE03_w / 1000,
   hiwageshare = hiwagejobs / totjobs,
   lowageshare = lowagejobs / totjobs,
   resdens = (total_c / 1000) / (Shape_Area / 1e6),
   jobdens = totjobs / (Shape_Area / 1e6), # Units are 1000 jobs/km2
   lwjobdens = lowagejobs / (Shape_Area / 1e6),
   hwjobdens = hiwagejobs / (Shape_Area / 1e6),
   ce01dens = ce01 / (Shape_Area / 1e6),
   ce03dens = ce03 / (Shape_Area / 1e6),
   ce12dens = ce12 / (Shape_Area / 1e6),
   wtdinc1 = wtdinc / 1000)

model.data <- inner_join(model.data, route.stats, by = c("route_id.1" = "route_id"))

write.table(model.data, "data/RidershipCensusComparison_Model_FINAL.csv", 
            sep = ",", row.names = FALSE)