library(dplyr)
library(RSQLite)
library(walkscoreAPI)
# library(stargazer) # Output regression results. http://www.princeton.edu/~otorres/NiceOutputR.pdf
# library(effects) # Generate marginal effects for plotting
# library(ggplot2)
# library(reshape2)
# library(sandwich)

# Connect to GTFS database
db <- dbConnect(SQLite(), "data/GTFS/Valley Metro/05192014 download/GTFS.sql")

# Data preparation -------------------------------------------------------------

# Query to retrieve all route IDs and route types from the GTFS database
routes <- dbGetQuery(db, "SELECT route_id, id FROM routes")

# Aggregate walkscore to routes 
routes$ws <- 0

for(i in routes$route_id) {
	# Select all stop that are part of this route for any trip during the week
	routes$ws[routes$route_id == i] <-   
		dbGetQuery(db, paste0("SELECT avg(ws) AS ws_mean FROM stops_wlkscr WHERE 
      stop_id IN (
    	SELECT DISTINCT stop_id FROM stop_times WHERE trip_id IN (
    	SELECT trip_id FROM trips WHERE route_id = \"", i, "\"))"))
}

routes$ws <- as.numeric(unlist(routes$ws))
model.data <- read.table("data/RidershipCensusComparison_ToModel.csv", sep = ",", 
  header = TRUE, row.names = NULL, stringsAsFactors = FALSE)

model.data <- left_join(model.data, routes, by = "route_id")
model.data$ws <- unlist(model.data$ws)

# Read in LEHD 2011 jobs and resident data
lehd.2011 <- read.table("data/ServiceAreas_JobsWithin_LEHD_Join.csv", sep = ",", 
  header = TRUE, row.names = NULL, stringsAsFactors = FALSE)

model.data <- left_join(model.data, lehd.2011, by = "route_id")

# Merge in route-level population-weighted median income data
# Before you do this, you have to run 01_HousholdIncome.R 
# Or read the csv
# route.stats <- read.table("output/RouteStatsIncome.csv", sep = ",", 
#   header = TRUE, row.names = NULL)
model.data <- left_join(model.data, route.stats)

# Create required covariates ---------------------------------------------------------------------
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
   resdens = (total_c / 1000) / (Shape_Area / 1e6), # In response to final review comments
   jobdens = totjobs / (Shape_Area / 1e6), # Units are 1000 jobs/km2
   lwjobdens = lowagejobs / (Shape_Area / 1e6),
   hwjobdens = hiwagejobs / (Shape_Area / 1e6),
   ce01dens = ce01 / (Shape_Area / 1e6),
   ce03dens = ce03 / (Shape_Area / 1e6),
   ce12dens = ce12 / (Shape_Area / 1e6),
   wtdinc1 = wtdinc / 1000)


model.data <- inner_join(model.data, route.stats, by = c("route_id.1" = "route_id"))

write.table(model.data, "data/RidershipCensusComparison_Model_FINAL.csv", sep = ",", row.names = FALSE)

model.data <- read.table("data/RidershipCensusComparison_Model_FINAL.csv", 
                         header = TRUE, sep = ",", row.names = NULL)




# Compare walkscore and residential/job density
cor(model.data$ws, model.data$resdens)
cor(model.data$ws, model.data$jobdens)
