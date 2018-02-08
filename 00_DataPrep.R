library(dplyr)
library(RSQLite)
library(walkscoreAPI)
library(stargazer) # Output regression results. http://www.princeton.edu/~otorres/NiceOutputR.pdf
library(effects) # Generate marginal effects for plotting
library(ggplot2)
library(reshape2)
library(sandwich)
# library(Hmisc)

# setwd("D:/Dropbox/Work/FTA fare and service equity/")

# Function definitions ---------------------------------------------------------

robSE <- function(glmob) {
	sand_vcov <- sandwich(glmob)
	sand_se <- sqrt(diag(sand_vcov))
	print(sand_se) # Match Table II's "robust" standard errors

	print("--------------------------------------")
	
	robust_z <- glmob$coef / sand_se
	print(robust_z)
	
	# https://stats.stackexchange.com/questions/11676/pseudo-r-squared-formula-for-glms
	# https://modtools.wordpress.com/2014/10/30/rsqglm/
	# This isn't exactly a traditional R2
	r2 <- 1 - glmob$deviance/glmob$null.deviance
	
	# But this pearson correlation should be
	# https://stats.stackexchange.com/questions/87963/
	# does-the-slope-of-a-regression-between-observed-and-predicted-values-always-equa
	pears <- cor(glmob$y, glmob$fitted.values) ^ 2
	  
	print(paste0("r2:", r2))
	print(paste0("pearson:", pears))
}

corstars <- function(x) {
	x <- as.matrix(x)
	R <- rcorr(x)$r
	p <- rcorr(x)$P
 	mystars <- ifelse(p < .01, "**|", ifelse(p < .05, "* |", "  |"))
 	R <- format(round(cbind(rep(-1.111, ncol(x)), R), 3))[,-1]
 	Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
 	diag(Rnew) <- paste(diag(R), "  |", sep="")
 	rownames(Rnew) <- colnames(x)
 	colnames(Rnew) <- paste(colnames(x), "|", sep="")
 	Rnew <- as.data.frame(Rnew)
 	Rnew
}

# https://modtools.wordpress.com/2014/10/30/rsqglm/

RsqGLM <- function(obs = NULL, pred = NULL, model = NULL) {
  # version 1.2 (3 Jan 2015)
  
  model.provided <- ifelse(is.null(model), FALSE, TRUE)
  
  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
    
  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop ("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
    if (length(obs) != length(pred)) stop ("'obs' and 'pred' must be of the same length (and in the same order).")
    if (!(obs %in% c(0, 1)) | pred < 0 | pred > 1) stop ("Sorry, 'obs' and 'pred' options currently only implemented for binomial GLMs (binary response variable with values 0 or 1) with logit link.")
    logit <- log(pred / (1 - pred))
    model <- glm(obs ~ logit, family = "binomial")
  }
  
  null.mod <- glm(obs ~ 1, family = family(model))
  loglike.M <- as.numeric(logLik(model))
  loglike.0 <- as.numeric(logLik(null.mod))
  N <- length(obs)
  
  # based on Nagelkerke 1991:
  CoxSnell <- 1 - exp(-(2 / N) * (loglike.M - loglike.0))
  Nagelkerke <- CoxSnell / (1 - exp((2 * N ^ (-1)) * loglike.0))
  
  # based on Allison 2014:
  McFadden <- 1 - (loglike.M / loglike.0)
  Tjur <- mean(pred[obs == 1]) - mean(pred[obs == 0])
  sqPearson <- cor(obs, pred) ^ 2
  
  return(list(CoxSnell = CoxSnell, Nagelkerke = Nagelkerke, McFadden = McFadden, Tjur = Tjur, sqPearson = sqPearson))
}


# Connect to database
db <- dbConnect(SQLite(), "data/GTFS/Valley Metro/05192014 download/GTFS.sql")


# Get walkscores ---------------------------------------------------------------

stops <- read.table("output/Stops_Snapped2Streets_XY.csv", sep = ",", header = TRUE, row.names = NULL)

# Can only have 5000 API queries per day
# Reset the limits as required
for(i in 5001:7500) { 
	stops$WS[i] <- getWS(stops[i, "longitude"], stops[i, "latitude"], "cc5c893dd721068ffd1f8af23317bf17")$walkscore
}

# Add walkscore info to database
dbWriteTable(db, "stops_wlkscr", stops)

write.table(stops, "output/Stops_withWalkScore.csv", sep = ",", row.names = FALSE)

ws <- read.table("output/Stops_withWalkScore.csv", sep = ",", header = TRUE, row.names = NULL)

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
