library(dplyr)
library(ggplot2)
# devtools::install_github("f1kidd/fmlogit")
library(fmlogit)

# Data import and cleaning -----------------------------------------------------
model.data <- read.table("data/RidershipCensusComparison_Model_FINAL.csv", 
                         header = TRUE, sep = ",", row.names = NULL)


fmnl_input <- model_data %>% 
  mutate(pct_other_c = 1 - pct_white_c - pct_black_c - pct_latino_c - pct_asian_c,
         pct_other_r = 1 - pct_white_r - pct_black_r - pct_latino_r - pct_asian_r)


fmnl_input <- model_data %>% 
  mutate(pct_other_c = 1 - pct_white_c - pct_black_c - pct_latino_c,
         pct_other_r = 1 - pct_white_r - pct_black_r - pct_latino_r)


x1 <-select(model_data, pct_latino_c, pct_black_c, pct_asian_c)
x <- select(model_data, pct_latino_c, pct_black_c, pct_asian_c, resdens,
            jobdens, mode_collapse_f)

# x <- select(model_data, pct_poc_c, resdens, jobdens, mode_collapse_f)

x$mode_collapse_f <- factor(x$mode_collapse_f)

Y <- select(model_data, pct_white_r, pct_black_r, pct_latino_r, pct_asian_r, pct_other_r)

# Latino and white are too closely correlated. 
# Check on how those data were constructed to make sure they're correct. 

# Modeling ---------------------------------------------------------------------
results0 <- fmlogit(Y, x1, MLEmethod = "BHHH")
summary(results0)

results1 <- fmlogit(Y, x, MLEmethod = "BHHH")
summary(results1)

# Can I use the mlogit package to build a model matrix in a form where
# every route has many rows and irrelevant observations have zeros in them to 
# model alternative-specific variables?



# Notes on interpreting coefficients:
# https://www.statalist.org/forums/forum/general-stata-discussion/general/1380908-interpretation-of-multivariate-fractional-regression

# Others
# https://stats.stackexchange.com/questions/183601/interpreting-proportions-that-sum-to-one-as-independent-variables-in-linear-regr


# Example ----------------------------------------------------------------------

data <- spending
X <- data[, 2:5] # Explanatory variables
y <- data[, 6:11] # Dependent variables
results1 <- fmlogit(y,X)
summary(results1)

# Diagnostics
this <- fmnl_input$pct_white_c + fmnl_input$pct_black_c + fmnl_input$pct_latino_c + 
  fmnl_input$pct_other_c

this2 <- fmnl_input$pct_white_r + fmnl_input$pct_black_r + fmnl_input$pct_latino_r + 
  fmnl_input$pct_other_r

ggplot(fmnl_input, aes(x = pct_latino_c, y = pct_other_c)) + geom_point()


ggplot(fmnl_input, aes(x = pct_latino_c, y = pct_white_c)) + geom_point()
ggplot(fmnl_input, aes(x = pct_poc_c, y = pct_latino_c)) + geom_point()

ggplot(fmnl_input, aes(x = pct_black_c, y = pct_white_c)) + geom_point()

ggplot(fmnl_input, aes(x = pct_latino_c, y = pct_latino_r)) + geom_point()
ggplot(fmnl_input, aes(x = pct_black_c, y = pct_black_r)) + geom_point()

cor(fmnl_input$pct_latino_c, fmnl_input$pct_other_c) # high
cor(fmnl_input$pct_latino_c, fmnl_input$pct_black_c) # high
cor(fmnl_input$pct_latino_c, fmnl_input$pct_white_c) # high
cor(fmnl_input$pct_latino_c, fmnl_input$pct_asian_c) # High

cor(fmnl_input$pct_black_c, fmnl_input$pct_other_c) # low
