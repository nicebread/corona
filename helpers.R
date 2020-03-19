library(shiny)
library(rio)
library(tidyverse)
library(ggrepel)
library(stringr)
library(lubridate)
library(tidyr)
library(magrittr)
library(plotly)
#library(drc)
options(shiny.sanitize.errors = FALSE)

## ======================================================================
## Some helper functions
## ======================================================================

removeZero <- function(x) {
	x[x==0] <- NA
	return(x)
}


# helper function for exponential reference line
growth <- function(x, percGrowth=33, intercept=100) {intercept*(1 + percGrowth/100)^(x-1)}

# estimate growth curve
# Extract some data for testing the function
# day = dat_ECDC %>% filter(country %in% c("Germany"), cum_cases > 50) %>% pull("day_in_dataset")
# cases = dat_ECDC %>% filter(country %in% c("Germany"), cum_cases > 50) %>% pull("cum_cases")
# df = dat_ECDC %>% filter(country %in% c("Germany"), cum_cases > 50)

#summary(lm(log(cases) ~ 1 + day_in_dataset, data=df))
#summary(nls(cum_cases ~ intercept*(1+b)^day_in_dataset, start = c(b = 0.30, intercept = 50), data=df))

estimate_exponential_curve <- function(day, cases, min_cases) {
  fit_nls <- nls(cases ~ intercept*(1+b)^day, start = c(b = 0.30, intercept = min_cases))
  return(fit_nls)
}


estimate_logistic_curve <- function(day, cases) {
	model <- drm(cases ~ day, fct = L.3())
	return(model)
}
