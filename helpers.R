library(shiny)
library(shinyWidgets)
library(rio)
library(tidyverse)
library(ggrepel)
library(stringr)
library(lubridate)
library(tidyr)
library(magrittr)
library(plotly)
library(lme4)
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

# for using the fitted coefficients
growth_m1 <- function(x, intercept, slope) {intercept*(slope^(x-1))}

# estimate growth curve
estimate_exponential_curves <- function(df, target="cum_cases", random_slopes=FALSE) {
	# decrease "day_since_start" by 1 to get a meaningful intercept
	df$day_since_start_m1 <- df$day_since_start - 1
	n_countries <- length(unique(df$country))
	
	if (n_countries == 1) {
		# single country
		f <- formula(paste0("log(", target, ") ~ 1 + day_since_start_m1"))
		fit <- lm(f, data=df)
		intercept <- coef(fit)[1]
		slope <- coef(fit)[2]
		RE <- NULL
	} else {
		# multiple country: do mixed effects model
		if (random_slopes == TRUE) {
			f <- formula(paste0("log(", target, ") ~ 1 + day_since_start_m1 + (1 + day_since_start_m1 | country)"))
		} else {
			f <- formula(paste0("log(", target, ") ~ 1 + day_since_start_m1 + (1 | country)"))
		}
	
	  fit <- lmer(f, data=df)
		intercept <- fixef(fit)[1]
		slope <- fixef(fit)[2]
		RE <- coef(fit)$country
	}
	
  return(list(n_countries = n_countries, fit=fit, intercept=intercept, slope=slope, RE=RE))
}


# not used yet:
estimate_logistic_curve <- function(day, cases) {
	model <- drm(cases ~ day, fct = L.3())
	return(model)
}


get_nth_label <- function(x) {
	if (x==1) {
		return("1st")
	} else if (x==2) {
		return("2nd")
	} else if (x==3) {
		return("3rd")
	} else if (x > 3) {
		return(paste0(x, "th"))
	} else {
		return("Error")
	}
}