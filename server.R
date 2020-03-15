# 2020 Felix Schönbrodt, MIT license
# This data visualization is inspired by the Financial Times: https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441
library(shiny)
library(rio)
library(tidyverse)
library(ggrepel)
library(stringr)
library(lubridate)
library(tidyr)
library(magrittr)
library(plotly)
options(shiny.sanitize.errors = FALSE)

# load country level population data: How many people live in each country?
pop <- import("data/API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv", header=TRUE)
pop$country <- pop[, 1]
pop$population <- pop[, "2018"]
pop <- pop %>% select(country, population) %>% arrange(country)
pop[pop$country == "Korea, Rep.", "country"] <- "Korea"
pop[pop$country == "United States", "country"] <- "USA"

# load in US state population data
# State population data from 2019 US Census: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html
state_pop = read.csv('data/state_population_estimates_2019.csv', stringsAsFactors = FALSE) %>%
  mutate(., population = as.numeric(gsub(',', '', population_2019))) %>%
  dplyr::select(., -population_2019)

today <- Sys.Date()
recent_ECDC_file <- list.files(pattern="ECDC") %>% tail(1)
recent_CSSE_confirmed_file <- list.files(pattern="CSSE_confirmed") %>% tail(1)
recent_CSSE_deaths_file <- list.files(pattern="CSSE_deaths") %>% tail(1)
recent_CSSE_recovered_file <- list.files(pattern="CSSE_recovered") %>% tail(1)

last_ECDC_download <- recent_ECDC_file %>% str_match(pattern="_(.*)\\.xls") %>% extract2(2)
last_CSSE_confirmed_download <- recent_CSSE_confirmed_file %>% str_match(pattern="_confirmed_(.*)\\.csv") %>% extract2(2)

## ======================================================================
## Download latest data
## ======================================================================

## ======================================================================
## Data sources:
## European Centre for Disease Prevention and Control
## https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

## Johns Hopkins CSSE:
## https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
## ======================================================================
	

print("Checking and possibly update ECDC data.")
# try ten days from today backwards to download the latest data file	
for (backwards in 0:10) {
	dataDate <- today - backwards
	if (dataDate > last_ECDC_download) {
	  print(paste0("Try to download data file from ", dataDate))
		tryCatch({
		  download.file(paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", dataDate, ".xls"), destfile=paste0("ECDC_", dataDate, ".xls"))
			ECDC_dataDate <- dataDate
			break;
		},
		error=function(cond) {
		  message(paste("ECDC URL does not seem to exist: ", cond))
		})
	} else {
	  print("No updated ECDC data found")
	  break;
	}
}

if (today > last_CSSE_confirmed_download)	{
	print("Downloading recent CSSE data file from GitHub ...")
	
	download.file("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", destfile=paste0("CSSE_confirmed_", today, ".csv"))	
	
	download.file("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", destfile=paste0("CSSE_deaths_", today, ".csv"))	
	
	download.file("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv", destfile=paste0("CSSE_recovered_", today, ".csv"))	
	
	recent_CSSE_confirmed_file <- list.files(pattern="CSSE_confirmed") %>% tail(1)
	recent_CSSE_deaths_file <- list.files(pattern="CSSE_deaths") %>% tail(1)
	recent_CSSE_recovered_file <- list.files(pattern="CSSE_recovered") %>% tail(1)
	last_CSSE_confirmed_download <- last_CSSE_deaths_download <- today
} else {
	print("No updated CSSE file available.")
}


# load data files
dat0_ECDC <- import(recent_ECDC_file)
dat0_CSSE_confirmed <- import(recent_CSSE_confirmed_file)
dat0_CSSE_deaths <- import(recent_CSSE_deaths_file)
dat0_CSSE_recovered <- import(recent_CSSE_recovered_file)

# ---------------------------------------------------------------------
#  preprocess the data sets, make common format

dat_ECDC <- dat0_ECDC %>% 
	rename(
		# assign shorted var names
		country = CountryExp,
		new_cases = NewConfCases,
		new_deaths = NewDeaths		
	) %>% 
	mutate(
		date = as.Date(DateRep)
	) %>% 
	group_by(country) %>% 
	arrange(country, date) %>% 
	mutate(
		cum_cases = cumsum(new_cases),
		cum_deaths = cumsum(new_deaths),
		overall_cum_cases = max(cum_cases),
		cum_cases_l1 = lag(cum_cases),
		dailyGrowth = cum_cases / cum_cases_l1 - 1,
		day_in_dataset = 1:n(),
		# country label only at the last data point of each timeline:
		country_label = if_else(day_in_dataset == max(day_in_dataset), as.character(country), NA_character_)
	)
	

dat_ECDC$country[dat_ECDC$country == "South Korea"] <- "Korea"
dat_ECDC$country[dat_ECDC$country == "United States of America"] <- "USA"
	
dat_ECDC <- inner_join(dat_ECDC, pop, by="country") %>%
  mutate(cum_cases_per_100000 = cum_cases / (population/100000))

ECDC_data_date <- max(dat_ECDC$date)


# ---------------------------------------------------------------------
#  Preprocess CSSE data


# ---------------------------------------------------------------------
#  merge three CSSE data sets

# transform to long format for country data
dat_CSSE_confirmed <- dat0_CSSE_confirmed %>% select(-3, -4) %>% pivot_longer(-c(1:2), names_to="date.original", values_to="cum_cases")
dat_CSSE_deaths <- dat0_CSSE_deaths %>% select(-3, -4) %>% pivot_longer(-c(1:2), names_to="date.original", values_to="cum_deaths")
dat_CSSE_recovered <- dat0_CSSE_recovered %>% select(-3, -4) %>% pivot_longer(-c(1:2), names_to="date.original", values_to="cum_recovered")

dat_CSSE_combined <- inner_join(dat_CSSE_confirmed, dat_CSSE_deaths) %>% inner_join(dat_CSSE_recovered)
colnames(dat_CSSE_combined)[2] <- c("country")

  
dat_CSSE <- dat_CSSE_combined %>%  group_by(country, date.original) %>%
	# aggregate countries which have multiple states in the data base
  summarise(
		cum_cases = sum(cum_cases),
		cum_deaths = sum(cum_deaths),
		cum_recovered = sum(cum_recovered)
	) %>%
  ungroup() %>%
	mutate(
		date = mdy(date.original)
	) %>% 
	group_by(country) %>% 
	arrange(country, date) %>% 
	mutate(
		overall_cum_cases = max(cum_cases),
		cum_cases_l1 = lag(cum_cases),
		dailyGrowth = cum_cases / cum_cases_l1 - 1,
		day_in_dataset = 1:n(),
		# country label only at the last data point of each timeline:
		country_label = if_else(day_in_dataset == max(day_in_dataset), as.character(country), NA_character_)
	)
	
dat_CSSE$country[dat_CSSE$country == "Korea, South"] <- "Korea"
dat_CSSE$country[dat_CSSE$country == "US"] <- "USA"

dat_CSSE <- inner_join(dat_CSSE, pop, by="country") %>%
  mutate(cum_cases_per_100000 = cum_cases / (population/100000))

CSSE_data_date <- max(dat_CSSE$date)


# US states: transform to long format with state-by-state US data
dat_CSSE_US_states <- dat_CSSE_combined %>%
  dplyr::select(., state = `Province/State`, everything()) %>%
  dplyr::filter(., country == 'US', !grepl(',',state), state != 'Grand Princess') %>% #filter out county-level data
  dplyr::select(-country) %>%
  #pivot_longer(-1, names_to="date.original", values_to="cum_cases") %>% 
  #  group_by(state, date.original) %>%
  #summarise(cum_cases = sum(cum_cases)) %>%
  ungroup() %>%
  mutate(
    date = mdy(date.original)
  ) %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(
    overall_cum_cases = max(cum_cases),
    cum_cases_l1 = lag(cum_cases),
    dailyGrowth = cum_cases / cum_cases_l1 - 1,
    day_in_dataset = 1:n(),
    # state label only at the last data point of each timeline:
    state_label = if_else(day_in_dataset == max(day_in_dataset), as.character(state), NA_character_),
    # keep country label as USA for all US state data
    country = 'USA',
    country_label = if_else(day_in_dataset == max(day_in_dataset), as.character(country), NA_character_)
  ) %>%
  left_join(state_pop, by = 'state') %>%
  mutate(cum_cases_per_100000 = cum_cases / (population/100000))





# helper function for exponential reference line
growth <- function(x, percGrowth=33, intercept=100) {intercept*(1 + percGrowth/100)^(x-1)}

# estimate growth curve
# Extract some data for testing the function
# day = dat_ECDC %>% filter(country %in% c("Germany", "Italy", "France"), cum_cases > 50) %>% pull("day_in_dataset")
# cases = dat_ECDC %>% filter(country %in% c("Germany", "Italy", "France"), cum_cases > 50) %>% pull("cum_cases")
# df = dat_ECDC %>% filter(country %in% c("Germany", "Italy", "France"), cum_cases > 50)

#summary(lm(log(cases) ~ 1 + day_in_dataset, data=df))
#summary(nls(cum_cases ~ intercept*(1+b)^day_in_dataset, start = c(b = 0.30, intercept = 50), data=df))

estimate_daily_growth_rate <- function(day, cases, min_cases) {
  fit_nls <- nls(cases ~ intercept*(1+b)^day, start = c(b = 0.30, intercept = min_cases))
  return(fit_nls)
}

shinyServer(function(input, output, session) {
  
  # Startup flags to indicate whether session has just started up (in which case use defaults) for both state/country
  startupflag <- TRUE
  startupflag_state <- TRUE
  
  # dat_startfilter stores the reduced data set (reduced by input$start_cumsum)
  dat_startfilter <- reactiveVal(data.frame())
	current_data_date <- reactiveVal(NA)  # stores the current data date for the selected data set. This is displayed in the heading of the plot.
	observe({
    print("dat_startfilter")
		
		dat <- NULL
		if (input$datasource == "ECDC") {
			dat <- dat_ECDC
			current_data_date(ECDC_data_date)
		}
		if (input$datasource == "CSSE") {
			dat <- dat_CSSE
			current_data_date(CSSE_data_date)
		}
		if (input$datasource == "CSSE_State") {
		  dat <- dat_CSSE_US_states
		  current_data_date(CSSE_data_date)
		}
		
    dat_startfilter(dat %>% filter(
      cum_cases >= input$start_cumsum,
      overall_cum_cases >= input$minCases
    ))
  })
  

# Country Selection -------------------------------------------------------

	# dynamically populate country selector, based on available choices AFTER the start_cumsum filter
  last_country_selection <- reactiveVal()
  observe({
    print(paste("last_country_selection update: ", paste(input$country_selection, collapse=", ")))
    last_country_selection(input$country_selection)
  })
  
  output$country_selector <- renderUI({
    print(paste0("SELECTOR; startupflag = ", startupflag))
    
    available_countries <- unique(dat_startfilter()$country)
    
    # keep the countries that were chosen before
    if (startupflag == FALSE) {
      selection <- intersect(isolate(last_country_selection()), available_countries)
    } else {
      # default value at app start
      print("SELECTOR: USING DEFAULT SELECTION")
      selection <- intersect(c("Iran", "Singapore", "United Kingdom", "Sweden", "Germany", "USA", "France", "Italy", "Spain", "Korea"), available_countries)
      startupflag <<- FALSE
    }
    
    tagList(checkboxGroupInput(inputId = "country_selection", #name of input
                               label = "Countries to display:",
                               choices = available_countries,
                               selected = selection))
  })
  observeEvent(input$selectAllCountries, {
    print("selectALL BUTTON")
    available_countries <- unique(dat_startfilter()$country)
    updateCheckboxGroupInput(session, "country_selection", selected = available_countries)
  })
  observeEvent(input$deselectAllCountries, {
    print("DEselectALL BUTTON")
    updateCheckboxGroupInput(session, "country_selection", selected = "")
  })
  

# State Selection ---------------------------------------------------------
	# dynamically populate state selector, based on available choices AFTER the start_cumsum filter
	last_state_selection <- reactiveVal()
	observe({
	  print(paste("last_state_selection update: ", paste(input$state_selection, collapse=", ")))
	  last_state_selection(input$state_selection)
	})
	
	output$state_selector <- renderUI({
	  print(paste0("SELECTOR; startupflag_state = ", startupflag_state))
	  available_states <- unique(dat_startfilter()$state)

	  # keep the states that were chosen before
	  if (startupflag_state == FALSE) {
	    selection <- intersect(isolate(last_state_selection()), available_states)
	  } else {
	    # default value at app start
	    print("SELECTOR: USING DEFAULT SELECTION")
	    # Default states at start based on highest totals as of 3/14/2020
	    selection <- intersect(c("California", "New York", "Massachusetts", "Washington"), available_states)
	  }
	  
	  tagList(checkboxGroupInput(inputId = "state_selection", #name of input
	                             label = "States to display:",
	                             choices = available_states,
	                             selected = selection))
	})
	observeEvent(input$selectAllStates, {
	  print("selectALL BUTTON")
	  available_states <- unique(dat_startfilter()$state)
	  updateCheckboxGroupInput(session, "state_selection", selected = available_states)
	})
	observeEvent(input$deselectAllStates, {
	  print("DEselectALL BUTTON")
	  updateCheckboxGroupInput(session, "state_selection", selected = "")
	})
	
	# dat_selection stores the data set filterd by state selection
  	dat_selection <- reactiveVal(data.frame())
  	max_day_since_start <- reactiveVal(NA)
  	observe({
  	  print("DAT_SELECTION_STATE")
  	  if(input$datasource == 'CSSE_State'){
    	  if (!is.null(input$state_selection)) {
    	    print(input$state_selection)
    	    d0 <- dat_startfilter() %>% 
						filter(state %in% input$state_selection) %>% 
						mutate(cum_deaths_plus_one = cum_deaths + 1)
    	    
    	    if (nrow(d0) > 0) {
    	      dat_selection(d0 %>% mutate(day_since_start = 1:n()))
    	      max_day_since_start(max(dat_selection()$day_since_start))
    	    } else {
    	      dat_selection(data.frame())
    	    }
    	    
    	  } else {
    	    dat_selection(data.frame())
    	  }
  	  }else{
  	    print("DAT_SELECTION_COUNTRY")
  	    if (!is.null(input$country_selection)) {
  	      print(input$country_selection)
  	      d0 <- dat_startfilter() %>% 
						filter(country %in% input$country_selection) %>% 
						mutate(cum_deaths_plus_one = cum_deaths + 1)
  	      
  	      if (nrow(d0) > 0) {
  	        dat_selection(d0 %>% mutate(day_since_start = 1:n()))
  	        max_day_since_start(max(dat_selection()$day_since_start))
  	      } else {
  	        dat_selection(data.frame())
  	      }
  	      
  	    } else {
  	      dat_selection(data.frame())
  	    }
  	  }
  	 
  	})
  	
	# update offset slider when start_cumsum changes
	observe({
	  print("UPDATE OFFSET SLIDER")
	  val <- input$start_cumsum
	  updateSliderInput(session, "offset", value = val)
	})
	
	
	# on target change: update sliders
	observeEvent(input$target, { 
		isolate({
			if (input$target %in% c("cum_cases", "cum_deaths", "cum_recovered"))
				updateSliderInput(session, "offset", min = 0, max = 5000, value = 100, step = 5)
			if (input$target == "cum_cases_per_100000") 
				updateSliderInput(session, "offset", value = 0.1, min = 0, max = 10, step = 0.05)	
		})
	})
	
	
	# update the estimated growth whenever:
	# - button is pressed
	# - the data set selection changes
	# - the target variable changes
	observeEvent(c(input$estimateGrowth, input$target, dat_selection()), {
	  print("estimation BUTTON")
		if (isolate(input$showReferenceLine == FALSE)) {
			print("Skipping estimation, no reference line shown")
			return()
		}
	  isolate({
	    # decrease day_since_start by 1, so that it starts with 0, and the intercept is the actual intercept in the plot
			fit <- NULL
	    tryCatch({
				if (input$target == "cum_cases") {
					fit <- estimate_daily_growth_rate(day=dat_selection()$day_since_start-1, cases=dat_selection()$cum_cases, min_cases=input$start_cumsum)
				}
				if (input$target == "cum_deaths") {
					fit <- estimate_daily_growth_rate(day=dat_selection()$day_since_start-1, cases=dat_selection()$cum_deaths, min_cases=input$start_cumsum)
				}
				if (input$target == "cum_recovered") {
					fit <- estimate_daily_growth_rate(day=dat_selection()$day_since_start-1, cases=dat_selection()$cum_recovered, min_cases=input$start_cumsum)
				}
				if (input$target == "cum_cases_per_100000") {
					fit <- estimate_daily_growth_rate(day=dat_selection()$day_since_start-1, cases=dat_selection()$cum_cases_per_100000, min_cases=0.1)
				}
	    },
	    error=function(e) return(NULL)
	  )
	  })
	  
		if (!is.null(fit)) {
			print(summary(fit))
		  updateSliderInput(session, "offset", value = as.numeric(coef(fit)["intercept"]))
		  updateSliderInput(session, "percGrowth", value = as.numeric(coef(fit)["b"]*100))
		} else {
			print("no fit possible")
		}
	  
	})
	
	
	# ---------------------------------------------------------------------
	# the plot
	innerplot <- function() {
		
	  print("PLOT:")
		print(str(dat_selection()))
	  
		if (nrow(dat_selection()) == 0) {
			return(list(h3("No data selected.")))
		}
	  
		target_label <- switch(input$target, 
			"cum_cases" = "confirmed cases", 
			"cum_cases_per_100000" = "confirmed cases", 
			"cum_deaths" = "confirmed deaths")
		
		y_label <- paste0("Cumulative number of ", target_label, ifelse(input$logScale == TRUE, " (log scale)", ""), ifelse(input$target == "cum_cases_per_100000", ", per 100,000", ""))
		
	# For log scale: deaths +1 to avoid log error
	if (input$target == "cum_deaths" & min(dat_selection()$cum_deaths) == 0) {		
		real_target <- "cum_deaths_plus_one"
	} else {
		real_target <- input$target
	}
	
	print(paste0("Using target variable ", real_target))
		
	# Plot countries
	if (!'state' %in% names(dat_selection())) {
		p1 <- ggplot(dat_selection(), aes_string(x="day_since_start", y=real_target, color='country')) + 
			geom_point() + 
			geom_line() + 
			scale_color_discrete(guide = FALSE) +
			theme_bw() + 
			labs(
				title = paste0("Visualization based on data from ", input$datasource, ". Data set from ", current_data_date()),
			  subtitle = ifelse(real_target == "cum_deaths_plus_one" & input$logScale == TRUE, "Deaths increased by 1 to avoid log(0)", ""),
			  caption = "Source: http://shinyapps.org/apps/corona/", 
			  x = paste0("Days since ", input$start_cumsum, "th case"), y = y_label)
		if (input$usePlotly == FALSE) {
			p1 <- p1 + geom_label_repel(aes(label = country_label), nudge_x = 1, na.rm = TRUE)
		}
	} else { 
	  # Plot US states
	  p1 <- ggplot(dat_selection(), aes_string(x="day_since_start", y=input$target, color='state')) + 
	    geom_point() + 
	    geom_line() + 
	    scale_color_discrete(guide = FALSE) +
	    theme_bw() + 
			labs(
				title = paste0("Visualization based on data from CSSE US data by state. Data set from ", current_data_date()),
			  subtitle = ifelse(real_target == "cum_deaths_plus_one" & input$logScale == TRUE, "Deaths increased by 1 to avoid log(0)", ""),
			  caption = "Source: http://shinyapps.org/apps/corona/", 
			  x = paste0("Days since ", input$start_cumsum, "th case"), y = y_label)
	  if (input$usePlotly == FALSE) {
	  	p1 <- p1 + geom_label_repel(aes(label = state_label), nudge_x = 1, na.rm = TRUE)
	  }
	  startupflag_state <<- FALSE # Once one graph of states has been completed, turn off startup flag for states
	}
		if (input$logScale == TRUE) {
		  p1 <- p1 + coord_trans(y = "log10")
		}
		if (input$target == "cum_cases") {
			p1 <- p1 + scale_y_continuous(breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000))
		}
		if (input$target == "cum_cases_per_100000") {
			p1 <- p1 + scale_y_continuous()
		}
		
		if (input$showReferenceLine == TRUE) {
		  p1 <- p1 + 
		    stat_function(fun = growth, args=list(percGrowth=input$percGrowth, intercept=input$offset), color="black", linetype="dashed") +
		    annotate(label=paste0(input$percGrowth, "% growth rate"), x=max_day_since_start(), y=growth(max_day_since_start()+1, percGrowth=input$percGrowth, intercept=input$offset), geom="text", hjust=1)
		}
		
		return(p1)
	
	}
	output$resNormal <- renderPlot(innerplot())
	output$resInteractive <- renderPlotly(innerplot())
	output$normalPlot <- renderUI({
		tagList(
			plotOutput("resNormal", height="700px")
		)
	})
	output$interactivePlot <- renderUI({
		tagList(
			plotlyOutput("resInteractive", height="700px")
		)
	})

	output$DownloadFig <- downloadHandler(
	  filename = "COVID19.pdf",
	  content = function(file){
	    tempReport <- file.path(tempdir(), "COVID19.Rmd")
	    file.copy("COVID19.Rmd", tempReport, overwrite = TRUE)
	    params <- list(dat_selection = dat_selection(),
	                   logScale = input$logScale,
	                   start_cumsum = input$start_cumsum,
	                   datasource = input$datasource,
	                   current_data_date = current_data_date(),
	                   percGrowth = input$percGrowth,
	                   offset = input$offset,
	                   max_day_since_start = max_day_since_start(),
	                   showReferenceLine = input$showReferenceLine
	    )
	    rmarkdown::render(tempReport, output_file = file,
	                      params = params,
	                      envir = new.env(parent = globalenv())
	    )
	  }
	)
	
})
