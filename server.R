# 2020 Felix Schönbrodt, CC0 license (free to reuse without need of attribution)
# This data visualization is inspired by the Financial Times: https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441
library(shiny)
library(rio)
library(tidyverse)
library(ggrepel)
library(stringr)
library(lubridate)

today <- Sys.Date()
last_ECDC_download <- str_match(list.files(pattern="ECDC"), "_(.*)\\.xls")[1, 2]
last_CSSE_download <- str_match(list.files(pattern="CSSE"), "_(.*)\\.csv")[1, 2]

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
	
	
	# try ten days from today backwards to download the latest data file	
	ECDC_success <- FALSE
	for (backwards in 0:10) {
		dataDate <- today - backwards
		print(paste0("Try to download data file from ", dataDate))
		
		tryCatch({
			if (ECDC_success == FALSE) {
			  download.file(paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", dataDate, ".xls"), destfile=paste0("ECDC_", dataDate, ".xls"))
				ECDC_success <- TRUE
				ECDC_dataDate <- dataDate
			}
		  
			if (ECDC_success) break;
		},
		error=function(cond) {
		  message(paste("ECDC URL does not seem to exist: ", cond))
		}
		)
	}
	
download.file("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", destfile=paste0("CSSE_", today, ".csv"))	


# load data files
dat0_ECDC <- import(paste0("ECDC_", ECDC_dataDate, ".xls"))
dat0_CSSE <- import(paste0("CSSE_", today, ".csv"))

# preprocess the data sets, make common format
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
		overall_cum_cases = max(cum_cases),
		cum_cases_l1 = lag(cum_cases),
		dailyGrowth = cum_cases / cum_cases_l1 - 1,
		day_in_dataset = 1:n(),
		# country label only at the last data point of each timeline:
		country_label = if_else(day_in_dataset == max(day_in_dataset), as.character(country), NA_character_)
	)
dat_ECDC$country[dat_ECDC$country == "South Korea"] <- "Korea, South"

ECDC_data_date <- max(dat_ECDC$date)


dat0_CSSE <- dat0_CSSE %>% select(-1, -3, -4)
colnames(dat0_CSSE)[1] <- c("country")


# transform to long format
dat_CSSE <- dat0_CSSE %>% 
	pivot_longer(-1, names_to="date.original", values_to="cum_cases") %>% 
  
  # aggregate countries which have multiple states in the data base
  group_by(country, date.original) %>%
  summarise(cum_cases = sum(cum_cases)) %>%
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
	
CSSE_data_date <- max(dat_CSSE$date)

# helper function for reference line
growth <- function(x, percGrowth=33, intercept=100) {intercept*(1 + percGrowth/100)^(x-1)}

shinyServer(function(input, output, session) {
  
  startupflag <- TRUE
  
  # dat_startfilter stores the reduced data set (reduced by input$start_cumsum)
  dat_startfilter <- reactiveVal(data.frame())
	current_data_date <- reactiveVal(NA)
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
		
    dat_startfilter(dat %>% filter(
      cum_cases >= input$start_cumsum,
      overall_cum_cases >= input$minCases
    ))
  })
  
  
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
			selection <- intersect(c("Iran", "Singapore", "United Kingdom", "Sweden", "Germany", "United States of America", "France", "Italy", "Spain", "South Korea"), available_countries)
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
	
	
	# dat_selection stores the data set filterd by country selection
	dat_selection <- reactiveVal(data.frame())
	max_day_since_start <- reactiveVal(NA)
	observe({
	    print("DAT_SELCTION")
	    if (!is.null(input$country_selection)) {
	      print(input$country_selection)
	      d0 <- dat_startfilter() %>% filter(country %in% input$country_selection)
	      
	      if (nrow(d0) > 0) {
	        dat_selection(d0 %>% mutate(day_since_start = 1:n()))
	        max_day_since_start(max(dat_selection()$day_since_start))
	      } else {
	        dat_selection(data.frame())
	      }

	    } else {
	      dat_selection(data.frame())
	    }
	})
	
	# update offset slider when start_cumsum changes
	observe({
	  print("UPDATE OFFSET SLIDER")
	  val <- input$start_cumsum
	  updateSliderInput(session, "offset", value = val)
	})
	
	
	# the plot
	output$res <- renderUI({
		
	  print("PLOT:")
		print(str(dat_selection()))
	  
		if (nrow(dat_selection()) == 0) {
			return(list(h3("No data selected.")))
		}
	  
		p1 <- ggplot(dat_selection(), aes(x=day_since_start, y=cum_cases, color=country)) + 
			geom_point() + 
			geom_line() + 
			scale_y_continuous(breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000)) + 
			geom_label_repel(aes(label = country_label), nudge_x = 1, na.rm = TRUE) + scale_color_discrete(guide = FALSE) +
			theme_bw() + 
			xlab(paste0("Days since ", input$start_cumsum, "th case")) + 
			ylab("Cumulative number of diagnosed cases") +
			ggtitle(paste0("Visualization based on data from ", input$datasource, ". Data set from ", current_data_date()))
		
		if (input$logScale == TRUE) {
		  p1 <- p1 + coord_trans(y = "log10")
		}
		
		if (input$showReferenceLine == TRUE) {
		  p1 <- p1 + 
		    stat_function(fun = growth, args=list(percGrowth=input$percGrowth, intercept=input$offset), color="black", linetype="dashed") +
		    annotate(label=paste0(input$percGrowth, "% growth rate"), x=max_day_since_start(), y=growth(max_day_since_start()+1, percGrowth=input$percGrowth, intercept=input$offset), geom="text", hjust=1)
		}
				
		return(tagList(
		  renderPlot(p1, res=100, height=600, width="auto")
		))
	
	})
	
})