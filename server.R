# 2020 Felix Schönbrodt, CC0 license (free to reuse without need of attribution)
# This data visualization is inspired by the Financial Times: https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441
library(shiny)
library(rio)
library(tidyverse)
library(ggrepel)
library(stringr)
library(lubridate)
library(tidyr)
library(magrittr)

# load population data: How many people live in each country?
pop <- import("data/API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv", header=TRUE)
pop$country <- pop[, 1]
pop$population <- pop[, "2018"]
pop <- pop %>% select(country, population) %>% arrange(country)
pop[pop$country == "Korea, Rep.", "country"] <- "Korea"
pop[pop$country == "United States", "country"] <- "USA"

today <- Sys.Date()
recent_ECDC_file <- list.files(pattern="ECDC") %>% tail(1)
recent_CSSE_file <- list.files(pattern="CSSE") %>% tail(1)
last_ECDC_download <- recent_ECDC_file %>% str_match(pattern="_(.*)\\.xls") %>% extract2(2)
last_CSSE_download <- recent_CSSE_file %>% str_match(pattern="_(.*)\\.csv") %>% extract2(2)

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
	
if (today < last_ECDC_download)	{	
	print("Downloading recent ECDC data file from Github ...")
	
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
} else {
	print("No updated ECDC file available.")
}
	
if (today < last_CSSE_download)	{
	print("Downloading recent CSSE data file from Github ...")
	download.file("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", destfile=paste0("CSSE_", today, ".csv"))	
	
	recent_CSSE_file <- list.files(pattern="CSSE") %>% tail(1)
	last_CSSE_download <- today
} else {
	print("No updated CSSE file available.")
}


# load data files
dat0_ECDC <- import(recent_ECDC_file)
dat0_CSSE <- import(recent_CSSE_file)

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
	
dat_ECDC <- inner_join(dat_ECDC, pop, by="country") %>%
  mutate(cum_cases_per_100000 = cum_cases / (population/100000))

ECDC_data_date <- max(dat_ECDC$date)


# ---------------------------------------------------------------------
#  Preprocess CSSE data

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
	
dat_CSSE <- inner_join(dat_CSSE, pop, by="country") %>%
  mutate(cum_cases_per_100000 = cum_cases / (population/100000))

CSSE_data_date <- max(dat_CSSE$date)


# ---------------------------------------------------------------------
#  Harmonize country labels between data sets	

dat_ECDC$country[dat_ECDC$country == "South Korea"] <- "Korea"
dat_ECDC$country[dat_ECDC$country == "United States of America"] <- "USA"

dat_CSSE$country[dat_CSSE$country == "Korea, South"] <- "Korea"
dat_CSSE$country[dat_CSSE$country == "US"] <- "USA"



# helper function for reference line
growth <- function(x, percGrowth=33, intercept=100) {intercept*(1 + percGrowth/100)^(x-1)}

# estimate growth curve
#day = dat_ECDC %>% filter(country=="Germany", cum_cases > 50) %>% pull("day_in_dataset")
#cases = dat_ECDC %>% filter(country=="Germany", cum_cases > 50) %>% pull("cum_cases_per_100000")

estimate_daily_growth_rate <- function(day, cases, min_cases) {
  fit_nls <- nls(cases ~ intercept*(1+b)^day, start = c(b = 0.30, intercept = min_cases))
  return(fit_nls)
}

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
	
	
	# on target change: update sliders
	observeEvent(input$target, { 
		isolate({
			if (input$target == "cum_cases") 
				updateSliderInput(session, "offset", min = 1, max = 1000, value = 100, step = 1)
			if (input$target == "cum_cases_per_100000") 
				updateSliderInput(session, "offset", value = 0.1, min = 0, max = 2, step = 0.05)	
		})
	})
	
	observeEvent(c(input$estimateGrowth, input$target, dat_selection()), {
	  print("estimation BUTTON")
	  isolate({
	    # decrease day_since_start by 1, so that it starts with 0, and the intercept is the actual intercept in the plot
			fit <- NULL
	    tryCatch({
				if (input$target == "cum_cases") {
					fit <- estimate_daily_growth_rate(day=dat_selection()$day_since_start-1, cases=dat_selection()$cum_cases, min_cases=input$start_cumsum)
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
	
	
	# the plot
	output$res <- renderUI({
		
	  print("PLOT:")
		print(str(dat_selection()))
	  
		if (nrow(dat_selection()) == 0) {
			return(list(h3("No data selected.")))
		}
	  
		y_label <- paste0("Cumulative number of confirmed cases", ifelse(input$logScale == TRUE, " (log scale)", ""), ifelse(input$target == "cum_cases_per_100000", ", per 100,000", ""))
		
		p1 <- ggplot(dat_selection(), aes_string(x="day_since_start", y=input$target, color="country")) + 
			geom_point() + 
			geom_line() + 
			geom_label_repel(aes(label = country_label), nudge_x = 1, na.rm = TRUE) + scale_color_discrete(guide = FALSE) +
			theme_bw() + 
			xlab(paste0("Days since ", input$start_cumsum, "th case")) + 
			ylab(y_label) +
			ggtitle(paste0("Visualization based on data from ", input$datasource, ". Data set from ", current_data_date()))
		
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
				
		return(tagList(
		  renderPlot(p1, res=100, height=600, width="auto")
		))
	
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