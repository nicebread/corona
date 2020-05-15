## ======================================================================
## Load newest data files
## ======================================================================

source("helpers.R")

## ======================================================================
## Data sources:
## European Centre for Disease Prevention and Control
## https://www.ecdc.europa.eu/en/publications-www/download-todays-data-geographic-distribution-covid-19-cases-worldwide

## Johns Hopkins CSSE:
## https://github.com/CSSEGISandwww/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
## ======================================================================
	
## ======================================================================
## Download latest case/death data
## ======================================================================

today <- Sys.Date()
recent_ECDC_file <- list.files("data/ECDC_data/", pattern="ECDC") %>% tail(1)
recent_CSSE_confirmed_file <- list.files("data/CSSE_data/", pattern="CSSE_confirmed") %>% tail(1)
recent_CSSE_deaths_file <- list.files("data/CSSE_data/", pattern="CSSE_deaths") %>% tail(1)
recent_CSSE_recovered_file <- list.files("data/CSSE_data/", pattern="CSSE_recovered") %>% tail(1)

last_ECDC_download <- recent_ECDC_file %>% str_match(pattern="ECDC_(.*)\\.xlsx") %>% extract2(2)
last_CSSE_confirmed_download <- recent_CSSE_confirmed_file %>% str_match(pattern="_confirmed_(.*)\\.csv") %>% extract2(2)

downloadFlag <- FALSE  # is set tto TRUE if a new file has been downloaded

print("Checking and possibly update ECDC data.")
# try ten days from today backwards to download the latest data file	
for (backwards in 0:10) {
	dataDate <- today - backwards
	if (dataDate > last_ECDC_download) {
	  print(paste0("Try to download data file from ", dataDate))
		tryCatch({
		  download.file(paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", dataDate, ".xlsx"), destfile=paste0("data/ECDC_data/ECDC_", dataDate, ".xlsx"))
			ECDC_dataDate <- dataDate
			downloadFlag <- TRUE
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
	
	tryCatch({
		download.file("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", destfile=paste0("data/CSSE_data/CSSE_confirmed_", today, ".csv"))	
	
		download.file("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", destfile=paste0("data/CSSE_data/CSSE_deaths_", today, ".csv"))	
	
		download.file("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", destfile=paste0("data/CSSE_data/CSSE_recovered_", today, ".csv"))	
	
		recent_CSSE_confirmed_file <- list.files(pattern="CSSE_confirmed") %>% tail(1)
		recent_CSSE_deaths_file <- list.files(pattern="CSSE_deaths") %>% tail(1)
		recent_CSSE_recovered_file <- list.files(pattern="CSSE_recovered") %>% tail(1)
		last_CSSE_confirmed_download <- last_CSSE_deaths_download <- today
		downloadFlag <- TRUE
	},
	error=function(cond) {
		  message(paste("ECDC URL does not seem to exist: ", cond))
	})
} else {
	print("No updated CSSE file available.")
}


