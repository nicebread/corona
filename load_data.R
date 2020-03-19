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
	
# load country level population data: How many people live in each country?
pop <- import("data/population_data/API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv", header=TRUE)
pop$country <- pop[, 1]
pop$population <- pop[, "2018"]
pop <- pop %>% select(country, population) %>% arrange(country)
pop[pop$country == "Korea, Rep.", "country"] <- "South Korea"
pop[pop$country == "United States", "country"] <- "USA"
pop[pop$country == "Russian Federation", "country"] <- "Russia"
pop[pop$country == "Iran, Islamic Rep.", "country"] <- "Iran"
print(sort(unique(pop$country)))

# load in US state population data
# State population data from 2019 US Census: https://www.census.gov/www/datasets/time-series/demo/popest/2010s-state-total.html
state_pop = read.csv('data/population_data/state_population_estimates_2019.csv', stringsAsFactors = FALSE) %>%
  mutate(., population = as.numeric(gsub(',', '', population_2019))) %>%
  dplyr::select(., -population_2019)


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


print("Checking and possibly update ECDC data.")
# try ten days from today backwards to download the latest data file	
for (backwards in 0:10) {
	dataDate <- today - backwards
	if (dataDate > last_ECDC_download) {
	  print(paste0("Try to download data file from ", dataDate))
		tryCatch({

		  download.file(paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", dataDate, ".xls"), destfile=paste0("data/ECDC_data/ECDC_", dataDate, ".xlsx"))
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
	
	tryCatch({
		download.file("https://github.com/CSSEGISandwww/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", destfile=paste0("data/CSSE_data/CSSE_confirmed_", today, ".csv"))	
	
		download.file("https://github.com/CSSEGISandwww/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", destfile=paste0("data/CSSE_data/CSSE_deaths_", today, ".csv"))	
	
		download.file("https://github.com/CSSEGISandwww/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv", destfile=paste0("data/CSSE_data/CSSE_recovered_", today, ".csv"))	
	
		recent_CSSE_confirmed_file <- list.files(pattern="CSSE_confirmed") %>% tail(1)
		recent_CSSE_deaths_file <- list.files(pattern="CSSE_deaths") %>% tail(1)
		recent_CSSE_recovered_file <- list.files(pattern="CSSE_recovered") %>% tail(1)
		last_CSSE_confirmed_download <- last_CSSE_deaths_download <- today
	},
	error=function(cond) {
		  message(paste("ECDC URL does not seem to exist: ", cond))
	})
} else {
	print("No updated CSSE file available.")
}


# load data files
dat0_ECDC <- import(paste0("data/ECDC_data/", recent_ECDC_file))
dat0_CSSE_confirmed <- import(paste0("data/CSSE_data/", recent_CSSE_confirmed_file))
dat0_CSSE_deaths <- import(paste0("data/CSSE_data/", recent_CSSE_deaths_file))
dat0_CSSE_recovered <- import(paste0("data/CSSE_data/", recent_CSSE_recovered_file))

# ---------------------------------------------------------------------
#  preprocess the data sets, make common format

dat_ECDC <- dat0_ECDC %>% 
	rename(
		# assign shorted var names
		country = `Countries and territories`,
		new_cases = Cases,
		new_deaths = Deaths		
	) %>% 
	mutate(
		date = as.Date(paste0(Year, "-", Month, "-", Day))
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
	
dat_ECDC$dailyGrowth[is.nan(dat_ECDC$dailyGrowth) | is.infinite(dat_ECDC$dailyGrowth)] <- NA

dat_ECDC$country[dat_ECDC$country == "South_Korea"] <- "South Korea"
dat_ECDC$country[dat_ECDC$country == "United_States_of_America"] <- "USA"
dat_ECDC$country[dat_ECDC$country == "United_Kingdom"] <- "United Kingdom"
	
dat_ECDC <- inner_join(dat_ECDC, pop, by="country") %>%
  mutate(
		cum_cases_per_100000 = cum_cases / (population/100000),
		cum_deaths_per_100000 = cum_deaths / (population/100000)
	)

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

  
dat_CSSE0 <- dat_CSSE_combined %>%  group_by(country, date.original) %>%
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
	
dat_CSSE0$dailyGrowth[is.nan(dat_CSSE0$dailyGrowth) | is.infinite(dat_CSSE0$dailyGrowth)] <- NA
	
dat_CSSE0$country[dat_CSSE0$country == "Korea, South"] <- "South Korea"
dat_CSSE0$country[dat_CSSE0$country == "US"] <- "USA"

dat_CSSE <- inner_join(dat_CSSE0, pop, by="country") %>%
	mutate(
		cum_cases_per_100000 = cum_cases / (population/100000),
		cum_deaths_per_100000 = cum_deaths / (population/100000)
	)

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
  mutate(
		cum_cases_per_100000 = cum_cases / (population/100000),
		cum_deaths_per_100000 = cum_deaths / (population/100000)
	)
	
dat_CSSE_US_states$dailyGrowth[is.nan(dat_CSSE_US_states$dailyGrowth) | is.infinite(dat_CSSE_US_states$dailyGrowth)] <- NA
