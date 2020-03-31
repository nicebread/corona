today <- Sys.Date()
recent_ECDC_file <- list.files("data/ECDC_data/", pattern="ECDC") %>% tail(1)
recent_CSSE_confirmed_file <- list.files("data/CSSE_data/", pattern="CSSE_confirmed") %>% tail(1)
recent_CSSE_deaths_file <- list.files("data/CSSE_data/", pattern="CSSE_deaths") %>% tail(1)
recent_CSSE_recovered_file <- list.files("data/CSSE_data/", pattern="CSSE_recovered") %>% tail(1)


# load data files
dat0_ECDC <- import(paste0("data/ECDC_data/", recent_ECDC_file))
dat0_CSSE_confirmed <- import(paste0("data/CSSE_data/", recent_CSSE_confirmed_file))
dat0_CSSE_deaths <- import(paste0("data/CSSE_data/", recent_CSSE_deaths_file))
dat0_CSSE_recovered <- import(paste0("data/CSSE_data/", recent_CSSE_recovered_file))

# load country level population data: How many people live in each country?
pop <- import("data/population_data/API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv", header=TRUE)
pop$country <- pop[, 1]
pop$population <- pop[, "2018"]
pop <- pop %>% select(country, population) %>% arrange(country)
pop[pop$country == "Korea, Rep.", "country"] <- "South Korea"
pop[pop$country == "United States", "country"] <- "USA"
pop[pop$country == "Russian Federation", "country"] <- "Russia"
pop[pop$country == "Iran, Islamic Rep.", "country"] <- "Iran"
#print(sort(unique(pop$country)))

# load in US state population data
# State population data from 2019 US Census: https://www.census.gov/www/datasets/time-series/demo/popest/2010s-state-total.html
state_pop = read.csv('data/population_data/state_population_estimates_2019.csv', stringsAsFactors = FALSE) %>%
  mutate(., population = as.numeric(gsub(',', '', population_2019))) %>%
  dplyr::select(., -population_2019)


# ---------------------------------------------------------------------
#  preprocess the data sets, make common format

dat_ECDC <- dat0_ECDC %>% 
	rename(
		# assign shorted var names
		country = countriesAndTerritories,
		new_cases = cases,
		new_deaths = deaths		
	) %>% 
	mutate(
		date = as.Date(paste0(year, "-", month, "-", day))
	) 
	
	
dat_ECDC$country[dat_ECDC$country == "South_Korea"] <- "South Korea"
dat_ECDC$country[dat_ECDC$country == "United_States_of_America"] <- "USA"
dat_ECDC$country[dat_ECDC$country == "United_Kingdom"] <- "United Kingdom"
	
	
dat_ECDC <- dat_ECDC %>% 
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

	
dat_ECDC <- inner_join(dat_ECDC, pop, by="country") %>%
  mutate(
		cum_cases_per_100000 = cum_cases / (population/100000),
		cum_deaths_per_100000 = cum_deaths / (population/100000),
		cum_deaths_noZero = removeZero(cum_deaths),
		cum_deaths_per_100000_noZero = removeZero(cum_deaths_per_100000)
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

dat_CSSE_combined <- inner_join(dat_CSSE_confirmed, dat_CSSE_deaths) # %>% inner_join(dat_CSSE_recovered)
colnames(dat_CSSE_combined)[2] <- c("country")

dat_CSSE_combined$country[dat_CSSE_combined$country == "Korea, South"] <- "South Korea"
dat_CSSE_combined$country[dat_CSSE_combined$country == "US"] <- "USA"
dat_CSSE_combined$country[dat_CSSE_combined$country == "Taiwan*"] <- "Taiwan"

# This data set is a mess ...
# Why is there a province "Recovered" in Canada?
# dat_CSSE_combined %>% pull(country) %>% unique()
# dat_CSSE_combined %>% pull(country) %>% table()
# dat_CSSE_combined %>% pull(country) %>% table() %>% table()
# dat_CSSE_combined %>% filter(country=="USA") %>% print(n=200)
# dat_CSSE_combined %>% filter(country=="Canada") %>% print(n=1000)
# dat_CSSE_combined %>% filter(country=="Germany") %>% print(n=1000)
# dat_CSSE_combined %>% filter(`Province/State`=="Recovered") %>% print(n=1000)

  
dat_CSSE0 <- dat_CSSE_combined %>%  
	filter(
		!`Province/State` %in% c("Diamond Princess", "Recovered")
	) %>% 
	group_by(country, date.original) %>%
	# aggregate countries which have multiple states in the data base
  summarise(
		cum_cases = sum(cum_cases),
		cum_deaths = sum(cum_deaths)#,
		#cum_recovered = sum(cum_recovered)
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
	

dat_CSSE <- inner_join(dat_CSSE0, pop, by="country") %>%
	mutate(
		cum_cases_per_100000 = cum_cases / (population/100000),
		cum_deaths_per_100000 = cum_deaths / (population/100000),
		cum_deaths_noZero = removeZero(cum_deaths),
		cum_deaths_per_100000_noZero = removeZero(cum_deaths_per_100000)
	)

CSSE_data_date <- max(dat_CSSE$date)


# # US states: transform to long format with state-by-state US data
# dat_CSSE_US_states <- dat_CSSE_combined %>%
#   dplyr::select(., state = `Province/State`, everything()) %>%
#   dplyr::filter(., country == 'US', !grepl(',',state), state != 'Grand Princess') %>% #filter out county-level data
#   dplyr::select(-country) %>%
#   #pivot_longer(-1, names_to="date.original", values_to="cum_cases") %>%
#   #  group_by(state, date.original) %>%
#   #summarise(cum_cases = sum(cum_cases)) %>%
#   ungroup() %>%
#   mutate(
#     date = mdy(date.original)
#   ) %>%
#   group_by(state) %>%
#   arrange(state, date) %>%
#   mutate(
#     overall_cum_cases = max(cum_cases),
#     cum_cases_l1 = lag(cum_cases),
#     dailyGrowth = cum_cases / cum_cases_l1 - 1,
#     day_in_dataset = 1:n(),
#     # state label only at the last data point of each timeline:
#     state_label = if_else(day_in_dataset == max(day_in_dataset), as.character(state), NA_character_),
#     # keep country label as USA for all US state data
#     country = 'USA',
#     country_label = if_else(day_in_dataset == max(day_in_dataset), as.character(country), NA_character_)
#   ) %>%
#   left_join(state_pop, by = 'state') %>%
#   mutate(
# 		cum_cases_per_100000 = cum_cases / (population/100000),
# 		cum_deaths_per_100000 = cum_deaths / (population/100000),
		# cum_deaths_noZero = removeZero(cum_deaths),
		# cum_deaths_per_100000_noZero = removeZero(cum_deaths_per_100000)
# 	)
#
# dat_CSSE_US_states$dailyGrowth[is.nan(dat_CSSE_US_states$dailyGrowth) | is.infinite(dat_CSSE_US_states$dailyGrowth)] <- NA
