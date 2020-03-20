# 2020 Felix Schönbrodt, MIT license
# This data visualization is inspired by the Financial Times: https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441

source("helpers.R", local=TRUE)
source("load_data.R", local=TRUE)

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
  	  print("DAT_SELECTION_US_STATES")
  	  if(input$datasource == 'CSSE_State'){
    	  if (!is.null(input$state_selection)) {
    	    print(input$state_selection)
    	    d0 <- dat_startfilter() %>% 
						filter(state %in% input$state_selection) %>% 
						mutate(
							cum_deaths_noZero = removeZero(cum_deaths),
							cum_deaths_per_100000_noZero = removeZero(cum_deaths_per_100000)
						)
    	    
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
						mutate(
							cum_deaths_noZero = removeZero(cum_deaths),
							cum_deaths_per_100000_noZero = removeZero(cum_deaths_per_100000)
						)
  	      
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
				updateSliderInput(session, "offset", min = 1, max = 5000, value = 100, step = 5)
			if (input$target %in% c("cum_cases_per_100000", "cum_deaths_per_100000"))
				updateSliderInput(session, "offset", value = 0.1, min = 0.025, max = 10, step = 0.025)	
			if (input$target == "dailyGrowth")
				updateCheckboxInput(session, "showReferenceLine", value = FALSE)
				updateCheckboxInput(session, "logScale", value=FALSE)
		})		
	})
	
	# on plotly use: disable logScale
	observeEvent(input$usePlotly, {
		if (input$usePlotly == TRUE) updateCheckboxInput(session, "logScale", value=FALSE)
	})
	
	
	# update the estimated growth whenever:
	# - button is pressed
	# - the data set selection changes
	# - the target variable changes
	observeEvent(c(input$estimateGrowth, input$target, dat_selection(), input$estRange, input$showReferenceLine), {
	  print("estimation BUTTON")
		if (isolate(input$showReferenceLine == FALSE)) {
			print("Skipping estimation, no reference line shown")
			return()
		}
	  isolate({
	    # decrease day_since_start by 1, so that it starts with 0, and the intercept is the actual intercept in the plot
			fit <- NULL
			
			ds0 <- dat_selection()
			print(str(ds0))
			
			if (nrow(ds0) == 0) return()
			ds <- ds0[ds0$day_since_start >= input$estRange[1] & ds0$day_since_start <= input$estRange[2], ] %>% 
				as.data.frame()
			
			# input <- list(estRange=c(1, 100), target="cum_cases", start_cumsum = 100)
			# ds <- dat_CSSE %>% filter(country=="Italy", cum_cases >= 100) %>%
			# 	mutate(day_since_start = 1:n()) %>%
			# 	filter(day_since_start >= input$estRange[1], ds$day_since_start <= input$estRange[2]) %>%
			# 	as.data.frame()
			# ds$day_since_start <- 1:nrow(ds)
			# start_min_cases = 100
			days <- ds$day_since_start - 1
			cases <- ds[, input$target]
			start_min_cases <- ifelse(grepl(input$target, "100000", fixed=TRUE), 0.1, input$start_cumsum)
			
	    tryCatch({
					fit <- estimate_exponential_curve(day=days, cases=cases, min_cases=start_min_cases)
	    },
	    error=function(e) return(NULL)
	  )
	  })
	  
		if (!is.null(fit)) {
			print(summary(fit))
			intercept <- predict(fit, newdata=data.frame(day=0))
		  updateSliderInput(session, "offset", value = as.numeric(intercept))
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
	  
		y_label_0 <- switch(input$target, 
			"cum_cases" = "Cumulative number of confirmed cases", 
			"cum_cases_per_100000" = "Cumulative number of confirmed cases, per 100,000",
			"cum_deaths_per_100000" = "Cumulative number of confirmed deaths, per 100,000", 
			"cum_deaths" = "Cumulative number of confirmed deaths",
			"dailyGrowth" = "Daily growth of confirmed cases in %"
		)
		
		y_label <- paste0(y_label_0, ifelse(input$logScale == TRUE, " (log scale)", ""))
		
	# For log scale: deaths +1 to avoid log error
	if (input$target %in% c("cum_deaths", "cum_deaths_per_100000")) {
		real_target <- paste0(input$target, "_noZero")
	} else {
		real_target <- input$target
	}
	
	print(paste0("Using target variable ", real_target))
	print(summary(dat_selection()$cum_deaths_per_100000_noZero))
		
	# Plot countries
	#if (!'state' %in% names(dat_selection())) {

		if ('state' %in% names(dat_selection())) {
			p1 <- ggplot(dat_selection(), aes_string(x="day_since_start", y=real_target, color='state'))			
			startupflag_state <<- FALSE # Once one graph of states has been completed, turn off startup flag for states
		} else {
			p1 <- ggplot(dat_selection(), aes_string(x="day_since_start", y=real_target, color='country'))			
		}
		
		# if estimation range is restricted: show grey rect
		if ((input$estRange[1]>1 | input$estRange[2]<max_day_since_start()) & input$showReferenceLine == TRUE) {
			p1 <- p1 + 
			annotate(geom="rect", xmin=input$estRange[1], xmax=min(input$estRange[2], max_day_since_start()), ymin=input$start_cumsum, ymax=max(dat_selection()[, input$target])*1.05, fill="azure2", alpha=.3) +
			annotate(geom="text", x=input$estRange[1], y=max(dat_selection()[, input$target]), label="Curve estimated based on values in the shaded rectangle", hjust=0, size=3)
		}
		
		
		if (input$usePlotly == FALSE) {
			if ('state' %in% names(dat_selection())) {
				p1 <- p1 + geom_label_repel(aes(label = state_label), nudge_x = 1, na.rm = TRUE)
			} else {
				p1 <- p1 + geom_label_repel(aes(label = country_label), nudge_x = 1, na.rm = TRUE)
			}
	  }
		
				
		if (input$target == "dailyGrowth") {
			p1 <- p1 + geom_smooth(span=input$smoother_span, se=input$smoother_se)
		} else {
			p1 <- p1 + 
				geom_point() + 
				geom_line()
		}
		
		p1 <- p1 +	scale_color_discrete(guide = FALSE) +
			theme_bw() + 
			labs(
				title = paste0("Visualization based on data from ", input$datasource, ". "),
			  subtitle = paste0("Data set from ", current_data_date()),
			  caption = "Source: http://shinyapps.org/apps/corona/", 
			  x = paste0("Days since ", input$start_cumsum, "th case"), y = y_label)

		
				# TODO: I think this can be safely deleted
		#}  else {
# 	  # Plot US states
# 	  p1 <- ggplot(dat_selection(), aes_string(x="day_since_start", y=input$target, color='state')) +
# 	    geom_point() +
# 	    geom_line() +
# 	    scale_color_discrete(guide = FALSE) +
# 	    theme_bw() +
# 			labs(
# 				title = "Visualization based on data from CSSE US data by state.",
# 			  subtitle = paste0("Data set from ", current_data_date()),
# 			  caption = "Source: http://shinyapps.org/apps/corona/",
# 			  x = paste0("Days since ", input$start_cumsum, "th case"), y = y_label)
# 	  if (input$usePlotly == FALSE) {
# 	  	p1 <- p1 + geom_label_repel(aes(label = state_label), nudge_x = 1, na.rm = TRUE)
# 	  }
# 	  startupflag_state <<- FALSE # Once one graph of states has been completed, turn off startup flag for states
# 	}
		if (input$logScale == TRUE) {
		  p1 <- p1 + coord_trans(y = "log10")
		}
		if (input$target == "cum_cases") {
			p1 <- p1 + scale_y_continuous(breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000))
		}
		if (input$target %in% c("cum_cases_per_100000", "cum_deaths_per_100000", "cum_deaths")) {
			p1 <- p1 + scale_y_continuous()
		}
		if (input$target == "dailyGrowth") {
			p1 <- p1 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
		}
		
		if (input$showReferenceLine == TRUE) {
		  p1 <- p1 + 
		    stat_function(fun = growth, args=list(percGrowth=input$percGrowth, intercept=input$offset), color="black", linetype="dashed", xlim=c(max(input$estRange[1], min(dat_selection()$day_since_start)), min(input$estRange[2], max_day_since_start()))) +
				stat_function(fun = growth, args=list(percGrowth=input$percGrowth, intercept=input$offset), color="grey80", linetype="dotted") +
		    annotate(label=paste0(input$percGrowth, "% growth rate"), x=max_day_since_start(), y=growth(max_day_since_start()+1, percGrowth=input$percGrowth, intercept=input$offset), geom="text", hjust=1)
		}
		
		return(p1)
	
	}
	output$resNormal <- renderPlot(innerplot(), res=100)
	output$resInteractive <- renderPlotly(innerplot())
	output$normalPlot <- renderUI({
		tagList(
			plotOutput("resNormal", height=700)
		)
	})
	output$interactivePlot <- renderUI({
		tagList(
			plotlyOutput("resInteractive", height=700)
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
	                   showReferenceLine = input$showReferenceLine,
	                   target = input$target,
	                   smoother_span = input$smoother_span,
	                   smoother_se = input$smoother_se,
	                   estRange = input$estRange
	    )
	    rmarkdown::render(tempReport, output_file = file,
	                      params = params,
	                      envir = new.env(parent = globalenv())
	    )
	  }
	)
	
})
