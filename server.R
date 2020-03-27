# 2020 Felix Schönbrodt, MIT license
# This data visualization is inspired by the Financial Times: https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441

source("helpers.R", local=TRUE)
#source("download_data.R", local=TRUE)
source("preprocess_data.R", local=TRUE)

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
	auto_fit <- reactiveVal(NULL)
	observeEvent(c(input$estimateGrowth, input$target, dat_selection(), input$estRange, input$showReferenceLine), {
	  print("estimation BUTTON")
		
		ds0 <- dat_selection()
		
		if (isolate(input$showReferenceLine == FALSE | nrow(ds0) == 0)) {
			print("Skipping estimation, no reference line shown")
			return()
		}
	  isolate({
	    # decrease day_since_start by 1, so that it starts with 0, and the intercept is the actual intercept in the plot
			fit <- NULL
			
			ds <- ds0[ds0$day_since_start >= input$estRange[1] & ds0$day_since_start <= input$estRange[2], ] %>% 
				as.data.frame()
			
	    tryCatch({
					fit <- estimate_exponential_curves(ds, target=input$target, random_slopes=TRUE)
	    },
	    error=function(e) {
				return(NULL)
			}
	  )
	  })
		
		print(summary(fit$fit))
	  
		if (!is.null(fit)) {
			print(summary(fit$fit))
		} else {
			print("no fit possible")
		}
	  
		auto_fit(fit)
	})
	
	
	
	# import annotation list from textarea
	
	annotation_list <- reactiveVal(data.frame())	
	observeEvent(c(input$annotation, input$showAnnotation), {
		if (input$annotation != "" & input$showAnnotation == TRUE) {

			annotation_df <- read.table(text=input$annotation, header=TRUE, sep=",", fill=TRUE, row.names=NULL, as.is=TRUE)
			annotation_df <- sapply(annotation_df, str_squish) %>% 
				as.data.frame(stringsAsFactors=FALSE) %>%
				rename(country=Country, date=StartDate, label=Label) %>% 
				mutate(
					date=as.Date(date, format="%Y-%m-%d")
				)
				
			print("ANNOTATION-LIST:")
			print(annotation_df)
			annotation_list(annotation_df)	
		} else {
			return(data.frame())
		}	
	})

			
			
			
			
	
	
	# ---------------------------------------------------------------------
	# the plot
	innerplot <- function() {
		
	  print("PLOT:")
		print(str(dat_selection()))
		
		ds <- dat_selection()
		
		# for local testing: create an input object
		# ds <- dat_ECDC %>% filter(country %in% c("Germany"), cum_cases > 50) %>% mutate(day_since_start = 1:n())
		# input <- list(target="cum_cases", logScale=FALSE, estRange=c(1, 100), showReferenceLine=TRUE, start_cumsum=100, usePlotly=FALSE, datasource="CSSE", percGrowth=30, offset=100)
		# max_day_since_start <- function() return(25)
		# current_data_date <- function() return("2020-03-24")
	  
		if (nrow(ds) == 0) {
			return(list(h3("No data selected.")))
		}
	  
		y_label_0 <- switch(input$target, 
			"cum_cases" = "Cumulative number of confirmed cases", 
			"cum_cases_per_100000" = "Cumulative number of confirmed cases, per 100,000 capita (adjusted)",
			"cum_deaths_per_100000" = "Cumulative number of confirmed deaths, per 100,000 capita (adjusted)", 
			"cum_deaths" = "Cumulative number of confirmed deaths",
			"dailyGrowth" = "Daily growth of confirmed cases in %"
		)
		
		y_label <- paste0(y_label_0, ifelse(input$logScale == TRUE, " (log scale)", ""))
		
	# For log scale: deaths +1 to avoid log error
	if (input$logScale == TRUE & input$target %in% c("cum_deaths", "cum_deaths_per_100000")) {
		real_target <- paste0(input$target, "_noZero")
	} else {
		real_target <- input$target
	}
	
	print(paste0("Using target variable ", real_target))
		
		if ('state' %in% names(ds)) {
			p1 <- ggplot(ds, aes_string(x="day_since_start", y=real_target, color='state'))			
			startupflag_state <<- FALSE # Once one graph of states has been completed, turn off startup flag for states
		} else {
			p1 <- ggplot(ds, aes_string(x="day_since_start", y=real_target, color='country'))			
		}
		
		# if estimation range is restricted: show grey rect
		if ((input$estRange[1]>1 | input$estRange[2]<max_day_since_start()) & input$showReferenceLine == TRUE) {
			p1 <- p1 + 
			annotate(geom="rect", xmin=input$estRange[1], xmax=min(input$estRange[2], max_day_since_start()), ymin=input$start_cumsum, ymax=max(ds[, input$target])*1.05, fill="azure2", alpha=.3) +
			annotate(geom="text", x=input$estRange[1], y=max(ds[, input$target]), label="Curve estimated based on values in the shaded rectangle", hjust=0, size=3)
		}
		
		
		if (input$usePlotly == FALSE) {
			if ('state' %in% names(ds)) {
				p1 <- p1 + geom_label_repel(aes(label = state_label), hjust=1, vjust=1, nudge_x = 1, na.rm = TRUE)
			} else {
				p1 <- p1 + geom_label_repel(aes(label = country_label), hjust=1, vjust=1, nudge_x = 1, na.rm = TRUE)
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
			  caption = ifelse(input$target %in% c("cum_cases_per_100000", "cum_deaths_per_100000", "cum_deaths"),
			                   "Source: http://shinyapps.org/apps/corona/ \n Adjusted cumulative cases per capita: 100,000 x (cumulative cases / population)",
			                   "Source: http://shinyapps.org/apps/corona/"), 
			  x = paste0("Days since ", get_nth_label(input$start_cumsum), " case"), y = y_label)

		
		if (input$logScale == TRUE) {
		  p1 <- p1 + coord_trans(y = "log10")
		}
		if (input$target == "cum_cases") {
			#p1 <- p1 + scale_y_continuous(breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000))
			p1 <- p1 + scale_y_continuous()
		}
		if (input$target %in% c("cum_cases_per_100000", "cum_deaths_per_100000", "cum_deaths")) {
			p1 <- p1 + scale_y_continuous()
		}
		if (input$target == "dailyGrowth") {
			p1 <- p1 + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
		}
		
		# ---------------------------------------------------------------------
		#  show fit line(s)
		if (input$showReferenceLine == TRUE) {
		 
		 	# average (fixed-effect) line
		  p1 <- p1 + 
				stat_function(fun = growth_m1, args=list(slope=exp(auto_fit()$slope), intercept=exp(auto_fit()$intercept)), color="black", size=1, linetype="dashed", xlim=c(max(input$estRange[1], min(ds$day_since_start)), min(input$estRange[2], max_day_since_start()))) +
				stat_function(fun = growth_m1, args=list(slope=exp(auto_fit()$slope), intercept=exp(auto_fit()$intercept)), color="black", size=1, linetype="dotted") +
		    annotate(label=paste0(round((exp(auto_fit()$slope)-1)*100), "% ", ifelse(auto_fit()$n_countries == 1, "", "average"), " growth rate"), x=max_day_since_start(), y=growth_m1(max_day_since_start()+1, slope=exp(auto_fit()$slope), intercept=exp(auto_fit()$intercept)), geom="text", hjust=1)
				
				
				# if multiple countries: add individual lines
				if (auto_fit()$n_countries > 1 & input$showRandomSlopes==TRUE) {
					for (co in 1:nrow(auto_fit()$RE)) {
						p1 <- p1 + stat_function(fun = growth_m1, args=list(slope=exp(auto_fit()$RE[co, 2]), intercept=exp(auto_fit()$RE[co, 1])), color="grey80", linetype="dotted")
					}
				}				
		}
		
	
		if (input$annotation != "" & input$showAnnotation == TRUE & input$target != "dailyGrowth") {
	
			annotation_df2 <- inner_join(annotation_list() %>% filter(country %in% unique(ds$country)), ds %>% select(country, date, one_of(input$target, real_target, "day_since_start")), by=c("country", "date"))
			
			if (nrow(annotation_df2) > 0) {
				p1 <- p1 + geom_point(data=annotation_df2, shape=9, size=4) + geom_label_repel(data=annotation_df2, aes_string(label="label"), force = 20, nudge_x=-4, nudge_y=10, size=3)
			}			
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


	## ======================================================================
	## other UI outputs
	## ======================================================================
	
	output$ui_estimationWarning <- renderUI({	
		if (input$showReferenceLine == TRUE & length(input$country_selection) > 1) {
			return(tagList(p(
				'Warning: You fit the exponential curve to more than one country, this might lead to strange results.', 
				style = "font-style: italic; font-size: 0.85em; color:red; line-height:110%"
				  )))
		}		
	})

	output$ui_annotationWarning <- renderUI({	
		if (input$annotation != "" & input$showAnnotation == TRUE & nrow(annotation_list())==0) {
			return(tagList(p(
				'Warning: There seems to be an error in your annotation CSV - no annotations displayed.', 
				style = "font-style: italic; font-size: 0.85em; color:red; line-height:110%"
				  )))
		}		
	})
	
})
