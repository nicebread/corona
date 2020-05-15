# 2020 Felix Schönbrodt, MIT license
# This data visualization is inspired by the Financial Times: https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441

source("helpers.R", local=TRUE)
source("load_data.R", local=TRUE) # load the cached data
#source("download_data.R", local=TRUE)
#source("preprocess_data.R", local=TRUE)


shinyServer(function(input, output, session) {
  
  # Startup flags to indicate whether session has just started up (in which case use defaults) for both state/country
  startupflag <- TRUE
  startupflag_state <- TRUE
	
	# downloadNote <- reactiveVal("")
	# output$ui_downloadNote <- renderUI({
	# 		print(paste0("Rendering ", downloadNote()))
	# 		return(tagList(p(
	# 			"TEST: ",
	# 			downloadNote(),
	# 			style = "font-style: italic; font-size: 1em; color:grey; line-height:110%")))
	# })
	#
	#
	# At each start of the app: check if new downloads are available
	observeEvent({input}, {
		print("CHECKING FOR NEW DATA")
		#downloadNote("Checking for new data files ...")
		source("download_data.R", local=TRUE)
		if (downloadFlag == TRUE) {
			#downloadNote("Preprocessing new data files ...")
			source("preprocess_data.R", local=TRUE)
			#downloadNote("Loading new data files ...")
			source("load_data.R", local=TRUE)
		}
		#downloadNote("")
	})
	
	
	# on target change: update sliders
	observeEvent(input$target, { 
		isolate({
			if (input$target %in% c("cum_cases")) {
				updateSliderInput(session, "offset", min = 1, max = 5000, value = 100, step = 5)
				updateSliderInput(session, "refLineOffset", min = 1, max = 5000, value = 100, step = 5)
				updateSliderInput(session, "align_value", min = 0, max = 1000, value = 100, step = 5)
			} else if (input$target %in% c("cum_deaths_noZero")) {
				updateSliderInput(session, "offset", min = 1, max = 500, value = 100, step = 1)
				updateSliderInput(session, "refLineOffset", min = 1, max = 500, value = 100, step = 1)					
				updateSliderInput(session, "align_value", min = 0, max = 500, value = 50, step = 5)
			} else if (input$target %in% c("cum_cases_per_100000", "cum_deaths_per_100000_noZero")) {				
				updateSliderInput(session, "offset", value = 0.1, min = 0.025, max = 20, step = 0.025)	
				updateSliderInput(session, "refLineOffset", value = 0.1, min = 0.025, max = 20, step = 0.025)	
				updateSliderInput(session, "align_value", min = 0, max = 10, value = 0.05, step = 0.025)
			} else if (input$target == "dailyGrowth") {
				#updateCheckboxInput(session, "showReferenceLine", value = FALSE)
				updateRadioGroupButtons(session, "logScale", selected='linear')
				updateSliderInput(session, "align_value_daily", min = 0, max = 1000, value = 100, step = 5)
			}
		})		
	})
	
  
  # dat_startfilter stores the reduced data set (reduced by input$align_value)
  dat_startfilter <- reactiveVal(data.frame())
	current_data_date <- reactiveVal(NA)  # stores the current data date for the selected data set. This is displayed in the heading of the plot.
	observeEvent({
		input$datasource
		input$target
		input$align_value
		input$align_value_daily
		input$minCases}, {
		
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
		
		print(paste0("STARTFILTER; minCases = ", input$minCases))
		if (input$target != "dailyGrowth") {
			dat_startfilter(dat[dat$overall_cum_cases >= input$minCases & dat[, input$target] >= input$align_value, ])
		} else {
			dat_startfilter(dat[dat$overall_cum_cases >= input$minCases & dat$cum_cases >= input$align_value_daily, ])
		}
    
  }, ignoreNULL=FALSE)
  

# Country Selection -------------------------------------------------------

	# dynamically populate country selector, based on available choices AFTER the align_value filter
  last_country_selection <- reactiveVal()
  observe({
    print(paste("last_country_selection update: ", paste(input$country_selection, collapse=", ")))
    last_country_selection(input$country_selection)
  })
  
  output$country_selector <- renderUI({
    print(paste0("SELECTOR; startupflag = ", startupflag))
    
		input$minCases  # react to this slider
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
    available_countries <- unique(dat_startfilter()$country)
    updateCheckboxGroupInput(session, "country_selection", selected = available_countries)
  })
  observeEvent(input$deselectAllCountries, {
    updateCheckboxGroupInput(session, "country_selection", selected = "")
  })
  

# State Selection ---------------------------------------------------------
	# dynamically populate state selector, based on available choices AFTER the align_value filter
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
	  available_states <- unique(dat_startfilter()$state)
	  updateCheckboxGroupInput(session, "state_selection", selected = available_states)
	})
	observeEvent(input$deselectAllStates, {updateCheckboxGroupInput(session, "state_selection", selected = "")})
	
	# dat_selection stores the data set filterd by state selection
  	dat_selection <- reactiveVal(data.frame())
  	max_day_since_start <- reactiveVal(NA)
  	observe({
  	  print("DAT_SELECTION_US_STATES")
  	  if(input$datasource == 'CSSE_State'){
    	  if (!is.null(input$state_selection)) {
    	    print(input$state_selection)
    	    d0 <- dat_startfilter() %>% 
						filter(state %in% input$state_selection)
    	    
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
						filter(country %in% input$country_selection)
  	      
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
  	
	# update offset slider when align_value changes
	observeEvent(c(input$logScale, input$align_value), {
	  print("UPDATE OFFSET SLIDER")
	  updateSliderInput(session, "refLineOffset", value = input$align_value)
		updateSliderInput(session, "offset", value = input$align_value)
	})
	
	
	# on plotly use: disable logScale
	observeEvent(input$usePlotly, {
		if (input$usePlotly == TRUE) updateCheckboxInput(session, "logScale", value='linear')
	})
	
		
	# update the estimated growth whenever:
	# - button is pressed
	# - the data set selection changes
	# - the target variable changes
	auto_fit <- reactiveVal(NULL)
	observeEvent(c(input$estimateGrowth, input$target, dat_selection(), input$estRange, input$fitLineType), {
	  print("REESTIMATING FIT")
		
		ds0 <- dat_selection()
		print(paste0("Type of fit: ", input$fitLineType, "; n=", nrow(ds0)))
		
		if (input$fitLineType %in% c("none", "manual") | nrow(ds0) == 0 |  input$target == "dailyGrowth") {
			print("Skipping estimation, no reference line shown")
			return()
		}
	  isolate({
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
		
		#print(summary(fit$fit))
	  
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
		
		ds <- dat_selection()
		
		# for local testing: create an input object
		# ds <- dat_ECDC %>% filter(country %in% c("Germany"), cum_cases > 50) %>% mutate(day_since_start = 1:n())
		# input <- list(target="cum_cases_per_100000", logScale='linear', estRange=c(1, 100), fitLineType="automatic", align_value=100, usePlotly=FALSE, datasource="CSSE", percGrowth=30, offset=100)
		# max_day_since_start <- function() return(25)
		# current_data_date <- function() return("2020-03-24")
	  
		if (nrow(ds) == 0) {
			return(list(h3("No data selected.")))
		}
	  
		y_label_0 <- switch(input$target, 
			"cum_cases" = "Cumulative number of confirmed cases", 
			"cum_cases_per_100000" = "Cumulative number of confirmed cases, per 100,000 capita (adjusted)",
			"cum_deaths_per_100000_noZero" = "Cumulative number of confirmed deaths, per 100,000 capita (adjusted)", 
			"cum_deaths_noZero" = "Cumulative number of confirmed deaths",
			"dailyGrowth" = "Daily growth of confirmed cases in %"
		)
		
		y_label <- paste0(y_label_0, ifelse(input$logScale == 'log', " (log scale)", ""))
		
		
		x_label <- switch(input$target, 
			"cum_cases" = paste0("Days since ", get_nth_label(input$align_value), " confirmed case"), 
			"cum_cases_per_100000" = paste0("Days since ", input$align_value, " confirmed cases per 100,000 capita"),
			"cum_deaths_per_100000_noZero" = paste0("Days since ", input$align_value, " deaths per 100,000 capita"), 
			"cum_deaths_noZero" = paste0("Days since ", get_nth_label(input$align_value), " death"),
			"dailyGrowth" = paste0("Days since ", get_nth_label(input$align_value_daily), " confirmed case")
		)		
	
		
		if ('state' %in% names(ds)) {
			p1 <- ggplot(ds, aes_string(x="day_since_start", y=input$target, color='state'))			
			startupflag_state <<- FALSE # Once one graph of states has been completed, turn off startup flag for states
		} else {
			p1 <- ggplot(ds, aes_string(x="day_since_start", y=input$target, color='country'))			
		}
		
		# if estimation range is restricted: show grey rect
		if ((input$estRange[1]>1 | input$estRange[2]<max_day_since_start()) & input$fitLineType != "none") {
			
			YMIN <- min(ds[, input$target], na.rm=TRUE)*0.95
			YMAX <- max(ds[, input$target], na.rm=TRUE)*1.05
					
			p1 <- p1 + 
			annotate(geom="rect", xmin=input$estRange[1], xmax=min(input$estRange[2], max_day_since_start()), ymin=YMIN, ymax=YMAX, fill="azure2", alpha=.3) +
			annotate(geom="text", x=input$estRange[1], y=YMAX, label="Curve estimated based on values in the shaded rectangle", hjust=0, size=3)
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
			  x = x_label, y = y_label)

		
		if (input$logScale == 'log') {
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
			p1 <- p1 + scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 0.65))
		}
		
		# ---------------------------------------------------------------------
		#  show fit line(s)
		if (input$fitLineType == "automatic" & !is.null(auto_fit()$fit) & input$target != "dailyGrowth") {
		 
		 	# average (fixed-effect) line
		  p1 <- p1 + 
				stat_function(fun = growth_m1, args=list(slope=exp(auto_fit()$slope), intercept=exp(auto_fit()$intercept)), color="black", size=0.8, linetype="dashed", xlim=c(max(input$estRange[1], min(ds$day_since_start)), min(input$estRange[2], max_day_since_start()))) +
				stat_function(fun = growth_m1, args=list(slope=exp(auto_fit()$slope), intercept=exp(auto_fit()$intercept)), color="black", size=0.8, linetype="dotted") +
		    annotate(label=paste0(round((exp(auto_fit()$slope)-1)*100), "% ", ifelse(auto_fit()$n_countries == 1, "", "average"), " estimated growth rate"), x=max_day_since_start(), y=growth_m1(max_day_since_start()+1, slope=exp(auto_fit()$slope), intercept=exp(auto_fit()$intercept)), geom="text", hjust=1)
				
								
				# if multiple countries: add individual lines
				if (auto_fit()$n_countries > 1 & input$showRandomSlopes==TRUE) {
					for (co in 1:nrow(auto_fit()$RE)) {
						p1 <- p1 + stat_function(fun = growth_m1, args=list(slope=exp(auto_fit()$RE[co, 2]), intercept=exp(auto_fit()$RE[co, 1])), color="grey80", linetype="dotted")
					}
				}				
		}
		
		if (input$fitLineType == "manual" & input$target != "dailyGrowth") {
		  p1 <- p1 + 
				stat_function(fun = growth_m1, args=list(slope=(input$percGrowth/100+1), intercept=input$offset), color="black", size=0.8, linetype="dashed", xlim=c(max(input$estRange[1], min(ds$day_since_start)), min(input$estRange[2], max_day_since_start()))) +
		    annotate(label=paste0(input$percGrowth, "% growth rate"), x=max_day_since_start(), y=growth_m1(max_day_since_start()+1, slope=input$percGrowth/100+1, intercept=input$offset), geom="text", hjust=1)
		}
		
		
		# ---------------------------------------------------------------------
		# Reference lines (doubling every two days, three, four days)
		if (input$logScale == 'log' & input$refLines == TRUE) {
			if (input$fitLineType == "automatic") {				
				OFFSET <- exp(auto_fit()$intercept)
			} else {				
				OFFSET <- input$refLineOffset
			}
			
		p1 <- p1 + 
				stat_function(fun = growth_m1, args=list(slope=2^(1/2), intercept=OFFSET), color="grey80", size=0.8, linetype="dotted")  + 
				stat_function(fun = growth_m1, args=list(slope=2^(1/3), intercept=OFFSET), color="grey80", size=0.8, linetype="dotted") +
				stat_function(fun = growth_m1, args=list(slope=2^(1/5), intercept=OFFSET), color="grey80", size=0.8, linetype="dotted") +
		    annotate(label="doubling every 2 days", x=max_day_since_start(), y=growth_m1(max_day_since_start()+1, slope=2^(1/2), intercept=OFFSET), geom="text", hjust=1, vjust=0, color="grey80", angle=32) +
		    annotate(label="doubling every 3 days", x=max_day_since_start(), y=growth_m1(max_day_since_start()+1, slope=2^(1/3), intercept=OFFSET), geom="text", hjust=1, vjust=0, color="grey80", angle=19) +
		    annotate(label="doubling every 5 days", x=max_day_since_start(), y=growth_m1(max_day_since_start()+1, slope=2^(1/5), intercept=OFFSET), geom="text", hjust=1, vjust=0, color="grey80", angle=10)
			}
		
		
		
		
		# ---------------------------------------------------------------------
		# Annotations
	
		if (input$annotation != "" & input$showAnnotation == TRUE & input$target != "dailyGrowth") {
	
			annotation_df2 <- inner_join(annotation_list() %>% filter(country %in% unique(ds$country)), ds %>% select(country, date, one_of(input$target, "day_since_start")), by=c("country", "date"))
			
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
	                   align_value = input$align_value,
	                   datasource = input$datasource,
	                   current_data_date = current_data_date(),
	                   percGrowth = input$percGrowth,
	                   offset = input$offset,
	                   max_day_since_start = max_day_since_start(),
	                   fitLineType = input$fitLineType,
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
	
	output$ui_estimationNote <- renderUI({	
		if (input$fitLineType == "automatic" & length(input$country_selection) > 1) {
			return(tagList(p(
				'The exponential curve has been fit with a hierarchical log-linear model with random intercepts and random slopes. The bold curve shows the fixed (i.e., the average) effect across all countries.', style = "font-style: italic; font-size: 0.85em; line-height:110%"),
				code("lme4 code: log(target_variable) ~ 1 + day_since_start + (1 + day_since_start | country)")
			))
		}		
		if (input$fitLineType == "automatic" & length(input$country_selection) == 1) {
			return(tagList(p(
				'The exponential curve has been fit with a log-linear model.', style = "font-style: italic; font-size: 0.85em; line-height:110%"),
				code("R code: log(target_variable) ~ 1 + day_since_start")
			))
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
