library(shiny)
library(shinyjs)
library(shinythemes)

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-sm", `data-toggle` = "popover",
	  	`data-container`="body",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      icon("question")
    )
  )
}


# Define UI for PPV application
shinyUI(fluidPage(theme = shinytheme("spacelab"),

	title = "Corona / Covid-19 growth",
	shinyjs::useShinyjs(),

	h2(HTML("Visualization of Covid-19 confirmed cases")),
	HTML('<div class="alert alert-warning alert-dismissible" role="warning"><button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>Please report any bugs as issues at <a href="https://github.com/nicebread/corona">Github</a>, or contribute with pull requests!</div>'),

	br(),

	# Sidebar to select inputs
	fluidRow(
		# ---------------------------------------------------------------------
		
		column(4,
		  h3("Data source:"),
			  radioButtons("datasource", "", c(
					"European Centre for Disease Prevention and Control" = "ECDC", 
					"Johns Hopkins CSSE" = "CSSE", 
					'Johns Hopkins CSSE - US States' = 'CSSE_State')
				),
			
		  h3("Target variable:"),
				radioButtons("target", "", c(
						"Confirmed cumulative cases" = "cum_cases",
						"Confirmed cumulative deaths" = "cum_deaths",
						"Confirmed cumulative cases per 100,000" = "cum_cases_per_100000"),
						selected = "cum_cases"
					),
				# conditionalPanel(
				# 			  	condition = "input.datasource == 'ECDC'",  # no "recovered" in ECDC data set
				# 			    radioButtons("target", "", c(
				# 		"Confirmed cumulative cases" = "cum_cases",
				# 		"Confirmed cumulative deaths" = "cum_deaths",
				# 		"Confirmed cumulative cases per 100,000" = "cum_cases_per_100000"),
				# 		selected = "cum_cases"
				# 	),
				# ),
				# conditionalPanel(
				# 			  	condition = "input.datasource != 'ECDC'",
				# 			    radioButtons("target", "", c(
				# 		"Confirmed cumulative cases" = "cum_cases",
				# 		"Confirmed cumulative deaths" = "cum_deaths",
				# 		"Confirmed cumulative recovered" = "cum_recovered",
				# 		"Confirmed cumulative cases per 100,000" = "cum_cases_per_100000"),
				# 		selected = "cum_cases"
				# 	),
				# ),
		  
		  
		  h3("Display options:"),
			  checkboxInput("logScale", "Print y-axis as log scale", value=FALSE),
			
			
			h3("Reference line:"),
			  p("If you click on the button, both intercept and exponential growth rate are estimated from the current data in the plot. Using the two sliders, you can manually adjust the reference line.", style = "font-style: italic; font-size: 0.85em; color:grey; line-height:105%"
			  ),
			  actionButton("estimateGrowth", "Fit growth rate to current country selection"),
			  checkboxInput("showReferenceLine", "Show reference line", value=TRUE),
			  sliderInput("percGrowth", label = "% daily growth:", min = 0, max = 100, value = 33, step = 1),
			  sliderInput("offset", label = "Offset at start:", min = 1, max = 5000, value = 100, step = 5),
			
			h3("Filter:"),
			p("Filter countries/states that have less then this amount of cumulative cases. Those countries are not displayed in the filter checkboxes below and not shown in the plot.", 
			  style = "font-style: italic; font-size: 0.85em; color:grey; line-height:105%"),
			sliderInput("minCases", label = "Minimum overall cases per country/state:", min = 1, max = 10000, value = 100, step = 10),	
			sliderInput("start_cumsum", label = "Start display at the day with at least X cumulative cases:", min = 1, max = 1000, value = 100, step = 1),	
			# Panels for selecting states/countries appear conditionally based on the chosen data
			conditionalPanel(
			  condition = "input.datasource == 'CSSE_State'",
			  h2("State selection:"),
			  actionButton("selectAllStates", "Select all states"),
			  actionButton("deselectAllStates", "Deselect all states"),
			  htmlOutput("state_selector")

			),
  		conditionalPanel(
  		  condition = "input.datasource != 'CSSE_State'",
  			h2("Country selection:"),
  			  actionButton("selectAllCountries", "Select all countries"),
  			  actionButton("deselectAllCountries", "Deselect all countries"),
  			  htmlOutput("country_selector")
  			
  			# ---------------------------------------------------------------------
  			# Output column
  		)),
		column(8,
			conditionalPanel(
				condition = "input.usePlotly != true",
				uiOutput("normalPlot")
			),
			conditionalPanel(
				condition = "input.usePlotly == true",
				uiOutput("interactivePlot")
			),
			checkboxInput("usePlotly", "Use interactive plot (experimental!)", value=FALSE),
			downloadButton("DownloadFig", "Download Plot"),
		)
	),
	HTML('This visualization is inspired by a figure from the <a href="https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441">Financial Times</a>, created by <a href="https://twitter.com/jburnmurdoch" target="_blank">John Burn-Murdoch</a>.<br>'),
	HTML('Data sources for Covid-19 cases : <a href="https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"  target="_blank">European Centre for Disease Prevention and Control</a> and <a href="https://github.com/CSSEGISandData/COVID-19"  target="_blank">Johns Hopkins CSSE</a> (both are updated daily)<br>'),
	HTML('Data sources for country population: <a href="https://data.worldbank.org/indicator/SP.POP.TOTL"  target="_blank">The World Bank data</a><br><br>'),
	HTML('2020. Contributors: <a href="https://www.nicebread.de" target="_blank">Felix Schönbrodt</a>, <a href="https://github.com/astefan1" target="_blank">Angelika Stefan</a>, <a href="https://github.com/zuphilip"  target="_blank">Philipp Zumstein</a>, <a href="https://github.com/pab2163" target="_blank">Paul A. Bloom</a><br>'),
	HTML('Open source code on Github: <a href="https://github.com/nicebread/corona"  target="_blank">https://github.com/nicebread/corona</a>')
))
