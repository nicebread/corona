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

	h2(HTML("Corona / Covid-19 growth visualization")),
	HTML('<div class="alert alert-warning alert-dismissible" role="warning"><button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>Please report any bugs as issues at <a href="https://github.com/nicebread/corona">Github</a>, or contribute with pull requests!</div>'),

	br(),

	# Sidebar to select inputs
	fluidRow(
		# ---------------------------------------------------------------------
		
		column(4,
			radioButtons("datasource", "Data source:",
			               c("European Centre for Disease Prevention and Control" = "ECDC",
			                 "Johns Hopkins CSSE" = "CSSE")),
			h3("Reference line:"),
			checkboxInput("showReferenceLine", "Show reference line", value=TRUE),
			sliderInput("percGrowth", label = "% daily growth:", min = 0, max = 100, value = 33, step = 1),
			sliderInput("offset", label = "Offset at start:", min = 1, max = 1000, value = 100, step = 1),
			
			h3("Filter:"),
			p(HTML("Filter countries that have less then this amount of cumulative cases:"), helpPopup("?", "Those countries are not displayed in the filter checkboxes below and not shown in the plot.", placement='right', trigger='hover'), style = "font-style: italic; font-size: 0.85em; color:grey; line-height:30%"),
			sliderInput("minCases", label = "Minimum overall cases per country:", min = 1, max = 10000, value = 100, step = 10),	
			sliderInput("start_cumsum", label = "Start display at the day with at least X cumulative cases:", min = 1, max = 1000, value = 100, step = 1),	
				
			h3("Display options"),
			checkboxInput("logScale", "Print y-axis as log scale", value=TRUE),
			h2("Country selection:"),
			actionButton("selectAllCountries", "Select all countries"),
			actionButton("deselectAllCountries", "Deselect all countries"),
			htmlOutput("country_selector")
			
			# ---------------------------------------------------------------------
			# Output column
			
		),
			
		column(8,
			htmlOutput("res")
		)			
	),
	HTML('This visualization is based on a figure from the <a href="https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441">Financial Times</a>.<br>'),
	HTML('Data sources: <a href="https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide">European Centre for Disease Prevention and Control</a> and <a href="https://github.com/CSSEGISandData/COVID-19">Johns Hopkins CSSE</a> (both are updated daily)'),
	p('2020 Felix Schönbrodt, <a href="https://www.nicebread.de">https://www.nicebread.de</a>')
))