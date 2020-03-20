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
	HTML('<div class="alert alert-info alert-dismissible" role="info"><button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>Disclaimer: This visualization is for research and educational purposes only and is not intended to be a tool for decision-making. There are many uncertainties and debates about the details of COVID-19 infection and case numbers. Please read the section "Putting these numbers in context" below.</div>'),

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
						"Confirmed cumulative cases per 100,000" = "cum_cases_per_100000",
						"Confirmed cumulative deaths per 100,000" = "cum_deaths_per_100000",
						"Daily growth of confirmed cases" = "dailyGrowth"),
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
		  
		  
			
				conditionalPanel(   # do not show reference line for daily growth plot
				  condition = "input.target != 'dailyGrowth'",
			
					h3("Reference line:"),
				  p("If you click on the button, both intercept and exponential growth rate are estimated from the current data in the plot. Using the two sliders, you can manually adjust the reference line. This is mostly useful when only a single country is selected.", style = "font-style: italic; font-size: 0.85em; color:grey; line-height:110%"
				  ),
				  actionButton("estimateGrowth", "Fit growth rate to current country selection"),
				  checkboxInput("showReferenceLine", "Show reference line", value=TRUE),
					sliderInput("estRange", label = "Estimate growth rate between these 'days since X cumulative cases' only:", min = 1, max = 100, value = c(1, 100), step = 1),	
				  sliderInput("percGrowth", label = "% daily growth:", min = 0, max = 100, value = 33, step = 1),
				  sliderInput("offset", label = "Offset at start:", min = 1, max = 5000, value = 100, step = 5)
			),		
			
			h3("Filter:"),
			p("Filter countries/states that have less then this amount of cumulative cases. Those countries are not displayed in the filter checkboxes below and not shown in the plot.", 
			  style = "font-style: italic; font-size: 0.85em; color:grey; line-height:105%"),
			sliderInput("minCases", label = "Minimum overall cases per country/state:", min = 1, max = 10000, value = 100, step = 10),	
			sliderInput("start_cumsum", label = "Start display at the day with at least X cumulative cases:", min = 1, max = 1000, value = 100, step = 5),	
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
		       fluidRow(column(10,
		                       h3("Display options:"),
														conditionalPanel(
									 				  	condition = "input.target != 'dailyGrowth'",
															
															checkboxInput("usePlotly", "Use interactive plot (experimental!)", value=FALSE),

															conditionalPanel(
										 						condition = "input.usePlotly == false",
			                       	 	checkboxInput("logScale", "Print y-axis as log scale", value=FALSE)
															)
													  ),
														conditionalPanel(
									 				  	condition = "input.target == 'dailyGrowth'",
															
															sliderInput("smoother_span", label = "Smoother span:", min = 0.15, max = 2, value = 0.75, step = .01),	
															checkboxInput("smoother_se", "Show smoother CI", value=FALSE)
													  )
														
													),
		                       
		                column(2,
		                       HTML("<br><br><br><br>"),
										 				conditionalPanel(
										 					condition = "input.usePlotly == false",
		                      		downloadButton("DownloadFig", "Download Plot")
														)
										)
						),
				
			conditionalPanel(
				condition = "input.usePlotly != true",
				uiOutput("normalPlot")
			),
			conditionalPanel(
				condition = "input.usePlotly == true",
				uiOutput("interactivePlot")
			),
			
			h2("Putting these numbers in context"),
			help("This is a growing (and non-exhaustive!) collection of caveats."),
			HTML('
			<ul>
			<li>The numbers are confirmed cases after testing. There presumably is a huge <b>rate of undetected cases</b>. One paper published in Science magazine estimates that 86% of all infections were undocumented <a href="https://science.sciencemag.org/content/early/2020/03/13/science.abb3221.abstract" target="_blank">(Li et al., 2020)</a></li>
			<li><b>Between-country comparisons of absolute numbers are difficult</b> as there are huge differences in the amount of testing. (Countries that test more get more confirmed cases, at least unless all cases are detected. Countries that do not test at all have no reported cases at all.)</li>
			<li>If testing practices do not change within a country (which is a big IF), temporal <b>within-country comparisons can be considered valid</b>.</li>
			<ul>
				<li>That also means: If countries slow down testing after/during an intervention, it looks like as if the intervention is effective.</li>
			</ul>
			<li>Between-country <b>comparisons of deaths and death rates are also problematic</b>. For example, <a href="https://twitter.com/G_House__MD/status/1238485575591698433?s=20" target="_blank">Germany does not do/ does less post-mortem testing</a> for Covid-19, while Italy [citation needed] and <a href="https://www.cdc.gov/coronavirus/2019-ncov/hcp/guidance-postmortem-specimens.html" target="_blank">USA</a> seem to do that. This might explain the huge differences in death rates between Germany, Italy, and other countries.</li>
			<li>There are huge <b>differences between countries in <i>who</i> gets tested</b>. Some test only severe case, some test random samples of people (also without symptoms). In the former case, calculated death rates are much higher due to selection bias.</li>
			<li>Related to the previous point: <b>Case-fatality risks</b> (i.e., "How many of the sick people die?") <b>are notoriously hard to calculate</b> and prone to many biases (<a href="https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0003846" target="_blank">Lipsitch et al., 2015</a>).</li>
			<li><b>Demographics differ between countries / affected regions</b>. It has been argued that <a href="https://www.wired.com/story/why-the-coronavirus-hit-italy-so-hard/" target="_blank">Italy has been hit hard</a> because it has a comparably old population (in particular the affected regions).</li>
			<li>The exponential curve which you can fit to the data is <i>not</i> an epidemiological model (although in early stages an epidemic can show exponential growth). Importantly, the projection of the exponential curve is <i>not</i> a proper epidemiological forecast of the development. For a more refined simulation based on the SIR model, see this <a href="https://alhill.shinyapps.io/COVID19seir/?fbclid=IwAR2aXJT79M2AmZxMdy8jsiEuSC4i7ijU8Av6oB4dmlZIeJ2VQgL7Tt3QGxA" target="_blank">interactive app</a>.</li>
			</ul>	
			'),
			
			h3("Other sources of such considerations:"),
			HTML('				
				<ul>
				<li><a href="https://www.cebm.net/global-covid-19-case-fatality-rates/">Global Covid-19 Case Fatality Rates (Jason Oke, Carl Heneghan)</a></li>
				<li><a href="https://blog.datawrapper.de/coronaviruscharts/#considerations">17 (or so) responsible live visualizations about the coronavirus, for you to use</a> by Lisa Charlotte Rost</li>
				</ul>
				
				<p>Several Twitter threads warn against specific Corona visualizations (which seem to contain errors), or against visualizing this data at all, or that only experts in epidemiology should do such visualizations:</p>
				<ul>
				<li><a href="https://twitter.com/danitte/status/1240305200541216769?s=20">Dania Orta-Alemán</a> points out wrong visualizations</li>
				<li>Amanda Makulec: <a href="https://medium.com/nightingale/ten-considerations-before-you-create-another-chart-about-covid-19-27d3bd691be8">Ten Considerations Before You Create Another Chart About COVID-19</a> (blog post)</li>
				</ul>
			')
		)
	),
	HTML('This visualization is inspired by a figure from the <a href="https://www.ft.com/content/a26fbf7e-48f8-11ea-aeb3-955839e06441">Financial Times</a>, created by <a href="https://twitter.com/jburnmurdoch" target="_blank">John Burn-Murdoch</a>.<br>'),
	HTML('Data sources for Covid-19 cases : <a href="https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"  target="_blank">European Centre for Disease Prevention and Control</a> and <a href="https://github.com/CSSEGISandData/COVID-19"  target="_blank">Johns Hopkins CSSE</a> (both are updated daily)<br>'),
	HTML('Data sources for country population: <a href="https://data.worldbank.org/indicator/SP.POP.TOTL"  target="_blank">The World Bank data</a><br><br>'),
	HTML('2020. Contributors: <a href="https://www.nicebread.de" target="_blank">Felix Schönbrodt</a>, <a href="https://github.com/astefan1" target="_blank">Angelika Stefan</a>, <a href="https://github.com/zuphilip"  target="_blank">Philipp Zumstein</a>, <a href="https://github.com/pab2163" target="_blank">Paul A. Bloom</a><br>'),
	HTML('Open source code on Github: <a href="https://github.com/nicebread/corona"  target="_blank">https://github.com/nicebread/corona</a>')
))



# Other apps:

# https://alhill.shinyapps.io/COVID19seir/?fbclid=IwAR2aXJT79M2AmZxMdy8jsiEuSC4i7ijU8Av6oB4dmlZIeJ2VQgL7Tt3QGxA
# https://gorkang.shinyapps.io/2020-corona/
# https://covid19-dash.github.io/

