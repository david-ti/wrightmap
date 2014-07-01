library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Wright Map"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
	wellPanel(
    		checkboxInput('data', 'Show data options', TRUE),
    		conditionalPanel(condition='input.data',
			wellPanel(
  				checkboxInput('files', 'Show file options', TRUE),
  				conditionalPanel(condition='input.files',
    					fileInput('eap', 'Choose Person Estimates File'),
    					fileInput('shw', 'Choose Show File'),
    					selectInput("p.type", "Person estimates type:", 
                			choices = c("EAP", "MLE", "WLE"))
         			)
    			),
    			wellPanel(
    				checkboxInput('table', 'Show table options', FALSE),
    				conditionalPanel(condition='input.table',
    					selectInput("type","Parameter type:", choices = c("default","Thresholds" = "thresholds","Deltas" = "deltas")),
    					sliderInput("throld","Threshold",min=.01,max = .99, value = .5, step = .01),
    					uiOutput("item.table"),
    					uiOutput("step.table"),
    					uiOutput("interactions.table")
    					
    				)
    			)
    		)
	),
	wellPanel(
		checkboxInput('disp',"Show display options",FALSE),
		conditionalPanel(condition='input.disp',
			checkboxInput('autotitle',"Create title automatically",TRUE),
			conditionalPanel(condition = "!input.autotitle",
				textInput('title',"Title")
			),
			wellPanel(
				checkboxInput('pers', 'Show person display options', FALSE),
				conditionalPanel(condition='input.pers',
    					checkboxInput('use.hist', 'Histogram?', TRUE)
    				)
    			),
    			wellPanel(
    				checkboxInput('item', 'Show item display options', FALSE),
    				conditionalPanel(condition='input.item',
    					checkboxInput('show.thr.lab', 'Show Threshold Labels', TRUE),
					textInput('axis.logits',"Logit axis label","Logits"),
		    			sliderInput(
		    				"cex", 
		                	"Symbol size", 
		                	min = 1,
		               	max = 5, 
		                	value = 1.2, step = .1
		            	)
		       	)
		    	)
     	)
     )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
  #textOutput("model"),
    plotOutput("wmap"),
    verbatimTextOutput("command")
  )
))