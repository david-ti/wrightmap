library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  # Application title
  titlePanel("Wright Map"),
  
  # Sidebar with a slider input for number of observations
  sidebarLayout(
  sidebarPanel(
  	wellPanel(
    	radioButtons('showPane', 'Show:', choices = c("File options" = "files","Data options" = "data","Text options" = "labels","Person display options" = "person.disp","Symbol options" = "sym.disp","Item color options" = "color.disp"))
    	),
  		conditionalPanel(condition="input.showPane=='files'",
  			selectInput('datatype',"Type of data", choices = c("ConQuest output" = "CQ","R object" = "R")),
  			conditionalPanel(condition = "input.datatype == 'R'",
  				radioButtons('make_from',"Input item parameters",choices = c("deltas","thresholds"),inline = TRUE),
  				textInput('thetas',"Name of person parameters object"),
  				textInput('thresholds',"Name of item parameters object"),
  				textInput('slopes',"Name of slopes object (if not specified, assumed all are 1)")
  			),
  			conditionalPanel(condition="input.datatype == 'CQ'",
    			fileInput('eap', 'Choose Person Estimates File'),
    			fileInput('shw', 'Choose Show File'),
    			selectInput("p.type", "Person estimates type:", choices = c("EAP", "MLE", "WLE"))
    		)
    	),
    	conditionalPanel(condition = "input.showPane == 'data'",
    		conditionalPanel(condition = "input.datatype == 'CQ'",
    			uiOutput("item.table"),
    			uiOutput("step.table"),
    			uiOutput("interactions.table")
    		),
    		conditionalPanel(condition = "input.make_from == 'deltas' || input.datatype == 'CQ'",
    			selectInput("which_type","Graph which parameters?", choices = c("default","Thresholds" = "thresholds","Deltas" = "deltas"))
    		)
    		
    	),
		conditionalPanel(condition="input.showPane == 'labels'",
			checkboxInput('autotitle',"Create title automatically",TRUE),
			conditionalPanel(condition = "!input.autotitle",
				textInput('title',"Title")
			),
			checkboxInput('show.thr.lab', 'Show Threshold Labels', TRUE),
			textInput('axis.logits',"Logit axis label","Logits"),
			textInput('axis.persons',"Respondents axis label","Respondents"),
			textInput('axis.items',"Items axis label","Items")
    	),
    	conditionalPanel(condition="input.showPane=='person.disp'",
    		checkboxInput('use.hist', 'Histogram?', TRUE)
    	),
    	conditionalPanel(condition="input.showPane=='sym.disp'",
		    sliderInput(
		    				"cex", 
		                	"Symbol size", 
		                	min = 1,
		               	max = 5, 
		                	value = 1.2, step = .1
		    ),
		    selectInput('sym_by',"Choose symbols by",choices = c("all","step","item")),
		    uiOutput("sym_pickers")
		),
		conditionalPanel(condition="input.showPane=='color.disp'",
		    selectInput('color_by',"Choose colors by",choices = c("all","step","item")),
		    uiOutput("color_pickers")
		)
		
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
  #textOutput("model"),
  	tabsetPanel(type = "tabs",
    	tabPanel("Wright map",
    		plotOutput("wmap"),
    		conditionalPanel(condition = "input.which_type!='deltas' || (input.datatype == 'R' && input.make_from == 'thresholds')",
    			sliderInput("throld","Threshold",min=.01,max = .99, value = .5, step = .01)
    		),
    		verbatimTextOutput("command")
    	),
    	tabPanel("Fit plot",
    		uiOutput("fitPlot.ui"),
    		sliderInput("width","Plot width",min = 1, max = 100, value = 100, step = 1)
    		
    	)
    )
  )
)))