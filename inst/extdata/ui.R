library(shiny)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  # Application title
  titlePanel("Wright Map"),
  
  
  sidebarLayout(
  sidebarPanel(
  	wellPanel(
    	radioButtons('showPane', 'Show:', choices = c("File options" = "files","Data options" = "data","Formatting options" = "format","Dimension options" = "dims", "Text options" = "labels","Item labels" = "label.items","Person display options" = "person.disp","Symbol options" = "sym.disp","Item color options" = "color.disp"))
    	),
  		conditionalPanel(condition="input.showPane=='files'",
  			selectInput('datatype',"Type of data", choices = c("ConQuest output" = "CQ","R object" = "R")),
  			conditionalPanel(condition = "input.datatype == 'R'",
  				conditionalPanel(condition = "input.selectedTab == 'wmap'",
  					textInput('thetas',"Name of person parameters object"),
  					textInput('thresholds',"Name of item parameters object"),
  					textInput('slopes',"Name of slopes object (if not specified, assumed all are 1)"),
  					textInput('c.params',"Name of guessing parameters object (if not specified, assumed all are 0)")
  				),
  				conditionalPanel(condition = "input.selectedTab == 'fitgraph'",
  					textInput('fitEst',"Name of fit estimates object"),
  					textInput('fitLB',"Name of fit lower bounds object"),
  					textInput('fitUB',"Name of fit upper bounds object")
  				),
  				conditionalPanel(condition = "input.selectedTab == 'difplot'",
  					textInput('difEst',"Name of estimates object"),
  					textInput('difErr',"Name of errors object")
  				)
  			),
  			conditionalPanel(condition="input.datatype == 'CQ'",
    			fileInput('shw', 'Choose Show File'),
    			conditionalPanel(condition = "input.selectedTab == 'wmap'",
    				fileInput('eap', 'Choose Person Estimates File'),
    				selectInput("p.type", "Person estimates type:", choices = c("EAP", "MLE", "WLE"))
    			)
    		)
    	),
    	conditionalPanel(condition = "input.showPane == 'data'",
    		conditionalPanel(condition = "input.selectedTab == 'wmap'",
	    		conditionalPanel(condition = "input.datatype == 'CQ'",
	    			uiOutput("item.table"),
	    			uiOutput("step.table"),
	    			uiOutput("interactions.table")
	    		),
	    		conditionalPanel(condition = "input.datatype == 'R'",
	    			radioButtons('make_from',"Input item parameters",choices = c("deltas","thresholds"),selected = "thresholds",inline = TRUE)),
	    		conditionalPanel(condition = "input.make_from == 'deltas' || input.datatype == 'CQ'",
	    			selectInput("which_type","Graph which parameters?", choices = c("default","Thresholds" = "thresholds","Deltas" = "deltas"))
	    		)
	    	),
	    	conditionalPanel(condition = "input.selectedTab == 'fitgraph'",
	    		radioButtons("fit.type", "Fit type",choices = c("Weighted" = "W", "Unweighted" = "U"))
	    	),
	    	conditionalPanel(condition = "input.selectedTab == 'fitgraph' || input.selectedTab == 'difplot'",
	    		selectInput("pick.table","Graph which table?",choices = c("Please select files" = "none"))),
	    	conditionalPanel(condition = "input.selectedTab == 'difplot'",
	    		selectInput("grouptype","Group by",choices = c("Please select files" = "none")),
	    		selectInput("group","Group",choices = c("Please select files" = "none"))
	    	)
    		
    	),
    	conditionalPanel(condition = "input.showPane == 'format'",
    		textInput("minl","Minimum Logit"),
    		textInput("maxl","Maximum Logit"),
    		sliderInput("itemprop","Item proportion",min = .25,max = .9,value = formals(wrightMap.default)[["item.prop"]],step = .01)
    	),
    	conditionalPanel(condition = "input.showPane == 'dims'",
    		uiOutput("dim.items")
    	),
		conditionalPanel(condition="input.showPane == 'labels' && input.selectedTab == 'wmap'",
			checkboxInput('autotitle',"Create title automatically",TRUE),
			conditionalPanel(condition = "!input.autotitle",
				textInput('title',"Title")
			),
			textInput('axis.logits',"Logit axis label","Logits"),
			textInput('axis.persons',"Respondents axis label","Respondents"),
			textInput('axis.items',"Items axis label","Items"),
			uiOutput("dim.labels")
    	),
    	conditionalPanel(condition = "input.showPane=='label.items'",
    		checkboxInput('show.thr.lab', 'Show Threshold Labels', TRUE),
    		selectInput("label_items_rows","Rows of item labels",choices = c(1,2,3)),
    		checkboxInput("label.items.ticks","Tick marks",value = TRUE),
    		conditionalPanel(condition = "input.label_items_rows == 1",
    			sliderInput("label.items.srt","Rotate item labels",min = 0,max= 90,value = 0,step = 5)),
    		selectInput("label_items","Label items:",choices = c("Default" = "default","Numbers" = "num","Custom" = "custom")),
    		#checkboxInput('autolabel',"Create item labels automatically",value = TRUE),
    		conditionalPanel(condition = "input.label_items == 'custom'",
    			uiOutput("item.labels"))
    	),
    	conditionalPanel(condition="input.showPane=='person.disp'",
    		checkboxInput('use.hist', 'Histogram?', TRUE),
    		uiOutput("dim.colors")
    	),
    	conditionalPanel(condition="input.showPane=='sym.disp'",
    		checkboxInput('show.thr.sym', 'Show Threshold Symbols', TRUE),
		    sliderInput(
		    				"cex", 
		                	"Symbol size", 
		                	min = 1,
		               	max = 5, 
		                	value = 1.2, step = .1
		    ),
		    selectInput('sym_by',"Choose symbols by",choices = c("all","step","dim","item"),selected="all"),
		    uiOutput("sym_pickers")
		),
		conditionalPanel(condition="input.showPane=='color.disp'",
		    selectInput('color_by',"Choose colors by",choices = c("all","step","dim","item")),
		    uiOutput("color_pickers")
		)
		
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
  #textOutput("model"),
  	tabsetPanel(type = "tabs",
    	tabPanel("Wright map",
    		verbatimTextOutput("wmap.command"),
    		plotOutput("wmap"),
    		conditionalPanel(condition = "input.which_type!='deltas' || (input.datatype == 'R' && input.make_from == 'thresholds')",
    			sliderInput("throld","Threshold",min=.05,max = .95, value = .5, step = .05,animate = animationOptions(loop = TRUE,interval=500))
    		),
    		textInput("est","Individual ability estimate"),
    		textInput("err","Individual standard error"),
    		value = "wmap"
    	),
    	tabPanel("Fit plot",
    		uiOutput("fitPlot.ui"),
    		sliderInput("width","Plot width",min = 1, max = 100, value = 100, step = 1),
    		verbatimTextOutput("fitplot.command"),
    		value = "fitgraph"
    	),
    	tabPanel("Dif plot",
    		plotOutput("difplot"),
    		verbatimTextOutput("difplot.command"),
    		value = "difplot"
    	),
    	id = "selectedTab"
    ),
    h4("David Torres Irribarra & Rebecca Freund (2014)")
  )
)))