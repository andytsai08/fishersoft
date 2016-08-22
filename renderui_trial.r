#this version you can only upload one dataset
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

header <- dashboardHeader(title = "Fishersoft",
			dropdownMenu(type = "notifications",
  				notificationItem(
    			text = "8 datasets saved on Fishersoft",
    			icon("table"),
    			status="primary"
  			),
 		 		notificationItem(
    				text = "12 plots saved on Fishersoft",
    				icon("area-chart"),
    				status = "primary"
  			),
  			notificationItem(
    			text = "Server load at 86%",
    			icon = icon("exclamation-triangle"),
    			status = "primary"
  			)
			)
	)

sidebar <- 	dashboardSidebar(
			  sidebarMenu(
      			menuItem("Home", tabName = "home", icon = icon("cloud-upload")),
      			menuItem("Manage", tabName = "manage", icon = icon("signal")),
      			menuItem("Analyze", tabName = "analyze", icon = icon("percent")),
      			menuItem("Plot", tabName = "plot", icon = icon("line-chart")),
      			menuItem("Machine Learning", tabName = "ml", icon = icon("bar-chart")),
      			menuItem("Download", tabName = "download", icon = icon("cloud-download"))
    		  )
			)

body <-	dashboardBody(
		  tabItems(
			tabItem(tabName = "home",
			    fluidRow(
				box(title = "Import Your Data Here!", status="primary", solidHeader = TRUE, 
				  fileInput(inputId = "fileinput", "Upload", 
				    accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
				  tags$hr(),
				  checkboxInput('header', 'Header', TRUE),
				  radioButtons('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),','),
				  radioButtons('quote', 'Quote', c(None='','Double Quote'='"', 'Single Quote'="'"),'"')
    			)
			  )
			),
					
			tabItem(tabName = "manage",
				uiOutput("ui_manage")
			),

			tabItem(tabName = "analyze",
				uiOutput("ui_analyze")
			),

			tabItem(tabName = "plot",
				uiOutput("ui_plot")
			),

			tabItem(tabName = "ml",
				uiOutput("ui_ml")
			),

			tabItem(tabName = "download",
				uiOutput("ui_download")
			)
		)
	)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
	#creates referencable dataset for later use
	inFile <- reactive({input$fileinput})
	dataset_ref <- reactive({read.csv(inFile()$datapath, header=input$header, sep=input$sep, quote=input$quote)})

	#manage tab
	output$ui_manage <- renderUI({
		list(
			box(title = "Table", status="primary", solidHeader = TRUE, DT::dataTableOutput('contents')),
			box(title = "Select Columns", status="primary", solidHeader = TRUE, 
				checkboxGroupInput('show_vars', 'Columns in dataset to show:', names( dataset_ref() ) , selected = names( dataset_ref() ) ), 
				p(class = 'text-center', downloadButton('x3', 'Download Filtered Data'))
			)
			)
	})

	output$contents <- DT::renderDataTable(dataset_ref()[,input$show_vars, drop=FALSE], server=FALSE)

	output$x3 <- downloadHandler('my_dataset.csv', content = function(file) {
    	#write csv is working but the download is not what i want, so something is wrong with row selector
    	s = input$contents_rows_all
    	write.csv(dataset_ref(), file)
   	})
}

shinyApp(ui = ui, server = server)