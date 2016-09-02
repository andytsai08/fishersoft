#################### Libraries ########################################################################################


library(shiny)  # shiny app
library(shinydashboard) # dashboard layout


#################### Header ###########################################################################################


header <- dashboardHeader(title = "Fishersoft")


#################### Sidebar ##########################################################################################


sidebar <- dashboardSidebar(
	sidebarMenu(
		menuItem("Upload Data", tabName = "upload", icon = icon("cloud-upload")), 
		menuItem("Repository", tabName = "repo", icon = icon("archive"), 
			menuSubItem("Datasets", tabName = "data", icon = icon("folder")), 
			menuSubItem("Analyses", tabName = "analyses", icon = icon("folder")), 
			menuSubItem("Plots", tabName = "plots", icon = icon("folder"))
		), 
		menuItem("Data Processing", tabName = "processing", icon = icon("magic")), 
		menuItem("Analysis", tabName = "analysis", icon = icon("calculator")),
		menuItem("Visualization", tabName = "vis", icon = icon("line-chart")), 
		menuItem("Download", tabName = "download", icon = icon("cloud-download"))
	)
)


#################### Body #############################################################################################


body <- dashboardBody(
	tabItems(
		tabItem(tabName = "upload", 
			fluidRow(
				column(width = 7, 
					box(title = "Add dataset to the repository", status = "primary", solidHeader = TRUE, width = 12,
						h4("Name of dataset"),
						p("Only numbers (except the first character), letters, and underscores are allowed."),
						textInput(inputId = "uploadName", label = NULL, value = "", width = "250px"),
						h4("Description"),
						p("Optional short description of the dataset"),
						textInput(inputId = "uploadDes", label = NULL, value = "", width = "400px"),
						fileInput(inputId = "fileUpload", label = NULL, 
							accept = c(
								"text/csv",
								"text/comma-separated-values",
								"text/tab-separated-values",
								"text/plain",
								".csv",
								".tsv" 
							)
						), 
						checkboxInput(inputId = "header", label = "Header", value = TRUE), 
						radioButtons(inputId = "sep", label = "Separater", inline = TRUE, 
							choices = c(Comma = ',', Semicolon = ';', Tab = '\t')
						), 
						radioButtons(inputId = "quote", label = "Quote", inline = TRUE,
							choices = c(None = '','Double Quote' = '"', 'Single Quote' = "'")
						), 
						div(actionButton(inputId = "uploadAdd", label = "Add"), align = "right")
					), 
					box(width = 12, 
						p("For sample datasets to upload, you can first download the sample file", 
							a(href = "sample1.csv", "sample1.csv"), # available for download when launched in browser
							"or",
							a(href = "sample2.csv", "sample2.csv"), # available for download when launched in browser
							"and then upload them."
						)
					)
				), 
				column(width = 5, 
					box(width = 12, background = "blue", 
						textInput(inputId = "searchDataRepo", label = "Search dataset repository", 
							placeholder = "Dataset name or description"
						)
					), 
					box(width = 12, background = "navy", title = "Dataset Repository", status = "primary", 
						solidHeader = TRUE, style = "overflow-y:scroll; max-height:450px", 
						uiOutput("dataRepo")
					), 
					column(width = 12, 
						div(actionButton("emptyDataRepo", "Empty repository"), align = "right")
					)
				)
			)
		)
	)
)


#################### Compile ##########################################################################################

ui <- dashboardPage(header, sidebar, body)
