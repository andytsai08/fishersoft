#################### Libraries and functions ##########################################################################


library(shiny)  # shiny app
library(shinydashboard) # dashboard layout
source("functions.R") # helper functions


#################### Server function ##################################################################################
server <- function(input, output) {
	repo_data <- reactiveValues(added = list()) # list to store added datasets and their info
	add_counter <- reactiveValues(time = 0) # counter for adding dataset

	observeEvent(input$uploadAdd, {
		add_counter$time <- add_counter$time + 1
		in_file <- input$fileUpload
		if (is.null(in_file) == FALSE & input$uploadName != "") {
			dataset_file <- read.csv(in_file$datapath, header = input$header, sep = input$sep, quote = input$quote)
			repo_data$added[[input$uploadName]] <- list(
				name = input$uploadName,
				des = input$uploadDes,
				counter = add_counter$time, 
				dataset = dataset_file
			)
		}
	})

	searched_datasets <- reactive({
		if (length(repo_data$added) == 0) {
			return("Repository is empty")
		} else if (input$searchDataRepo == "") {
			return(repo_data$added)
		} else if (length(repo_data$added) > 0) {
			all_name <- unlist(extract_ll(repo_data$added, "name"))
			all_des <- unlist(extract_ll(repo_data$added, "des"))
			matched_name <- grep(pattern = input$searchDataRepo, x = all_name)
			matched_des <- grep(pattern = input$searchDataRepo, x = all_des)
			matched_indexes <- union(matched_name, matched_des)
			if (length(matched_indexes) == 0) {
				return("No matched result")
			} else {
				return(repo_data$added[matched_indexes])
			}
		}
	})

	output$dataRepo <- renderUI({
		if (is.character(searched_datasets())) {
			return(searched_datasets())
		} else {
			dataset_boxes <- lapply(1:length(searched_datasets()), function(i) {
				dataset_name <- searched_datasets()[[i]]$name
				dataset_des <- searched_datasets()[[i]]$des
				dataset_rows <- nrow(searched_datasets()[[i]]$dataset)
				dataset_cols <- ncol(searched_datasets()[[i]]$dataset)
				dataset_delete_id <- paste0("dataset_delete", searched_datasets()[[i]]$counter)

				box(width = 12, status = "primary", solidHeader = TRUE, title = dataset_name, 
					p(paste0("Description: ", dataset_des), style = "color:#000"), 
					p(paste0("Number of rows: ", dataset_rows), style = "color:#000"),
					p(paste0("Number of columns: ", dataset_cols), style = "color:#000"),
					div(actionButton(inputId = dataset_delete_id, label = NULL, icon = icon("trash")), align = "right")
				)
			})
		}
	})

	observeEvent(input$emptyDataRepo, {
		repo_data$added <- list()
	})

	
} # end of server function