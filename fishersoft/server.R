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

	output$dataRepo_search <- renderUI({
		if (is.character(searched_datasets())) {
			return(searched_datasets())
		} else {
			dataset_boxes <- lapply(1:length(searched_datasets()), function(i) {
				dataset_name <- searched_datasets()[[i]]$name
				dataset_des <- searched_datasets()[[i]]$des
				dataset_rows <- nrow(searched_datasets()[[i]]$dataset)
				dataset_cols <- ncol(searched_datasets()[[i]]$dataset)

				box(width = 12, status = "primary", solidHeader = TRUE, title = dataset_name, 
					p(paste0("Description: ", dataset_des), style = "color: #000"), 
					p(paste0("Number of rows: ", dataset_rows), style = "color: #000"),
					p(paste0("Number of columns: ", dataset_cols), style = "color: #000")
				)
			})
		}
	})

	observeEvent(input$emptyDataRepo, {
		repo_data$added <- list()
	})
	
	output$testing <- renderText({
		unlist(extract_ll(repo_data$added, "counter"))
	})

	output$dataRepo_details <- renderUI({
		if (length(repo_data$added) == 0) {
			h1("Repository is empty.", align = "center")
		} else {
			detail_boxes <- lapply(1:length(repo_data$added), function(i) {
				title <- repo_data$added[[i]]$name
				description <- h4(paste0("Description: ", repo_data$added[[i]]$des))
				delete_id <- paste0("dataset_del", repo_data$added[[i]]$counter)

				box(title = title, solidHeader = TRUE, status = "primary", collapsible = TRUE, 
					width = 6, collapsed = TRUE, 
					description, 
					div(DT::renderDataTable(repo_data$added[[i]]$dataset), style = "padding: 10px; overflow-x: auto"), 
					div(actionButton(delete_id, label = NULL, icon("trash-o")), align = "right")
				)
			})
		}
	})

	observe({
		if (length(repo_data$added) == 0) {
			return(NULL)
		}
		delete_ids <- paste0("dataset_del", unlist(extract_ll(repo_data$added, "counter")))
		delete_vals <- sapply(delete_ids, function(id) input[[id]])
		if (any(sapply(delete_vals, is.null))) {
			return(NULL)
		}
		if (all(delete_vals == 0)) {
			return(NULL)
		}
		isolate({
			repo_data$added[[which(delete_vals > 0)]] <- NULL
		})
	})
	
} # end of server function