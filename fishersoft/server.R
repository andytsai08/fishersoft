#################### Libraries and functions ##########################################################################


library(shiny)  # shiny app
library(shinydashboard) # dashboard layout
source("functions.R") # helper functions


#################### Server function ##################################################################################


server <- function(input, output) {


	#################### Set up storage and querying variables ########################################################


	repo_data <- reactiveValues(added = list()) # list to store added datasets and their info
	add_counter <- reactiveValues(time = 0) # counter for adding dataset; useful for functions querying specific datasets


	#################### Tab for uploading datasets ###################################################################


	observeEvent(input$uploadAdd, { # observe adding a new dataset to the dataset repo
		add_counter$time <- add_counter$time + 1
		in_file <- input$fileUpload
		if (is.null(in_file) == FALSE & input$uploadName != "") { # upload only when there is a file and user given name
			dataset_file <- read.csv(in_file$datapath, header = input$header, sep = input$sep, quote = input$quote)
			repo_data$added[[input$uploadName]] <- list(
				name = input$uploadName, # user given name for the dataset
				des = input$uploadDes, # user given description for the dataset 
				counter = add_counter$time, # the counter for querying the dataset
				dataset = dataset_file # the actual dataset in data frame
			)
		}
	})

	searched_datasets <- reactive({ # the datasets that match the user's search
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
				return(repo_data$added[matched_indexes]) # output the matched datasets
			}
		}
	})

	output$dataRepo_search <- renderUI({
		if (is.character(searched_datasets())) { # if no matched datasets
			return(searched_datasets()) # output the string describing the results 
		} else {
			dataset_boxes <- lapply(1:length(searched_datasets()), function(i) { # loop through each matched dataset
				dataset_name <- searched_datasets()[[i]]$name
				dataset_des <- searched_datasets()[[i]]$des
				dataset_rows <- nrow(searched_datasets()[[i]]$dataset)
				dataset_cols <- ncol(searched_datasets()[[i]]$dataset)

				box(width = 12, status = "primary", # for each matched dataset, create a box to display info
					solidHeader = TRUE, title = dataset_name, 
					p(paste0("Description: ", dataset_des), style = "color: #000"), 
					p(paste0("Number of rows: ", dataset_rows), style = "color: #000"),
					p(paste0("Number of columns: ", dataset_cols), style = "color: #000")
				)
			})
		}
	})

	observeEvent(input$emptyDataRepo, { # observe emptying the dataset repo
		repo_data$added <- list() # set the storage variable to an empty list
	})


	#################### Tab for the dataset repo #####################################################################


	output$dataRepo_details <- renderUI({
		if (length(repo_data$added) == 0) {
			h1("Repository is empty.", align = "center")
		} else {
			detail_boxes <- lapply(1:length(repo_data$added), function(i) { # loop through each item in the storage list
				title <- repo_data$added[[i]]$name
				description <- h4(paste0("Description: ", repo_data$added[[i]]$des))
				delete_id <- paste0("dataset_del", repo_data$added[[i]]$counter)

				box(title = title, solidHeader = TRUE, status = "primary", collapsible = TRUE, # create a box for display 
					width = 6, collapsed = TRUE, 
					description, 
					div(DT::renderDataTable(repo_data$added[[i]]$dataset), style = "padding: 10px; overflow-x: auto"), 
					div(actionButton(delete_id, label = NULL, icon("trash-o")), align = "right")
				)
			})
		}
	})

	observe({ # for deleting datasets from the repo
		if (length(repo_data$added) == 0) { # do nothing when the storage list is empty
			return(NULL)
		}
		delete_ids <- paste0("dataset_del", unlist(extract_ll(repo_data$added, "counter")))
		delete_vals <- sapply(delete_ids, function(id) input[[id]]) # number of clicks for the trash buttons
		if (any(sapply(delete_vals, is.null))) { # if any button is null
			return(NULL) # then do nothing; this is to prevent bug when a button has just been set to NULL
		}
		if (all(delete_vals == 0)) { # if all buttons have not been clicked
			return(NULL)
		}
		isolate({ # no dependency on delete_vals in the scope to avoid error
			repo_data$added[[which(delete_vals > 0)]] <- NULL
		})
	})
	
} # end of server function