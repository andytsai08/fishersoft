

#################### THINGS TO WORK ON ################################################################################

# take out the "value" box for displaying variable details; use the main box instead
# review the codes for filtering table and resolve the bugs
# analysis for cross tabulation
# visualization for correlation, multiple regression/ single regression, survival analysis
# implement the variable cart inspection feature
# make the app EPIC database specific in the back end


#################### BUGS TO BE RESOLVED ##############################################################################


# if only one character variable is selected for a table, adding a variable for filtering will exit out
# numeric does not have this issue
# If only one variable is selected for a table, adding a variable/filtering will cause the app to exit out

# Also need to consider the case when some computation gives error

# cannot convert ExamDate to Date object; use tryCatch for all the data processing to account for error


#################### Libraries and Data ###############################################################################


library(shiny) # for shiny app
library(shinydashboard) # for shiny dashboard layout
library(DT) # for using the JavaScript library DataTables; use the newest version downloaded via github
source("Functions-web-browser.R") 
load("allData.rda") # load all the tables (as data frames) from the EPIC ETL database (maintained by Adam)


#################### Set Up Table Info Outside Server Function ########################################################


tableNames <- ls() # get a character vector of all the names of the data frames
nTable <- length(tableNames) # number of tables (data frames)
dataTableNames <- paste(tableNames, "TableOutput", sep = "") # names for the data frame inputs
dataTableSelected <- paste(dataTableNames, "_rows_selected", sep = "") # the names for rows selected of the data tables

tables <- lapply(1:length(tableNames), function(i) {
  menuSubItem(tableNames[i], tabName = tableNames[i], icon = icon("table")) # make menu sub items for the data tables
})

tableTabs <- lapply(1:length(tableNames), function(i) { 
  dataTableName <- paste(tableNames[i], "TableOutput", sep = "") # the output names of the data tables
  tabItem(tabName = tableNames[i], h2(paste(tableNames[i], sep = "")), 
          DT::dataTableOutput(dataTableName)) # make tab items to output the data tables
})


#################### Username and Password ############################################################################


access_username <- "username"
access_password <- "password"


#################### Envrionment Scoping ##############################################################################


env <- environment() # set an environment for accessing the above objects in the server function


#################### Server Function ##################################################################################


server <- function(input, output) {


  #################### Log in UI ######################################################################################


  access <- reactiveValues(granted = FALSE)
  
  output$loginUI <- renderUI({
    if (access$granted == FALSE) { # if access not granted
      return() # render nothing
    } else if (access$granted == TRUE) { # if access granted
        fluidRow(
          column(width = 8, 
            tags$div(tableTabs, # put the sub menu item tabs in place
                          tabItem(tabName = "variables", # tab for variable cart
                                  br(), 
                                  tableOutput("test"), ############### TESTING ########################################
                                  textOutput("test2"), ############### TESTING ########################################
                                  uiOutput("variableCartBoxes") # the boxes in variable cart
                          ),
                          tabItem(tabName = "processData", # tab for processing data
                                  br(),
                                  tabsetPanel(
                                    tabPanel(title = "Filter Tables", # tab for filtering tables
                                             br(), 
                                             uiOutput("filterUIOutput") # UI for filtering tables
                                    ), 
                                    tabPanel(title = "Merge Tables", 
                                             br(), 
                                             uiOutput("mergeUIOutput")), # tab for merging tables
                                    tabPanel(title = "Convert Variable Types", # tab for converting variable type
                                             br(), 
                                             uiOutput("convertUIOutput"))
                                  )
                          ),
                          tabItem(tabName = "analysis", # tab for analysis
                            br(),
                            tabsetPanel(
                              tabPanel(title = "Correlation", 
                                br(),
                                uiOutput("corrUIOutput")
                                ), 
                              tabPanel(title = "Regression", 
                                br(),
                                uiOutput("regressUIOutput")
                                )
                              )
                            ), 
                          class = "tab-content")
          ),
          column(width = 4, 
            br(),
            box(title = "VARIABLE CART", width = 12, status = "primary", solidHeader = TRUE,
              background = "navy", 
              style = "overflow-y: scroll; max-height: 600px",
              uiOutput("cart_var_details")
            ), 
            column(width = 12, div(actionButton("emptyVariableCart", "Empty variable cart"), align = "right"))
          )
        )
    }
  })
  
  observeEvent(input$login, {
    if (input$username == access_username & input$password == access_password) {
      access$granted <- TRUE # grant access when username and password are correct
    } else {
      access$granted <- FALSE
    }
  })
  
  observeEvent(input$logout, { # deny access when logging out 
    access$granted <- FALSE
  })
  

  #################### Table Info and Selection #######################################################################

  
  tableInfoList <- reactive({tableInfo_list(tableNames = tableNames, envir = env)}) # list of all the info data frames 
  
  ####### Information tables for exploring data #######

  for (i in 1:nTable) { # loop through each data frame
    local({ # to loop inside the server function
      local_i <- i
      output[[dataTableNames[local_i]]] <- DT::renderDataTable({
        tableInfoList()[[local_i]] # output the infomation data frames in DT format
      })
    })
  }
  
  ####### Row selection by the user #######

  numRowSelected <- reactive({
    eachTable <- rep(NA, times = nTable) # integer vector; number of selected rows for each table
    for (i in 1:nTable) {
      eachTable[i] <- length(input[[dataTableSelected[i]]]) 
    }
    return(list(sum(eachTable, na.rm = FALSE), eachTable)) 
    # first object is the total number of selected rows; 
    # second object contains the number of selected rows for each table
  })
    
  ####### All info for the selected tables #######

  existing <- reactive({
    if (numRowSelected()[[1]] > 0) {
          varCartSelected_list(tableNames = tableNames, table_numRow_selected = numRowSelected()[[2]], 
                         table_rowSelected_names = dataTableSelected, table_infoList = tableInfoList(), 
                         input = input, envir = env)
    } else {
      return(NULL)
    }
  })
  

  #################### Eixting and Created Tables #####################################################################


  newTables <- reactiveValues(tableList = list()) # list of created tables
  createCounter <- reactiveValues(times = 0) # counter for creating new variables

  created <- reactive({
    if (length(newTables$tableList) > 0) {
      return(createdTable_list(newTables$tableList))
    } else {
      return(NULL)
    }
  })

  allTables <- reactive({
    if (length(existing()) + length(created()) == 0) {
      return(NULL)
    } else if (length(existing()) == 0) {
      return(created())
    } else if (length(created()) == 0) {
      return(existing())
    } else if (length(existing()) > 0 & length(created()) > 0) {
      return(append_ll(existing(), created()))
    }
  })


  #################### Filter Tables ##################################################################################


  ####### UI for Filtering Tables #######

  filterContainer1 <- reactiveValues(uiComponents = list()) # initiation UI container 1 for variable and condition
  filterContainer2 <- reactiveValues(uiComponents = list()) # initiation UI container 2 for operator
  filterContainer3 <- reactiveValues(uiComponents = list()) # initiation UI container 3 for value
  filterContainer4 <- reactiveValues(uiComponents = list()) # initiation UI container 4 for remove
  
  output$filterContainer1 <- renderUI({filterContainer1$uiComponents}) # UI output container 1
  output$filterContainer2 <- renderUI({filterContainer2$uiComponents}) # UI output container 2
  output$filterContainer3 <- renderUI({filterContainer3$uiComponents}) # UI output container 3
  output$filterContainer4 <- renderUI({filterContainer4$uiComponents}) # UI output container 4
  
  output$filterUIOutput <- renderUI({ # UI for the tab
    if (length(allTables()) == 0) { # if no tables
      h1("Variable cart is empty.", align = "center") # then show "Variable cart is empty."
    } else {
      ####### Box for typing the new (filtered) table name and description for the table #######
      div(box(width = 12,
              h4("New table name"),
              textInput("newFilterNameInput", label = NULL, width = "250px"), 
              # NOTE: the new table name should not contain space; should only have underscore
              h4("Description"),
              textInput("newFilterDesInput", label = NULL, width = "550px")
              ), 
          
          ####### Box for adding filter criteria #######

          box(width = 12,
              fluidRow(
                column(width = 12, 
                       fluidRow(
                         column(width = 12, 
                                selectInput("filterSelectInput", label = "Select table", choices = allTables()[[1]]),
                                # action button to select table for filtering
                                
                                p(actionButton("filterAddButton", "Add variable"), actionButton("resetFilterBtn", "Reset"))
                                # action button to add variable; action button to clear all criteria)
                         )
                       ), 
                       br(),
                       fluidRow(
                         column(width = 4, uiOutput("filterContainer1")), 
                         column(width = 3, uiOutput("filterContainer2")), 
                         column(width = 3, uiOutput("filterContainer3")), 
                         column(width = 1, uiOutput("filterContainer4"))
                       ), 
                       br(), 
                       br(),
                       br(),
                       br(),
                       fluidRow(
                         column(width = 3, actionButton("filterCreate", "Filter table"))
                         )
                    )
                )
              )
          ) 
    }
  })
  
  ####### Give the possible variables as criteria for the selected table #######

  filterChoices <- reactive({
    table <- input$filterSelectInput # the user's choice of table
    matchedIndex <- allTables()[[1]] == table # index the table name
    selectedVars <- allTables()[[4]][matchedIndex][[1]] # find the selected variable names for the table
    return(selectedVars) # output the variable names
  })
  
  ####### The selected variable types for the selected table #######

  filterVarTypes <- reactive({
    table <- input$filterSelectInput # the user's choice of table
    matchedIndex <- allTables()[[1]] == table # index the table name
    types <- allTables()[[5]][matchedIndex][[1]] # find the selected variable types for the table
    return(types) # ouput the names of the variable types
  })
  
  ####### The selected table #######

  filterTable <- reactive({
    table <- input$filterSelectInput # the user's choice of table
    matchedIndex <- allTables()[[1]] == table # index the table name
    df <- allTables()[[3]][matchedIndex][[1]]
    return(df)
  })
  
  ####### Unique values for the selected variables #######

  filterUniqueVals <- reactive({
    uniqueList <- list()
    for (i in 1:ncol(filterTable())) {
      uniqueList[[i]] <- sort(unique(filterTable()[ , i]))
    }
    return(uniqueList)
  })
    
  ####### Observe the clicking of the add button and generate new UI element #######

  observe({
    if (is.null(input$filterAddButton) || input$filterAddButton == 0) return(NULL)
    
    isolate({
      filterContainer1$uiComponents <- append(
        filterContainer1$uiComponents, 
        list(
          list(
            "variable" = div(my_selectInput("filterVariable", input$filterAddButton, choices = filterChoices()), 
                             style = "float:left; width:100%; height:60px; overflow-y:scroll"), 
            "condition" = div(radioButtons(inputId = paste0("filterCondition", input$filterAddButton), label = "", 
                                           choices = c("AND", "OR", "NONE"), inline = TRUE), 
                              style = "float:left; width:100%; height:60px")
            )
          )
        )
    })
    
    isolate({
      filterContainer2$uiComponents <- append(
        filterContainer2$uiComponents, 
        list(
          list(
            "operator" = div(my_selectInput("filterOperator", input$filterAddButton, 
                                            choices = c("=", "!=", ">", "<", ">=", "<="), 
                                            label = "Operator"), 
                             style = "float:left; height:60px; width:100%; overflow-y:scroll"), 
            br(), br(), br(), br(), br(), br()
            )
          )
        )
    })
    
    isolate({
      filterContainer3$uiComponents <- append(
        filterContainer3$uiComponents, 
        list(
          list(
            "value" = div(my_textInput("filterValue", input$filterAddButton), style = "float:left; width:100%"), 
            br(), br(), br(), br(), br(), br()
            )
          )
        )
    })
    
    isolate({
      filterContainer4$uiComponents <- append(
        filterContainer4$uiComponents, 
        list(
          list(
            "remove" = div(removeButton("filterRemove", input$filterAddButton, icon = icon("times"), width = "100%"), 
                           style = "float:left"), 
            br(), br(), br(), br(), br(), br()
            )
          )
        )
    })
  })
  
  ####### Find the existing indices for the UI elements #######

  filterIds <- reactive({
    variableIds <- sapply(filterContainer1$uiComponents, function(uiCom) {
      uiCom$variable$children[[1]]$children[[2]]$children[[1]]$attribs$id
    })
    Ids <- gsub(pattern = "filterVariable", replacement = "", x = variableIds)
    return(Ids)
  })
  
  ####### Observe self-delete to delete the whole UI container #######

  observe({
    if (length(filterContainer1$uiComponents) == 0) return(NULL)
    filterRemoveIds <- paste0("filterRemove", filterIds())
    filterRemoveVals <- sapply(filterRemoveIds, function(id) input[[id]])
    
    if (any(sapply(filterRemoveVals, is.null))) return(NULL)
    if (all(filterRemoveVals == 0)) return(NULL)
    
    isolate({
      filterContainer1$uiComponents[[which(filterRemoveVals > 0)]] <- NULL
      filterContainer2$uiComponents[[which(filterRemoveVals > 0)]] <- NULL
      filterContainer3$uiComponents[[which(filterRemoveVals > 0)]] <- NULL
      filterContainer4$uiComponents[[which(filterRemoveVals > 0)]] <- NULL
    })
  })
  
  ####### Find the input values #######

  filterVariables <- reactive({find_inputValues(name = "filterVariable", ids = filterIds(), input = input)})
  filterConditions <- reactive({find_inputValues(name = "filterCondition", id = filterIds(), input = input)})
  filterOperators <- reactive({find_inputValues(name = "filterOperator", id = filterIds(), input = input)})
  filterValues <- reactive({find_inputValues(name = "filterValue", id = filterIds(), input = input)})
  
  ####### Keep the user-selected inputs and change UI elements based on variable type #######

  observe({
    if (length(filterContainer1$uiComponents) == 0) return(NULL)
    
    #### Find the input ids ####
    filterVariableIds <- paste0("filterVariable", filterIds())
    filterConditionIds <- paste0("filterCondition", filterIds())
    filterOperatorIds <- paste0("filterOperator", filterIds())
    filterValueIds <- paste0("filterValue", filterIds())
    
    #### Find the input values ####
    filterVariableVals <- sapply(filterVariableIds, function(id) input[[id]])
    filterConditionVals <- sapply(filterConditionIds, function(id) input[[id]])
    filterOperatorVals <- sapply(filterOperatorIds, function(id) input[[id]])
    filterValueVals <- lapply(filterValueIds, function(id) input[[id]])
    
    #### Find the variable types ####
    filterTypes <- sapply(filterVariableVals, find_matched, varNames = filterChoices(), 
                          varTypes = filterVarTypes())

    isolate({
      for (i in 1:length(filterIds())) {
        filterVariableId <- filterVariableIds[i]
        matchedIndex <- filterVariableIds == filterVariableId
        
        ## Variable ##
        filterContainer1$uiComponents[matchedIndex][[1]]$variable <- div(
          selectInput(inputId = filterVariableId, label = "Variable", choices = filterChoices(), 
                      selected = filterVariableVals[i]), 
          style = "float:left; width:100%; height:60px")
        
        ## Condition ##
        filterContainer1$uiComponents[matchedIndex][[1]]$condition <- div(
          radioButtons(inputId = filterConditionIds[i], label = "", choices = c("AND", "OR", "NONE"), inline = TRUE, 
                       selected = filterConditionVals[i]), 
          style = "float:left; width:100%; height:60px")
        
        ## Operator and value (based on the variable type) ##
        if (filterTypes[i] == "integer" | filterTypes[i] == "numeric") {
          filterContainer2$uiComponents[matchedIndex][[1]]$operator <- div(
            selectInput(inputId = filterOperatorIds[i], label = "Operator", 
                        choices = c("=", "!=", ">", "<", ">=", "<="), selected = filterOperatorVals[i]), 
            style = "float:left; height:60px; width:100%")
          
          filterContainer3$uiComponents[matchedIndex][[1]]$value <- div(
            textInput(inputId = filterValueIds[i], label = "Value", value = filterValueVals[[i]]), 
            style = "float:left; width:100%")
          
        } else if (filterTypes[i] == "character") {
          filterContainer2$uiComponents[matchedIndex][[1]]$operator <- div(
            selectInput(inputId = filterOperatorIds[i], label = "Operator", 
                        choices = c("=", "!="), selected = filterOperatorVals[i]), 
            style = "float:left; height:60px; width:100%")
          
          variable <- filterVariableVals[i]
          matchedVarIndex <- which(colnames(filterTable()) == variable)
          filterContainer3$uiComponents[matchedIndex][[1]]$value <- div(
            selectInput(inputId = filterValueIds[i], label = "Class", multiple = TRUE, 
                        choices = filterUniqueVals()[[matchedVarIndex]], selected = filterValueVals[[i]]), 
            style = "float:left; height:60px; width:100%")
        }
      }
    }) 
  })
  
  ####### Observe the reset button and make the whole UI container NULL #######

  observeEvent(input$resetFilterBtn, {
    filterContainer1$uiComponents <- NULL
    filterContainer2$uiComponents <- NULL
    filterContainer3$uiComponents <- NULL
    filterContainer4$uiComponents <- NULL
  })
  
  ####### Observe the clicking of filter table and create the filtered table #######

  
  observeEvent(input$filterCreate, {
    createCounter$times <- createCounter$times + 1
    filteredIndex <- eval_filter(df = filterTable(), variables = filterVariables(), operators = filterOperators(), 
                                 values = filterValues(), conditions = filterConditions())
    newTables$tableList[[input$newFilterNameInput]] <- list(filterTable()[filteredIndex, ], # filtered table
                                                                input$newFilterDesInput, # description
                                                                createCounter$times) # counter 
  })


  #################### Merge Tables ###################################################################################


  ####### UI for merging table #######

  output$mergeUIOutput <- renderUI({ # UI for the tab
    if (length(allTables()) == 0) { # if no tables
      h1("Variable cart is empty.", align = "center") # then show "Variable cart is empty."
    } else {

      ####### Box for typing the new (merged) table name and description for the table #######

      div(box(width = 12,
              h4("New table name"),
              textInput("newMergeNameInput", label = NULL, width = "250px"),
              h4("Description"),
              textInput("newMergeDesInput", label = NULL, width = "550px")
      ),
      
      ####### Box for adding table selections #######

      box(width = 12, 
          fluidRow(
            column(width = 8,
                   fluidRow(column(width = 12, uiOutput("mergeTableSelect"))), 
                   br(), 
                   br(),
                   fluidRow(column(width = 12, uiOutput("mergeVariableSelect"))),
                   br(), 
                   br(), 
                   br(),
                   br(), 
                   br(),
                   fluidRow(column(width = 3, actionButton("mergeCreate", "Merge tables")))
                   )
            )
          )
      )
    }
  })
  
  output$mergeTableSelect <- renderUI({ 
    selectizeInput(inputId = "mergeTable", label = "Select tables", choices = allTables()[[1]], multiple = TRUE, 
                   options = list(maxItems = 2))
  })
  
  
  ####### A list of the selected data frames to merge #######

  mergeSelectedTables <- reactive({
    matchIndex <- match(x = input$mergeTable, table = allTables()[[1]])
    return(allTables()[[3]][matchIndex])
  })
  
  ####### The variables avaiable for the tables to be mergered on #######

  mergeTableVars <- reactive({
    matchIndex <- match(x = input$mergeTable, table = allTables()[[1]])
    return(allTables()[[4]][matchIndex])
  })
  
  ####### UI for selecting the variables to merge the table #######

  output$mergeVariableSelect <- renderUI({
    if (length(input$mergeTable) <= 1) {
      h4("Need to select more tables.", align = "left")
    } else if (length(Reduce(intersect, mergeTableVars())) == 0) {
      h4("No common variables.", align = "left")
    } else {
      selectInput(inputId = "mergeVariable", label = "Merge with variable", 
                  choices = Reduce(intersect, mergeTableVars()), multiple = FALSE)
    }
  })
  
  ####### Observe the clicking of "merge tables" and call function to merge the selected tables #######

  
  observeEvent(input$mergeCreate, {
    createCounter$times <- createCounter$times + 1
    merged <- merge(mergeSelectedTables()[[1]], mergeSelectedTables()[[2]], by = input$mergeVariable)
    newTables$tableList[[input$newMergeNameInput]] <- list(merged, input$newMergeDesInput, createCounter$times)
  })


  #################### Convert Variable Types #########################################################################


  ####### UI for converting variable type #######

  convert_message <- reactiveValues(mesg = vector())

  output$convertUIOutput <- renderUI({
    if (length(allTables()) == 0) { # if no tables
      h1("Variable cart is empty.", align = "center") # then show "Variable cart is empty."
    } else {

      ####### Box for typing the new table name and description for the table #######

      div(box(width = 12,
              h4("New table name"),
              textInput("newConvertNameInput", label = NULL, width = "250px"),
              h4("Description"),
              textInput("newConvertDesInput", label = NULL, width = "550px")
      ),
      
      ####### Box for adding table and variable selections #######

      box(width = 12, 
          fluidRow(
            column(width = 8,
                   fluidRow(column(width = 12, uiOutput("convertTableSelect"))), 
                   br(), 
                   br(),
                   fluidRow(column(width = 12, uiOutput("convertVariableSelect"))),
                   br(), 
                   br(),
                   fluidRow(column(width = 12, uiOutput("convertType"))), 
                   br(),
                   br(), 
                   br(),
                   fluidRow(column(width = 3, actionButton("convertCreate", "Convert variable"))),
                   br(), 
                   fluidRow(column(width = 12, p(convert_message$mesg)))
                   )
            )
          )
      )
    }
  })

  output$convertTableSelect <- renderUI({ 
    selectInput(inputId = "convertTable", label = "Select table", choices = allTables()[[1]])
  })

  convertVars <- reactive({
    table <- input$convertTable # the user's choice of table
    matchedIndex <- allTables()[[1]] == table # index the table name
    selectedVars <- allTables()[[4]][matchedIndex][[1]] # find the selected variable names for the table
    return(selectedVars)
  })

  convertVarTypes <- reactive({
    table <- input$convertTable # the user's choice of table
    matchedIndex <- allTables()[[1]] == table # index the table name
    types <- allTables()[[5]][matchedIndex][[1]] # find the selected variable types for the table
    return(types)
  })

  convertChoices <- reactive({
    var_type <- paste0(convertVars(), " (", convertVarTypes(), ")")
  })

  output$convertVariableSelect <- renderUI({
    selectInput(inputId = "convertVar", label = "Select variable", choices = convertChoices())
  })

  convertToChoices <- reactive({
    type <- convertVarTypes()[convertChoices() == input$convertVar]
    if (type == "integer" | type == "numeric") {
      options <- "character"
    } else if (type == "character") {
      options <- c("Date", "integer/numeric")
    } else if (type == "Date") {
      options <- "character"
    }
    return(options)
  })

  output$convertType <- renderUI({
    selectInput(inputId = "convertTo", label = "Convert to", choices = convertToChoices())
  })

  ####### Observe the clicking of convert variable and call function to convert variable #######

  observeEvent(input$convertCreate, {

    createCounter$times <- createCounter$times + 1

    table <- input$convertTable # the user's choice of table
    matchedIndex <- allTables()[[1]] == table # index the table name
    df <- allTables()[[3]][matchedIndex][[1]]
    var_name <- convertVars()[convertChoices() == input$convertVar]

    converted_list <- convert_var(df, var_name, input$convertTo)
    convert_message$mesg <- paste0("New table created. ", converted_list[[2]])
    newTables$tableList[[input$newConvertNameInput]] <- list(converted_list[[1]], 
      input$newConvertDesInput, 
      createCounter$times)
  })


  #################### UI Boxes for Existing Variables and Created Variables ##########################################


  output$variableCartBoxes <- renderUI({ 
    if (length(allTables()) == 0) { # if no tables
      h1("Variable cart is empty.", align = "center") # then show "Variable cart is empty." 
      } else { 

        ####### Set up the boxes that contain the info data frames for existing variables #######

        if (length(existing()) > 0) {
          boxes <- lapply(1:length(existing()[[1]]), function(i) {  
            # list of boxes to generate for UI; loop through each selected table  
            tables <- existing()[[2]][[i]] # info data frame  
            titles <- paste(existing()[[1]][i], " (", nrow(tables), ")", sep = "")  
            # title of the box; name of the data frame and its number of selected rows 
            
            deleteInput <- paste(existing()[[1]][i], "_deleteInput", sep = "") # delete button inputId 
            
            box(title = titles, solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,  
                div(renderTable(tables), style = "overflow-x: auto"), # render table; auto scroll bar for the x-axis 
                div(actionButton(deleteInput, label = NULL, icon("trash-o")), align = "right") # render delete button 
                ) 
          })
        } else {
          boxes <- NULL
        }
        
        ####### Set up the boxes that contain the info data frames for the created boxes #######

        if (length(newTables$tableList) > 0) {
          newTable_boxes <- lapply(1:length(newTables$tableList), function(i) {
            table <- tableInfo_df(newTables$tableList[[i]][[1]]) # info data frame
            title <- paste0(names(newTables$tableList)[i], " (", nrow(table), ")")
            description <- newTables$tableList[[i]][[2]]
            
            deleteInput <- paste0("newTablesDelete", newTables$tableList[[i]][[3]]) # corresponds to counter
              
            box(title = title, solidHeader = TRUE, status = "warning", collapsible = TRUE, width = 12, 
                p(paste("Description: ", description, sep = "")),
                div(renderTable(table), style = "overflow-x: auto"), 
                div(actionButton(deleteInput, label = NULL, icon("trash-o")), align = "right")
                )
          })
        } else {
          newTable_boxes <- NULL
        }
        
        ####### Set up the main box #######

        existingBoxTitle <- paste("Existing Variables (", numRowSelected()[[1]], ")", sep = "") # existing box title 
        newBoxTitle <- paste0("New Variables (", length(unlist(created()[[4]])), ")")

        div(box(title = existingBoxTitle, solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12, 
                tags$div(boxes)), 
            box(title = newBoxTitle, solidHeader = TRUE, status = "warning", collapsible = TRUE, width = 12, 
                tags$div(newTable_boxes)) 
            ) # output for renderUI 
      } 
    })


  
  #################### Boxes for Displaying Variable Details ##########################################################


  all_var_details <- reactive({
    var_details(table_list = allTables())
  })

  output$cart_var_details <- renderUI({
    if (length(allTables()) == 0) {
      detail_boxes <- NULL
    } else {
      detail_boxes <- lapply(1:length(all_var_details()), function(i) {
        table_name <- all_var_details()[[i]]$table_name
        var_name <- all_var_details()[[i]]$var_name
        var_type <- all_var_details()[[i]]$var_type
        output_id <- all_var_details()[[i]]$detail_output_id

        box(solidHeader = TRUE, collapsible = TRUE, width = 12, status = "primary", collapsed = TRUE,
          title = paste0(table_name, " : ", var_name, " (", var_type, ")"),
          htmlOutput(output_id)
        )
      })
    }
  })

  observe({
    if (length(allTables()) == 0) {
      return(NULL)
    } else {
      for (i in 1:length(all_var_details())) {
        local({
          local_i <- i
          unique_vals <- all_var_details()[[local_i]]$unique_vals
          if (class(unique_vals) == "character" | class(unique_vals) == "factor") {
            output_vals <- lapply(1:length(unique_vals), function(j) {
              p(unique_vals[j], style = "color:#000")
            })
          } else {
            output_vals <- list(p(paste0("Minimum: ", min(unique_vals, na.rm = TRUE)), style = "color:#000"), 
              p(paste0("Maximum: ", max(unique_vals, na.rm = TRUE)), style = "color:#000")
            )
          }
          output[[all_var_details()[[local_i]]$detail_output_id]] <- renderUI({
            tags$div(output_vals)
          })
        })
      }
    }
  })


  
  #################### Analysis: Correlation ##########################################################################


  output$corrUIOutput <- renderUI({ 
    if (length(allTables()) == 0) {
      h1("Variable cart is empty.", align = "center")
      } else {
        box(width = 12,
          fluidRow(column(width = 12, selectInput(inputId = "corrMethod", label = "Correlation method", 
            choices = c("Pearson", "Kendall", "Spearman")))),
          fluidRow(column(width = 12, selectInput(inputId = "corrMissHandle", label = "Missing data handling", 
            choices = c("No deletion", "Casewise deletion", "Pairwise deletion")))),   
          fluidRow(column(width = 12, selectInput(inputId = "corrTable", 
            label = "Select table", 
            choices = allTables()[[1]]))), 
          fluidRow(column(width = 12, uiOutput("corrVarChoices"))),
          br(),
          fluidRow(column(width = 3, actionButton("corrCompute", "Perform analysis"))),
          br(),
          br(), 
          box(width = 12, title = "Results", solidHeader = TRUE, status = "primary", 
            verbatimTextOutput("corrResult"))
          )
      }
  })

  corrVarOptions <- reactive({
    table <- input$corrTable # the user's choice of table
    matchedIndex <- allTables()[[1]] == table # index the table name
    vars <- allTables()[[4]][matchedIndex][[1]] # find the selected variable for the table
    return(vars)
  })

  corrSelectedTable <- reactive({
    table <- input$corrTable
    matchedIndex <- allTables()[[1]] == table
    selectedTable <- allTables()[[3]][matchedIndex][[1]]
    return(selectedTable)
  })

  output$corrVarChoices <- renderUI({
    selectInput(inputId = "corrVars", label = "Select variables", choices = corrVarOptions(), multiple = TRUE)
  })

  corrResultOutput <- reactiveValues(result = "Analysis results will be displayed.")

  observeEvent(input$corrCompute, {
    corrResultOutput$result <- computeCorr(df = corrSelectedTable(), var_names = input$corrVars, 
      method = input$corrMethod, NA_handle = input$corrMissHandle)
  })

  output$corrResult <- renderPrint({
    print(corrResultOutput$result)
  })



  #################### Analysis: Regression ###########################################################################


  output$regressUIOutput <- renderUI({
    if (length(allTables()) == 0) {
      h1("Variable cart is empty.", align = "center")
      } else {
        box(width = 12,
          fluidRow(column(width = 12, selectInput(inputId = "regressMethod", label = "Regression method", 
            choices = c("Least squares", "Logistic", "Probit")))),
          fluidRow(column(width = 12, selectInput(inputId = "regressMissHandle", label = "Missing data handling", 
            choices = c("No deletion", "Casewise deletion")))),   
          fluidRow(column(width = 12, selectInput(inputId = "regressTable", 
            label = "Select table", 
            choices = allTables()[[1]]))), 
          fluidRow(column(width = 12, uiOutput("regressDepChoices"))),
          fluidRow(column(width = 12, uiOutput("regressIndChoices"))),
          br(),
          fluidRow(column(width = 3, actionButton("regressCompute", "Perform analysis"))),
          br(),
          br(), 
          box(width = 12, title = "Results", solidHeader = TRUE, status = "primary", 
            verbatimTextOutput("regressResult"))
          )
      }
  })
  
  regressVarOptions <- reactive({
    table <- input$regressTable # the user's choice of table
    matchedIndex <- allTables()[[1]] == table # index the table name
    vars <- allTables()[[4]][matchedIndex][[1]] # find the selected variable for the table
    return(vars)
  })

  regressSelectedTable <- reactive({
    table <- input$regressTable
    matchedIndex <- allTables()[[1]] == table
    selectedTable <- allTables()[[3]][matchedIndex][[1]]
    return(selectedTable)
  })

  output$regressDepChoices <- renderUI({
    selectInput(inputId = "regressDepVar", label = "Dependent variable", choices = regressVarOptions())
  })

  output$regressIndChoices <- renderUI({
    selectInput(inputId = "regressIndVar", label = "Independent variable", choices = regressVarOptions(), 
      multiple = TRUE)
  })

  regressResultOutput <- reactiveValues(result = "Analysis results will be displayed.")

  observeEvent(input$regressCompute, {
    regressResultOutput$result <- computeRegress(df = regressSelectedTable(), dep_var = input$regressDepVar, 
      ind_var = input$regressIndVar, method = input$regressMethod, NA_handle = input$regressMissHandle)
  })

  output$regressResult <- renderPrint({
    print(regressResultOutput$result)
  })


  #################### Analysis: Something ############################################################################



















  #################### Delete and Empty Buttons #######################################################################


  ####### Empty button #######

  proxyNames <- paste(tableNames, "_proxy", sep = "") # proxy names for the data tables
  
  for (i in 1:nTable) { # loop through each table
    proxyName <- proxyNames[i] # current proxy name
    assign(proxyName, dataTableProxy(dataTableNames[i])) # assign proxy name to the proxy of the table
  }
  
  observeEvent(input$emptyVariableCart, { # observe the event when the empty cart button is pressed
    # Clear all the UI containers for filtering the tables
    filterContainer1$uiComponents <- NULL
    filterContainer2$uiComponents <- NULL
    filterContainer3$uiComponents <- NULL
    filterContainer4$uiComponents <- NULL
    
    for (i in 1:nTable) { # loop through each table proxy
      selectRows(get(proxyNames[i]), NULL) # make the selected rows NULL
    } # the complete loop makes all the selected rows NULL
  })
  
  ####### Delete selected tables #######
  deleteNames <- paste(tableNames, "_deleteInput", sep = "") # all the delete button inputIds
  for (i in 1:nTable) { # loop through each table to see whether it needs to be deleted
    local({ # to loop inside the server function
      local_i <- i
      observeEvent(input[[deleteNames[local_i]]], { # if the delete button is clicked
        # Clear all the UI containers for filtering the tables
        filterContainer1$uiComponents <- NULL
        filterContainer2$uiComponents <- NULL
        filterContainer3$uiComponents <- NULL
        filterContainer4$uiComponents <- NULL
        
        selectRows(get(proxyNames[local_i]), NULL) # make the seleted rows NULL
      })
    })
  }
  
  ####### Delete created tables #######
  
  observe({
    if (length(newTables$tableList) > 0) {
      deleteNum <- extract_ll(newTables$tableList, 3) # get the counter
      filtered_deleteNames <- paste0("newTablesDelete", deleteNum) # delete ids
      for (i in 1:length(filtered_deleteNames)) {
          observeEvent(input[[filtered_deleteNames[i]]], {
            newTables$tableList[[names(newTables$tableList)[i]]] <- NULL
          })
      }
    }
  })
  
  
  #################### TESTING ########################################################################################
   # output$test2 <- renderText({
   #   all_var_details()[[2]]$unique_vals
   # })
   
   #output$test <- renderTable({
     
     #created()[[3]][[1]]
   #})


}
