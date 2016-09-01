tableInfo_df <- function(dataframe) {
  ####### Description #######
  # Purpose: Given a data frame, find its variable (column) names, variable type, and amount of missing data.
  
  # Arguments
  # dataframe: data frame
  
  # Output: a data frame with variable name, variable type, and amount of missing data 
  
  ####### Code #######
  n <- nrow(dataframe) # number of rows in the data frame
  varNames <- colnames(dataframe) # variable names
  varClasses <- sapply(dataframe, class) # variable types
  
  missCounts <- rep(NA, times = ncol(dataframe)) # initiation vector for counting missing data for each variable
  for (i in 1:ncol(dataframe)) { # loop through each column (variable)
    naIndex <- which(is.na(dataframe[ , i])) # indicies of NAs
    missCounts[i] <- length(naIndex) + sum(dataframe[-naIndex, i] == "") 
    # missing data are noted as NA and empty character string ""
  }
  
  missPercents <- round(missCounts / n * 100, digits = 2) # percent of missing data
  missFractions <- paste(missCounts, "/", n, sep = "") # missing data in fraction
  missInfo <- paste(missPercents, "%", " (", missFractions, ")", sep = "") # put percent and fraction together
  
  info_df <- data.frame(varNames, varClasses, missInfo) # put all the info in a data frame 
  colnames(info_df) <- c("Variable", "Type", "Missing Data") # rename the column names
  row.names(info_df) <- NULL # remove row names
  return(info_df) # output
}
#######################################################################################################################


tableInfo_list <- function(tableNames, ...) {
  ####### Description #######
  # Purpose: Given a character vector of the names of data frames, return a list of information data frames for them.
  
  # Dependency: custom function tableInfo_df
  
  # Arguments
  # tableNames: character; names of the data frames in the environment
  # ... passes arguments for the function get {base}
  
  # Output: a list of information data frames; in the order of the input
  
  ####### Code #######
  n <- length(tableNames) # number of data frames
  tableInfo_list <- list() # initation list
  for (i in 1:n) { # loop through each data frame
    tableInfo_list[[i]] <- tableInfo_df(get(tableNames[i], ...)) # compute the information data frame
  }
  return(tableInfo_list) # output
}


#######################################################################################################################


varCartSelected_list <- function(tableNames, table_numRow_selected, table_rowSelected_names, table_infoList, 
                                    input, ...) { 
  ####### Description #######
  # Purpose: Find the information about the data frames that have their variables selected by the user. To be used in
  # conjunction with the package {DT}
  
  # Arguments
  # tableNames: character; the names of the data frames
  # table_numRow_selected: integer; the number of rows selected for each table
  # table_rowSelected_names: character; the names for referencing the input objects (_rows_selected {DT})
  # table_infoList: a list of information data frames (output of custom function tableInfo_list)
  # input: input of the server function for shiny
  # ... passes arguments for the function get {base}
  
  # Output: a list as the following objects (with matching indices)
  # 1. character vector; names of the data frames with rows selected
  # 2. list; a list of the information data frames for those selected data frames
  # 3. list; a list of selected data frames
  # 4. list; a list of character vectors with the selected variable names
  # 5. list; a list of character vectors indicating the selected variable types
  # 6. list; a list of lists of vectors for the unique values of each variable
  
  ####### Code #######
  selected_logical <- table_numRow_selected > 0 # logical vector indicating whether a table has some rows selected
  # TRUE if there is at least 1 row selected; FALSE if there are no rows selected
  
  selectedNames <- tableNames[selected_logical] # the selected table names
  selected_rowSelected_names <- table_rowSelected_names[selected_logical] 
  # the names for referencing the input objects (_rows_selected) that are associated with the selected tables
  
  selected_table_infoList <- table_infoList[selected_logical] # the selected information data frames
  
  n <- length(selectedNames) # number of selected tables
  selected_infoList <- list() # initiation list to store the selected information data frames with the selected rows
  selected_dfList <- list() # initiation list to store the selected data frames with the selected variables
  selected_varList <- list() # initiation list to store the selected variables for each table
  selected_typeList <- list() # initiation list to store the selected variable types
  unique_values <- list() # initiation list to store the unique values for each variable
  
  for (i in 1:n) { # loop through each selected table
    info_df <- selected_table_infoList[[i]] # the information data frame in the current iteration
    rowsSelected <- as.numeric(input[[selected_rowSelected_names[i]]]) # the indicies of the selected rows
    selected_infoList[[i]] <- as.data.frame(info_df[rowsSelected, ]) # the information data frame with the selected rows
    
    df <- get(selectedNames[i], ...) # the selected table
    selected_varNames <- as.character(info_df$Variable)[rowsSelected] # the selected variable names
    selected_df <- as.data.frame(df[ , selected_varNames]) # subset the table based on the selected variables
    selected_dfList[[i]] <- selected_df
    selected_varList[[i]] <- selected_varNames 
    
    selected_varTypes <- as.character(info_df$Type)[rowsSelected] # the selected variable types
    selected_typeList[[i]] <- selected_varTypes

    unique_var_values <- list() # list to store the unique values for the selected table
    for (j in 1:ncol(selected_df)) { # loop through each variable of the selected table
      unique_var_values[[j]] <- sort(unique(selected_df[ , j]), na.last = FALSE)
    }
    unique_values[[i]] <- unique_var_values
  }
  
  return(list(selectedNames, selected_infoList, selected_dfList, selected_varList, selected_typeList, unique_values)) 
  # output 
}


#######################################################################################################################


createdTable_list <- function(newTableList) {
  # newTableList is newTables$tableList

  n <- length(newTableList)
  newNames <- names(newTableList)
  dataframes <- extract_ll(newTableList, 1)
  new_infoList <- list()
  new_dfList <- list()
  new_varList <- list()
  new_typeList <- list()
  new_uniqueValList <- list()

  for (i in 1:n) {
    df <- dataframes[[i]]
    info_df <- tableInfo_df(df)
    new_infoList[[i]] <- as.data.frame(info_df)
    new_dfList[[i]] <- as.data.frame(dataframes[[i]])
    new_varList[[i]] <- as.character(info_df$Variable)
    new_typeList[[i]] <- as.character(info_df$Type)

    unique_values <- list()
    for (j in 1:ncol(df)) {
      unique_values[[j]] <- sort(unique(df[ , j]), na.last = FALSE)
    }
    new_uniqueValList[[i]] <- unique_values
  }

  return(list(newNames, new_infoList, new_dfList, new_varList, new_typeList, new_uniqueValList))
}


#######################################################################################################################


removeButton <- function(idName, idNum, label = "", ...) {
  ####### Description #######
  # Purpose: Generate an action button with sequential inputId; used for remove buttons
  
  # Arguments
  # id: integer; the index for naming the remove button
  # label: the name of the action button to appear on UI
  # ... passes arguments for the function actionButton {shiny}
  
  # Output: shiny widget action button
  
  ####### Code #######
  actionButton(inputId = paste(idName, idNum, sep = ""), label = label, ...)
}


#######################################################################################################################
my_selectInput <- function(idName, idNum, label = "Variable", ...) {
  ####### Description #######
  # Purpose: Generate a select input with sequential inputId
  
  # Arguments
  # idName: character; the common name for the select input
  # idNum: integer; the index for naming
  # label: the name of the select input to appear on UI
  # ... passes arguments for the function selectInput {shiny}
  
  # Output: shiny widget select input
  
  ####### Code #######
  selectInput(inputId = paste(idName, idNum, sep = ""), label = label, ...)
}

#######################################################################################################################
my_radioButtons <- function(idName, idNum, label = "Condition", ...) {
  radioButtons(inputId = paste(idName, idNum, sep = ""), label = label, ...)
}

#######################################################################################################################
my_textInput <- function(idName, idNum, label = "Value", value = "", ...) {
  textInput(inputId = paste0(idName, idNum), label = label, value = value, ...)
}

#######################################################################################################################
find_matched <- function(varNames, varTypes, variable) {
  varType <- varTypes[varNames == variable]
  return(varType)
}

#######################################################################################################################
find_inputValues <- function(name, ids, input) {
  inputIds <- paste0(name, ids)
  values <- lapply(inputIds, function(id) input[[id]])
  return(values)
}

#######################################################################################################################
eval_operation <- function(df, variable, operator, value) {
  df_variable <- eval(parse(text = paste0("df$", variable)))
  if (operator == "=") {
    matched <- match(x = df_variable, table = value)
    matchedIndex <- which(!is.na(matched))
  } else if (operator == "!=") {
    matched <- match(x = df_variable, table = value)
    matchedIndex <- which(is.na(matched))
  } else {
    variable_class <- eval(parse(text = paste0("class(df$", variable, ")")))
    if (variable_class == "Date") {
      if (operator == ">=") {
        matchedIndex <- which(df_variable >= value)
      } else if (operator == ">") {
        matchedIndex <- which(df_variable > value)
      } else if (operator == "<=") {
        matchedIndex <- which(df_variable <= value)
      } else if (operator == "<") {
        matchedIndex <- which(df_variable < value)
      }
    } else {
      eval_text <- paste0("which(df$", variable, operator, value, ")")
      matchedIndex <- eval(parse(text = eval_text))
    }
  }
  return(matchedIndex)
}

#######################################################################################################################
eval_filter <- function(df, variables, operators, values, conditions) {
  n <- length(variables)
  if (n == 1) {
    results <- eval_operation(df, variable = variables, operator = operators, value = values)
  } else if (n == 2) {
    results1 <- eval_operation(df, variable = variables[[1]], operator = operators[[1]], value = values[[1]])
    results2 <- eval_operation(df, variable = variables[[2]], operator = operators[[2]], value = values[[2]])
    if (conditions[1] == "AND") {
      results <- intersect(results1, results2)
    } else if (conditions[1] == "OR") {
      results <- union(results1, results2)
    }
  } else {
    results <- eval_operation(df, variable = variables[[1]], operator = operators[[1]], value = values[[1]])
    
    for (i in 2:(n-1)) {
      loop_results <- eval_operation(df, variable = variables[[i]], operator = operators[[i]], value = values[[i]])
      if (conditions[i - 1] == "AND") {
        results <- intersect(results, loop_results)
      } else if (conditions[i - 1] == "OR") {
        results <- union(results, loop_results)
      }
    }
    
    last_results <- eval_operation(df, variable = variables[[n]], operator = operators[[n]], value = values[[n]])
    if (conditions[n - 1] == "AND") {
      results <- intersect(results, last_results)
    } else if (conditions[n - 1] == "OR") {
      results <- union(results, last_results)
    }
  }
  return(results)
}

#######################################################################################################################
# Extract element from a list of lists; return a list
extract_ll <- function(list, index) {
  results <- list()
  n <- length(list)
  for (i in 1:n) {
    results[[i]] <- list[[i]][[index]]
  }
  return(results)
}



#######################################################################################################################
# Append the correspond lists in a list together; the two lists should be of the same length
append_ll <- function(list1, list2) {
  appended_list <- list()
  n <- length(list1)
  for (i in 1:n) {
    item1 <- list1[[i]]
    item2 <- list2[[i]]
    appended_list[[i]] <- append(item1, item2)
  }
  return(appended_list)
}

#######################################################################################################################

convert_var <- function(df, var_name, convert_to) {
  if (convert_to == "character") {
    converted <- tryCatch(
      {as.character(df[ , var_name])}, 
      error = function(e) {
        df[ , var_name]
      }
    )
  } else if (convert_to == "integer/numeric") {
    converted <- tryCatch(
      {as.numeric(df[ , var_name])}, 
      error = function(e) {
        df[ , var_name]
      }
    )
  } else if (convert_to == "Date") {
    converted <- tryCatch(
      {as.Date(df[ , var_name])}, 
      error = function(e) {
        df[ , var_name]
      }
    )
  }

  if (class(converted) == class(df[ , var_name])) {
    outcome <- "Conversion failed. Keep the original variable type."
  } else {
    outcome <- paste0("Variable converted to ", class(converted), ".")
  }

  df[ , var_name] <- converted
  return(list(df, outcome))
}

#######################################################################################################################

computeCorr <- function(df, var_names, method, NA_handle) {
  method <- tolower(method)

  if (NA_handle == "No deletion") {
    use <- "everything"
  } else if (NA_handle == "Casewise deletion") {
    use <- "complete.obs"
  } else if (NA_handle == "Pairwise deletion") {
    use <- "pairwise.complete.obs"
  }

  data <- df[ , var_names]

  result <- tryCatch(
  {
    cor(x = data, use = use, method = method)
  }, 
  error = function(console_error) {
    console_error
  }, 
  warning = function(console_warning) {
    console_warning
  }
  )

  return(result)
}

#######################################################################################################################

computeRegress <- function(df, dep_var, ind_var, method, NA_handle) {
  if (NA_handle == "No deletion") {
    use <- na.fail
  } else if (NA_handle == "Casewise deletion") {
    use <- na.omit
  }

  ind_str <- paste(ind_var, collapse = " + ")
  formula_eval <- as.formula(paste(dep_var, ind_str, sep = " ~ "))

  if (method == "Least squares") {
    tryCatch(
      {print(summary(lm(formula = formula_eval, data = df)))}, 
      error = function(console_error) {print(console_error)}, 
      warning = function(console_warning) {
        print(summary(lm(formula = formula_eval, data = df)))
        print(console_warning)
      }
    )
  } else if (method == "Logistic") {
    tryCatch(
      {print(summary(glm(formula = formula_eval, data = df, family = binomial(link = "logit"))))}, 
      error = function(console_error) {print(console_error)}, 
      warning = function(console_warning) {
        print(summary(glm(formula = formula_eval, data = df, family = binomial(link = "logit"))))
        print(console_warning)
      }
    )
  } else if (method == "Probit") {
    tryCatch(
      {print(summary(glm(formula = formula_eval, data = df, family = binomial(link = "probit"))))}, 
      error = function(console_error) {print(console_error)}, 
      warning = function(console_warning) {
        print(summary(glm(formula = formula_eval, data = df, family = binomial(link = "probit"))))
        print(console_warning)
      }
    )
  }
}


#######################################################################################################################
# take the output of varCartSelected_list and createdTable_list as the argument 
var_details <- function(table_list) {
  var_detail_list <- list()
  for (i in 1:length(table_list[[1]])) { # loop through each table
    for (j in 1:length(table_list[[4]][[i]])) { # loop through each variable
      temp_list <- list()
      temp_list$table_name <- table_list[[1]][i]
      temp_list$var_name <- table_list[[4]][[i]][j]
      temp_list$var_type <- table_list[[5]][[i]][j]
      temp_list$unique_vals <- table_list[[6]][[i]][[j]]
      temp_list$detail_output_id <- paste0("detail_output", i, "_", j)
      var_detail_list <- append(var_detail_list, list(temp_list))
    }
  }
  return(var_detail_list)
}


#######################################################################################################################

####### Use this for multiple merge
order_completeness <- function(df_names) {
  completeness <- rep(NA, times = length(df_names))
  for (i in 1:length(df_names)) {
    completeness[i] <- nrow(get(df_names[i])) # completeness indicated by the number of row
  }
  order_names <- df_names[order(completeness, decreasing = TRUE)] # order the table names in decreasing completeness
  order_list <- list()
  for (i in 1:length(order_names)) {
    order_list[[i]] <- get(order_names[i])
  }
  return(order_list)
}

multiple_merge <- function(var_name, df_list) {
  df <- df_list[[1]] # initiate the merging with the first, most complete data frame
  lower_var_name <- tolower(var_name)
  colnames(df)[tolower(colnames(df)) == lower_var_name] <- lower_var_name
  for (i in 2:length(df_list)) {
    new_df <- df_list[[i]]
    colnames(new_df)[tolower(colnames(new_df)) == lower_var_name] <- lower_var_name
    df <- merge(x = df, y = new_df, all.x = TRUE)
  }
  colnames(df)[colnames(df) == lower_var_name] <- var_name
  return(df)
}
#######################################################################################################################


