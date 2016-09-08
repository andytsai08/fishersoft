########## Extract element from a list of lists ##########
extract_ll <- function(list, ref) {
  results <- list()
  n <- length(list)
  for (i in 1:n) {
    results[[i]] <- list[[i]][[ref]]
  }
  return(results)
}

my_selectInput <- function(idName, idNum, label = "Variable", ...) {
  selectInput(inputId = paste(idName, idNum, sep = ""), label = label, ...)
}
my_textInput <- function(idName, idNum, label = "Value", value = "", ...) {
  textInput(inputId = paste0(idName, idNum), label = label, value = value, ...)
}
removeButton <- function(idName, idNum, label = "", ...) {
  actionButton(inputId = paste(idName, idNum, sep = ""), label = label, ...)
}