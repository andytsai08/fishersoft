########## Extract element from a list of lists ##########
extract_ll <- function(list, ref) {
  results <- list()
  n <- length(list)
  for (i in 1:n) {
    results[[i]] <- list[[i]][[ref]]
  }
  return(results)
}