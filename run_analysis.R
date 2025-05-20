run_analysis <- function(model = c("SPDE", "iCAR")) {
  model <- match.arg(model)
  

  # Source the appropriate analysis script
  if (model == "SPDE") {
    source("Analysis_SPDE.R", local = knitr::knit_global())
  } else if (model == "iCAR") {
    source("Analysis_iCAR.R", local = knitr::knit_global())
  }
  
  message("Analysis for model '", model, "' has been run.")
}
