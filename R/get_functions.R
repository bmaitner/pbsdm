#' @name get_functions
#' @title Internal function for getting available function names.
#' @description This function checks the available functions in the package to extract current options for dr and pnp fitting
#' @param type Type of function to get. Options are "pnp" for presence/background functions and "dr" for ratio functions.
#' @keywords internal
get_functions <- function(type = "pnp"){

  # Check type

  if(type %in% c("pnp","dr")){

    # add underscore to prevent future false positives on function name matching

      type <- paste(type,"_",sep = "")

  }else{

      stop("Invalid type, options are pnp and dr")

  }

  # Get fxs

    fxs <- data.frame(fx = ls(getNamespace("pbsdm"), all.names=TRUE))

  # Filter fxs

    fxs <- fxs$fx[grepl(pattern = type,x = fxs$fx)]

  # Return as vector

    return(fxs)

}
