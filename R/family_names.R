#' Get APG family names of plants
#'
#' This function generates family names based on
#' the Angiosperm Phylogeny Group system.
#'
#' @param x A data frame
#' @rdname family_name
#' @keywords bioregion
#' @importFrom brranching phylomatic_names
#' @return a vector with the family names
#'
#' @export
family_name <- function(x){
  y <- as.character(unique(x))
  m <- phylomatic_names(taxa=y, format = "isubmit", db = "apg")
  n <- strsplit(m, split="\\/")
  n <- sapply(n, function(i) i[1])
  res <- data.frame(cbind(species=y, family=n))
  res$family <- gsub("(^)([[:alpha:]])", "\\1\\U\\2", res$family, perl=TRUE)
  res
}

