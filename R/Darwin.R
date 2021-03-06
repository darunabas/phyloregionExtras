#' Darwin's naturalization conundrum
#'
#' \code{darwinize} computes standard effective size of
#' distance to nearest neighbors.
#'
#' @param x A data frame with list of species
#' @param phy A phylogenetic tree
#' @param iter Numeric, the number of permutations
#' @rdname darwinize
#' @keywords bioregion
#' @importFrom ape keep.tip
#' @importFrom stats cophenetic as.dist sd
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @return a vector with the mean distance between species
#'
#' @author Barnabas H. Daru \email{darunabas@@gmail.com}
#' @export
darwinize <- function(x, phy, iter){
  x <- unique(as.character(x))
  d <- keep.tip(phy, x)
  obs <- as.dist(cophenetic(d))
  sub_tree <- phy

  y <- NULL
  pb <- txtProgressBar(min = 0, max = iter, style = 3)
  for(i in seq_len(iter)){
    sub_tree$tip.label <- sub_tree$tip.label[sample(length(sub_tree$tip.label))]
    v <- keep.tip(sub_tree, x)
    v <- as.dist(cophenetic(v))
    y[i] <- mean(v)
    setTxtProgressBar(pb, i)
  }
  res <- (mean(obs)-mean(y))/sd(y)
  res
}
