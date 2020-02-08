#' Darwin's naturalization conundrum
#'
#' \code{darwin_hypothesis} computes standard effective size of
#' distance to nearest neighbors.
#'
#' @param dat A data frame
#' @param phy A phylogenetic tree
#' @param iter Numeric, the number of permutations
#' @rdname darwin_hypothesis
#' @keywords bioregion
#' @importFrom ape keep.tip
#' @importFrom stats cophenetic as.dist
#' @return a vector with the mean distance between species
#'
#' @author Barnabas H. Daru \email{darunabas@@gmail.com}
#' @export
darwin_hypothesis <- function(dat, phy, iter){
  dat <- unique(as.character(dat))
  d <- keep.tip(phy, dat)
  obs <- as.dist(cophenetic(d))
  sub_tree <- phy

  y <- NULL
  for(i in seq_len(iter)){
    sub_tree$tip.label <- sub_tree$tip.label[sample(length(sub_tree$tip.label))]
    v <- keep.tip(sub_tree, dat)
    v <- as.dist(cophenetic(v))
    y[i] <- mean(v)
  }
  res <- (mean(obs)-mean(y))/sd(y)
  res
}
