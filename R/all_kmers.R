#' Generate all kmers of length k
#'
#' @param k
#'
#' @return character vector of kmers lexicographically
#' @export
#'
#' @examples
#' all_kmers(3)
all_kmers <- function(k) {
  # all_kmers() Generates all kmers of length k
  ACGT <- list('A', 'C', 'G', 'T')
  kmers <- ACGT
  i = 1
  while (i < k) {
    kmers <- outer(FUN = paste0, kmers, ACGT)
    i <- i + 1
  }
  kmers <- sort(c(kmers))
  return(kmers)
}

