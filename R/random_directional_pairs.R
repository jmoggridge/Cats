#' Generate random directional pairs from a vector of names (nodes)
#'
#' @param nodes
#'
#' @return dataframe with pairs as rows
#' @export
#'
#' @examples
#' generate_pairings(c('Sporty', 'Ginger','Scary','Baby','Posh'))
generate_pairings <- function(nodes) {
  while (TRUE) {
    # make pairs; an edgelist representation of a digraph
    nodes2 <- sample(nodes)
    edges <- Map(f = c, nodes, nodes2)

    # degenerate edges to detect a graphs that aren't strongly connected
    unique_edges <- lapply(edges, function(x)
      unique(sort(x)))

    # repeat if not strongly connected, ie. if
    # any self-cycles instead of 2 nodes, 1 in, 1 out.
    flag1 <- any(lapply(unique_edges, length) == 1)
    flag2 <- length(unique(unique_edges)) != length(nodes)

    # if no flags, then exit with the current pairings
    if (!any(flag1, flag2)) {
      break
    }
  }
  # return edgelist as a dataframe
  return(data.frame(name1 = nodes, name2 = nodes2))
}
