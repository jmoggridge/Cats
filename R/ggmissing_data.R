

require(dplyr)
require(tidyr)
require(purrr)
require(forcats)
require(ggplot2)

#' Missing data plot
#'
#' @param df 
#'
#' @return ggplot
#' @export
#'
#' @examples 
#' gg_missing(df)
gg_missing <- function(df=df) {
  
  ## plots % of missing values for each variable in a dataframe
  # count missing values in each column;
  purrr::map2_dfc(.x=df, .y=names(df), .f=~sum(is.na(.))) %>% 
    pivot_longer(everything()) %>%
    # sort vars descending by % NA and make barplot
    arrange(value) %>%
    mutate(value = value/nrow(df),
           name = as_factor(name)) %>%
    # make a bar plot
    ggplot(aes(y = name, x = value)) +
    geom_col() +
    theme_light() +
    labs(
      x = '% NA values',
      y = 'Variable',
      title = paste0('Missing data in dataframe:\n"', deparse(substitute(df)), '"')
    )
}

