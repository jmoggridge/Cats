
#' Missing data plot
#'
#' @param df dataframe to check for missing data
#'
#' @return ggplot object
#' @export
#' @importFrom dplyr mutate arrange
#' @importFrom forcats as_factor
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map2_dfc
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_col labs theme_light

gg_missing <- function(df=df) {

  ## plots % of missing values for each variable in a dataframe
  # count missing values in each column;
  purrr::map2_dfc(.x=df, .y=names(df), .f=~sum(is.na(.))) %>%
    tidyr::pivot_longer(everything()) %>%
    # sort vars descending by % NA and make barplot
    dplyr::arrange(value) %>%
    dplyr::mutate(value = value/nrow(df),
           name = forcats::as_factor(name)) %>%
    # make a bar plot
    ggplot2::ggplot(aes(y = name, x = value)) +
    geom_col() +
    theme_light() +
    labs(
      x = '% NA values',
      y = 'Variable',
      title = paste0('Missing data in dataframe:\n"', deparse(substitute(df)), '"')
    )
}

