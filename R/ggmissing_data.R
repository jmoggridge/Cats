
#' Missing data plot
#'
#' @param df dataframe to check for missing data
#'
#' @return ggplot object
#' @export
#' @importFrom ggplot2 ggplot geom_col labs theme_light
#' @importFrom dplyr mutate arrange everything
#' @importFrom forcats as_factor
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map2_dfc
#' @importFrom magrittr %>%
#' @importFrom rlang .data

gg_missing <- function(df=df) {

  ## plots % of missing values for each variable in a dataframe
  # count missing values in each column;
  purrr::map2_dfc(.x=df, .y=names(df), .f=~sum(is.na(.))) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    # sort vars descending by % NA and make barplot
    dplyr::arrange(value) %>%
    dplyr::mutate(value = value/nrow(df),
           name = forcats::as_factor(name)) %>%
    # make a bar plot
    ggplot2::ggplot(
      ggplot2::aes(y = .data$name, x = .data$value)
      ) +
    ggplot2::geom_col() +
    ggplot2::theme_light() +
    ggplot2::labs(
      x = '% NA values',
      y = 'Variable',
      title = paste0('Missing data in dataframe:\n"',
                     deparse(substitute(df)), '"')
    )
}

gg_missing(iris)
