#' Gelman Plotting function
#'
#' This function allows you to display Gelman diagonstics for
#' # an entire JAGS model
#' @param list A list of Rhat values from JagsUI or saved from gelman.diag
#' @keywords convergence
#' @export
#' @examples
#' ggGelman()
#'
# Gelman for total model --------------------------------------------------

#this function plots one histogram overall for the whole model
# to diagnose gelman-rubin stats
#it requires an object that is a list format of the Gelman Diagonstic
# for a JAGS model - either via jagsUI, or gelman.diag in the coda package
ggGelman <- function(list){

  #this creates a dtaaframe out of the Rhat values from the model
  df <- data.frame(id = names(list),
                   Rhat = unlist(lapply(list, paste, collapse = ","))) %>%
    #splits Rhat by , when that list element had more than one value
    dplyr::mutate(Rhat = str_split(Rhat, ",")) %>%
    #unnests - so makes each Rhat a new row in the df
    tidyr::unnest(c(Rhat)) %>%
    #make sure Rhat is a numeric
    dplyr::mutate(Rhat = as.numeric(Rhat))


  #plot histogram and make sure all below 1.1
  plot <- ggplot2::ggplot(df, aes(x = Rhat)) +
    geom_histogram() +
    geom_vline(xintercept = 1.1, linetype = 2) +
    theme_bw() +
    scale_y_sqrt()

  return(plot)
}
