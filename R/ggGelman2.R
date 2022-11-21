#' Per Parameter Gelman Plotting function
#'
#' This function allows you to display Gelman diagonstics for
#' each parameter of choice in a JAGS model to find problem parameters
#' @param list A list of Rhat values from JagsUI or saved from gelman.diag
#' @keywords convergence
#' @export
#' @examples
#' ggGelman2()
#

# Graph RHat per parameter ------------------------------------------------

ggGelman2 <- function(list){

  #this creates a dtaaframe out of the Rhat values from the model
  df <- data.frame(id = names(list),
                   Rhat = unlist(lapply(list, paste, collapse = ","))) %>%
    #splits Rhat by , when that list element had more than one value
    mutate(Rhat = str_split(Rhat, ",")) %>%
    #unnests - so makes each Rhat a new row in the df
    unnest(c(Rhat)) %>%
    #make sure Rhat is a numeric
    mutate(Rhat = as.numeric(Rhat))

  #plot histogram and make sure all below 1.1
  plot <- ggplot(df, aes(x = Rhat)) +
    geom_histogram() +
    geom_vline(xintercept = 1.1, linetype = 2) +
    theme_bw() +
    scale_y_sqrt() +
    #this is facetted by the parameter ID so you
    # can find problematic parameters
    facet_wrap(~ id)

  return(plot)
}
