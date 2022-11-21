#' Raftery diagnostics distribution function
#'
#' This function allows you to display a histogram of the distribution
#' of the number of iterations needed as suggested by the Raftery diagnostic
#' @param mcmc MCMC samples from a JAGS run
#' @keywords convergence
#' @export
#' @examples
#' ggRaftery()
#

ggRaftery <- function(mcmc){

  #Get the raftery diagnostic for the mcmc samples
  raftery <- coda::raftery.diag(mcmc)

  #get the names of each parameter to match back in the dataframe
  names <- rownames(raftery[[1]]$resmatrix)

  #Get the raftery iteration ## from each chain in the 3 chains
  ch1 <- raftery[[1]]$resmatrix[,2]
  ch2 <- raftery[[2]]$resmatrix[,2]
  ch3 <- raftery[[3]]$resmatrix[,2]

  raf_all <- as.data.frame(cbind(names,
                                 ch1, ch2, ch3)) %>%
    dplyr::mutate(ch1 = as.numeric(ch1),
           ch2 = as.numeric(ch2),
           ch3 = as.numeric(ch3)) %>%
    tidyr::pivot_longer(ch1:ch3,
                 names_to = "chain",
                 values_to = 'iterations')

  #make a ggplot of the histogram of the iterations
  plot <- ggplot2::ggplot(raf_all, aes(x = iterations/3)) +
    geom_histogram() +
    theme_bw()

  #return that plot
  return(plot)

}

