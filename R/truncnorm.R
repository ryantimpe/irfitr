#' Calculate initial denominator and numerator distributions using trunacated norm sampling. For use in \code{ir_split_into_bands()}.
#'
#' Creates a data frame of denominator and numerator distributions for the starting point before iteration.
#'
#' @param n number of observations.
#' @param b.mean mean value of distribution.
#' @param b.sd standard deviation of distribution, expressed as % of mean.
#' @param df_bands data frame of price bands, output of \code{ir_band_split()}
#' @return A data frame of each of the price bands from \code{df_bands}, as well as the probability distributions for the numerator and denominators.
#' @export

ir_dist_truncnorm_a <- function(n, b.mean, b.sd, df_bands){ #pb.sd as %
  ###
  #Denominator
  ###

  #Create a sample of possible ratios
  #Since there is 1 ratio for each Denominator, this means we are making a sample of the denominator
  # ...and we know the ratio of each of those unit denominators
  samp <- truncnorm::rtruncnorm( n = n, a=0, b=max(df_bands$.band_max, na.rm=TRUE)*2.5,
                                 mean = b.mean, sd = b.mean* b.sd)

  #Allocate those prices/units to a price band category
  banded <- cut(samp, c(as.numeric(df_bands$.band_min), Inf))

  #Aggregate them...
  banded_denom <- as.numeric(table(banded))

  # %Share of units that belong to each price band
  denom_dist <- banded_denom / sum(banded_denom)

  ###
  # Numerator
  ###
  #In the band key file, the average ratio (.band_mean) of a unit in that band is the geometric mean...
  #... edited this mean to be the geomean of the inner 50% of possible ratio to account for the edges

  #Revenue by band is the Unit Distribution * Geo Mean ratio in each band
  banded_numer <- df_bands$.band_mean * denom_dist
  #Normalize
  numer_dist <- banded_numer / sum(banded_numer)

  #List of bands and unit shares
  #!!! using a tibble is much slower!
  banded_list <- data.frame(.band = df_bands$.band,
                            .band_min = df_bands$.band_min,
                            .band_max = df_bands$.band_max,
                            .band_mean= df_bands$.band_mean,
                            .b_numer_dist = numer_dist,
                            .b_denom_dist = denom_dist,
                            stringsAsFactors = FALSE)

  return(banded_list)

}
