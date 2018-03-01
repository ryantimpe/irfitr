#' Process array of price band strings for use in \code{ir_split_into_bands()}.
#'
#' Splits an array of strings into minimum, maximum, and mean values for use in the \code{ir_split_into_bands()} function.
#'
#' @param bands An array of strings containing the elements of the ratio dimension target.
#' @return A tibble of the input strings, along with the minimum, maximum, and geometric means each one represents.
#'
#' @examples
#'
#' bands <- c("<$200", "$200-$500", "$500+")
#' ir_band_split(bands)
#'
#' @export

ir_band_split <- function(bands){
  tibble(.band = unique(bands)) %>%
    #Using fill = right will suppress the warning
    separate(.band, c(".band_min", ".band_max"), sep = "-|<|lt|LT", remove = FALSE, fill = "right") %>%
    mutate_at(vars(starts_with(".band_")), funs(as.numeric(gsub("[^0-9]", "", .)))) %>%
    mutate(.band_min = ifelse(is.na(.band_min), 0, .band_min)) %>%
    arrange(.band_min) %>%
    mutate(.band_max = lead(.band_min),
           .band_min = ifelse(is.na(.band_min), 0, .band_min)) %>% #This seems cleaner
    mutate(.band_mean = purrr::map2_dbl(.band_min, .band_max, ~ir_band_mean(.x, .y)))
}

#' Compute mean price in a ratio band for use within \code{ip_band_split()}.
#'
#' Geometric mean of each price band. For unbounded lower bounds, assumes a lower bound of 1/2 the upper bound.
#' For unbounded upper bounds, assumes and upper bound of 1.5x the lower bound.
#'
#' @param bands An array of strings containing the elements of the ratio dimension target.
#' @return A single value containing the geometric mean of two values.
#' @export

ir_band_mean <- function(mn, mx){
    mx <- if(is.na(mx)){mn * 1.5} else {mx}
    mn <- if(mn == 0 | is.na(mn)){mx/2} else {mn}

    pb_mean <- (mn * mx)^(1/2)

    return(pb_mean)
}
