#' Wrapper function for \code{ir_split_into_bands()}, pre-populated with Price Tool inputs.
#'
#' Creates PRICE dimension containing the min and max range of a PRICE, calculated with End-User Spending / Shipments.
#'
#' @param df A data frame of categorical dimensions and two value columns: the numerator and denominator.
#' @param ratio_name Name of the new categorical dimension containing ratio bands (e.g. Price)
#' @param sd Standard deviation of truncated normal distribution, expressed as a percent of mean. Can be a single number, or a data frame of values differing by element.
#' @param sd_default If \code{sd} is a data frame, value to use for missing elements.
#' @param sd_name If \code{sd} is a data frame, name of value to use as standard deviatation.
#' @param minimum_distribution Optional value of the minimum % share of any new band element. (e.g. 0.01 means no bands can contain less than 1% of group total.)
#' @param seed_numer Optional data frame of initial starting distributions of the numerator.
#' @param seed_numer_wght Value between 0 and 1 of how much weight should be added to \code{seed_numer}. Larger values may not converge.
#' @param seed_denom Optional data frame of initial starting distributions of the denominator.
#' @param seed_denom_wght Value between 0 and 1 of how much weight should be added to \code{seed_denom}. Larger values may not converge.
#' @param ratio_bounds Optional data frame of minimum and maximum ratios for specific rows or groups. Uses \code{dplyr::left_join()}, so all values will be replicated over all elements in excluded dimensions.
#' @param ratio_bounds_names Names of the minimum and maximum ratio values in \code{ratio_bounds}.
#' @param smash_param For out-of-bound ratios, how much to increase/decrease ratio above/below min/max before next iteration.
#' @param max_iteration Maximum number of iterations before ceasing iterations for unconverged model runs.
#' @param save.intermediates Logical. Print out all intermediate calculations with results.
#' @param save.ratio Logical. Print out final calculated ratios with results.
#' @param show.messages Logical. Print iteration reports to console.
#' @return A data frame with additional Price Band dimension. Total over this dimension ties to initial data
#'
#' @export

ir_pricetool <- function(df, target_dim,
                         numerator = "End-User Spending", denominator = "Shipments",
                         sd = 1/3, sd_default = 1/3, sd_name = "sd",
                         ratio_name = "PRICE",
                         minimum_distribution = 0,
                         seed_numer = NULL, seed_numer_wght = 0.5,
                         seed_denom = NULL, seed_denom_wght = 0.5,
                         ratio_bounds = NULL, ratio_bounds_names = c("min", "max"),
                         smash_param = 1/10, max_iterations = 40,
                         save.intermediates = FALSE, save.ratio = FALSE, show.messages = TRUE){

  dat <- ir_split_into_bands(df = df,
                             target_dim = target_dim,
                             numerator = numerator, denominator = denominator,
                             ratio_name = ratio_name,
                             sd = sd, sd_default = sd_default, sd_name = sd_name,
                             minimum_distribution = minimum_distribution,
                             seed_numer = seed_numer, seed_numer_wght = seed_numer_wght,
                             seed_denom = seed_denom, seed_denom_wght = seed_denom_wght,
                             ratio_bounds = ratio_bounds, ratio_bounds_names = ratio_bounds_names,
                             smash_param = smash_param, max_iterations = max_iterations,
                             save.intermediates = save.intermediates, save.ratio = save.ratio,
                             show.messages = show.messages)


  return(dat)

} #End Price Tool
