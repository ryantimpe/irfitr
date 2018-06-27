#' Scale numerator and denominator to ratios that align to bands for use in \code{ir_split_into_bands()}.
#'
#' Creates new ratios for rows of data with ratios that exceed the min/max. Recalculates the numerators and denominators, scales to targets, and performs new check.
#' This function is designed to be iterated over multiple times until all ratio checks pass.
#'
#' @param df A data frame of categorical dimensions, as well as intermediary data from \code{ir_split_into_bands()}.
#' @param dim An array of strings of the names of categorical dimensions to group data.
#' @param smash_param For out-of-bound ratios, how much to increase/decrease ratio above/below min/max before next iteration.
#' @return A data frame in same shape as input, with numerator and denominator scaled to new ratio targets
#'
#' @export

ir_ratio_scale <- function(df, dims, smash_param){
  dat <- df %>%
    #Knock ratios into place
    mutate(.ratio = case_when(
      !is.na(.f_ratio) ~ .f_ratio, #Frozen ratios have priority over min/max
      .ratio_check == "LOW"  ~ .band_min + (.band_max - .band_min)*smash_param,
      .ratio_check == "HIGH" ~ .band_max - (.band_max - .band_min)*smash_param,
      TRUE ~ .ratio
    )) %>%
    mutate(.ratio = ifelse(is.nan(.ratio), .band_mean, .ratio)) %>%
    #Implied new numerators and denominators
    mutate(.numer_new = ((.denom * .ratio)), #Average old and new numer to maintain ratio
           .denom_new = ((.numer / .ratio)), #Don't want to overshoot
           .ratio_impl = .numer_new / .denom_new #This was just to check
    ) %>%
    group_by_(.dots = as.list(dims)) %>%
    mutate(.numer_dist = .numer_new / sum(.numer_new),
           .denom_dist = .denom_new / sum(.denom_new)) %>%
    ungroup() %>%
    mutate(.numer = .numer_dist * .numer_nobands,
           .denom = .denom_dist * .denom_nobands) %>%
    mutate(.numer = ifelse(.denom == 0, 0, .numer),
           .denom = ifelse(.numer == 0, 0, .denom)) %>%
    mutate(.ratio = .numer / .denom) %>%
    mutate(.ratio_check = case_when(
      !is.na(.f_ratio) & .ratio < .f_ratio ~ "LOW",
      !is.na(.f_ratio) & .ratio > .f_ratio ~ "HIGH",
      .ratio < .band_min ~ "LOW",
      .ratio > .band_max ~ "HIGH",
      TRUE ~ ""
    ))

  return(dat)
}
