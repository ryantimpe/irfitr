#' Add a new "band" dimension to a data frame.
#'
#' This new "band" dimension contains the min and max range of a ratio over two metrics in the data.
#' e.g. From shipment and spending data, split data frame into a range of price (spending / shipments) bands of items.
#'
#' @param df A data frame of categorical dimensions and two value columns: the numerator and denominator.
#' @param ratio_name Name of the new categorical dimension containing ratio bands (e.g. Price)
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
#' @return A data frame with additional band dimension. Total over this dimension ties to initial data
#'
#' @export

ir_split_into_bands <- function(df, target_dim, numerator, denominator,
                         ratio_name = "Ratio",
                         minimum_distribution = 0,
                         seed_numer = NULL, seed_numer_wght = 0.5,
                         seed_denom = NULL, seed_denom_wght = 0.5,
                         ratio_bounds = NULL, ratio_bounds_names = c("min", "max"),
                         smash_param = 1/10, max_iterations = 40,
                         save.intermediates = FALSE, save.ratio = FALSE, show.messages = TRUE){

  if(!is.null(seed_numer) && !is.null(seed_denom)){
    message("It's not recommended to use both a numerator seed and denominator seed. If necessary, but function does not converge, try lowering the weights.")
  }

  message("Beginning ratio splitting. This may take a few moments...")

  ##
  #Process target_bands
  ##

  if(!is.data.frame(target_dim)){
    target_bands <- ir_band_split(target_dim)
  }

  ##
  # Initialize Data frame
  ##
  dat <- df

  names(dat)[names(dat) == numerator] <-   ".numer_nobands"
  names(dat)[names(dat) == denominator] <- ".denom_nobands"

  dims <- names(dat)[!(names(dat) %in% c(".numer_nobands", ".denom_nobands"))]

  ###
  # Additional inputs
  ###

  ##
  # Numerator seed
  ##
  if(!is.null(seed_numer)){
    inp_numer <- seed_numer

    names(inp_numer)[names(inp_numer) == numerator] <- ".numer"
    names(inp_numer)[names(inp_numer) == ratio_name]  <- ".band"

    inp_numer <- inp_numer %>%
      group_by_at(vars(-.band, -.numer)) %>%
      mutate(.i_numer_dist = .numer / sum(.numer, na.rm=TRUE)) %>%
      ungroup() %>%
      select(-.numer)
  } else {
    inp_numer <- NULL
  }

  ##
  # Denominator seed
  ##
  if(!is.null(seed_denom)){
    inp_denom <- seed_denom

    names(inp_denom)[names(inp_denom) == denominator] <- ".denom"
    names(inp_denom)[names(inp_denom) == ratio_name]  <- ".band"

    inp_denom <- inp_denom %>%
      group_by_at(vars(-.band, -.denom)) %>%
      mutate(.i_denom_dist = .denom / sum(.denom, na.rm=TRUE)) %>%
      ungroup() %>%
      select(-.denom)
  } else {
    inp_denom <- NULL
  }

  ##
  # Ratio bounds
  ##
  if(!is.null(ratio_bounds)){
    inp_bounds <- ratio_bounds

    names(inp_bounds)[names(inp_bounds) == ratio_bounds_names[1]] <- ".i_band_min"
    names(inp_bounds)[names(inp_bounds) == ratio_bounds_names[2]] <- ".i_band_max"

    #TODO: Add script to split this df into many if needed

  } else {
    inp_bounds <- NULL
  }

  #####
  # Expand data frame into bands
  #####

  #Weights for seed inputs
  wght_seed_numer <- max(0, min(seed_numer_wght, 1))
  wght_seed_numer <- c(wght_seed_numer, 1-wght_seed_numer)

  wght_seed_denom <- max(0, min(seed_denom_wght, 1))
  wght_seed_denom <- c(wght_seed_denom, 1-wght_seed_denom)

  #intialize data frame with bands
  dat0 <- dat %>%
    mutate(.ratio_nobands = .numer_nobands / .denom_nobands) %>%
    # mutate(pb_dist = purrr::map(.ratio_nobands, ~ir_dist_truncnorm_a(.x, 1/3, target_bands))) %>%
    mutate(pb_dist = purrr::map(.ratio_nobands, ~ir_dist_truncnorm_a(1000, .x, 1/3, target_bands))) %>%
    unnest() %>%
    #Seed numerator input
    do(
      if(!is.null(inp_numer)){
        left_join(., inp_numer,
                  by = names(inp_numer)[!(names(inp_numer) %in% c(".i_numer_dist"))]) %>%
          mutate_at(vars(dplyr::contains("_numer_dist")), funs(ifelse(!is.na(.), ., 0))) %>%
          mutate(.x_numer_dist = wght_seed_numer[2]*.b_numer_dist + wght_seed_numer[1]*.i_numer_dist)
      } else {
        mutate(., .x_numer_dist = .b_numer_dist)
      }
    ) %>%
    #Seed denominator input
    do(
      if(!is.null(inp_denom)){
        left_join(., inp_denom,
                  by = names(inp_denom)[!(names(inp_denom) %in% c(".i_denom_dist"))]) %>%
          mutate_at(vars(dplyr::contains("_denom_dist")), funs(ifelse(!is.na(.), ., 0))) %>%
          mutate(.x_denom_dist = wght_seed_denom[2]*.b_denom_dist + wght_seed_denom[1]*.i_denom_dist)
      } else {
        mutate(., .x_denom_dist = .b_denom_dist)
      }
    ) %>%
    #Additional Ratio min/max info
    do(
      if(!is.null(inp_bounds)){
        left_join(., inp_bounds,
                  by = names(inp_bounds)[!(names(inp_bounds) %in% c(".i_band_min", ".i_band_max"))]) %>%
          #If the input min/max is gt/lt the band min/max, 0 out the distribution
          mutate(.x_numer_dist = case_when(
            is.na(.i_band_min) & is.na(.i_band_max) ~ .x_numer_dist,
            !is.na(.i_band_min) & (.i_band_min > .band_max) ~ 0,
            !is.na(.i_band_max) & (.i_band_max < .band_min) ~ 0,
            TRUE ~ .x_numer_dist
          )) %>%
          #Use input min/max if applicable
          mutate(.band_min = ifelse((.i_band_min > .band_min) & !is.na(.i_band_min), .i_band_min, .band_min),
                 .band_max = ifelse((.i_band_max < .band_max) & !is.na(.i_band_max), .i_band_max, .band_max))
      } else {.}
    ) %>%
    #Initial numerators and denominators
    mutate(.numer = .numer_nobands * .x_numer_dist,
           .denom = .denom_nobands * .x_denom_dist) %>%
    #Need a .band_max
    mutate(.band_max = ifelse(is.na(.band_max), .band_min * 2, .band_max)) %>%
    mutate(.ratio = .numer / .denom)

  ##
  # Check ratios
  ##
  dat1 <- dat0 %>%
    mutate(.ratio_check = case_when(
      .ratio < .band_min ~ "LOW",
      .ratio > .band_max ~ "HIGH",
      TRUE ~ ""
    ))

  message(paste("Initial Distribution ------------"))
  message(dat1 %>% count(.ratio_check))

  ##
  # Iteration loop
  ##
  count_oob <- 9999
  iteration <- 1

  dat2 <- dat1 %>%
    mutate(.numer_dist = .x_numer_dist,
           .denom_dist = .x_denom_dist)

  while(count_oob > 0 && iteration <= max_iterations){
    dat2 <- dat2 %>%
      ir_ratio_scale(dims = dims, smash_param = smash_param)

    message(paste("Round", iteration, "------------"))
    ratio.check <- dat2 %>% count(.ratio_check)
    message(paste0("High ratios: ", ratio.check %>% filter(.ratio_check == "HIGH") %>% as.integer(),
                   "\nLow ratios: ", ratio.check %>% filter(.ratio_check == "LOW") %>% as.integer()))

    count_oob <- dat2 %>% filter(.ratio_check != "") %>% nrow()

    iteration <- iteration + 1
  }

  ###
  # Remove Denoms below minimum distribution
  ###

  if(minimum_distribution <= 0){
    dat3 <- dat2
  } else {
    min_dist <- minimum_distribution
    #If the value is large, it's probably written at % and needs to be fixed
    if(min_dist >= 1){min_dist <- min_dist/100}

    #Large values are not useful, set to a max of 5%
    if(min_dist > 0.05){
      message("Minimum % distribution is very large. Reducing to a maximum of 5%.")
      min_dist <- 0.05
    }

    dat2_check_min <- dat2 %>%
      filter(.numer_dist > 0 | !is.na(.numer_dist)) %>%
      filter(.numer_dist < min_dist)

    message(paste(nrow(dat2_check_min), "instances where the distribution of", denominator, "is less than", round(min_dist*100), "%"))

    dat3 <- dat2 %>%
      mutate(.numer_new = ifelse(.numer_dist < min_dist, 0, .numer),
             .denom_new = ifelse(.numer_dist < min_dist, 0, .denom)) %>%
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
        .ratio < .band_min ~ "LOW",
        .ratio > .band_max ~ "HIGH",
        TRUE ~ ""
      ))

    message(paste("Second wave - Removing", denominator, "with less than", round(min_dist*100), "%"))
    message(paste("Initial Distribution ------------"))
    message(dat3 %>% count(.ratio_check))

    #New iterations
    count_oob <- dat3 %>% filter(.ratio_check != "") %>% nrow()
    iteration <- 1

    while(count_oob > 0 && iteration <= max_iterations){
      dat3 <- dat3 %>%
        ir_ratio_scale(dims = dims, smash_param = smash_param)

      message(paste("Wave 2, Round", iteration, "------------"))
      ratio.check <- dat3 %>% count(.ratio_check)
      message(paste0("High ratios: ", ratio.check %>% filter(.ratio_check == "HIGH") %>% as.integer(),
                     "\nLow ratios: ", ratio.check %>% filter(.ratio_check == "LOW") %>% as.integer()))

      count_oob <- dat3 %>% filter(.ratio_check != "") %>% nrow()

      iteration <- iteration + 1
    }
  }

  ##
  #Clean Output
  ##
  dat_out <- dat3
  if(!save.ratio){
    dat_out <- dat_out %>% select(-.ratio)
  }
  names(dat_out)[names(dat_out) == ".band"] <- ratio_name
  names(dat_out)[names(dat_out) == ".ratio"] <- paste0(ratio_name, "_calc")
  names(dat_out)[names(dat_out) == ".numer"] <- numerator
  names(dat_out)[names(dat_out) == ".denom"] <- denominator
  if(!save.intermediates){
    dat_out <- dat_out %>% select(-dplyr::starts_with("."))
  }

  return(dat_out)
} #End Price Tool
