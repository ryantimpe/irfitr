#' Add a new "band" dimension to a data frame.
#'
#' This new "band" dimension contains the min and max range of a ratio over two metrics in the data.
#' e.g. From shipment and spending data, split data frame into a range of price (spending / shipments) bands of items.
#'
#' @param df A data frame of categorical dimensions and two value columns: the numerator and denominator.
#' @param ratio_name Name of the new categorical dimension containing ratio bands (e.g. Price)
#' @param sd Standard deviation of truncated normal distribution, expressed as a percent of mean. Can be a single number, or a data frame of values differing by element.
#' @param sd_default If \code{sd} is a data frame, value to use for missing elements.
#' @param sd_name If \code{sd} is a data frame, name of value to use as standard deviatation.
#' @param minimum_distribution Optional value of the minimum percent share of any new band element. (e.g. 0.01 means no bands can contain less than 1% of group total.)
#' @param seed_numer Optional data frame of initial starting distributions of the numerator.
#' @param seed_numer_wght Value between 0 and 1 of how much weight should be added to \code{seed_numer}. Larger values may not converge.
#' @param seed_denom Optional data frame of initial starting distributions of the denominator.
#' @param seed_denom_wght Value between 0 and 1 of how much weight should be added to \code{seed_denom}. Larger values may not converge.
#' @param ratio_input Optional data frame of average price per unit in a ratio band for all or a subset of rows or columns.
#' @param ratio_input_name Name of input ratio value in \code{ratio_input}.
#' @param ratio_bounds Optional data frame of minimum and maximum ratios for specific rows or groups. Uses \code{dplyr::left_join()}, so all values will be replicated over all elements in excluded dimensions.
#' @param ratio_bounds_names Names of the minimum and maximum ratio values in \code{ratio_bounds}.
#' @param ratio_freeze Optional data frame of to freeze ratios for specific rows or groups. Uses \code{dplyr::left_join()}, so all values will be replicated over all elements in excluded dimensions.
#' @param ratio_freeze_names Name of the freeze ratio values in \code{ratio_freeze}.
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
                         sd = 1/3, sd_default = 1/3, sd_name = "sd",
                         minimum_distribution = 0,
                         seed_numer = NULL, seed_numer_wght = 0.5,
                         seed_denom = NULL, seed_denom_wght = 0.5,
                         ratio_input = NULL, ratio_input_name = "Ratio_calc",
                         ratio_bounds = NULL, ratio_bounds_names = c("min", "max"),
                         ratio_freeze = NULL, ratio_freeze_name = "Ratio_calc",
                         smash_param = 1/10, max_iterations = 40,
                         save.intermediates = FALSE, save.ratio = FALSE, show.messages = TRUE){

  if(!is.null(seed_numer) && !is.null(seed_denom)){
    message("Only use both a numerator seed and denominator seed if they are from the same view and the implied ratios are within bounds. If the function does not converge, try lowering the weights.")
  }

  message("Beginning ratio splitting. This may take a few moments...")

  ##
  #Process target_bands
  ##

  if(!is.data.frame(target_dim)){
    target_bands <- ir_band_split(target_dim)
  } else {
    target_bands <- target_dim
  }

  ##
  # Initialize Data frame
  ##
  dat <- df

  names(dat)[names(dat) == numerator] <-   ".numer_nobands"
  names(dat)[names(dat) == denominator] <- ".denom_nobands"

  dims <- names(dat)[!(names(dat) %in% c(".numer_nobands", ".denom_nobands"))]

  ###
  # Additional inputs -----
  ###

  ##
  # Seeds ----
  ##

  #Weights for seed inputs
  wght_seed_numer <- max(0, min(seed_numer_wght, 1))
  wght_seed_numer <- c(wght_seed_numer, 1-wght_seed_numer)

  wght_seed_denom <- max(0, min(seed_denom_wght, 1))
  wght_seed_denom <- c(wght_seed_denom, 1-wght_seed_denom)

  #Update May 25, 2018 - If only one seed is supplied (eg denom w/o numerator), create a seed for the other to initiallize the ratios correctly

  #1 - Numerator, but no Denominator
  if(!is.null(seed_numer) & is.null(seed_denom)){
    inp_numer <- seed_numer

    names(inp_numer)[names(inp_numer) == numerator] <- ".numer"
    names(inp_numer)[names(inp_numer) == ratio_name]  <- ".band"

    inp_numer <- inp_numer %>%
      group_by_at(vars(-.band, -.numer)) %>%
      mutate(.i_numer_dist = .numer / sum(.numer, na.rm=TRUE)) %>%
      ungroup() %>%
      select(-.numer)

    inp_denom <- inp_numer %>%
      left_join(target_bands %>% select(.band, .band_mean), by = ".band") %>%
      mutate(.denom = .i_numer_dist / .band_mean) %>%
      select(-.i_numer_dist, -.band_mean) %>%
      group_by_at(vars(-.band, -.denom)) %>%
      mutate(.i_denom_dist = .denom / sum(.denom, na.rm=TRUE)) %>%
      ungroup() %>%
      select(-.denom)

    #Set denom weights to numer weights
    wght_seed_denom <- wght_seed_numer

  } else if(!is.null(seed_denom) & is.null(seed_numer)){
    #2- Denominator but no Numerator
    inp_denom <- seed_denom

    names(inp_denom)[names(inp_denom) == denominator] <- ".denom"
    names(inp_denom)[names(inp_denom) == ratio_name]  <- ".band"

    inp_denom <- inp_denom %>%
      group_by_at(vars(-.band, -.denom)) %>%
      mutate(.i_denom_dist = .denom / sum(.denom, na.rm=TRUE)) %>%
      ungroup() %>%
      select(-.denom)

    inp_numer <- inp_denom %>%
      left_join(target_bands %>% select(.band, .band_mean), by = ".band") %>%
      mutate(.numer = .i_denom_dist * .band_mean) %>%
      select(-.i_denom_dist, -.band_mean) %>%
      group_by_at(vars(-.band, -.numer)) %>%
      mutate(.i_numer_dist = .numer / sum(.numer, na.rm=TRUE)) %>%
      ungroup() %>%
      select(-.numer)

    #Set numer weights to denom weights
    wght_seed_numer <- wght_seed_denom

  } else if(!is.null(seed_denom) & !is.null(seed_numer)){
    #3 - Numerator and Denominator supplied (no interaction)
    inp_numer <- seed_numer
    inp_denom <- seed_denom

    names(inp_numer)[names(inp_numer) == numerator] <- ".numer"
    names(inp_numer)[names(inp_numer) == ratio_name]  <- ".band"
    names(inp_denom)[names(inp_denom) == denominator] <- ".denom"
    names(inp_denom)[names(inp_denom) == ratio_name]  <- ".band"

    inp_numer <- inp_numer %>%
      group_by_at(vars(-.band, -.numer)) %>%
      mutate(.i_numer_dist = .numer / sum(.numer, na.rm=TRUE)) %>%
      ungroup() %>%
      select(-.numer)

    inp_denom <- inp_denom %>%
      group_by_at(vars(-.band, -.denom)) %>%
      mutate(.i_denom_dist = .denom / sum(.denom, na.rm=TRUE)) %>%
      ungroup() %>%
      select(-.denom)

  } else {
    #4 - No inputs
    inp_numer <- NULL
    inp_denom <- NULL
  }
  
  ##
  # Ratio input ----
  ##
  if(!is.null(ratio_input)){
    inp_ratio <- ratio_input
    
    names(inp_ratio)[names(inp_ratio) == ratio_name] <- ".band"
    names(inp_ratio)[names(inp_ratio) == ratio_input_name] <- ".i_band_mean"
    
    #MUST have the .band dimension in this work, else distribution will fail
    if(!(".band" %in% names(inp_ratio))){
      stop(paste("Input 'ratio_input' must contain a ratio band dimension called", ratio_name, "\n",
                 "This input overwrites the generic average price of each band when calculating the initial distribution."))
    }
      
  } else {
    inp_ratio <- NULL
  }

  ##
  # Ratio bounds ----
  ##
  if(!is.null(ratio_bounds)){
    inp_bounds <- ratio_bounds

    names(inp_bounds)[names(inp_bounds) == ratio_bounds_names[1]] <- ".i_band_min"
    names(inp_bounds)[names(inp_bounds) == ratio_bounds_names[2]] <- ".i_band_max"

    #TODO: Add script to split this df into many if needed

  } else {
    inp_bounds <- NULL
  }
  
  ##
  # Ratio Freezing ----
  ##
  if(!is.null(ratio_freeze)){
    inp_freeze <- ratio_freeze
    
    names(inp_freeze)[names(inp_freeze) == ratio_name] <- ".band"
    names(inp_freeze)[names(inp_freeze) == ratio_freeze_name] <- ".f_ratio"
    
    inp_freeze_test <- inp_freeze %>%
      left_join(target_bands, by = ".band") %>% 
      mutate(.fail = case_when(
        .f_ratio < .band_min ~ TRUE,
        .f_ratio > .band_max & !is.na(.band_max) ~ TRUE,
        TRUE ~ FALSE
      ))
    
    if(any(inp_freeze_test$.fail)){
      stop("Supplied input 'ratio_freeze' values must be within price bands.")
    }
    rm(inp_freeze_test)
    
    #TODO: Add script to split this df into many if needed
    
  } else {
    inp_freeze <- NULL
  }

  ##
  # Standard deviation ----
  ##
  if(!(is.data.frame(sd) || (is.numeric(sd) && length(sd) == 1 && sd < 1 && sd > 0))){
    stop("Standard deviation (sd) should be a single number between 0 and 1 or a data frame of values between 0 and 1.")
  }
  if(is.data.frame(sd)){
    inp_sd <- sd

    names(inp_sd)[names(inp_sd) == sd_name] <- ".i_sd"

    #TODO: Add script to split this df into many if needed

  } else {
    inp_sd <- NULL
  }

  #####
  # Expand data frame into bands ----
  #####

  dat0 <- dat %>%
    #intialize data frame with bands ----
    # This only distributes the Denominator for now, June 26, 2018
    mutate(.ratio_nobands = .numer_nobands / .denom_nobands) %>%
    do(
      if(is.null(inp_sd)){
        mutate(., pb_dist = purrr::map(.ratio_nobands, ~ir_dist_truncnorm_a(1000, .x, sd, target_bands)))
      } else {
        left_join(., inp_sd,
                  by = names(inp_sd)[!(names(inp_sd) %in% c(".i_sd"))]) %>%
          mutate(.i_sd = ifelse(is.na(.i_sd), sd_default, .i_sd)) %>%
          mutate(pb_dist = purrr::map2(.ratio_nobands, .i_sd, ~ir_dist_truncnorm_a(1000, .x, .y, target_bands)))
      }
    ) %>%
    unnest() %>%
    #Next distribute the Numerator
    # The do loop allows for overwriting the .band_mean with ratio_input
    do(
      if(!is.null(inp_ratio)){
        left_join(., inp_ratio,
                  by = names(inp_ratio)[!(names(inp_ratio) %in% c(".i_band_mean"))]) %>% 
          mutate(.x_band_mean = ifelse(!is.na(.i_band_mean), .i_band_mean, .band_mean))
      } else {
        mutate(., .x_band_mean = .band_mean)
      }
    ) %>% 
    #Now calculate numerator distribution with this new .x_band_mean
    mutate(.b_numer_dist = .b_denom_dist * .x_band_mean) %>% 
    group_by_at(vars(dims)) %>% 
    mutate(.b_numer_dist = .b_numer_dist / sum(.b_numer_dist)) %>% 
    ungroup() %>% 
    #Seed numerator input ----
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
    #Seed denominator input ----
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
    #Additional Ratio min/max info ----
    do(
      if(!is.null(inp_bounds)){
        left_join(., inp_bounds,
                  by = names(inp_bounds)[!(names(inp_bounds) %in% c(".i_band_min", ".i_band_max"))]) %>%
          #If the input min/max is gt/lt the band min/max, 0 out the distribution
          mutate(.x_numer_dist = case_when(
            is.na(.i_band_min) & is.na(.i_band_max) ~ .x_numer_dist,
            !is.na(.i_band_min) & (.i_band_min >= .band_max) ~ 0,
            !is.na(.i_band_max) & (.i_band_max <= .band_min) ~ 0,
            TRUE ~ .x_numer_dist
          )) %>%
          #Use input min/max if applicable
          mutate(.band_min = ifelse((.i_band_min > .band_min) & !is.na(.i_band_min), .i_band_min, .band_min),
                 .band_max = ifelse((.i_band_max < .band_max) & !is.na(.i_band_max), .i_band_max, .band_max))
      } else {.}
    ) %>%
    #Initial numerators and denominators ----
    mutate(.numer = .numer_nobands * .x_numer_dist,
           .denom = .denom_nobands * .x_denom_dist) %>%
    #Need a .band_max - min*2 method fails with data is bad and ratio is way higher than upper band
    # mutate(.band_max = ifelse(is.na(.band_max), .band_min * 2, .band_max)) %>%
    #JUne 22, 2018 update
    rowwise() %>% 
    mutate(.band_max = ifelse(is.na(.band_max), max(.band_min * 2, .ratio_nobands*2), .band_max)) %>%
    ungroup() %>% 
    mutate(.ratio = .numer / .denom)

  ##
  # Check ratios ----
  ##
  dat1 <- dat0 %>%
    mutate(.ratio_check = case_when(
      .ratio < .band_min ~ "LOW",
      .ratio > .band_max ~ "HIGH",
      TRUE ~ ""
    ))

  message(paste("Initial Distribution ------------"))
  ratio.check <- dat1 %>% count(.ratio_check)
  ratio.check.h <- ratio.check %>% filter(.ratio_check == "HIGH") %>% pull()
  ratio.check.l <- ratio.check %>% filter(.ratio_check == "LOW") %>% pull()
  message(paste0("High ratios: ", ratio.check.h,
                 "\nLow ratios: ", ratio.check.l))

  ##
  # Iteration loop
  ##
  count_oob <- 9999
  iteration <- 1

  #Bring in additional inputs here
  dat2 <- dat1 %>%
    mutate(.numer_dist = .x_numer_dist,
           .denom_dist = .x_denom_dist) %>% 
    #Freeze select bands with inp_freeze and .f_ratio
    do(
      if(!is.null(inp_freeze)){
        left_join(., inp_freeze, 
                  by = names(inp_freeze)[!(names(inp_freeze) %in% c(".f_ratio"))])
      } else {.}
    )

  while(count_oob > 0 && iteration <= max_iterations){
    dat2 <- dat2 %>%
      ir_ratio_scale(dims = dims, smash_param = smash_param)

    message(paste("Round", iteration, "------------"))
    ratio.check <- dat2 %>% count(.ratio_check)
    ratio.check.h <- ratio.check %>% filter(.ratio_check == "HIGH") %>% pull()
    ratio.check.l <- ratio.check %>% filter(.ratio_check == "LOW") %>% pull()
    message(paste0("High ratios: ", ratio.check.h,
                   "\nLow ratios: ", ratio.check.l))

    count_oob <- dat2 %>% filter(.ratio_check != "") %>% nrow()

    iteration <- iteration + 1
  }

  message("Wave 1 ratio splitting complete!")

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
      filter(.denom_dist > 0 | is.na(.denom_dist)) %>%
      filter(.denom_dist < min_dist)

    message("------------------------")
    message(paste(nrow(dat2_check_min), "instances where the distribution of", denominator, "is less than", round(min_dist*100), "%"))

    dat3 <- dat2 %>%
      mutate(.numer_new = ifelse(.denom_dist < min_dist, 0, .numer),
             .denom_new = ifelse(.denom_dist < min_dist, 0, .denom)) %>%
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

    # if(nrow(dat2_check_min) > 0){
    #   message(paste("Initial Redistribution ------------"))
    #   ratio.check <- dat3 %>% count(.ratio_check)
    #   ratio.check.h <- ratio.check %>% filter(.ratio_check == "HIGH") %>% pull()
    #   ratio.check.l <- ratio.check %>% filter(.ratio_check == "LOW") %>% pull()
    #   message(paste0("High ratios: ", ratio.check.h,
    #                  "\nLow ratios: ", ratio.check.l))
    # }

    #New iterations
    count_oob <- dat3 %>% filter(.ratio_check != "") %>% nrow()
    count_oob <- 9999
    iteration <- 1

    # dat3 <- dat2

    while(count_oob > 0 && iteration <= max_iterations){
      dat3 <- dat3 %>%
        # mutate(.numer = ifelse(.numer_dist < min_dist, 0, .numer),
        #        .denom = ifelse(.numer_dist < min_dist, 0, .denom)) %>%
        ir_ratio_scale(dims = dims, smash_param = smash_param)

      message(paste("Wave 2, Round", iteration, "------------"))
      ratio.check <- dat3 %>% count(.ratio_check)
      ratio.check.h <- ratio.check %>% filter(.ratio_check == "HIGH") %>% pull()
      ratio.check.l <- ratio.check %>% filter(.ratio_check == "LOW") %>% pull()
      message(paste0("High ratios: ", ratio.check.h,
                     "\nLow ratios: ", ratio.check.l))

      count_oob <- dat3 %>% filter(.ratio_check != "") %>% nrow()

      iteration <- iteration + 1
    }

    message("Wave 2 ratio splitting complete!")
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
