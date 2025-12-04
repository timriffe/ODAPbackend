#' @title `smooth_flexible`
#' @description This is a wrapper around `smooth_flexible_chunk` that allows us to work with `.id` variable and create the output for corresponding groups. Smoothes population or death counts using various methods from the `DemoTools` package and paragraph five from "Method protocol for evaluating census population data by age and sex".
#' @param data_in tibble or data.frame. A tibble with two numeric columns - population or death counts with supported names: `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`, and corresponding numeric `Age` - provided in single age intervals, 5-year age intervals, or abridged age format e.g. with ages 0, 1, 5, etc.
#' @param variable character. A scalar with the variable name which is to be graduated. The list of possible options includes `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`.
#' @param fine_method the `method` argument of `graduate()` function from `DemoTools` that graduates grouped data. Possible options include  `auto`, `none`, `sprague`, `beers(ord)`, `beers(mod)`, `grabill`, `pclm`, `mono`, `uniform`.
#' @param rough_method the `method` argument of `smooth_age_5()` function from `DemoTools` that smoothes populations in 5-year age groups using various methods. Possible options include `auto`, `none`, `Carrier-Farrag`, `KKN`, `Arriaga`, `United Nations`, `Strong`, `Zigzag`.
#' @param constrain_infants logical, if age 0 is a separate age class, shall we constrain its proportion within the age group 0-5 in the output? Default to `TRUE`.
#' @param u5m numeric. Under-five mortality rate. Defaults to `NULL`.
#' @param age_out character. The desired age structure of the output file. Possible options include `single` - for single years, `5-year` - for 5-year data, and `abridged` - for abridged data, e.g. 0, 1, 5, etc.
#' @param Sex character. Either `m` for males, `f` for females, or `t` for total (default). Please note that in case this parameter is not explicitly provided, the function will scan the data for the column with the corresponding name and use its levels automatically.
#' @param by_args character. A vector of columns should be also included in the output. These columns are usually ones that are used for `.id` construction. Defaults to `NULL`. It is important to not include `Sex` in this vector.
#' @param i18n An optional i18n object for translation.
#' @importFrom dplyr mutate group_nest first across pull
#' @importFrom tidyr unnest unite
#' @importFrom purrr set_names map
#' @importFrom tidyselect all_of everything
#' @importFrom rlang .data
#' @importFrom DemoTools age2int is_single is_abridged graduate_uniform names2age calcAgeAbr groupAges smooth_age_5 graduate
#' @return data_out. A list of lists. For each separate group specified by the `.id` column, the function will generate a list that contains 2 lists: data and figures. Data tibble with two numeric columns - smoothed counts for the chosen `variable` and `Age` - chosen age grouping and one character column indicating `.id` groups. May optionally include the columns identified in `by_args`. And two figures data.frames - `data_adjusted` and `data_original` corresponding to the data.frames with pre and post-old age mortality adjustment and a `ggplot2` figure visualizing the corresponding differences.
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' fpath <- system.file("extdata", 
#' "single_hmd_spain.csv.gz", 
#' package = "ODAPbackend")
#' data_in <- read_csv(fpath) |>
#'   dplyr::select(-1)
#' ex1 <- smooth_flexible(
#'  data_in,
#'  variable     = "Exposures",
#'  rough_method = "auto",
#'  fine_method  = "auto",
#'  age_out      = "abridged",
#'  u5m          = NULL,
#'  Sex          = "t",
#'  constrain_infants = TRUE,
#'  by_args      = "Year"
#')
#'
#' # data
#' ex1$data_out
#' # arguments
#' ex1$arguments
#' # figures
#' ex1$figures$`1_2012`$figure
#' ex1$figures$`2_2012`$figure
#' ex1$figures$`3_2012`$figure
#' 
smooth_flexible <- function(data_in,
                            variable      = "Deaths",
                            age_out       = c("single", "abridged", "5-year"),
                            fine_method   = c("auto", "none", "sprague",
                                              "beers(ord)", "beers(mod)",
                                              "grabill", "pclm", "mono",
                                              "uniform"),
                            rough_method  = c("auto", "none", "Carrier-Farrag",
                                              "KKN", "Arriaga", "United Nations",
                                              "Strong", "Zigzag"),
                            u5m               = NULL,
                            constrain_infants = TRUE,
                            Sex = "t",
                            by_args = NULL,
                            i18n = NULL) {
  
  ## f_args <- capture_args()
  
  if (!(".id" %in% colnames(data_in))) {
    data_in <- data_in |>
      mutate(.id = "all")
  }
  
  # id <-  unique(data_in$.id)

  # if there is a single subset then use sex and is defauдt for total
  
  if (!"Sex" %in% colnames(data_in)){
    data_in$Sex <- Sex
  }
  
  group_func <- function(group_data, i18n) {
    
    Sex <- first(group_data$Sex)
    
    smooth_flexible_chunk(
      data_in           = group_data,
      variable          = variable,
      age_out           = age_out,
      fine_method       = fine_method,
      rough_method      = rough_method,
      u5m               = u5m,
      Sex               = Sex,
      constrain_infants = constrain_infants,
      i18n              = i18n
    )
  }

  # by_args <- names(data_in)[!names(data_in) %in% c("Age", "Deaths", "Exposures", "Mx_emp", "Sex")]

  results <-
    data_in |>
    mutate(Sex = substr(Sex, 1, 1),
           Sex = tolower(Sex)) |>
    # dplyr::select(all_of(c(".id", "Sex", "Age", variable, by_args))) |>
    group_nest(across(all_of(c(".id", by_args)))) |>  # Use across to group by multiple columns
    mutate(result = map(.data$data, ~ group_func(.x, i18n = i18n)))  # Apply function
    
  # Process each group separately
  # results <- data_in |>
  #   mutate(Sex = substr(Sex, 1, 1), Sex = tolower(Sex)) |>
  #   group_split(.id, .keep = TRUE) |>
  #   map(~ group_func(.x))
  #
  
  nms <- results |>
    dplyr::select(all_of(c(".id", by_args))) |>
    unite("one", everything(), sep = "_") |>
    pull(.data$one)
  
  smoothed_data <- results |>
    mutate(data = map(.data$result, ~ .x[["data"]]))|>
    dplyr::select(-c(.data$result)) |>
    unnest(.data$data)
  
  figures <- results$result |>
    map("figure") |>
    set_names(nms)
  
  return(list(data_out  = smoothed_data, 
              # arguments = f_args,
              figures   = figures
              ))
}

#' @title `graduate_auto`
#' @description Smoothed population or death counts with moving averages. The method was adopted from paragraph five of the "Method protocol for evaluating census population data by age and sex".
#' @param data_in tibble or data.frame. A tibble with two numeric columns - population or death counts with supported names: `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`, and corresponding `Age` - provided in single age intervals, 5-year age intervals, or abridged age format e.g. with ages 0, 1, 5, etc.
#' @param variable character. A scalar with the variable name which is to be graduated. The list of possible options includes `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`.
#' @param age_out character. A scalar with the desired age grouping output. Includes three possible options - `single` for single ages, `5-year` - for 5-year age groups, and `abridged` - for abridged data.
#' @param constrain_infants logical. A scalar that indicates whether the infant proportions should be constrained or left as is. Defaults to `TRUE`.
#' @param u5m numeric. Under-five mortality rate. Defaults to NULL.
#' @param Sex character. Either `m` for males, `f` for females, or `t` for total (default). Please note that in case this parameter is not explicitly provided, the function will scan the data for the column with the corresponding name and use its levels automatically.
#' @param i18n Optional internationalization object for translating plot labels and titles. Defaults to NULL.
#' @importFrom dplyr mutate group_by filter pull select summarise
#' @importFrom tibble tibble
#' @importFrom rlang := !! .data
#' @importFrom DemoTools is_single is_abridged check_heaping_bachi groupAges ageRatioScore mav graduate_mono calcAgeAbr age2int graduate_uniform names2age lt_rule_4m0_D0 lt_rule_4m0_m0
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_color_manual scale_linetype_manual labs theme_minimal theme element_text
#' @return data_out. A list with three elements: `data_out` - a tibble with two numeric columns - smoothed counts for the chosen variable and `Age` - chosen age grouping, `plot` - a ggplot2 object comparing original vs graduated data, and `arguments` - a list of arguments used in the function.
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # 5-year age data
#' fpath <- system.file("extdata", 
#' "five_hmd_spain.csv.gz", 
#' package = "ODAPbackend")
#' data_in <- read_csv(fpath) |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' ex1 <- graduate_auto(data_in,
#'                      age_out  = "single",
#'                      variable = "Deaths",
#'                      constrain_infants = TRUE)
#' 
#' # data
#' ex1$data_out
#' # arguments
#' ex1$arguments
#' 

graduate_auto <- function(data_in,
                          age_out  = c("single", "abridged", "5-year"),
                          variable = NULL,
                          u5m      = NULL,
                          Sex      = c("t", "f", "m"),
                          constrain_infants = TRUE,
                          i18n     = NULL) {
  
  ## f_args  <- capture_args()
  age_out <- match.arg(age_out, c("single", "abridged", "5-year"))
  Sex     <- match.arg(Sex,     c("t", "f", "m"))
  
  # check if data comes in single ages
  Age     <- data_in$Age
  age_in  <- case_when(is_single(Age)       ~ "single",
                       is_abridged(Age)     ~ "abridged",
                       all(age2int(Age) == 5,
                           na.rm = TRUE)    ~ "5-year",   
                       TRUE                 ~ "other")
  
  # if not single or abridged, then force either abridged or 5-year,
  # depending on whether infants given.
  if(age_in == "other") {
    
    has_infants <- age2int(Age)[1] == 1
    varb        <- data_in[, variable, drop = TRUE]
    varb1       <- graduate_uniform(varb, Age)
    age1        <- names2age(varb1)
    
    if(has_infants) {
      ageN   <- calcAgeAbr(age1)
      varb   <- groupAges(varb1, age1, AgeN = ageN)
      Age    <- names2age(varb)
      age_in <- "abridged"
      
    } else {
      
      varb   <- groupAges(varb1, age1, N = 5)
      age_in <- "5-year"
      Age    <- names2age(varb)
      
    }
    
    data_in <- tibble(Age = Age,
                      !!variable := varb)
  }
  
  # if data is abridged, then group first two ages. Next we can just use 5-year data protocol.
  # Also calculate the proportion in first ages in case we want to retutn the abridged data.
  if(age_in == "abridged") { 
    
    # data and Age as vectors
    varb <- data_in[, variable, drop = TRUE]
    Age  <- data_in$Age
    
    # calculate the distribution of first two ages for data_out if needed in future
    fst_ages     <- varb[1:2]
    pct_fst_ages <- fst_ages / sum(fst_ages)
    
    # group first two ages into one. uses non-standard evaluation
    data_in <- data_in |>
      mutate(Age = c(0, 0, Age[-c(1:2)])) |>
      group_by(Age) |>
      summarise(!!variable := sum(!!sym(variable)), .groups = "drop") |>
      select(!!variable, Age)
    
  }
  
  # if single, then save variables and calculate the proportion in first ages in case age_out is abridged
  if(age_in == "single") {
    
    # data and Age as vectors
    varb         <- data_in[, variable, drop = TRUE]
    Age          <- data_in$Age
    
    # calculate the distribution of first two ages for data_out if needed in future
    fst_ages     <- varb[1:5]
    fst_ages     <- c(fst_ages[1], sum(fst_ages[-1]))
    pct_fst_ages <- fst_ages / sum(fst_ages)
    
    # now we calculate adult bachi index. NOTE we need this only in case of the single year data.
    # Otherwise we jump to 5-year protocol
    bachi <- check_heaping_bachi(
      varb,
      Age     = Age,
      ageMin  = 23, # same, we can play with this if we want
      ageMax  = 77, # following their example, not explicitly stated
      method  = "pasex",
      details = TRUE
    )
    
    # some indexes that we will use in the analysis. Only valid for single age data
    # overall bachi index adults
    index <- bachi$index
    
    # pct for every number
    pct <- bachi$pct
    
    # BachiProp0and5
    # Proportion of heaping concentrated in digits 0 and 5 adults
    prp0and5 <- (sum(pct[c(1, 6)]) - 20) / index
    
    # Max2prop
    # Proportion of heaping concentrated in the most preferred 2 digits adults
    mxprop2 <- (sum(sort(pct, decreasing = TRUE)[c(1, 2)]) - 20) / index
    
  }
  
  # if infants are not separated, we can try to do a better job
  # by
  if(age_in == "5-year") { 
    
    varb <- data_in[, variable, drop = TRUE]
    Age  <- data_in$Age
    
    if(age_out != "5-year") {
      
      if(!is.null(u5m)) {
        
        # in odd case that child mortality is given, but Sex is not specified:
        if(is.null(Sex)) {
          
          Sex <- "t"
          warning("Sex argument not given. We assumed total (Sex = 't'). We use this variable to inform splitting the infant age group.")
          
        }
        
        # we need this variable for indirect method applied
        stopifnot(Sex %in% c("f", "m", "t")) 
        
        if(variable == "Deaths") {
          
          D5 <- varb[1]
          P5 <- D5 / u5m
          
        }
        
        if(variable %in% c("Exp", "Exposures", "Pop", "Population")) {
          
          P5 <- varb[1]
          D5 <- P5 * u5m
          
        }
        
        if(Sex %in% c("f", "m")) {
          
          D0   <- lt_rule_4m0_D0(D04 = D5, 
                                 M04 = u5m, 
                                 Sex = Sex)
          
          M0   <- lt_rule_4m0_m0(D04 = D5, 
                                 M04 = u5m, 
                                 Sex = Sex)
          
          P0   <- D0 / M0
          
        } else {
          
          D0m  <- lt_rule_4m0_D0(D04 = D5, 
                                 M04 = u5m, 
                                 Sex = "m")
          
          D0f  <- lt_rule_4m0_D0(D04 = D5, 
                                 M04 = u5m, 
                                 Sex = "f")
          
          D0   <- (D0m + D0f) / 2
          
          M0m  <- lt_rule_4m0_m0(D04 = D5, 
                                 M04 = u5m, 
                                 Sex = "m")
          
          M0f  <- lt_rule_4m0_m0(D04 = D5, 
                                 M04 = u5m, 
                                 Sex = "f")
          
          M0   <- (M0m + M0f) / 2
          P0   <- D0 / M0
          
        }
        
        if(variable == "Deaths") {
          
          D1_4 <- D5 - D0
          varb <- c(D0, D1_4, varb[-1])
          Age  <- c(Age[1], 1, Age[-1])
          
        }
        
        if(variable %in% c("Exp", "Exposures", "Pop", "Population")) {
          P1_4 <- P5 - P0
          varb <- c(P0, P1_4, varb[-1])
          Age  <- c(Age[1], 1, Age[-1])
          
        }
        
        # For purposes of continued decision-making
        age_in       <- "abridged"
        fst_ages     <- varb[1:2]
        pct_fst_ages <- fst_ages / sum(fst_ages)
        
      } else {
        
        if(constrain_infants) { 
          
          warning("Be mindful of results for the infant age group. Your output has a separate infant age group, but this was split from the input data without taking demographic knowledge into account. If you specify an under-5 mortality rate, u5m, we can do a better job.")
          
        }
        
      }
      
    }
    
  }
  
  # 3 different conditions are considered
  # 1) if ages are single and bachi is more than or equal to 30
  # is this case group data into 5 year ages and apply 5-year protocol
  # 2) if ages single and index < 30, then we can use the protocol for single ages
  # 3) if the data is grouped already, then use protocol for grouped data
  
  # case 1 - ages is single
  if(age_in == "single") {
    
    # if adult bachi more than 30, then we have o problem. We just apply the 5-year protol that works
    if(index >= 30) { 
      
      # group data in 5 years
      cmbn_5_yrs <- groupAges(Value = varb, 
                              Age   = Age, 
                              N     = 5)
      
      dat_5 <- tibble(!!variable := cmbn_5_yrs,
                      Age = as.integer(names(cmbn_5_yrs)))
      
      # apply protocol for 5 year data
      data_out <- graduate_auto_5(dat_5, variable = variable)
      
    } else {
      # Single year protocol is implemented if bachi is less than 30
      # NOTE: we separate kids and adults for now. We follow the protocol in case of adults adults
      # For kids, I save them separately, and since it is currently unclear what to do, I do the following:
      # IF the adult n for smoothing is less than 3, then I keep kids as is (n_kids = 1)
      # ELSE if it is more than 2, then I apply smoothing with n = 2 for consistency.
      # Otherwise we might have a very strange final distribution with erratic kids pattern.
      # Imagine adults being smoothed with n = 10, while kids left as is.
      # Why I use 2? Well simply because for 5-year data they say that max n for kids should be equal to 2. 
      # So I simply adopted it from there. We might change this in future, if we will not figure a better way of smoothing for kids.
      
      # We don't have years of education. 
      # So I choose the maximum n for mav from the two available in the table.
      n <- tibble(
        "min_bachi"    = c(4,    2,   0.75, 0,    8),
        "max_bachi"    = c(8,    4,   2,    0.75, 30),
        "second_index" = c(0.65, 0.6, 0.7,  0.55, 101),
        "ind"          = c(rep(prp0and5, 2), rep(mxprop2, 2), Inf),
        "mav_val_y"    = c(10, 6, 4, 2, 10),
        "mav_val_n"    = c(6,  4, 2, 1, 10)) |>
        filter(.data$min_bachi < index & .data$max_bachi >= index) |>
        mutate(my_n = ifelse(.data$ind > .data$second_index, .data$mav_val_y, .data$mav_val_n)) |>
        pull("my_n")
      
      # n for kids
      n_kids <- ifelse(n < 3, 1, 2)
      
      # First separate kids and adults
      
      # kids
      kids <- data_in |>
        filter(.data$Age < 18) |>
        pull(variable)
      
      # adults
      adults <- data_in |>
        filter(.data$Age > 17) |>
        pull(variable)
      
      # smoothing adults
      data_ad <- mav(
        Value = adults,
        Age   = Age[Age > 17],
        n     = n,
        tails = TRUE
      )
      
      # smoothing kids
      data_kd <- mav(
        Value = kids,
        Age   = Age[Age < 18],
        n     = n_kids,
        tails = TRUE
      )
      
      # combine adults and kids
      data_out <- tibble(!!variable := c(data_kd, data_ad),
                         Age = as.integer(names(c(data_kd, data_ad))))
      
    }
    
  } else { 
    
    # case 3, if the data is in 5 year age groups
    # If the data is already grouped and not abridged, then apply the 5 year method directly
    data_out <- graduate_auto_5(dat_5 = data_in, variable)
    
  }
  
  final_data_single <- is_single(data_out$Age)
  
  # (1) graduate_mono
  varb <- data_out[, variable, drop = TRUE]
  
  v2   <- graduate_mono(Value = varb, 
                        Age   = data_out$Age, 
                        OAG   = TRUE)
  
  age  <- as.integer(names(v2))
  
  # (2) regroup
  ageN <- switch(age_out,
                 "abridged" = calcAgeAbr(age),
                 "5-year"   = age - age %% 5,
                 "single"   = age)
  
  v3 <- groupAges(Value = v2, 
                  Age   = age, 
                  AgeN  = ageN)
  
  data_out <- tibble(!!variable := v3,
                     Age = as.integer(names(v3)))
  
  # (3) possibly constrain infants
  if(age_out %in% c("abridged", "single") & age_in %in% c("abridged", "single") & constrain_infants) {

    v_child      <- data_out[data_out$Age < 5, variable, drop = TRUE]
    vN           <- sum(v_child)
    v_child[1]   <- pct_fst_ages[1] * vN
    v_child[-1]  <- (1 - pct_fst_ages[1]) * vN * v_child[-1] / sum(v_child[-1])
    data_out[data_out$Age < 5, variable] <- v_child

  }

  # Pre-translate all UI text for plots (to avoid scope issues in nested functions)
  cat(sprintf("[GRADUATION] Pre-translating text | i18n_null=%s\n", is.null(i18n)))
  graduation_text <- translate_text("Graduation", i18n)
  age_text <- translate_text("Age", i18n)
  original_text <- translate_text("Original", i18n)
  graduated_text <- translate_text("Graduated", i18n)
  type_text <- translate_text("Type", i18n)
  variable_text <- translate_text(variable, i18n)

  cat(sprintf("[GRADUATION] Translations: graduation='%s', age='%s', original='%s', graduated='%s'\n",
              graduation_text, age_text, original_text, graduated_text))

  # Create plot comparing original vs graduated data
  plot_obj <- NULL
  tryCatch({
    # Combine original and graduated data
    original_df <- data.frame(
      Age = data_in$Age,
      Value = data_in[[variable]],
      Type = original_text
    )

    graduated_df <- data.frame(
      Age = data_out$Age,
      Value = data_out[[variable]],
      Type = graduated_text
    )

    # Check if we're converting from single ages to grouped ages
    # If so, normalize graduated values for fair visual comparison
    input_is_single <- DemoTools::is_single(data_in$Age)
    output_is_grouped <- !DemoTools::is_single(data_out$Age) &&
                        (age_out %in% c("5-year", "abridged"))

    if (input_is_single && output_is_grouped) {
      # For visual comparison, divide grouped values by typical interval width
      # This shows average per single age rather than sum
      age_intervals <- diff(c(data_out$Age, max(data_out$Age) + 5))
      graduated_df$Value <- graduated_df$Value / age_intervals
      cat(sprintf("[GRADUATION] Normalized graduated values for plot (single->grouped conversion)\n"))
    }

    # Rename columns to translated versions for hover tooltips
    names(original_df)[names(original_df) == "Age"] <- age_text
    names(original_df)[names(original_df) == "Value"] <- variable_text
    names(original_df)[names(original_df) == "Type"] <- type_text

    names(graduated_df)[names(graduated_df) == "Age"] <- age_text
    names(graduated_df)[names(graduated_df) == "Value"] <- variable_text
    names(graduated_df)[names(graduated_df) == "Type"] <- type_text

    plot_data <- rbind(original_df, graduated_df)

    # Create named vector for colors using translated keys
    color_values <- c("black", "blue")
    names(color_values) <- c(original_text, graduated_text)

    # Build plot using .data[[]] for runtime evaluation
    plot_obj <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[age_text]], y = .data[[variable_text]],
                                       color = .data[[type_text]],
                                       linetype = .data[[type_text]])) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(data = plot_data[plot_data[[type_text]] == original_text, ], size = 2) +
      ggplot2::scale_color_manual(name = type_text, values = color_values) +
      ggplot2::scale_linetype_manual(name = type_text, values = c("solid", "solid")) +
      ggplot2::labs(x = age_text, y = variable_text,
           title = paste0(graduation_text, ": ", variable_text)) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
      )

    cat(sprintf("[GRADUATION] Plot created successfully\n"))
  }, error = function(e) {
    cat(sprintf("[GRADUATION] Plot creation error: %s\n", e$message))
  })

  # Normalize data_out for download to ensure matching totals
  # This is different from plot normalization - here we preserve total sums
  data_out_normalized <- data_out

  # Check if we're converting from single ages to grouped ages
  input_is_single <- DemoTools::is_single(data_in$Age)
  output_is_grouped <- !DemoTools::is_single(data_out$Age) &&
                      (age_out %in% c("5-year", "abridged"))

  if (input_is_single && output_is_grouped) {
    # Calculate scaling factor to preserve total sum
    total_original <- sum(data_in[[variable]], na.rm = TRUE)
    total_graduated <- sum(data_out[[variable]], na.rm = TRUE)

    if (total_graduated > 0) {
      scaling_factor <- total_original / total_graduated
      data_out_normalized[[variable]] <- data_out[[variable]] * scaling_factor
      cat(sprintf("[GRADUATION] Applied normalization factor %.4f to preserve totals in downloaded data\n", scaling_factor))
    }
  }

  return(list(data_out = data_out_normalized,
              plot = plot_obj
              # arguments = f_args
              ))
}

#' @title `graduate_auto_5`
#' @description Implements the graduation with method protocol procedure for data with 5-year age groups.
#' @param dat_5 tibble or data.frame. A tibble with two columns - `Population` - or any other chosen numeric variable that comes in 5-year age groups with counts to be graduated and corresponding numeric `Age` column (lower bound of age group).
#' @param variable character. A scalar with the `variable` name which is to be graduated. Supported variable names are `Pop`, `Population`, `Exposures`, `Exp`, or `Death`.
#' @return A tibble with 2 columns - your chosen `variable` with graduated and smoothed counts and `Age`
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom rlang := !! .data
#' @importFrom DemoTools mav graduate_mono ageRatioScore
#' @return data_out. A tibble with two numeric columns - smoothed counts for the chosen `variable` and `Age` - chosen age grouping
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' fpath <- system.file("extdata", 
#' "five_hmd_spain.csv.gz", 
#' package = "ODAPbackend")
#' data_in <- read_csv(fpath) |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' graduate_auto_5(data_in, "Exposures")$Exposures
#' 

graduate_auto_5 <- function(dat_5, 
                            variable) {
  
  # separate data into kids and adults
  # figures say age 0-14, but this is only 3 ages, it will not work
  # then in text they say calculate age ratio for ages 0-24 AND then smooth only ages 0-19
  # I use the second option but there is a clear contradiction in text and 
  # I`m not sure it is exactly right
  # TR: OK, we leave this note here and can ask about it in future.
  
  kids <- dat_5 |>
    filter(.data$Age < 20)
  
  # ages 15 to Inf
  adults <- dat_5 |>
    filter(.data$Age > 14)
  
  # calculate the age ratio score before smoothing separately for kids and adults
  # Only for age score, we add one additional age to kids
  rsc_kids <- dat_5 |>
    filter(.data$Age < 24)
  
  age_rat_score_kids <- ageRatioScore(Value = rsc_kids[, variable, drop = TRUE],
                                      Age   = rsc_kids$Age) # check this
  
  # for adults use ages 15-19 : 70-74 for score calculation
  rsc_adults <- adults |>
    filter(.data$Age < 75)
  
  age_rat_score_adults <- ageRatioScore(
    Value  = rsc_adults[, variable, drop = TRUE],
    Age    = rsc_adults$Age,
    ageMin = min(rsc_adults$Age)
  )
  
  # smooth data with mav = 2 for adults
  dat5_mav_adults <- mav(
    Value = adults[, variable, drop = TRUE],
    Age   = adults$Age,
    n     = 2,
    tails = TRUE
  )
  
  # calculate the age ratio score after smoothing for adults
  age_rat_adults_2 <- tibble(!!variable := dat5_mav_adults,
                             Age = as.integer(names(dat5_mav_adults))) |>
    filter(.data$Age < 75)
  
  age_rat_score_adults_2 <-
    ageRatioScore(
      Value    = age_rat_adults_2[, variable, drop = TRUE],
      Age      = age_rat_adults_2$Age,
      ageMin   = min(age_rat_adults_2$Age)
    )
  
  # calculate the smoothing n for adults, new way
  n_choice_ad <- c(age_rat_score_adults < 4, age_rat_score_adults_2 < 4, age_rat_score_adults_2 >= 4)
  n           <- c(1, 2, 4)
  adult_n     <- max(n[n_choice_ad]) # we kkep the maximum of two
  
  # old way
  # adult_n <- tibble(unsm = age_rat_score_adults,
  #                   sm   = age_rat_score_adults_2) |>
  #   mutate(n = case_when(unsm < 4  ~ 1,
  #                        sm   < 4  ~ 2,
  #                        sm   >= 4 ~ 4)) |>
  #   pull(n)
  
  # calculate the smoothing n for kids, new way
  n_choice_kd <- c(age_rat_score_kids < 4, age_rat_score_kids >= 4)
  n           <- c(1, 2)
  kids_n      <- n[n_choice_kd]
  
  # old way  
  # kids_n <- tibble(unsm = age_rat_score_kids) |>
  #   mutate(n = ifelse(age_rat_score_kids < 4, 1, 2)) |>
  #   pull(n)
  
  # smooth kids
  data_kids   <- mav(
    Value = kids[, variable, drop = TRUE],
    Age   = kids$Age,
    n     = kids_n,
    tails = TRUE
  )
  
  # smooth adults
  data_adults <- mav(
    Value = adults[, variable, drop = TRUE],
    Age   = adults$Age,
    n     = adult_n,
    tails = TRUE
  )
  
  # combine NOTE redistribute with linear weight assumption
  # NOTE: for 5 year ages there is only 1 age group that blends 15-19
  # Age values are exactly the same, so we just keep one
  # Can be changed in future when we figure out what exactly are we supposed to do.
  data_full <- c(data_kids[-length(data_kids)], data_adults)
  
  data_out <- tibble(!!variable := data_full,
                     Age = as.integer(names(data_full)))
  
  return(data_out)
  
}

#' @title `smooth_flexible_chunk` 
#' @description Smoothes population or death counts using a variety of methods from the `DemoTools` package and paragraph five of the "Method protocol for the evaluation of census population data by age and sex".
#' @param data_in tibble or data.frame. A tibble with two numeric columns - population or death counts with supported names: `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`, and corresponding `Age` column - provided in single age intervals, 5-year age intervals, or abridged age format e.g. with ages 0, 1, 5 etc.
#' @param variable character. A scalar with the `variable` name which is to be graduated. The list of possible options includes `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`.
#' @param fine_method the `method` argument of `graduate()` function from `DemoTools` that graduates grouped data. Possible options include  `auto`, `none`, `sprague`, `beers(ord)`, `beers(mod)`, `grabill`, `pclm`, `mono`, `uniform`.
#' @param rough_method the `method` argument of `smooth_age_5()` function from `DemoTools` that smoothes populations in 5-year age groups using various methods. Possible options include `auto`, `none`, `Carrier-Farrag`, `KKN`, `Arriaga`, `United Nations`, `Strong`, `Zigzag`.
#' @param constrain_infants logical. If age 0 is a separate age class, shall we constrain its proportion within the age group 0-5 in the output? Default to `TRUE`.
#' @param u5m numeric. Under-five mortality rate. Defaults to `NULL`.
#' @param age_out character. The desired age structure of the output file. Possible options include `single` - for single years, `5-year` - for 5-year data, and `abridged` - for abridged data, e.g. 0, 1, 5, etc.
#' @param Sex character. Either `m` for males, `f` for females, or `t` for total (default). Please note that in case this parameter is not explicitly provided, the function will scan the data for the column with the corresponding name and use its levels automatically.
#' @param i18n An optional i18n object for translation.
#' @importFrom dplyr case_when mutate group_by summarize rename left_join select join_by pull
#' @importFrom tibble tibble
#' @importFrom rlang := !! sym .data
#' @importFrom DemoTools age2int is_single is_abridged graduate_uniform names2age calcAgeAbr groupAges smooth_age_5 graduate
#' @return data_out. A list with 3 elements: - `data` - tibble with two numeric columns - smoothed counts for the chosen `variable`, `Age` - chosen age grouping, and `Sex`, `figure` - a figure with corresponding data illustrating pre and post-adjustment data, `arguments` - a list of arguments used in the function.
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' fpath <- system.file("extdata", 
#' "abridged_hmd_spain.csv.gz", 
#'  package = "ODAPbackend")
#' data_in <- read_csv(fpath) |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' ex1 <- smooth_flexible_chunk(
#'   data_in,
#'   variable     = "Exposures",
#'   rough_method = "auto",
#'   fine_method  = "auto",
#'   constrain_infants = TRUE,
#'   age_out = "single",
#'   u5m     = NULL,
#'   Sex     = "t")
#' 
#' ex1$data$Exposures
#' ex1$figure$figure
#' ex1$arguments

smooth_flexible_chunk <- function(data_in,
                                  variable     = "Deaths",
                                  age_out      = c("single", "abridged", "5-year"),
                                  fine_method  = c("auto", "none", "sprague",
                                                   "beers(ord)", "beers(mod)",
                                                   "grabill", "pclm", "mono",
                                                   "uniform"),
                                  rough_method = c("auto", "none", "Carrier-Farrag",
                                                   "KKN", "Arriaga", "United Nations",
                                                   "Strong", "Zigzag"),
                                  u5m = NULL,
                                  Sex = c("t", "f", "m"),
                                  constrain_infants = TRUE,
                                  i18n = NULL) {
  
  ## f_args <- capture_args()
  
  data_orig <- data_in
  # ensure just one of each method is chosen. 
  # rough auto is compatible with a non-auto fine,
  # since we can always regroup to 5s. 
  # Likewise pclm rough is compatible with pclm fine; # UPDATE!!! pclm removed from rough methods
  # no pclm offsets in this implementation, and no explicit tail control.
  # we exclude MAV to avoid passing in special parameters.
  # coerce to lower case for friendlier arg passing
  # frough_method <- tolower(rough_method)
  rough_method <- match.arg(rough_method, c("auto", "none", "Carrier-Farrag",
                                            "KKN", "Arriaga", "United Nations",
                                            "Strong", "Zigzag"))
  
  fine_method <- tolower(fine_method)
  fine_method <- ifelse(fine_method == "beers", "beers(ord)", fine_method)
  fine_method <- match.arg(fine_method, tolower(c("auto", "none", "sprague",
                                                  "beers(ord)", "beers(mod)",
                                                  "grabill", "pclm", "mono", "uniform")))
  age_out     <- match.arg(age_out, c("single", "abridged", "5-year"))
  
  
  if ("Sex" %in% colnames(data_in)){
    Sex <- unique(data_in$Sex)
  }
  
  # Handles e.g. Total, total, t, etc
  Sex <- substr(Sex, 1, 1) |> 
    tolower()
  Sex <- match.arg(Sex, c("t", "f", "m"))
  
  # get variables
  value       <- data_in[, variable, drop = TRUE]
  age         <- data_in$Age
  has_infants <- age2int(age)[1] == 1 & age[1] == 0
  
  # # TODO
  # if(has_infants){
  #   # get prop0
  # }
  # 
  # detect incoming age categorization
  age_in      <- case_when(is_single(age)       ~ "single",
                           is_abridged(age)     ~ "abridged",
                           all(age2int(age) == 5,
                               na.rm = TRUE)    ~ "5-year",   
                           TRUE                 ~ "other")
  
  if(age_in == "single") {
    
    data1 <- data_in
    
  }
  
  #--------------------------------#
  # regularize no-standard ages    #
  #--------------------------------# 
  if(age_in == "other") {
    
    value       <- data_in[, variable, drop = TRUE]
    age         <- data_in$Age
    value1      <- graduate_uniform(Value = value,
                                    Age   = age)
    age1        <- names2age(value1)
    # If there is an infant group, we preserve it
    
    if(has_infants) {
      
      ageN       <- calcAgeAbr(age1)
      value      <- groupAges(Value = value1, 
                              Age   = age1, 
                              AgeN  = ageN)
      age        <- names2age(value)
      age_in     <- "abridged"
      
    } else {
      
      # otherwise, group to 5-year ages
      value      <- groupAges(Value = value1,
                              Age   = age1,
                              N     = 5)
      age_in     <- "5-year"
      age        <- names2age(value)
    }
    data_in <- tibble(Age = age,
                      !!variable := value)
  }
  
  # some helper objects for flexible outbound ages
  age1   <- min(age):max(age)
  ageN   <- switch(age_out,
                   "single"   = age1,
                   "5-year"   = age1 - age1 %% 5,
                   "abridged" = calcAgeAbr(age1))
  
  # -------------------------------------------------------#
  # simplest case, we do nothing to the age distribution,  #
  # but we *might* group data still...                     #
  # -------------------------------------------------------#
  if(rough_method == "none" & fine_method == "none") {
    
    if(age_out == age_in) {
      
      figure <- plot_smooth_compare(data_in = data_orig,
                                   data_out = data_in,
                                   variable = variable,
                                   i18n = i18n)
      
      return(list(data      = data_orig,
                  figure    = figure
                  ))
      
    } 
    
    if(age_out == "single") {
      
      warning("You requested no fine or rough methods,\nbut you want single age output. We assumed a uniform distribution over single ages within the age groups given.")
      
    }
    
    value     <- data_in[, variable, drop = TRUE]
    age       <- data_in$Age
    value1    <- graduate_uniform(Value = value,
                                  Age   = age)
    age1      <- names2age(value1)
    ageN      <- switch(age_out,
                        "single"   = age1,
                        "5-year"   = age1 - age1 %% 5,
                        "abridged" = calcAgeAbr(age1))
    value_out <- groupAges(Value = value1,
                           Age   = age1,
                           AgeN  = ageN)
    data_out  <- tibble(Age = names2age(value_out),
                        !!variable := value_out)
    
    figure <- plot_smooth_compare(data_in = data_orig,
                                  data_out = data_out,
                                  variable = variable,
                                  i18n = i18n)
    
    return(list(data      = data_out,
                figure    = figure
                ## arguments = f_args
                ))
    
  }
  
  # ------------------------#
  # I: Handle rough methods #
  # ------------------------#
  
  # this is a fallback data5
  data5 <- data_in |> 
    mutate(Age = .data$Age - .data$Age %% 5) |> 
    group_by(Age)  |> 
    summarize(!!variable := sum(!!sym(variable)))
  
  # (1) the case of auto everything (verify arguments to pass)
  if(rough_method == "auto") {
    
    data1 <- graduate_auto(data_in,
                           age_out  = "single",
                           variable = variable,
                           u5m      = u5m,
                           Sex      = Sex,
                           constrain_infants = constrain_infants)$data_out
    
    # regroup to 5, overrides previous one
    data5 <- data1 |> 
      mutate(Age = .data$Age - .data$Age %% 5) |> 
      group_by(Age) |> 
      summarize(!!variable := sum(!!sym(variable)))
    
  }
  
  # if the rough method was a specific one, we overwrite the value data5
  # smooth_age_5 does not have pclm option for methods argument REMOVED!
  if(rough_method %in% c("Carrier-Farrag", "KKN", "Arriaga",
                         "United Nations", "Strong", "Zigzag")) {
    
    # CHECK THIS
    data5 <- data5 |>
      mutate(!!variable := smooth_age_5(Value  = !!sym(variable),
                                        Age    = Age,
                                        method = rough_method)) |>
      mutate(Sex = Sex)
    
  }
  
  if(any(data5[, variable, drop = TRUE] < 0)) {
    neg_idx <- which(data5[, variable, drop = TRUE] < 0)
    neg_ages <- paste(data5$Age[neg_idx], collapse = ", ")
    neg_vals <- paste(round(data5[neg_idx, variable, drop = TRUE], 2), collapse = ", ")

    stop(paste0(
      "Smoothing produced negative values for '", variable, "'\n",
      "Method '", rough_method, "' produced negative values at ages: ", neg_ages, "\n",
      "Values: ", neg_vals, "\n",
      "This typically happens when data has extreme discontinuities (e.g., large jumps between age groups)\n",
      "Try using 'auto' method instead, or check your input data for irregularities"
    ))
  }
  
  # HERE
  # NOTE: can't return 5-year output yet even if desired, because
  # some graduation methods shoft between 5-year age groups, and this
  # 'light' smoothing might be desired.
  
  if(fine_method == "none" & age_out == "5-year") {
    
    figure <- plot_smooth_compare(data_in = data_orig,
                                  data_out = data5,
                                  variable = variable,
                                  i18n = i18n)
    
    return(list(data      = data5,
                figure    = figure
                # arguments = f_args

                ))
    
  }
  
  # -------------------------#
  # II: Handle fine methods  #
  # -------------------------#
  
  # I have an idea to keep the fine structure of our auto method while
  # constraining 5-year age groups to be whatever the above did. Odd,
  # I know, but maximally flexible? Here, we are presuming that (a) either
  # the data_in had no detectable 5-year heaping, or (b) any previously-selected
  # rough_method will have erased detectable smoothing. Otherwise, the auto method
  # will perturb at two levels, albeit not necessarily in the same way as if
  # data_in had both fine and rough methods as auto.
  if(fine_method == "none" & age_out %in% c("single", "abridged")) {
    
    if(age_in == "single") {
      # this is odd indeed: under what circumstances would we want to adjust 
      # 5-year age groups but NOT single ages? In this case, we are strict,
      # and we preserve all proportions inside single ages
      
      data5 <- data5 |>
        rename(age5   = Age,
               value5 = !!sym(variable))
      
      data1 <- data1 |> # data_in 
        mutate(age5 = .data$Age - .data$Age %% 5) |> 
        mutate(prop = !!sym(variable) / sum(!!sym(variable)), .by = "age5") |> 
        left_join(data5, by = join_by("age5")) |> 
        mutate(!!variable := .data$value5 * .data$prop) |>
        # mutate(!!variable := !!sym(variable) * .data$prop) |> 
        select("Age", !!sym(variable))
      
    }
    
    if(age_in != "single") {
      
      # TODO: also don't issue this warning if constrain_infants == TRUE
      # we can add that condition afterwards
      if (age_in == "5-year" & constrain_infants) {
        
        warning("We used graduate_mono() to split to single ages.\nThis (or another fine_method) was necessary because\nyou specified single-age output, but your input\ndoes not appear to be in single ages.")
        
      }
      
      fine_method <- "mono"
      # value  <- data_in |> 
      #             pull(!!sym(variable))
      # age    <- data_in |> 
      #             pull(Age)
      # value1 <- graduate_mono(Value = value,
      #                         Age = age,
      #                         OAG = TRUE)
      # age1   <- names2age(value1)
      # data1  <- tibble(Age = age1,
      #                 !!variable := value1)
    } 
    
  } 
  
  if(fine_method == "auto") {
    # Here we presume that data5 has no detectable sawtooth pattern,
    # meaning this was so in data_in or as the result of another 
    # rough_method having been applied. Otherwise, this will get picked
    # up in the auto method and taken care of with its MAV logic.
    if(rough_method != "auto") {
      
      data1 <- graduate_auto(data_in,
                             age_out  = "single",
                             variable = variable,
                             u5m      = u5m,
                             Sex      = Sex,
                             constrain_infants = constrain_infants)$data_out
    }
    
    data5 <- data5 |> 
      rename(age5   = Age,
             value5 = !!sym(variable))
    
    data1 <- data1 |> 
      mutate(age5 = .data$Age - .data$Age %% 5) |> 
      mutate(prop = !!sym(variable) / sum(!!sym(variable)), .by = "age5") |> 
      left_join(data5, by = join_by("age5")) |> 
      mutate(!!variable := .data$value5 * .data$prop) |>
      # mutate(!!variable := !!sym(variable) * .data$prop) |> # !!!!!!
      select("Age", !!sym(variable))
    
  }
  
  # THIS ONE CHECK
  if(fine_method %in% c("sprague", "beers(ord)", "beers(mod)", 
                        "grabill", "pclm", "mono", "uniform")) {
    value  <- data5 |> 
      pull(!!sym(variable))
    age    <- data5 |> 
      pull(Age)
    # THIS ONE DOES NOT WORK WITH NEGATIVES
    value1 <- graduate(Value  = value,
                       Age    = age,
                       method = fine_method,
                       OAG    = TRUE)
    age1   <- names2age(value1)
    data1  <- tibble(Age = age1,
                     !!variable := value1)
    
  }
  # we got to single ages anyway, it's pragmatic, trust me
  
  #-----------------------------#
  # III group to desired output #
  #-----------------------------#
  
  value     <- data1 |> # was data1
    pull(!!sym(variable))
  age       <- data1 |>
    pull(Age)
  
  # causes problems too if rough_method = "auto", fine_method  = "none"
  # and age_out  = "abridged"
  value_out <- groupAges(Value = value,
                         Age   = age,
                         AgeN  = ageN)
  
  age      <- names2age(value_out)
  data_out <- tibble(Age = age,
                     !!variable := value_out)
  
  # ---------------------------------------------#
  # IV handle constraining of infant proportions #
  # ---------------------------------------------#
  if(constrain_infants) { 
    
    varb <- data_in[, variable, drop = TRUE]
    Age  <- data_in$Age
    
    if(age_in == "5-year") { 
      
      if(age_out != "5-year") {
        
        if(!is.null(u5m)) {
          
          # in odd case that child mortality is given, but Sex is not specified:
          if(is.null(Sex)) {
            
            Sex <- "t"
            warning("Sex argument not given. We assumed total (Sex = 't'). We use this variable to inform splitting the infant age group.")
            
          }
          
          # we need this variable for indirect method applied
          stopifnot(Sex %in% c("f", "m", "t")) 
          
          if(variable == "Deaths") {
            
            D5 <- varb[1]
            P5 <- D5 / u5m
            
          }
          
          if(variable %in% c("Exp", "Exposures", "Pop", "Population")) {
            
            P5 <- varb[1]
            D5 <- P5 * u5m
            
          }
          
          if(Sex %in% c("f", "m")) {
            
            D0   <- lt_rule_4m0_D0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = Sex)
            
            M0   <- lt_rule_4m0_m0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = Sex)
            
            P0   <- D0 / M0
            
          } else {
            
            D0m  <- lt_rule_4m0_D0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = "m")
            
            D0f  <- lt_rule_4m0_D0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = "f")
            
            D0   <- (D0m + D0f) / 2
            
            M0m  <- lt_rule_4m0_m0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = "m")
            
            M0f  <- lt_rule_4m0_m0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = "f")
            
            M0   <- (M0m + M0f) / 2
            P0   <- D0 / M0
            
          }
          
          if(variable == "Deaths") {
            
            D1_4 <- D5 - D0
            varb <- c(D0, D1_4, varb[-1])
            Age  <- c(Age[1], 1, Age[-1])
            
          }
          
          if(variable %in% c("Exp", "Exposures", "Pop", "Population")) {
            P1_4 <- P5 - P0
            varb <- c(P0, P1_4, varb[-1])
            Age  <- c(Age[1], 1, Age[-1])
            
          }
          
          # For purposes of continued decision-making
          age_in       <- "abridged"
          fst_ages     <- varb[1:2]
          pct_fst_ages <- fst_ages / sum(fst_ages)
          
        } else {
          
          warning("Be mindful of results for the infant age group. Your output has a separate infant age group, but this was split from the input data without taking demographic knowledge into account. If you specify an under-5 mortality rate, u5m, we can do a better job.")
          
        }
        
      }
      
    }
    
    if(age_in == "abridged") {
      
      # calculate the distribution of first two ages for data_out if needed in future
      fst_ages     <- varb[1:2]
      pct_fst_ages <- fst_ages / sum(fst_ages)
      
    }
    
    if(age_in == "single") { 
      
      # calculate the distribution of first two ages for data_out if needed in future
      fst_ages     <- varb[1:5]
      fst_ages     <- c(fst_ages[1], sum(fst_ages[-1]))
      pct_fst_ages <- fst_ages / sum(fst_ages)
      
    }
    
    final_data_single <- is_single(data_out$Age)
    
    # (1) graduate_mono
    varb <- data_out[, variable, drop = TRUE]
    
    v2   <- graduate_mono(Value = varb, 
                          Age   = data_out$Age, 
                          OAG   = TRUE)
    
    age  <- as.integer(names(v2))
    
    # (2) regroup
    ageN <- switch(age_out,
                   "abridged" = calcAgeAbr(age),
                   "5-year"   = age - age %% 5,
                   "single"   = age)
    
    v3 <- groupAges(Value = v2, 
                    Age   = age, 
                    AgeN  = ageN)
    
    data_out <- tibble(!!variable := v3,
                       Age = as.integer(names(v3))) |>
      mutate(Sex = Sex)
    
    # (3) possibly constrain infants
    if(age_out %in% c("abridged", "single") & age_in %in% c("abridged", "single") & constrain_infants) {
      
      v_child      <- data_out[data_out$Age < 5, variable, drop = TRUE]
      vN           <- sum(v_child)
      v_child[1]   <- pct_fst_ages[1] * vN
      v_child[-1]  <- (1 - pct_fst_ages[1]) * vN * v_child[-1] / sum(v_child[-1])
      data_out[data_out$Age < 5, variable] <- v_child
    }
    
  }
  
  # return figure as elsewhere
  figure <- plot_smooth_compare(data_in = data_orig,
                                data_out = data_out,
                                variable = variable,
                                i18n = i18n)
  
  return(list(data      = data_out,
              figure    = figure
              # arguments = f_args

              ))
  
}

#' @title `plot_smooth_compare`
#' @description Plots the original counts and the smoothed counts over the chosen age range for ease of comparison.
#' @param data_in tibble or data.frame. A tibble with two numeric columns - population or death counts - provided in single age intervals, 5-year age intervals, or abridged age format e.g. with ages 0, 1, 5 etc, and corresponding Age `column`. 
#' @param data_out A tibble with two numeric columns - smoothed counts for the chosen variable as produced by the `smooth_flexible`, or `smooth_flexible_chunk`, and `Age` - corresponding Age column.
#' @param variable character. A scalar with the `variable` name which is to be plotted. The list of possible options includes `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`.
#' @param i18n An optional i18n object for translation.
#' @importFrom dplyr case_when mutate group_by summarize rename left_join select join_by pull
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_y_continuous scale_x_continuous theme_light theme element_text ggtitle
#' @importFrom scales pretty_breaks comma
#' @importFrom rlang := !! sym .data
#' @return list. A named list with 3 elements: `figure` - a plot of original versus adjusted data, `data_adjusted` - and `data_original`.
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' fpath <- system.file("extdata", 
#' "abridged_hmd_spain.csv.gz", 
#'  package = "ODAPbackend")
#' data_in <- read_csv(fpath, show_col_types = FALSE) |>
#'   dplyr::select(-1) |>
#'   dplyr::filter(.id == 1)
#' 
#' data_out <- smooth_flexible_chunk(
#'   data_in,
#'   variable     = "Exposures",
#'   rough_method = "auto",
#'   fine_method  = "auto",
#'   constrain_infants = TRUE,
#'   age_out      = "single",
#'   u5m          = NULL,
#'   Sex          = "t"
#' )
#'
#' results <- plot_smooth_compare(data_in,
#'                                data_out$data,
#'                                variable = "Exposures")
#' 
#' # smoothed data
#' results$data_adjusted
#' # original data
#' results$data_original
#' # figure
#' results$figure
#' 
plot_smooth_compare <- function(data_in, 
                                data_out, 
                                variable,
                                i18n = NULL) {
  
  # Translate the variable name for display if translation exists
  variable_display <- translate_text(variable, i18n)
  age_text <- translate_text("Age", i18n)
  title_text <- paste(translate_text("Original (black) and adjusted (red)", i18n), variable_display, translate_text("data", i18n))

  data_in <- data_in |> 
    mutate(
      single = is_single(.data$Age),
      AgeInt = age2int(.data$Age, OAvalue = 1),
      age_mid = if_else(.data$single, .data$Age, .data$Age + (.data$AgeInt / 2)),
      age_label = case_when(.data$Age == max(.data$Age) ~ paste0(max(.data$Age),"+"),
                            TRUE ~ paste0("[", .data$Age, ",", .data$Age + .data$AgeInt, ")")),
      plot_y = !!sym(variable) / .data$AgeInt)
  
  data_out <- data_out |> 
    mutate(
      single = is_single(.data$Age),
      AgeInt = age2int(.data$Age, OAvalue = 1),
      age_mid = if_else(.data$single, .data$Age, .data$Age + (.data$AgeInt / 2)),
      age_label = case_when(.data$Age == max(.data$Age) ~ paste0(max(.data$Age), "+"),
                            TRUE ~ paste0("[", .data$Age, ",", .data$Age + .data$AgeInt, ")")),
      plot_y = !!sym(variable) / .data$AgeInt)
  sym_variable_display <- sym("plot_y")
  sym_age_text <- sym("age_mid")

  figure <- 
  ggplot() +
    geom_line(data = data_in,  aes(x = !!sym_age_text, y = !!sym_variable_display), color = "black") +
    geom_point(data = data_in,  aes(x = !!sym_age_text, y = !!sym_variable_display), color = "black") +
    geom_line(data = data_out, aes(x = !!sym_age_text, y = !!sym_variable_display), color = "red", linewidth = 1) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks(), labels = comma) +
    theme_light() +
    theme(axis.text = element_text(color = "black"),
          plot.title = element_text(size = 12)) +
    labs(title = title_text,
         x = age_text,
         y = variable_display)
  
  
  return(lst(figure, 
             data_adjusted = data_out,
             data_original = data_in))
}










