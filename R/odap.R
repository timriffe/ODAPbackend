#' ODAP–OPAG Mortality and Population Redistribution Analysis
#'
#' This function prepares population data and mortality life table data (`nLx`) to perform age redistribution using the OPAG (Old-Age Population Age Group) method.  
#' It supports flexible input with country name/code, sex, and year filters, and handles both user-provided and WPP standard mortality data.  
#' The function outputs adjusted population estimates along with diagnostic plots comparing original vs. redistributed populations.
#'
#' @param data_in A data frame or tibble of population counts with columns including \code{age}, \code{pop}, and optionally \code{name}, \code{sex}, \code{year}, and \code{country_code}.
#' @param Age_fit Numeric vector of two ages defining the age range for fitting redistribution (default \code{c(60, 70)}).
#' @param AgeInt_fit Numeric vector of two age interval widths corresponding to \code{Age_fit} (default \code{c(10, 10)}).
#' @param Redistribute_from Numeric scalar age threshold above which population redistribution occurs (default \code{80}).
#' @param OAnew Numeric scalar indicating the new open-age group upper bound (default \code{100}).
#' @param method Character scalar specifying the redistribution method; one of \code{"mono"}, \code{"pclm"}, or \code{"uniform"} (default \code{"mono"}).
#' @param nLx Optional mortality life table data frame containing columns \code{age}, \code{nLx}, and grouping columns. If \code{NULL}, mortality data is pulled from the latest installed WPP package.
#' @param name Character vector of country names to filter by (default \code{"India"}).
#' @param country_code Numeric vector of country codes to filter by (default \code{356}).
#' @param year Numeric vector of years to filter by (default \code{1971}).
#' @param sex Character scalar indicating sex to filter by, e.g., \code{"M"} or \code{"F"} (default \code{"M"}).
#' @param i18n Optional internationalization object for translating plot labels and titles (default \code{NULL}).
#' @importFrom dplyr filter arrange across select mutate group_by reframe ungroup full_join group_nest
#' @importFrom tibble as_tibble
#' @importFrom DemoTools lt_single_mx OPAG groupAges
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_point geom_line aes scale_color_manual labs theme_minimal theme element_text
#' @importFrom tidyselect all_of any_of
#' @importFrom purrr map map2
#' @importFrom rlang .data %||% sym
#' @importFrom magrittr %>% 
#' @importFrom utils installed.packages data globalVariables
#' @export
#'
#' @details
#' The function:
#' \itemize{
#'   \item Standardizes and validates population input data.
#'   \item Automatically retrieves and filters WPP mortality data (\code{nLx}) if not supplied.
#'   \item Checks age interval consistency and aggregates mortality data if necessary.
#'   \item Applies the OPAG redistribution method per group (e.g. country, year, sex).
#'   \item Generates diagnostic plots comparing original and redistributed population counts.
#' }
#'
#' The function uses internal helpers such as \code{conditional_filter()},
#' \code{conditional_arrange()}, and \code{create_groupid()} to standardize and align data.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{data_out}}{A named list of OPAG redistribution results by group.}
#'   \item{\code{figures}}{A named list of \code{ggplot2} objects showing original vs. redistributed populations.}
#' }
#'
#' @seealso \code{\link[DemoTools]{OPAG}}, \code{\link{link[DemoTools]{lt_single_mx}}, \code{\link{\link[DemoTools]{groupAges}}
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' data_in <- tibble(
#'   age = seq(0, 100, by = 5),
#'   pop = runif(21, 10000, 100000),
#'   name = "India",
#'   country_code = 356,
#'   sex = "M",
#'   year = 1971
#' )
#'
#' res <- odap_opag(data_in, Redistribute_from = 80)
#'
#' # View redistributed data
#' res$data_out$India
#'
#' # Plot original vs redistributed population
#' print(res$figures$India)
#' }
odap_opag <- function(data_in           = NULL,
                      Age_fit           = c(60, 70),
                      AgeInt_fit        = c(10, 10),
                      Redistribute_from = 80, # highest age that is at least 10 years younger than max and divisible by 10
                      OAnew             = 100,
                      method            = c("mono", "pclm", "uniform"),
                      nLx               = NULL,
                      # we want for user to be able to choose country
                      # by name AND/OR code
                      name              = "India",
                      country_code      = 356,
                      # Here we indicate the sex and year to choose from
                      # latest wpp if needed
                      year              = 1971,
                      sex               = "M",
                      i18n              = NULL
                      ) {
  
  # chosen method
  method <- match.arg(method, c("mono", "pclm", "uniform"))


  # Helper: conditional filtering for user defined variables
  # e.g. if sex  exits and provided by user we use it in filtering
  conditional_filter <- function(df, col, values) {
    
    if(!is.null(values) && col %in% names(df)) {
      
      df %>% 
        filter(.data[[col]] %in% values)
      
    } else df
    
  }
  
  # Helper: conditional arrange for user defined variables
  conditional_arrange <- function(df, cols) {
    # Keep only columns that exist in df
    cols_exist <- intersect(cols, names(df))
    
    if (length(cols_exist) > 0) {
      df %>% arrange(across(all_of(cols_exist)))
    } else {
      df
    }
  }
  
  # ---------------------------------------------------------------------------- #
  # Part 1: This part prepares the data_in data for further analysis by:
  # introducing uniform names
  # adding group columns and id if missing
  # ---------------------------------------------------------------------------- #


  data_in        <- as_tibble(data_in)
  names(data_in) <- tolower(names(data_in))


  if(!"sex"          %in% names(data_in)) data_in$sex          <- sex
  if(!"country_code" %in% names(data_in)) data_in$country_code <- country_code %||% 1
  if(!"name"         %in% names(data_in)) data_in$name         <- name %||% "country"
  if(!"year"         %in% names(data_in)) data_in$year         <- year %||% 2020

  if(!".id"          %in% names(data_in)) {

    # Only use keys that exist in the data
    available_keys <- intersect(c("name", "sex", "year", "country_code"), names(data_in))
    data_in <- create_groupid(data_in, keys = available_keys)

  }
  
  # ---------------------------------------------------------------------------- #
  # Part 2: Here we deal with the standard data
  # first case is when the data is not provided by user
  # in this case, it is being pulled from the latest installed wpp package
  # if the data is provided by the user, then we filter corresponding info from it
  # by default we calculate nLx for chosen combination for single years
  # then if user provided data are in 5 or 10-year intervals, we group our nLx accordingly
  # if spacing is strange the warning is thrown
  # ---------------------------------------------------------------------------- #
  
  # check if nLx column is in the names of data_in
  # Note: column names were converted to lowercase at line 131, so check for "nlx"
  if("nlx" %in% names(data_in)) {

    nLx <- data_in %>%
      conditional_filter("country_code", country_code) %>%
      conditional_filter("name", name) %>%
      conditional_filter("year", year) %>%
      conditional_filter("sex", sex) %>%
      select(any_of(c("name", "country_code", "sex", "year", "age", "nlx")))
    # Rename nlx back to nLx for consistency with downstream code
    if("nlx" %in% names(nLx)) {
      names(nLx)[names(nLx) == "nlx"] <- "nLx"
    }

    }
    
  
  if(is.null(nLx)) {

    installed_wpp <- grep("^wpp\\d{4}$", rownames(installed.packages()), value = TRUE)
    
    if(length(installed_wpp) == 0) stop("No wpp package installed.")
    
    latest_wpp <- sort(installed_wpp, decreasing = TRUE)[1]
    
    if(parse_number("wpp2024") < 2022) { 
      
      warning("No single ages are availabe in wpp versions earlier that wpp2022.
              Consider updating the wpp package or change to five year solution.")
      
      }
    
    suppressPackageStartupMessages(library(latest_wpp, character.only = TRUE))
    data("mx1dt", package = latest_wpp)

    nLx <- mx1dt %>%
      as_tibble() %>%
      select(-.data$mxB) %>%
      conditional_filter("country_code", country_code) %>%
      conditional_filter("name", name) %>%
      conditional_filter("year", year) %>%
      pivot_longer(
        cols = c(.data$mxM, .data$mxF),
        names_to = "sex",
        values_to = "mx"
      ) %>%
      mutate(sex = substr(.data$sex, 3, 3)) %>%
      conditional_filter("sex", sex)
      
      group_vars <- intersect(c("name","country_code","sex","year"), names(nLx))
      
      nLx <- nLx %>%
        group_by(across(all_of(intersect(c("name", "country_code", "sex", "year"), names(.))))) %>%
      reframe(
        lt_single_mx(nMx = .data$mx, Age = .data$age), .groups = "drop") %>%
      select(.data$name, 
             .data$country_code, 
             .data$sex, 
             .data$year,
             age = .data$Age, 
             .data$AgeInt, 
             .data$nLx)
    # 
    # nLx <- nLx %>%
    #   as_tibble() %>%
    #   select(-"mxB") %>%
    #   conditional_filter("country_code", country_code) %>%
    #   conditional_filter("name", name) %>%
    #   conditional_filter("year", year) %>%
    #   pivot_longer(c("mxM", "mxF"), 
    #                names_to  = "sex", 
    #                values_to = "mx") %>%
    #   mutate(sex = substr(sex, 3, 3)) %>%
    #   conditional_filter("sex", sex) %>%
    #   group_by(across(all_of(intersect(c("name", "country_code", "sex", "year"), names(.))))) %>%
    #   reframe(lt_single_mx(nMx = .data$mx, Age = .data$age)) %>%
    #   ungroup() %>%
    #   select("name", "country_code", "sex", "year", age = "Age", "AgeInt", "nLx")
    
  } else {

    nLx <- as_tibble(nLx) %>%
      conditional_filter("country_code", country_code) %>%
      conditional_filter("name", name) %>%
      conditional_filter("year", year) %>%
      conditional_filter("sex", sex) %>%
      select(any_of(c("name", "country_code", "sex", "year", "age", "AgeInt", "nLx")))

  }
  
  # can be changed to is_single if needed
  age_diff    <- diff(sort(unique(data_in$age)))
  unique_diff <- unique(na.omit(age_diff)) # NA removed in case of strange OAG coding

  if(length(unique_diff) == 1) {

    age_spacing <- unique_diff

  } else {

    stop("Mixed or irregular age spacing: ", paste(unique_diff, collapse = ", "))

  }
  
  # --- Group nLx ages if needed --- #
  if(age_spacing %in% c(5, 10)) {


    nLx <- nLx %>%
      group_by(across(all_of(intersect(c("name", "country_code", "sex", "year"), names(.))))) %>%
      reframe({
        dat <- pick(everything())
        age_vals <- seq(0, max(dat$age), by = age_spacing)
        nLx_vals <- groupAges(Value = dat$nLx, Age = dat$age, N = age_spacing)
        tibble(age = age_vals, nLx = nLx_vals)
      })

  } else {
  }
  
 
  # ---------------------------------------------------------------------------- #
  # Part 3: Now we have user data and reference data and we can use the OPAG function
  # ---------------------------------------------------------------------------- #
  
  # we conditionally arrange both datasets to ensure that ids of Pop
  # match row by row with nLx. This makes it easier to work with data further
  data_in <- conditional_arrange(data_in, c("name", "country_code", "sex", "year", "age"))
  nLx     <- conditional_arrange(nLx, c("name", "country_code", "sex", "year", "age"))

  # Note: .id and .id_label will be added via full_join below
  # No need to manually assign here since nLx may have different row count after filtering
  
  # result <- data_in %>% 
  #   full_join(nLx) %>%
  #   group_nest(.data$.id, .data$.id_label) %>% 
  #   mutate(
  #     results = map(data, ~ {
  #       Age_vals  <- unique(.x$age)
  #       Pop_vals  <- .x$pop
  #       nLx_vals  <- .x$nLx
  #       
  #       OPAG(
  #         Pop               = Pop_vals,
  #         Age_Pop           = Age_vals,
  #         nLx               = nLx_vals,
  #         Age_nLx           = Age_vals,
  #         Age_fit           = Age_fit,
  #         AgeInt_fit        = AgeInt_fit,
  #         Redistribute_from = Redistribute_from,
  #         OAnew             = OAnew,
  #         method            = method
  #       )
  #     }),
  #     
  #     plots = map2(data, results, ~ {
  #       old <- tibble(pop = .x$pop, 
  #                     age = .x$age) %>%
  #         filter(age > Redistribute_from)
  #       
  #       new <- tibble(pop = .y$Pop_out, 
  #                     age = .y$Age_out) %>%
  #         filter(age > Redistribute_from)
  #       
  # # A figure to compare results with original
  #       ggplot() +
  #         geom_point(data = old, aes(x = age, y = pop, color = "Old"), size = 2) +
  #         geom_line( data = new, aes(x = age, y = pop, color = "New"), linewidth = 1) +
  #         scale_color_manual(name = "Population", 
  #                            values = c("Old" = "black",
  #                                       "New" = "red")) +
  #         labs(
  #           x = "Age",
  #           y = "Population"
  #         ) +
  #         theme_minimal(base_size = 14) +
  #         theme(
  #           legend.position = "bottom",
  #           plot.title = element_text(face = "bold", hjust = 0.5)
  #         )
  #     })
  #   )
  

  # Pre-translate all UI text for plots (to avoid scope issues in map2)
  title_text <- translate_text("Population Redistribution (OPAG Method)", i18n)
  age_text <- translate_text("Age", i18n)
  pop_text <- translate_text("Population", i18n)
  original_text <- translate_text("Original", i18n)
  redistributed_text <- translate_text("Redistributed", i18n)
  type_text <- translate_text("Type", i18n)


  # Join only on 'age' to attach nLx values
  # Don't join on sex/name/year/country_code because:
  # 1. Those are either grouping variables (user's data structure)
  # 2. Or WPP metadata (which mortality table was used)
  # The user selected ONE mortality table to apply to ALL groups
  result <- data_in %>%
    full_join(nLx, by = "age") %>%
    group_nest(.data$.id, .data$.id_label) %>%
    mutate(
      results = map(.data$data, ~ {
        Age_vals <- unique(.x$age)
        Pop_vals <- .x$pop
        nLx_vals <- .x$nLx
        OPAG(
          Pop = Pop_vals,
          Age_Pop = Age_vals,
          nLx = nLx_vals,
          Age_nLx = Age_vals,
          Age_fit = Age_fit,
          AgeInt_fit = AgeInt_fit,
          Redistribute_from = Redistribute_from,
          OAnew = OAnew,
          method = method
        )
      }),
      plots = map2(.data$data, .data$results, function(.x, .y) {
        old <- tibble(pop = .x$pop, age = .x$age) %>%
          filter(.data$age > Redistribute_from)
        new <- tibble(pop = .y$Pop_out, age = .y$Age_out) %>%
          filter(.data$age > Redistribute_from)

        # Note: translated text variables (title_text, age_text, etc.)
        # are captured from parent scope where i18n is available


        # Rename columns to translated versions for hover tooltips (following lifetable pattern)
        names(old)[names(old) == "age"] <- age_text
        names(old)[names(old) == "pop"] <- pop_text
        old[[type_text]] <- original_text

        names(new)[names(new) == "age"] <- age_text
        names(new)[names(new) == "pop"] <- pop_text
        new[[type_text]] <- redistributed_text


        # Create named vector for colors using translated keys
        color_values <- c("black", "red")
        names(color_values) <- c(original_text, redistributed_text)

        # Build plot using .data[[]] for runtime evaluation (NOT !!sym())
        ggplot() +
          geom_point(data = old, aes(x = .data[[age_text]], y = .data[[pop_text]], color = .data[[type_text]]), size = 2) +
          geom_line(data = new, aes(x = .data[[age_text]], y = .data[[pop_text]], color = .data[[type_text]]), linewidth = 1) +
          scale_color_manual(name = type_text, values = color_values) +
          labs(x = age_text, y = pop_text, title = title_text) +
          theme_minimal(base_size = 14) +
          theme(
            legend.position = "bottom",
            plot.title = element_text(face = "bold", hjust = 0.5)
          )
      })
    )
  
  results        <- result$results
  names(results) <- result$.id_label
  figures        <- result$plots
  names(figures) <- names(results)

  return(list(data_out  = results, 
              figures   = figures
  ))
  
}
