#' @title `lt_flexible`
#' @description Wrapper for calculation of abridged-age or single-age life table.
#' @param data_in data.frame or tibble. Should contain numeric columns `Age`, `Deaths`, and `Exposures`, or `nMx` or any of the following life table functions: `nqx`, `ndx`, `lx`. Can have a `.id` column in which case the results will be generated for each group as specified by this column.
#' @param OAnew integer. Desired open age group (5-year ages only). Default to `100`. If higher, then rates will be extrapolated.
#' @param age_out character. Indicates whether single or abridged life table output is desired. Takes values `single`, or `abridged`. Defaults to `single`.
#' @param extrapFrom integer. Age from which to impute extrapolated mortality.
#' @param extrapFit integer vector. Ages to include in model fitting. Defaults to all ages >= 60.
#' @param radix numeric. Life table radix, `l(0)`. Defaults to `100000`.
#' @param extrapLaw character. If extrapolating, which parametric mortality law should be invoked? Options include `"Kannisto"`, `"Kannisto_Makeham"`, `"Makeham"`, `"Gompertz"`, `"GGompertz"`, `"Beard"`, `"Beard_Makeham"`, `"Quadratic"`. Defaults to `Kannisto` if the highest age is at least 90, otherwise o `Makeham`.
#' @param SRB numeric. The sex ratio at birth (boys/girls). Defaults to `1.05`.
#' @param a0rule character. Rule for `a(x)` calculation Either `ak` (default) or `cd`.
#' @param axmethod character. Method used for `a(x)` calculation. Either `pas` or `un`.
#' @param Sex character. Either `m` for males, `f` for females or `t` for total. This variable defaults to `t`. If there is more than one sex in the data, then the life table will be calculated for each sex.
#' @param by_args character. A vector of columns should be also included in the output. These columns are usually ones that are used for `.id` construction. Defaults to `NULL`. It is important to not include `Sex` in this vector.
#' @importFrom dplyr mutate filter group_modify first bind_rows group_by ungroup .data case_match arrange
#' @importFrom purrr map_lgl
#' @importFrom tidyselect all_of
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @importFrom DemoTools age2int lt_id_d_q lt_id_l_q
#' @importFrom rlang set_names .data
#' @importFrom future plan multisession
#' @importFrom furrr future_map_dfr
#' @importFrom parallelly availableCores
#' @return A list with 3 elements: `data_out` - single or abridged life table of data.frame format with corresponding columns:
#' Age integer. Lower bound of abridged age class,
#' AgeInt integer. Age class widths.
#' nMx numeric. Age-specific central death rates.
#' nAx numeric. Average time spent in the interval by those deceased.
#' nqx numeric. Age-specific conditional death probabilities.
#' lx numeric. Lifetable survivorship
#' ndx numeric. Lifetable deaths distribution.
#' nLx numeric. Lifetable exposure.
#' Sx numeric. Survivor ratios in uniform 5-year age groups.
#' Tx numeric. Lifetable total years left to live above age x.
#' ex numeric. Age-specific remaining life expectancy.
#' .id character A group indicator for which the results will be generated. In case of missing the .id column will return `all`
#' Sex - corresponding sex
#' by_args - chosen additional arguments
#' `arguments` - a list of arguments used in fitting `lt_flexible` function, `arguments2` - a list of arguments used for `lt_flexible_chunk` fitting. 
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' library(future) # ? why this one?
#' # single age data
#' fpath <- system.file("extdata", 
#' "single_hmd_spain.csv.gz", 
#' package = "ODAPbackend")
#' data_in <- read_csv(fpath) |>
#'   dplyr::select(-1)
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,  # Default NULL, computed later
#'   extrapLaw  = NULL,
#'   radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)"
#' )$data_out
#' 

lt_flexible <- function(data_in,
                        OAnew      = 100,
                        age_out    = "single",
                        extrapFrom = 80,
                        extrapFit  = NULL,  # Default NULL, computed later
                        extrapLaw  = NULL,
                        radix      = 1e+05,
                        SRB        = 1.05,
                        a0rule     = "Andreev-Kingkade",
                        axmethod   = "UN (Greville)",
                        Sex = "t",
                        by_args = NULL) {
  
  if (!"Sex" %in% colnames(data_in)){
    data_in$Sex <- Sex
  }
  
  data_in <- data_in |>
    mutate(Sex = substr(.data$Sex, 1, 1),
           Sex = ifelse(.data$Sex == "t", "b", .data$Sex),
           Sex = tolower(.data$Sex))
  
  # match potential names. In case raw data is supplied
  nms <- case_match(
    names(data_in),
    c("Deaths", "deaths") ~ "Deaths",
    c("Population", "population",
      "Exposures", "Exp") ~ "Exposures",
    c("nMx", "mx", "Mx")  ~ "nMx",
    c("lx", "nlx")        ~ "lx",
    c("dx", "ndx")        ~ "ndx",
    c("qx", "nqx")        ~ "nqx",
    c("px", "npx")        ~ "npx",
    .default = names(data_in)
  )
  
  # fix names
  names(data_in) <- nms
  
  if (!(".id" %in% colnames(data_in))) {
    data_in <- data_in |>
      mutate(.id = "all")
  }
  
  # Set extrapFit here, avoiding circular reference in defaults
  if (is.null(extrapFit)) {
    extrapFit <- unique(data_in$Age)[unique(data_in$Age) >= 60]
  }
  
  # by_args <- names(data_in)[!names(data_in) %in% c("Age", "Deaths", "Exposures",
  #                                                   "Mx_emp", "Rates", "AgeInt")]
  
  # simplest case - build from existing nMx
  if("nMx" %in% names(data_in)) {
    
    data_in <- data_in |> 
      mutate(Mx_emp = .data$nMx)
    
  } 
  
  # if both deaths and Exposures are present calculate nMx and build LT
  if ((!"nMx" %in% names(data_in)) & (all(c("Deaths", "Exposures") %in% names(data_in)))) { 
    
    data_in <- data_in |>
      mutate(Mx_emp = .data$Deaths / .data$Exposures)
    
  }
  
  # from ndx - decrement
  if(("ndx" %in% names(data_in)) & (!"Mx_emp" %in% names(data_in)) & (!"nqx" %in% names(data_in))) {
    
    data_in <-  data_in |> 
      group_by(.data$.id) |>
      mutate(nqx = lt_id_d_q(.data$ndx))
    
  } 
  
  # from survaval function
  if(("lx" %in% names(data_in)) & (!"Mx_emp" %in% names(data_in)) & (!"nqx" %in% names(data_in))) {
    
    data_in <-  data_in |> 
      group_by(.data$.id) |>
      mutate(nqx = lt_id_l_q(.data$lx))
  }
  
  ## future::plan(future::multisession, workers = parallelly::availableCores() - 3)


  data_out <-
    data_in |>
    ungroup() |> 
    arrange(.data$.id, .data$Age) |> 
    # Map over them in parallel and row-bind results:
    group_by(.data$.id) |> 
    group_modify(
      ~lt_flexible_chunk(.x, 
                        Sex        = NULL,  
                        OAnew      = OAnew,  
                        extrapFrom = extrapFrom,
                        extrapFit  = extrapFit, 
                        radix      = radix,
                        extrapLaw  = extrapLaw,
                        SRB        = SRB,
                        a0rule     = a0rule,
                        axmethod   = axmethod,
                        age_out    = age_out),
      .keep = TRUE
    ) |> 
    ## group_by(.data$.id, .add = TRUE) %>%
    ## group_split() %>%
    ## furrr::future_map_dfr(
    ##   ~ lt_flexible_chunk(
    ##     .x,
    ##     Sex        = NULL,
    ##     OAnew      = OAnew,
    ##     extrapFrom = extrapFrom,
    ##     extrapFit  = extrapFit,
    ##     radix      = radix,
    ##     extrapLaw  = extrapLaw,
    ##     SRB        = SRB,
    ##     a0rule     = a0rule,
    ##     axmethod   = axmethod,
    ##     age_out    = age_out
    ##   )
    ## ) %>%
    ungroup()  |> 
    as_tibble()
  
  return(list(data_out = data_out))
  
}

#' @title `lt_flexible_chunk`
#' @description Calculate an abridged or a single-age life table.
#' @param data_in_chunk a `data.frame` or `tibble`. 3 numeric columns `Age`, `Deaths`, and `Exposures`, or `nMx` or any of the following life table functions: `nqx`, `npx`, `ndx`, `lx`. Can optionally have columns `Sex` and `.id` in which case the table will be calculated for each level in these columns. Can also have an additional column.
#' @param OAnew integer. Desired open age group (5-year ages only). Default `100`. If higher then rates are extrapolated.
#' @param age_out character. Indicates whether single or abridged life table output is desired. takes values `single`, `abridged`. Defaults to `single`.
#' @param extrapFrom integer. Age from which to impute extrapolated mortality. Defaults to `NULL`.
#' @param extrapFit integer vector. Ages to include in model fitting. Defaults to all ages >= 60.
#' @param radix numeric. Life table radix, `l(0)`. Default `100000`.
#' @param extrapLaw character. If extrapolating, which parametric mortality law should be invoked? Options include `Kannisto`, `Kannisto_Makeham`, `Makeham`, `Gompertz`, `GGompertz`, `Beard`, `Beard_Makeham`, `Quadratic`. Defaults to `Kannisto` if the highest age is at least 90, otherwise to `Makeham`.
#' @param SRB numeric. the sex ratio at birth (boys/girls). Defaults to `1.05`.
#' @param a0rule character. An a(0) calculation rule. Either `ak` (default) or `cd`.
#' @param axmethod character. A method used for a(0) calculation. Either `pas` or `un`.
#' @param Sex character. Either `m` for males, `f` for females, or `t` for total (default).
#' @param age_out character. Indicates whether single or abridged life table output is desired. Takes values `single`, or `abridged`. Defaults to `single`.
#' @return A list with two elements: A single or abridged life table of data.frame format with corresponding columns:
#' Age integer. Lower bound of abridged age class,
#' AgeInt integer. Age class widths.
#' nMx numeric. Age-specific central death rates.
#' nAx numeric. Average time spent in the interval by those deceased.
#' nqx numeric. Age-specific conditional death probabilities.
#' lx numeric. Lifetable survivorship.
#' ndx numeric. Lifetable deaths distribution.
#' nLx numeric. Lifetable exposure.
#' Sx numeric. Survivor ratios in uniform 5-year age groups.
#' Tx numeric. Lifetable total years left to live above age x.
#' ex numeric. Age-specific remaining life expectancy.
#' Sex character. Sex.
#' @importFrom dplyr case_when case_match mutate
#' @importFrom DemoTools is_single lt_abridged age2int lt_abridged2single lt_single_mx lt_single_qx lt_single2abridged is_abridged lt_id_d_q lt_id_l_q
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' fpath <- system.file("extdata", 
#' "single_hmd_spain.csv.gz", 
#' package = "ODAPbackend")
#' data_in <- read_csv(fpath) |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)|> 
#'   # This step happens in lt_flexible...
#'   mutate(Mx_emp = Deaths / Exposures,
#'   Sex = substr(Sex,1,1) |> tolower())
#' 
#' data_out <-
#'   lt_flexible_chunk(data_in,
#'                     OAnew     = 100,
#'                    age_out   = "abridged",
#'                    extrapFrom = 80,
#'                    # extrapFit = Age[Age >= 60],
#'                    radix     = 1e+05,
#'                     extrapLaw = NULL,
#'                     SRB       = 1.05,
#'                    a0rule    = "ak",
#'                    axmethod  = "un")
#' 
#' data_out$data_out
#' 
 
lt_flexible_chunk <- function(
    data_in_chunk,
    Sex = NULL,
    OAnew      = 100,
    age_out    = "single", 
    extrapFrom = 80,
    extrapFit  = NULL,  # No circular references
    extrapLaw  = NULL,
    radix      = 1e+05,
    SRB        = 1.05,
    a0rule     = "Andreev-Kingkade",
    axmethod   = "UN (Greville)") {
  
  if (is.null(Sex)){
    Sex <- data_in_chunk[["Sex"]][1]
  }
  
  Age    <- data_in_chunk$Age
  AgeInt <- age2int(Age, OAvalue = 5)
  useMx_emp <- any(grepl("Mx_emp", colnames(data_in_chunk)))

  # TR: note if dx or lx were given, then direct conversion to nqx
  # already happened in lt_flexible, which calls this function
  if (useMx_emp) {
    Mx_emp <- data_in_chunk[["Mx_emp"]]
    nqx <- NULL
  } 
  
  if (!useMx_emp){
    nqx    <-  data_in_chunk[["nqx"]]
    Mx_emp <- NULL
  }
  
  a0rule <- case_match(a0rule,
                       "Andreev-Kingkade"  ~ "ak",
                       "Coale-Demeny"      ~ "cd",
                       .default = a0rule)
  
  axmethod <- case_match(axmethod,
                         "UN (Greville)" ~ "un",
                         "PASEX"         ~ "pas",
                         .default = axmethod)
  
  age_in <- case_when(is_single(Age)   ~ "single",
                      is_abridged(Age) ~ "abridged",
                      TRUE             ~ "problem")
  
  # TR: this can become the checker function I guess
  if (age_in == "problem") {
    stop(
      "Age groups appear irregular. Only single or standard abrdiged ages are supported now"
    )
  }
  
  if (age_in == "abridged" & age_out == "abridged") {
    
    # TR possibly more args to pass, or different arg management;
    # for instance, construct a completed list of args
    # and execute the function using do.call()
    
    data_out <- lt_abridged(nMx        = Mx_emp,
                            nqx        = nqx,
                            Age        = Age,
                            AgeInt     = AgeInt,
                            OAnew      = OAnew,
                            extrapFrom = extrapFrom,
                            extrapFit  = extrapFit,
                            radix      = radix,
                            extrapLaw  = extrapLaw,
                            SRB        = SRB,
                            a0rule     = a0rule,
                            axmethod   = axmethod,
                            Sex        = Sex)
  }
  
  # Compute `data_out` based on age conditions
  if (age_in == "abridged" & age_out == "single") {
    
    data_out <- lt_abridged2single(nMx        = Mx_emp,
                                   nqx        = nqx,
                                   Age        = Age,
                                   OAnew      = OAnew,  
                                   extrapFrom = extrapFrom,
                                   extrapFit  = extrapFit,
                                   radix      = radix,
                                   extrapLaw  = extrapLaw,
                                   SRB        = SRB,
                                   a0rule     = a0rule,
                                   axmethod   = axmethod,
                                   Sex        = Sex)
  }
  
  if (age_in == "single") {
    
    if (useMx_emp) {
      
      # useful in case we use lt_single_mx()
      # Don't check age_out yet here, because the abridge function requires a
      # precalculated lifetable, see below
      # TR same story; arg management should be complete and strategic
      data_out <- lt_single_mx(nMx        = Mx_emp,
                               Age        = Age,
                               OAnew      = OAnew,
                               extrapFrom = extrapFrom,
                               extrapFit  = extrapFit, # should we change it here too to 1 year intervals?
                               extrapLaw  = extrapLaw,
                               radix      = radix,
                               SRB        = SRB,
                               a0rule     = a0rule,
                               axmethod   = axmethod,
                               Sex        = Sex)
      
    } 
    
    if (!useMx_emp) {
      
      data_out <- lt_single_qx(
        nqx        = nqx,
        Age        = Age,
        OAnew      = OAnew,
        extrapFrom = extrapFrom,
        extrapFit  = extrapFit, # should we change it here too to 1 year intervals?
        extrapLaw  = extrapLaw,
        radix      = radix,
        SRB        = SRB,
        a0rule     = a0rule,
        axmethod   = axmethod,
        Sex        = Sex)
    }
  }
  
  if(age_in == "single" & age_out == "abridged") {
    
    data_out <- lt_single2abridged(lx  = data_out$lx,
                                   nLx = data_out$nLx,
                                   ex  = data_out$ex,
                                   Age = data_out$Age)
    
  }
  
  Sex <- case_match(Sex,
                    "m" ~ "Males",
                    "f" ~ "Females",
                    "b" ~ "Total",
                    "t" ~ "Total")
  
  # Add sex column to output
  data_out <- data_out |>
    mutate(Sex = Sex, .before = 1)
  
  return(data_out)
  
}

#' @title `lt_plot`
#' @description Plot wrapper, creates life table plot list returned by `lt_flexible()`
#' @details This function should be run after `lt_flexible()`, so that you can pass both `data_in` and `data_out`. There is no fallback at this time to generate `data_out` on the fly if missing. `extrapFrom` should be passed at this time to indicate the jump-off in the plot. In the future, this may be detected or passed in another way.
#' @importFrom dplyr group_split mutate group_nest full_join
#' @importFrom purrr map_lgl map2 map set_names
#' @importFrom tidyr unnest pivot_wider 
#' @param data_in a `data.frame` or `tibble` with columns `Age`, `Deaths`, and `Exposures` and `.id`
#' @param data_out `tibble` as produced by `lt_flexible()`
#' @param extrapFrom integer. Age from which to impute extrapolated mortality.
#' @param i18n An optional i18n object for translation.
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' fpath <- system.file("extdata", 
#' "single_hmd_spain.csv.gz", 
#' package = "ODAPbackend")
#' data_in <- read_csv(fpath) |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,  # Default NULL, computed later
#'   extrapLaw  = NULL,
#'  radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)"
#' )
#' 
#' ex1 <-  lt_plot(
#'   data_in = data_in,
#'   data_out = data_out$data_out,
#'   extrapFrom = 80)
#'   
#' ex1$`1`$nMx
#' ex1$`1`$lx
#' ex1$`1`$ndx
#' ex1$`1`$nqx
#' ex1$nMx
#' 
lt_plot <- function(data_in,
                    data_out, 
                    extrapFrom = extrapFrom,
                    i18n = NULL){
  

  ## future::plan(future::multisession, workers = parallelly::availableCores() - 3)


  if (!(".id" %in% colnames(data_in))){
    data_in <- data_in |>
      mutate(.id = "all")
  }
  
  id1 <- unique(data_in$.id)
  
  # library(tictoc)
  # tic()
  plots <- data_out |>
    group_split(.data$.id, .keep = TRUE) |>
    ## furrr::future_map(~ plot_lifetable(.x)) |>
    map(~ plot_lifetable(.x, i18n = i18n)) |>
    set_names(id1)
  # toc()
  
  # sorry JC, forgot this!
  d_in <-  data_in |>
    mutate(type = "d_in") |>
    mutate(id = .data$.id) |>
    group_nest(.data$.id, .data$type)
  
  d_out <- data_out |>
    mutate(type = "d_out")|>
    mutate(id = .data$.id) |>
    group_nest(.data$.id, .data$type)
  
  data <- d_in |>
    full_join(d_out, 
              by = c(".id", "type", "data")) |> 
    pivot_wider(names_from  = .data$type,
                values_from = .data$data) |>
    mutate(new = map2(.x = d_out,
                      .y = d_in, 
                      ~ plot_compare_rates(data_in  = .y,
                                           data_out = .x,
                                           extrapFrom = extrapFrom,
                                           i18n = i18n)))
  
  plots$nMx <- data$new
  
  return(plots)
  
}

# TODO: lt_summary() should create a table of useful summary statistics from the life table:
# e0, e65, 45q15, sd, IQR (from LifeIneq), mode (use Paola Vasquez' shorthand formula rather than spline method)
# DONE We have to think exactly what measures do we want here. The carcass is ready, changing it is a matter of minutes.
# TODO: TO finish the roxxygen description after we decide which functions we keep and on the output

#' @title `lt_summary`
#' @description Creates a table of useful summary statistics from the life table.
#' @param data_out a data.frame or tibble. The data.frame output of the `lt_flexible` function.
#' @param i18n An optional i18n object for translation.
#' @return A  data.frame containing the information on the following useful life table statistics:
#' e0 - life expectancy at birth
#' e65 - life expectancy at age 65
#' `S[1]` - The standard deviation in age at death from birth
#' `S[11]` - The standard deviation in age at death from age 10
#' IQR1 - interquartile range survivorship age from a life table
#' IQR2 - interquartile range survivorship age from a life table
#' IQR3 - interquartile range survivorship age from a life table
#' mod_age - modal age at death
#' q15_45 - probability that the person ages 54 will die at age 60
#' @importFrom tibble lst tibble
#' @importFrom LifeIneq ineq_sd ineq_iqr ineq_quantile 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything mutate reframe
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' fpath <- system.file("extdata", 
#' "single_hmd_spain.csv.gz", 
#' package = "ODAPbackend")
#' data_in <- read_csv(fpath) |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,  # Default NULL, computed later
#'   extrapLaw  = NULL,
#'   radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)"
#' )
#' 
#' lt_summary(data_out$data_out)
#' 
lt_summary <- function(data_out, i18n = NULL){
  
  if (!(".id" %in% colnames(data_out))){
    data_out <- data_out |>
      mutate(.id = "all")
  }
  
  out <- data_out |>
    reframe(lt_summary_chunk(data_out = .data, i18n = i18n), 
            .by = .data$.id)
  
  return(out)
}

# TODO: add column called 'label'
#' @title `lt_summary_chunk`
#' @description Creates a table of useful summary statistics from the lifetable.
#' @param data_out a data.frame or tibble. The data.frame output of the `lt_flexible` function.
#' @param i18n An optional i18n object for translation.
#' @return A  data.frame containing the information on the following useful lifetable statistics:
#' e0 - life expectancy at birth
#' e65 - life expectancy at age 65
#' `S[1]` - The standard deviation in age at death from birth
#' `S[11]` - The standard deviation in age at death from age 10
#' IQR1 - interquartile range survivorship age from a lifetable
#' IQR2 - interquartile range survivorship age from a lifetable
#' IQR3 - interquartile range survivorship age from a lifetable
#' mod_age - modal age at death
#' q15_45 - probability that the person ages 54 will die at age 60
#' @importFrom tibble lst tibble
#' @importFrom LifeIneq ineq_sd ineq_iqr ineq_quantile 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything mutate reframe
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' fpath <- system.file("extdata", 
#' "single_hmd_spain.csv.gz", 
#' package = "ODAPbackend")
#' data_in <- read_csv(fpath) |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,  # Default NULL, computed later
#'   extrapLaw  = NULL,
#'   radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)"
#' )
#' 
#' lt_summary_chunk(data_out$data_out)
#' 
lt_summary_chunk <- function(data_out, i18n = NULL) {
  
  e0  <- data_out$ex[data_out$Age == 0]
  e65 <- data_out$ex[data_out$Age == 65]
  S   <- ineq_sd(age = data_out$Age,
                 dx  = data_out$ndx,
                 ex  = data_out$ex,
                 ax  = data_out$nAx,
                 check = FALSE)
  
  IQR        <- ineq_iqr(age   = data_out$Age, 
                         lx    = data_out$lx, 
                         lower = 0.25,  
                         upper = 0.75)
  median_age <- ineq_quantile(age      = data_out$Age, 
                              lx       = data_out$lx, 
                              quantile = 0.5)[1]
  mod_age    <- modal_age(data_out)  
  l20        <- data_out$lx[data_out$Age == 20]
  l65        <- data_out$lx[data_out$Age == 65]
  p_20_65    <- l65 / l20
  q_20_65    <- 1 - p_20_65
  
  label_vec <- c("e_0",
                 translate_text("Median", i18n),
                 translate_text("Mode", i18n),
                 "e_65",
                 "\\sigma_0",
                 "\\sigma_{10}",
                 "IQR",
                 "{}_{45}q_{20}")

  message_vec <- c(translate_text("Life Expectancy at Birth", i18n),
                   translate_text("Median Age at Death", i18n),
                   translate_text("Modal Age at Death", i18n),
                   translate_text("Remaining Life Expectancy at Age 65", i18n),
                   translate_text("Lifespan Variation Calculated as Standard Deviation of Age at Death", i18n),
                   translate_text("Standard Deviation of Remaining Lifespan Conditional on Survival to Age 10", i18n),
                   translate_text("Interquartile Range of Age at Death Distribution", i18n),
                   translate_text("Conditional Probability of Death Between Ages 20 and 65", i18n))

  out <- tibble(e0,
                Median = median_age,
                Mode   = mod_age,
                e65,
                sd0  = S[1],
                sd10 = S[11],
                IQR,
                q_20_65) |>
    pivot_longer(everything(), names_to = "measure", values_to = "value") |>
    mutate(label = label_vec,
           message = message_vec)
  
  # Note from Jorge C: pleassseee let's leave the order as is. If we change this order
  # then I have to change the order (not adding any other columns or anything) in the
  # app_reports.R file from the front end. Long story short, this is hardcoded to be
  # the positions of the columns because of translation of the column names changing
  # depending on the language.
  out <- out[c("label", "value", "message")]

  # Translate column names (except .id)
  names(out)[names(out) == "measure"] <- translate_text("Measure", i18n)
  names(out)[names(out) == "value"] <- translate_text("Value", i18n)
  names(out)[names(out) == "label"] <- translate_text("Label", i18n)
  names(out)[names(out) == "message"] <- translate_text("Message", i18n)

  return(out)
  
}

# helper function that calculates the modal age at death
#  Formula for mode from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3000019/ Appendix A, A2

modal_age <- function(data_out) {
  
  # we have to remove the data from the first age, since many deaths are registered at this age.
  Age      <- data_out$Age[-1]
  dx       <- data_out$ndx[-1]
  
  ind  <- which.max(dx)
  
  dx0  <- dx[ind] 
  dx1  <- dx[ind - 1] 
  dx2  <- dx[ind + 1] 
  agem <- Age[ind]
  
  agem + ((dx0 - dx1) / (dx0 - dx1 + dx0 - dx2))
  
}

#' @title `lt_check`
#' @description Creates a plot of `e(0)` values from Human Mortality Database (HMD) and compare it with those obtained by user.
#' @param data_out a data.frame or tibble. The data.frame output of the `lt_flexible` function.
#' @return A  figure where the users `e(0)` values would be indicated in color and HMD values would be black:
#' @importFrom tibble lst tibble
#' @importFrom dplyr group_by select summarise group_by
#' @importFrom readr read_csv
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggplot geom_point theme_minimal aes theme element_blank element_text scale_x_log10 scale_y_continuous scale_color_discrete labs
#' @importFrom stats na.omit
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
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,  # Default NULL, computed later
#'   extrapLaw  = NULL,
#'   radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)"
#' )
#' 
#' lt_check(data_out$data_out)
#' 

lt_check <- function(data_out) {
  # read hmd data
   fpath <- system.file("extdata", 
   "hmd_qx_ex.csv.gz", 
   package = "ODAPbackend")

  data_hmd <- read_csv(fpath, show_col_types = FALSE)
  
  # how values were calculated
  # zz <- zz |>
  #   group_by(.id, Year, Sex) |>
  #   summarise(q0 = qx[Age == 0],
  #             e0 = ex[Age == 0],
  #             q45_60 = 1 - prod(1 - qx[Age %in% c(45:60)])) |>
  #   ungroup() |>
  #   dplyr::select(-c(.id, Year, Sex))
  
  # This is our data_out
  data_user <- data_out |>
    group_by(.data$.id, .data$Sex) |>
    summarise(q0 = .data$nqx[.data$Age == 0],
              e0 = .data$ex[.data$Age == 0],
              q45_60 = 1 - 
                prod(1 - 
                       .data$nqx[.data$Age 
                                 %in% c(45:60)])) |>
    ungroup() |>
    select(-c(.data$Sex))
  
  a <- ggplot() +
    geom_point(data = na.omit(data_hmd), 
               aes(x = .data$q0,
                   y = .data$e0), 
               alpha = 0.3) +
    geom_point(data = data_user,
               aes(
                 x = .data$q0,
                 y = .data$e0,
                 color = as.factor(.data$.id)
               ),
               size = 2) +
    theme_minimal() +
    labs(title = "Relationship between q0 and e0", 
         x = "q0 (Infant Mortality Rate)", 
         y = "e0 (Life Expectancy at Birth)") +
    scale_color_discrete(name = ".id column levels") +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_x_log10() +
    theme(
      legend.position = "bottom",
      legend.title    = element_text(face = "bold"),
      axis.text       = element_text(color = "black")
    )
  
  b <- ggplot() +
    geom_point(data = na.omit(data_hmd), 
               aes(x = .data$q45_60, 
                   y = .data$e0), 
               alpha = 0.3) +
    geom_point(data = data_user,
               aes(
                 x = .data$q45_60,
                 y = .data$e0,
                 color = as.factor(.data$.id)
               ),
               size = 2) +
    theme_minimal() +
    labs(title = "Relationship between q0 and 45q60", 
         x = "q0 (Infant Mortality Rate)", y = "45q60 (Probability of survivaving to 60\n conditional on surviving to 45)") +
    scale_color_discrete(name = ".id column levels") +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_x_log10() +
    theme(
      legend.position = "bottom",
      legend.title    = element_text(face = "bold"),
      axis.text       = element_text(color = "black")
    )
  
  fig <- plot_grid(a, b, nrow = 2)
  
  return(fig)
  
}
