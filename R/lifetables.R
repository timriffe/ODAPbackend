#' lt_flexible
#' @description Wrepper for calculation of abridged-age or a single-age lifetable.
#' @param data_in a `data.frame` or `tibble` with columns `Age``, `Deaths``, and `Exposures``. Can have a `.id` column in which case the results will be generated for each group as specified by this column.
#' @param OAnew integer. Desired open age group (5-year ages only). Default `100`. If higher then rates are extrapolated.
#' @param age_out character. Indicates weather single or abridged lifetable output is desired. takes values `"single"`, `"abridged"`. Defaults to "single"
#' @param extrapFrom integer. Age from which to impute extrapolated mortality.
#' @param extrapFit integer vector. Ages to include in model fitting. Defaults to all ages > =60.
#' @param radix numeric. Lifetable radix, `l0`. Default `100000`.
#' @param extrapLaw character. If extrapolating, which parametric mortality law should be invoked? Options include `"Kannisto"`, `"Kannisto_Makeham"`, `"Makeham"`, `"Gompertz"`, `"GGompertz"`, `"Beard"`, `"Beard_Makeham"`, `"Quadratic"`. Default `"Kannisto"` if the highest age is at least 90, otherwise `"makeham"`.
#' @param SRB numeric. the sex ratio at birth (boys / girls), default `1.05`
#' @param a0rule character. Either `"ak"` (default) or `"cd"`.
#' @param axmethod character. Either `"pas"` or `"un"`.
#' @param Sex character. Either `"m"` for males, `"f"` for females. This variable should be preswent in the dataframe. If there is more than pne sex in the data, then the lifetable will be calculazted for each sex present.
#' @return A single or abridged life table of data.frame format with corresponding columns:
#' Age integer. Lower bound of abridged age class,
#' AgeInt integer. Age class widths.
#' nMx numeric. Age-specific central death rates.
#' nAx numeric. Average time spent in interval by those deceased in interval.
#' nqx numeric. Age-specific conditional death probabilities.
#' lx numeric. Lifetable survivorship
#' ndx numeric. Lifetable deaths distribution.
#' nLx numeric. Lifetable exposure.
#' Sx numeric. Survivor ratios in uniform 5-year age groups.
#' Tx numeric. Lifetable total years left to live above age x.
#' ex numeric. Age-specific remaining life expectancy.
#' .id character A group indicator for which the results will be generated. In case of missing the .id columnwill return `all`
#' @importFrom dplyr case_when reframe first
#' @importFrom DemoTools is_single lt_abridged age2int lt_abridged2single lt_single_mx lt_single2abridged is_abridged
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' library(tidyverse)
#' Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
#' 247473,223014,172260,149338,127242,105715,79614,53660,
#' 31021,16805,8000,4000,2000,1000)
#' 
#' Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
#'             1335,3257,2200,4023,2167,4578,2956,4212,
#'             2887,2351,1500,900,500,300)
#' 
#' sex <- c("f", "m")
#' 
#' Age = c(0, 1, seq(5, 100, by = 5))
#' data_in <- tibble(Age,Deaths,Exposures)
#' dt <- rpois(n = 22, lambda = 1000)
#' d1 <- rpois(n = 22, lambda = 100000)
#' 
#' data_in <- expand_grid(sex, data_in) %>% 
#'   mutate(Deaths = ifelse(sex == "f", dt, Deaths),
#'          Exposures = ifelse(sex == "f", d1, Exposures)) %>% 
#'   mutate(.id = sex) %>% 
#'    mutate(Sex = sex) %>% 
#'   dplyr::select(-sex)
#'
#' 
#' data_out <- lt_flexible(
#'   data_in = data_in
#' )
#' }
lt_flexible <- function(data_in,
                        OAnew      = 100,
                        age_out    = "single",
                        extrapFrom = 80,
                        extrapFit  = NULL,  # Default NULL, computed later
                        extrapLaw  = NULL,
                        radix      = 1e+05,
                        SRB        = 1.05,
                        Sex        = "t",
                        a0rule     = "Andreev-Kingkade",
                        axmethod   = "UN (Greville)") {
  
  data_in <- data_in |>
    mutate(Sex = substr(Sex, 1, 1),
           Sex = ifelse(Sex == "t", "b", Sex)) |>
    mutate(Mx_emp = .data$Deaths / .data$Exposures)
  
  if (!(".id" %in% colnames(data_in))) {
    data_in <- data_in |>
      mutate(.id = "all")
  }
  
  # Set `extrapFit` here, avoiding circular reference in defaults
  if (is.null(extrapFit)) {
    extrapFit <- unique(data_in$Age)[unique(data_in$Age) >= 60]
  }
  
  data_out <- data_in |>
    reframe(
      lt_flexible_chunk(data_in    = .data, 
                        Sex        = first(Sex),  
                        OAnew      = OAnew,  
                        extrapFrom = extrapFrom,
                        extrapFit  = extrapFit, 
                        radix      = radix,
                        extrapLaw  = extrapLaw,
                        SRB        = SRB,
                        a0rule     = a0rule,
                        axmethod   = axmethod
      ), .by = .data$.id
    )
  
  return(data_out)
}

# [ ] allow lx, nMx, nqx inputs
# [ ] if data go up to 75+ we can't have jumpoff at 80, the value 80 needs
#     to be dynamically determined

#' lt_flexible_chunk
#' @description Calculate an abridged-age or a single-age lifetable.
#' @param data_in a `data.frame` or `tibble` with columns `Age``, `Deaths``, and `Exposures``
#' @param OAnew integer. Desired open age group (5-year ages only). Default `100`. If higher then rates are extrapolated.
#' @param age_out character. Indicates weather single or abridged lifetable output is desired. takes values `"single"`, `"abridged"`. Defaults to "single"
#' @param extrapFrom integer. Age from which to impute extrapolated mortality.
#' @param extrapFit integer vector. Ages to include in model fitting. Defaults to all ages > =60.
#' @param radix numeric. Lifetable radix, `l0`. Default `100000`.
#' @param extrapLaw character. If extrapolating, which parametric mortality law should be invoked? Options include `"Kannisto"`, `"Kannisto_Makeham"`, `"Makeham"`, `"Gompertz"`, `"GGompertz"`, `"Beard"`, `"Beard_Makeham"`, `"Quadratic"`. Default `"Kannisto"` if the highest age is at least 90, otherwise `"makeham"`.
#' @param SRB numeric. the sex ratio at birth (boys / girls), default `1.05`
#' @param a0rule character. Either `"ak"` (default) or `"cd"`.
#' @param axmethod character. Either `"pas"` or `"un"`.
#' @param Sex character. Either `"m"` for males, `"f"` for females (default).
#' @return A single or abridged life table iof data.frame format with corresponding columns:
#' Age integer. Lower bound of abridged age class,
#' AgeInt integer. Age class widths.
#' nMx numeric. Age-specific central death rates.
#' nAx numeric. Average time spent in interval by those deceased in interval.
#' nqx numeric. Age-specific conditional death probabilities.
#' lx numeric. Lifetable survivorship
#' ndx numeric. Lifetable deaths distribution.
#' nLx numeric. Lifetable exposure.
#' Sx numeric. Survivor ratios in uniform 5-year age groups.
#' Tx numeric. Lifetable total years left to live above age x.
#' ex numeric. Age-specific remaining life expectancy.
#' @importFrom dplyr case_when case_match mutate
#' @importFrom DemoTools is_single lt_abridged age2int lt_abridged2single lt_single_mx lt_single2abridged is_abridged
#' @export
#' @examples
#' \dontrun{
#' library(tibble)
#' Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
#' 247473,223014,172260,149338,127242,105715,79614,53660,
#' 31021,16805,8000,4000,2000,1000)
#' 
#' Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
#'             1335,3257,2200,4023,2167,4578,2956,4212,
#'             2887,2351,1500,900,500,300)
#'
#'Age = c(0, 1, seq(5, 100, by = 5))
#'data_in <- tibble(Age,Deaths,Exposures, Sex = "f")
#' data_out <- 
#'   lt_flexible(data_in,
#'               OAnew     = 100,
#'               age_out   = "single",  
#'               extrapFrom = 80,
#'               extrapFit = Age[Age >= 60], 
#'               radix     = 1e+05,
#'               extrapLaw = NULL,
#'               SRB       = 1.05,
#'               a0rule    = "ak",
#'               axmethod  = "un",
#'               Sex       = first(Sex))
#' }
lt_flexible_chunk <- function(
    data_in,
    Sex,
    OAnew      = 100,
    age_out    = "single", 
    extrapFrom = 80,
    extrapFit  = NULL,  # No circular references
    extrapLaw  = NULL,
    radix      = 1e+05,
    SRB        = 1.05,
    a0rule     = "Andreev-Kingkade",
    axmethod   = "UN (Greville)") {
  
  
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
  if (age_in == "problem"){
    stop(
      "Age groups appear irregular. Only single or standard abrdiged ages are supported now"
    )
  }
  
  if (age_in == "abridged" & age_out == "abridged") {
    
    # TR possibly more args to pass, or different arg management;
    # for instance, construct a completed list of args
    # and execute the function using do.call()
    AgeInt <- age2int(data_in$Age)
    
    data_out <- lt_abridged(Deaths     = data_in$Deaths,
                            Exposures  = data_in$Exposures,
                            Age        = data_in$Age,
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
    
    data_out <- lt_abridged2single(Deaths     = data_in$Deaths,
                                   Exposures  = data_in$Exposures,
                                   Age        = data_in$Age,
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
    # useful in case we use lt_single_mx()
    # Don't check age_out yet here, because the abridge function requires a
    # precalculated lifetable, see below
    # TR same story; arg management should be complete and strategic
    data_out <- lt_single_mx(nMx        = data_in$Mx_emp,
                             Age        = data_in$Age,
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
  
  if (age_in == "single" & age_out == "abridged") {
    
    data_out <- lt_single2abridged(lx  = data_out$lx,
                                   nLx = data_out$nLx,
                                   ex  = data_out$ex,
                                   Age = data_out$Age)
    
  }
  
  Sex <- case_match(Sex,
                    "m" ~ "Males",
                    "f" ~ "Females",
                    "b" ~ "Total")
  # 
  # # Add sex column to output
  data_out <- data_out |>
    mutate(Sex = Sex, .before = 1)
  
  return(data_out)
}

#' lt_plot
#' @description Plot wrapper, created lifetable plot list previously returned by `lt_flexible()`
#' @details This function should be run after `lt_flexible()`, so that you can pass both `data_in` and `data_out`. There is no fallback at this time to generate `data_out` on the fly if missing. We need to pass `extrapFrom` at this time indicate the jump-off in the plot. In the future this may be detected or passed in another way.
#' @importFrom dplyr group_split mutate group_nest full_join
#' @importFrom purrr map2 map set_names
#' @importFrom tidyr pivot_wider
#' @export
#' @param data_in a `data.frame` or `tibble` with columns `Age`, `Deaths`, and `Exposures` and `.id`
#' @param data_out `tibble` as produced by `lt_flexible()`
#' @param extrapFrom integer. Age from which to impute extrapolated mortality.
lt_plot <- function(data_in,
                    data_out, 
                    extrapFrom = extrapFrom){
  
  if (!(".id" %in% colnames(data_out))){
    data_out <- data_out |>
      mutate(.id = "all")
  }
  
  if (!(".id" %in% colnames(data_in))){
    data_in <- data_in |>
      mutate(.id = "all")
  }
  
  id1 <- unique(data_in$.id)
  
  plots <- data_out |>
    group_split(.data$.id, .keep = TRUE)|>
    map(~ plot_lifetable(.x)) %>% 
    set_names(id1)
  
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
    full_join(d_out) |> 
    pivot_wider(names_from  = .data$type,
                values_from = .data$data) |>
    mutate(new = map2(.x = d_out,
                      .y = d_in, 
                      ~ plot_compare_rates(data_in  = .y,
                                           data_out = .x,
                                           extrapFrom = extrapFrom)))
  
  plots$nMx <- data$new
  
  return(plots)
}


# TODO: lt_summary() should create a table of useful summary statistics from the lifetable:
# e0, e65, 45q15, sd, IQR (from LifeIneq), mode (use Paola Vasquez' shorthand formula rather than spline method)
# DONE We have to think exactly what measures do we want here. The carcass is ready, changing it is a matter of minutes.
# TODO: TO finish the roxxygen description after we decide which functions we keep and on the output

#' lt_summary
#' @description Creates a table of useful summary statistics from the lifetable.
#' @param data_out a data.frame. The data.frame output of the lt_flexible function.
#' @return A list with 2 data.frames containing the information on the following usefull statistics
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
#' \dontrun{
#' Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
#'                247473,223014,172260,149338,127242,105715,79614,53660,
#'                31021,16805,8000,4000,2000,1000)
#' 
#' Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
#'             1335,3257,2200,4023,2167,4578,2956,4212,
#'             2887,2351,1500,900,500,300)
#'
#' Age = c(0, 1, seq(5, 100, by = 5))
#' data_out <- 
#'   lt_flexible(data_in,
#'               OAnew     = 100,
#'               age_out   = "single",  
#'               extrapFrom = 80,
#'               extrapFit = NULL, 
#'               radix     = 1e+05,
#'               extrapLaw = NULL,
#'               SRB       = 1.05,
#'               a0rule    = "ak",
#'               axmethod  = "un")
#'               
#' lt_summary(data_out)
#' }
#' 
lt_summary <- function(data_out){
  
  if (!(".id" %in% colnames(data_out))){
    data_out <- data_out |>
      mutate(.id = "all")
  }
  
  out <- data_out %>%
    reframe(lt_summary_chunk(data_out = .data), .by = .data$.id)
  
  return(out)
}


# TODO: add column called 'label'
lt_summary_chunk <- function(data_out) { 
  
  e0  <- data_out$ex[data_out$Age == 0]
  e65 <- data_out$ex[data_out$Age == 65]
  S <- ineq_sd(age = data_out$Age, 
               dx = data_out$ndx, 
               ex = data_out$ex, 
               ax = data_out$nAx)
  

  IQR        <- ineq_iqr(age = data_out$Age, 
                         lx = data_out$lx, lower = 0.25,  upper = 0.75)

  # TR: corrected this; you were using ndx before, we only need for age 0...
  median_age <- ineq_quantile(age = data_out$Age, lx = data_out$lx, quantile = 0.5)[1]
  mod_age    <- modal_age(data_out)

  # survived to age 45 AND died at age 60
  # q15_45 <- (1 - data_out$nqx[data_out$Age == 45]) * data_out$nqx[data_out$Age == 60]
  # TR: nope 45q15 means probability of dying before age 60, conditional
  # on survival to age 15, often denoted as
  # ${}_{45}q_{15}$, i.e. where 45 is N = interval width
  # I switched it to 20 - 65 so 45q20

  l20     <- data_out$lx[data_out$Age == 20]
  l65     <- data_out$lx[data_out$Age == 65]
  p_20_65 <- l65 / l20
  q_20_65 <- 1 - p_20_65
  
  out <- tibble(e0, 
                Median = median_age,
                Mode = mod_age,
                e65, 
                sd0 = S[1], 
                sd10 = S[11],
                IQR,
                q_20_65) |> 
    pivot_longer(everything(),names_to = "measure", values_to = "value") |> 
    mutate(label = c("e_0","Median","Mode","e_65","\\sigma_0","\\sigma_{10}","IQR","{}_{45}q_{20}"),
           message = c("life expectancy at birth",
                       "median age at death",
                       "modal age at death",
                       "remaining life expectancy at age 65",
                       "lifespan variation calculated as standard deviation of age at death",
                       "standard deviation of remaining lifespan conditional on survival to age 10",
                       "interquartile range of age at death distribution",
                       "conditional probability of death between ages 20 and 65"))
  
  
  
  return(out)
  }


# helper function that calculates the modal age at death
#  Formula for mode from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3000019/ Appendix A, A2

modal_age <- function(data_out) {
  
  # we have to remove the data from the first age, since many 
  # deaths are registered at this age.
  Age  <- data_out$Age[-1]
  dx   <- data_out$ndx[-1]
  
  ind  <- which.max(dx)
  
  dx0  <- dx[ind] 
  dx1  <- dx[ind - 1] 
  dx2  <- dx[ind + 1] 
  agem <- Age[ind]
  
  agem + ((dx0 - dx1) / (dx0 - dx1 + dx0 - dx2))
  
  }

