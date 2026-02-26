
# check_heaping_general <- function(data, y) { 
#   
#   tbl <- tibble("level" = c(c("Highly accurate", "Fairly accurate", 
#                             "Approximate", "Rough","Very rough")),
#                 color = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"))
#   
#   has_single <- is_single(data$Age)
#   
#   if(has_single) {
#     
#     # go with 
#     bachi <- check_heaping_bachi(subset(data, select = y, drop = TRUE),  
#                                  data$Age, 
#                                  OAG = TRUE, 
#                                  method = "pasex")
#     
#     myers <- check_heaping_myers(subset(data, select = y, drop = TRUE), data$Age)
#     
#     res <- tibble("age scale" = "single",
#                   "method" = c("bachi", "myers"),
#                       "result" = c(bachi, myers)) |> 
#       mutate("level" = cut(.data$result, 
#                   breaks = c(0,1,2,5,15,80),
#                   labels = c("Highly accurate", "Fairly accurate", "Approximate",
#                              "Rough","Very rough")) |> as.character()) |> 
#       left_join(tbl, by = join_by("level"))
#     
#   } 
#     tblr <- tibble("method" = "roughness",
#                    "level"  = c(1.03))
#   # 5-year methods
#   roughness <- check_heaping_roughness(subset(data, select = y, drop = TRUE), data$Age, ageMin = 30)
#   sawtooth  <- check_heaping_sawtooth( subset(data, select = y, drop = TRUE), data$Age, ageMin = 30)
# 
#   r <- cut(roughness, breaks = c(0,.1,.2,.5,1.5,10),
#            labels = c("Highly accurate", "Fairly accurate", "Approximate",
#                       "Rough","Very rough")) |> as.character()
#   
#   s <- cut(sawtooth, breaks = c(1,1.03,1.1,1.5,3,10),
#            labels = c("Highly accurate", "Fairly accurate", "Approximate",
#                       "Rough","Very rough")) |> as.character()
#   
#     res5 <- tibble('age scale' = "5-year",
#                    "method"    = c("roughness", "sawtooth"),
#                    "result"    = c(roughness, sawtooth),
#                    "level"     = c(r, s)) |> 
#       left_join(tbl, by = join_by("level"))
#     
#   if (has_single){
#     
#     out <- bind_rows(res, res5)
#     
#   } else {
#     
#     out <- res5
#     
#   }
#   
#   return(out)
#   
# }

# add argument rate or proportion, units
# and add log, sqrt, logit
# Scale
# check_heaping_general <- function(data, y) { 
#   
#   if(!(".id" %in% colnames(data))) {
#     data <- data |>
#       mutate(.id = "all")
#   }
#   
#   labels <- c(
#     "Highly accurate",
#     "Fairly accurate",
#     "Approximate",
#     "Rough",
#     "Very rough"
#   )
#   
#   tbl <- tibble("level" = labels,
#                 color = c("#ffffb2", 
#                           "#fecc5c", 
#                           "#fd8d3c", 
#                           "#f03b20", 
#                           "#bd0026"))
#   
#   has_single <- is_single(unique(data$Age))
#   
#   if(has_single) {
#     
#     res <- data |>
#       select("Age", all_of(y), ".id") |>
#       group_nest(.data$`.id`) |>
#       mutate("bachi" = map(data, ~ 
#                              check_heaping_bachi(
#                                Value  = pull(.x, !!sym(y)),
#                                Age    = .x$Age,
#                                OAG    = TRUE,
#                                method = "pasex")),
#              "myers" = map(data, ~
#                              check_heaping_myers(
#                                Value = pull(.x, !!sym(y)),
#                                Age   = .x$Age)),
#              "age scale" = "single") |>
#       select(".id", "bachi", "myers", "age scale") |>
#       unnest(c("bachi", "myers")) |>
#       pivot_longer(-c(".id", "age scale"),
#                    names_to  = "method",
#                    values_to = "result") |>
#       group_by(.data$`.id`) |> 
#       mutate("level" = cut(.data$result, 
#                            breaks = c(0, 1, 2, 5, 15, 80),
#                            labels = labels),
#              "level" = as.character(.data$level)) |> 
#       left_join(tbl, by = c("level"))
#     
#   }
#   
#   # 5-year methods
#   classify <- function(x, method, breaks, labels) {
#     tibble(
#       result = x,
#       method = method,
#       level  = cut(x, breaks = breaks, labels = labels)
#     )
#   }
#   
#   res5 <- data |>
#     select("Age", all_of(y), ".id") |>
#     group_nest(.data$`.id`) |>
#     mutate(
#       "roughness" = map(data, ~ 
#                           check_heaping_roughness(
#                             Value  = pull(.x, !!sym(y)),
#                             Age    = .x$Age,
#                             ageMin = 30)),
#       "sawtooth" = map(data, ~ 
#                          check_heaping_sawtooth(
#                            Value  = pull(.x, !!sym(y)),
#                            Age    = .x$Age,
#                            ageMin = 30)),
#       "res" = map2(.x = .data$roughness, 
#                    .y = .data$sawtooth, ~ 
#                      bind_rows(
#                        classify(
#                          x      = .x,
#                          method = "roughness",
#                          breaks = c(0, .1, .2, .5, 1.5, 10),
#                          labels = labels),
#                        classify(
#                          x      = .y,
#                          method = "sawtooth",
#                          breaks = c(1, 1.03, 1.1, 1.5, 3, 10),
#                          labels = labels))),
#       "age scale" = "5-year") |>
#     select(-c("data", "roughness", "sawtooth")) |>
#     unnest("res") |> 
#     left_join(tbl, by = join_by("level"))
#   
#   if(has_single) {
#     
#     out <- bind_rows(res, res5)
#     
#   } else {
#     
#     out <- res5
#     
#   }
#   
#   return(out)
#   
# }


#' General age-heaping diagnostics
#' @description
#' Computes age-heaping and age-irregularity indices from age-specific counts, rates, or proportions. The set of diagnostics depends on the age scale: digit-preference indices are used for single-year ages, while irregularity indices are used for grouped ages.
#' @param data A data frame containing at least `Age` and the variable `y`. If `.id` is present, diagnostics are computed by group; otherwise all observations are treated as one group.
#' @param y Character string giving the name of the age-specific variable.
#' @param units Type of input data. One of `"count"`, `"rate"`, or `"proportion"`.
#' @importFrom tibble tibble
#' @importFrom dplyr full_join mutate left_join join_by bind_rows
#' @importFrom DemoTools check_heaping_bachi check_heaping_myers check_heaping_roughness check_heaping_sawtooth
#' @details
#' * Single-year ages: Bachi and Myers digit-preference indices.
#' * Grouped ages: roughness and sawtooth irregularity indices.
#' For `"count"` data, values are internally converted to proportions.
#' For `"rate"` and `"proportion"` data, values are normalized to sum to one
#' to ensure comparability across ages.
#' @return
#' A data frame with one row per index and group, containing:
#' \itemize{
#'   \item `method` – diagnostic index name
#'   \item `result` – numeric value
#'   \item `level` – qualitative data-quality category
#'   \item `color` – associated color code
#'   \item `age scale` – `"single"` or `"5-year"`
#'   \item `.id` – group identifier
#' }
#' @seealso
#' \code{\link{check_heaping_bachi}},
#' \code{\link{check_heaping_myers}},
#' \code{\link{check_heaping_roughness}},
#' \code{\link{check_heaping_sawtooth}}
#' @references
#' Bachi (1951); Myers (1940); United Nations (1955).
#' @examples
#' library(readr)
#' fpath       <- system.file("extdata", "single_hmd_spain.csv.gz", package = "ODAPbackend")
#' data_in     <- read_csv(fpath, col_select = c(-1), show_col_types = FALSE)
#' data_in$nMx <- data_in$Deaths / data_in$Exposures
#' data_in     <- data_in[data_in$.id == 1, ]
#' check_heaping_general(data = data_in, y = "Deaths", units = "count")
#' check_heaping_general(data = data_in, y = "nMx", units = "rate")
#' @export
#' 

check_heaping_general <- function(data, y, units = c("count", "rate", "proportion")) { 
  
  units <- match.arg(units)
  
  stopifnot(is.character(y), length(y) == 1)
  stopifnot(all(c("Age", y) %in% names(data)))
  
  # Ensure '.id' column exists
  if (!(".id" %in% names(data))) {
    data$.id <- "all"
  }
  
  # keep only columns we need
  data <- data |>
    select(c('.id', "Age", all_of(y)))
  
  # Perform checks
  # Split data by '.id', apply checks, and combine results
  split_data <- split(data, data$.id)
  labels     <- c("Highly accurate", "Fairly accurate", "Approximate", "Rough", "Very rough")
  color      <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")
  tbl        <- tibble("level" = labels,
                       "color" = color)
  
  check_one <- function(d = split_data) {      
    
    Y          <- d[[y]] 
    Age        <- d$Age
    has_single <- is_single(Age)
    
    if (units != "count") {
      
      Y <- Y / sum(Y)
      
    }
    
    roughness <- check_heaping_roughness(
      Value  = Y,
      Age    = Age,
      ageMin = 30)
    
    sawtooth <- check_heaping_sawtooth(
      Value  = Y,
      Age    = Age,
      ageMin = 30)
    
    roughness <- tibble("method" = "roughness",
                        "result" = roughness) |>
      mutate("level" = cut(.data$result,
                           breaks = c(0, .1, .2, .5, 1.5, 10),
                           labels = labels),
             "level" = as.character(.data$level))
    
    sawtooth <- tibble("method" = "sawtooth",
                       "result" = sawtooth) |>
      mutate("level" = cut(.data$result,
                           breaks = c(1, 1.03, 1.1, 1.5, 3, 10),
                           labels = labels),
             "level" = as.character(.data$level))
    
    out5 <- roughness |>
      full_join(sawtooth, by = join_by("method", "result", "level")) |>
      mutate("age scale" = "5-year") |> 
      left_join(tbl, by = c("level"))
    
    if(has_single) {
      
      bachi <- check_heaping_bachi(
        Value  = Y,
        Age    = Age,
        OAG    = TRUE,
        method = "pasex")
      
      myers <- check_heaping_myers(
        Value = Y,
        Age   = Age)
      
      out <- tibble("method"    = c("bachi", "myers"),
                    "result"    = c(bachi, myers),
                    "age scale" = "single") |>
        mutate("level" = cut(.data$result,
                             breaks = c(0, 1, 2, 5, 15, 80),
                             labels = labels),
               "level" = as.character(.data$level)) |> 
        left_join(tbl, by = c("level"))
      
      out <- bind_rows(out, out5)
      
    } else {
      
      out <- out5
      
    }
    
    if(units %in% c("rate", "proportion")) { 
      
      out <- out[!out$method == "roughness", ]
      
    }
    
    return(out)
    
  }
  
  res <-
    do.call(rbind, lapply(names(split_data), \(id) {
      cbind(check_one(split_data[[id]]), .id = id)
    }))
  
  return(res)
  
}



#' @title `check_heaping_user`
#' @description Check the age heaping for 5 or 1 year data, but this time give user control over minimum and maximum evaluation age.
#' @param data data.frame. User file from the read_data command with the minimum data on Exposures, Death and Age. Data ca be both in 5 and 1 year age intervals
#' @param y chracter.Variable name for which the heaping should be checked `Deaths` or `Exposures`
#' @param ageMin numeric.The minimum age from which to do the heaping evaluation 
#' @param ageMax numeric.The maximum age from which to do the heaping evaluation 
#' @return A data.frame with 2 columns `method` - the method used for age heaping evaluation and `result` - the resulting heaping measure
#' @importFrom stringr str_detect 
#' @importFrom DemoTools check_heaping_roughness check_heaping_bachi check_heaping_myers check_heaping_sawtooth
#' @export
#' @examples
#' \dontrun{
#' check_heaping_general(
#'     data = data,
#'     y = "Exposures")
#' }
#' 

check_heaping_user <- function(data, y, ageMin, ageMax) { 
  
  if(is_single(data$Age)) { 
    
    # go with 
    bachi <- check_heaping_bachi(subset(data, select = y, drop = TRUE),  
                                 data$Age, 
                                 OAG = TRUE, 
                                 ageMin = ageMin,
                                 ageMax = ageMax,
                                 method = "orig")
    
    myers <- check_heaping_myers(subset(data, select = y, drop = TRUE),  
                                 data$Age,
                                 ageMin = ageMin,
                                 ageMax = ageMax)
    
    res <- data.frame(method = c("bachi", "myers"),
                      result = c(bachi, myers))
    
  } else { 
    
    roughness <- check_heaping_roughness(subset(data, select = y, drop = TRUE), 
                                         data$Age, 
                                         ageMin = ageMin,
                                         ageMax = ageMax)
    
    sawtooth  <- check_heaping_sawtooth( subset(data, select = y, drop = TRUE), 
                                         data$Age,
                                         ageMin = ageMin,
                                         ageMax = ageMax)
    
    res <- data.frame(method = c("roughness", "sawtooth"),
                      result = c(roughness, sawtooth))
    
  }
  
  return(res)
  
}
