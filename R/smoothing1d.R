#' One–Dimensional Smoothing Wrapper for Age or Time Series
#' \code{smooth1d()} provides a unified interface for smoothing one–dimensional demographic or time–series data (e.g., age schedules or time trends).
#' The function supports multiple smoothing engines including \code{supsmu}, \code{lowess}, \code{loess}, cubic splines, and GAM-based methods.
#' Optional transformations (log, sqrt, logit) can be applied prior to smoothing.
#' The function automatically handles grouped data via a \code{.id} column.
#' @param data_in A data.frame or tibble containing the input data.
#' @param units Character string. One of:
#'   \itemize{
#'     \item \code{"count"}
#'     \item \code{"rate"}
#'     \item \code{"proportion"}
#'   }
#' @param X Character string. Name of the independent variable (typically \code{"Age"} or \code{"Time"}).
#' @param Y Character string. Name of the dependent variable.
#' @param scale Optional transformation applied before smoothing:
#'   \itemize{
#'     \item \code{"log"}
#'     \item \code{"sqrt"}
#'     \item \code{"logit"}
#'     \item \code{"none"}
#'   }
#' @param method Smoothing engine:
#'   \itemize{
#'     \item \code{"supsmu"}
#'     \item \code{"lowess"}
#'     \item \code{"loess"}
#'     \item \code{"cubicspline"}
#'     \item \code{"gam-tp"} (thin plate regression spline)
#'     \item \code{"gam-ps"} (P-spline)
#'   }
#' @param smoothing_par Numeric smoothing parameter. Interpretation depends on method.
#' @param xout Optional vector of evaluation points. Defaults to unique values of \code{X}.
#' @importFrom dplyr mutate group_split bind_rows case_when
#' @importFrom dplyr as_tibble
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot geom_point geom_line theme theme_bw
#' @importFrom ggplot2 element_text scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 scale_color_brewer labs
#' @importFrom scales pretty_breaks
#' @importFrom stats supsmu loess smooth.spline predict qlogis
#' @importFrom mgcv gam s
#' @importFrom signal interp1
#' @importFrom magrittr %>%
#' @importFrom forcats as_factor 
#' @return A named list with:
#'   \itemize{
#'     \item \code{result} — Tibble containing smoothed values
#'     \item \code{plot} — ggplot object comparing raw and smoothed values
#'   }
#'
#' @details
#' If the input data does not contain a \code{.id} column, a default group \code{"all"} is created.
#' Weights default to 1 if not supplied.
#' Transformations are applied before smoothing but not automatically back-transformed.
#'
#' @examples
#' # Example 1: Age smoothing of mortality rates (log scale)
#' library(readr)
#'
#' fpath <- system.file(
#'   "extdata",
#'   "single_hmd_spain.csv.gz",
#'   package = "ODAPbackend"
#' )
#'
#' data_in <- read_csv(fpath, col_select = c(-1), show_col_types = FALSE)
#' data_in$nMx <- data_in$Deaths / data_in$Exposures
#' data_in <- data_in[data_in$.id == 1, ]
#' names(data_in) <- c(".id", "Time", "Age", "Sex",
#'                     "Deaths", "Exposures", "nMx")
#'
#' z <- smooth1d(
#'   data_in = data_in,
#'   units   = "rate",
#'   X       = "Age",
#'   Y       = "nMx",
#'   method  = "supsmu",
#'   scale   = "log"
#' )
#'
#' z$result
#' z$plot
#'
#' # Example 2: Time smoothing of counts
#' df <- data.frame(
#'   Time   = seq(1950, 2015, by = 5),
#'   Deaths = c(403564.9, 426012.0, 466215.9, 523753.8,
#'              560874.1, 545608.3, 555335.9, 594584.6,
#'              608425.3, 638167.3, 655438.6, 689386.4,
#'              740519.0, 804439.3)
#' )
#'
#' z <- smooth1d(
#'   data_in = df,
#'   units   = "count",
#'   X       = "Time",
#'   Y       = "Deaths",
#'   method  = "supsmu",
#'   scale   = "none"
#' )
#'
#' z$result
#' z$plot
#'
#' @export
#' 

smooth1d <- function(data_in,
                     units  = c("count", "rate", "proportion"),
                     X      = c("Age", "Time"),
                     Y      = c("Exposures"),
                     scale  = c("log", "sqrt", "logit", "none"),
                     method = c("supsmu", "lowess", "loess", "cubicspline", "gam-tp", "gam-ps"),
                     smoothing_par = 1,
                     xout          = unique(data_in[[X]])) {
  
  units  <- match.arg(units)
  scale  <- match.arg(scale)
  X      <- match.arg(X)
  
  
  # Ensure '.id' column exists
  if (!(".id" %in% names(data_in))) {
    data_in$.id <- "all"
  }
  
  if (!"weights" %in% colnames(data_in)){
    data_in <- data_in |> 
      mutate(weights = 1)
  }
  
  # here transformation occurs
  # we give warning if it is strange
  
  if(units == "counts" & scale != "none") {
    
    warning("You are applying transformation to counts which is strange")
    
  }
  
  split_data <- data_in %>%
    mutate(
      {{ Y }} := {
        
        y <- !!sym(Y)
        
        switch(scale,
               log   = log(y),
               sqrt  = sqrt(y),
               logit = qlogis(y),
               none  = y
        )
      }, .by = ".id"
    ) %>%
    group_split(.data$`.id`)
  
  
  for_one <- function(data) {
    
    x <- data[[X]]
    y <- data[[Y]]
    w <- data[["weights"]]
    
    if (method == "supsmu") {
      
      smoothing_par <- case_when(smoothing_par < 0 ~ 0,
                                 smoothing_par > 10 ~ 10,
                                 TRUE ~ smoothing_par)
      
      fit <- supsmu(x = x, 
                    y = y, 
                    wt = w,
                    span = "cv", 
                    bass = smoothing_par)
      # ---------------------------------------------------------------#
      # TR: would be better to follow this function with interpolate() #
      # instead of fixing this option?                                 #
      # Then again maybe keep it cuz most methods have predict()?      #
      # ---------------------------------------------------------------# 
      pred <- interp1(fit$x, 
                      fit$y, 
                      xi = xout, 
                      method = 'pchip', 
                      extrap = TRUE)
      data_out <- tibble(!!X := xout, !!Y := pred)
    }
    
    if (method %in% c("lowess", "loess")) {
      
      degree_val <- ifelse(method == "lowess", 1, 2)
      
      fit  <- loess(y ~ x, 
                    weights = w, 
                    span = smoothing_par, 
                    degree = degree_val, 
                    control = loess.control(surface = "direct"))
      pred <- predict(fit, newdata = data.frame(x = xout))
      data_out <- tibble(!!X := xout, !!Y := pred)
    }
    
    if (method == "cubicspline") {
      if (is.numeric(smoothing_par)){
        fit  <- smooth.spline(x = x, 
                              y = y, 
                              w = w,
                              spar = smoothing_par)
      } else {
        fit  <- smooth.spline(x = x, 
                              y = y, 
                              w = w,
                              cv = FALSE)
      }
      
      pred <- predict(fit, x = data.frame(x = xout))
      data_out <- data.frame(pred)
      names(data_out) <- c(X, Y)
      data_out <- as_tibble(data_out)
    }
    
    # covers gam-tp and gam-ps
    if (grepl(method, pattern = "gam")) {
      
      smoothing_par <- ifelse(is.numeric(smoothing_par), smoothing_par, 1)
      lil_data <- tibble(x = x, y = y, w = w)
      bs <- gsub(method, pattern = "gam-", replacement = "")
      fit <- gam(y ~ + s(x, m = smoothing_par),
                 bs = bs,
                       data = lil_data, 
                       weights = w, 
                       method = "REML", 
                       select = TRUE, 
                       family = "gaussian")
      pred <- predict(fit, newdata = data.frame(x = xout))
      data_out <- tibble(!!X := xout, !!Y := pred)
    }
    
    return(data_out)
  }
  
  res <- split_data %>% 
    map(~ for_one(.x)) %>% 
    bind_rows(.id = ".id")
  
  compare <- split_data %>% 
    bind_rows(.id = ".id")
  
  compare$.id  <- as_factor(compare$.id)
  res$.id      <- as_factor(res$.id)
  
  fig <- ggplot() +
    geom_point(
      aes(x     = pull(compare, !!sym("X")), 
          y     = pull(compare, !!sym("Y")), 
          color = pull(compare, ".id")
      )) +
    geom_line(
      aes(x     = pull(res, !!sym("X")), 
          y     = pull(res, !!sym("Y")), 
          color = pull(res, ".id")
      )) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.text = element_text(color = "black")
    ) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks()) +
    labs(
      x = NULL,   
      y = NULL    
    ) + 
    scale_color_brewer(
      palette = "Set1",
      name = "Group"
    ) + 
    theme_bw()
  
  return(list(result = res,
              plot   = fig))
  
}