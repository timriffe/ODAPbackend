#' @title `plot_compare_rates`
#' @description Makes a line graph of the log10 transformed empirical mortality rate (Mx) data and fitted data from a chosen age. Can work with .id argument indicating groups 
#' @param data_in tibble or data.frame. Should contain the empirical numeric Mx value to be plotted and numeric `Age` values.
#' @param data_out tibble or data.frame. Modeled numeric Mx value to be plotted and numeric Age values.
#' @param extrapFrom numeric. `extrapFrom` is an age interval from which the life table was extrapolated.
#' @param i18n translation parameter for UI frontend. You can ignore this.
#' @return A line graph with a black line corresponding to empirical Mx data and a red line corresponding to modeled `Mx` data from the chosen extrapFrom value for each group specified by `.id` column. 
#' @importFrom ggplot2 ggplot geom_line scale_x_continuous scale_y_log10 theme_light geom_vline labs theme element_text
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr filter mutate pull
#' @importFrom rlang .data
#' @export
#' @examples
#' Exposures <- c(100958, 466275, 624134, 559559, 446736, 370653, 301862,
#'                249409, 247473, 223014, 172260, 149338, 127242, 105715,
#'                79614,  53660,  31021,  16805,  8000,   4000,   2000,
#'                1000)
#' 
#' Deaths <- c(8674, 1592, 618,  411,  755,  1098, 1100, 1357,
#'             1335, 3257, 2200, 4023, 2167, 4578, 2956, 4212,
#'             2887, 2351, 1500, 900,  500,  300)
#' 
#' sex     <- c("f")
#' Age     <- c(0, 1, seq(5, 100, by = 5))
#' data_in <- data.frame(Age, Deaths, Exposures)
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,
#'   # Default NULL, computed later
#'   extrapLaw  = NULL,
#'   radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)",
#'   Sex = "t"
#' )$data_out
#' 
#' plot_compare_rates(data_in, data_out, 80)
#' 
plot_compare_rates <- function(data_in, # raw mx to plot
                               data_out, # the data from lt
                               extrapFrom,
                               i18n = NULL) {
  
  ages_between_text <- translate_text("Ages between", i18n)
  age_text <- translate_text("Age", i18n)
  age_text_value <- translate_text("Age_value", i18n)
  nMx_text <- translate_text("nMx", i18n)
  age_specific_mortality_text <- translate_text("Age-specific Mortality", i18n)
  log_nMx_text <- translate_text("nMx (log scale)", i18n)
  comparison_text <- translate_text("Comparison of empirical nMx and lifetable nmx values", i18n)
  vertical_line_text <- translate_text("Vertical line indicates extrapolation jump-off age\nred dashed line indicates empirical rates,\nwhich differ from lifetable rates after the jump-off age", i18n)

  # plot the results
  input_single <- is_single(unique(data_in$Age))
  
  data_in_plot <-
    data_in |>
    # TODO: Shall we call it Age-specific Mortality?
    mutate(
      `Age-specific Mortality` = round(.data$Deaths / .data$Exposures, 8),
      AgeInt                   = age2int(.data$Age, OAG = FALSE),
      single                   = is_single(.data$Age),
      `Age Mid`                = if_else(single, .data$Age, .data$Age + (.data$AgeInt / 2)),
      age_label                = case_when(
        .data$Age == max(.data$Age) ~ paste0(max(.data$Age), "+"),
        TRUE ~ paste0("[", .data$Age, ",", .data$Age + .data$AgeInt, ")")
      )
    )
  
  if(!(".id" %in% colnames(data_in))) { 
    
    data_in_plot <-  data_in_plot |>
      mutate(.id = "all")
    
  }
  
  if (!(".id" %in% colnames(data_out))){
    data_out <- data_out |>
      mutate(.id = "all")
  }
  
  id <- unique(pull(data_in_plot, .data$.id))
  
  data_out_plot <- data_out |>
    mutate(
      AgeInt    = age2int(.data$Age, OAG = FALSE),
      single    = is_single(.data$Age),
      `Age Mid` = if_else(.data$single, .data$Age, .data$Age + (.data$AgeInt / 2)),
      age_plot  = if_else(.data$single, .data$Age, .data$Age + (.data$AgeInt / 2)),
      age_label = case_when(
        .data$Age == max(.data$Age) ~ paste0(max(.data$Age), "+"),
        TRUE ~ paste0(ages_between_text, " [", .data$Age, ",", .data$Age + .data$AgeInt, ")")
      ),
      nMx = round(.data$nMx, 8)
    )

  names(data_out_plot)[names(data_out_plot) == "Age Mid"] <- age_text_value
  names(data_out_plot)[names(data_out_plot) == "nMx"] <- nMx_text

  names(data_in_plot)[names(data_in_plot) == "Age Mid"] <- age_text_value
  names(data_in_plot)[names(data_in_plot) == "Age-specific Mortality"] <- age_specific_mortality_text

  sym_age_mid <- sym(age_text_value)
  sym_nMx <- sym(nMx_text)
  sym_age_specific_mortality <- sym(age_specific_mortality_text)

  figure <-
    ggplot() +
    geom_line(data = data_out_plot, aes(x = !!sym_age_mid, y = !!sym_nMx), linewidth = 0.8) +
    geom_line(
      data = filter(data_in_plot, .data$Age >= min(extrapFrom, max(data_out$Age))),
      aes(
        x = !!sym_age_mid,
        y = !!sym_age_specific_mortality
      ),
      lty = 2,
      col = "red",
      linewidth = 1
    ) +
    facet_wrap(~ .id) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_log10() +
    theme_light() +
    geom_vline(xintercept = extrapFrom, lty = 2) +
    labs(
      x = age_text,
      y = log_nMx_text,
      title = paste(comparison_text, id, sep = ", .id = "),
      subtitle = vertical_line_text
    ) +
    theme(
      axis.text = element_text(color = "black"),
      plot.subtitle = element_text(size = 12, color = "black")
    )

  
  # return a list with data and figure
  return(lst(
    nMx_plot = figure,
    nMx_plot_data = data_out_plot |>  
      select(!!sym_age_mid, "age_label", !!sym_nMx) |> 
      mutate(.id = id)
  ))
}

#' @title `plot_lifetable`
#' @description Creates a list of plots of life table functions. Can work with groups identified by `.id` argument.
#' @param data_out tibble or data.frame. Modeled life table generated by the 'lt_flexible` function.
#' @param i18n An optional i18n object for translation.
#' @return A ggplot2 line graph with facets showing the following variables: `ex`, `lx`, `nAx`, `ndx`, `nLx`, `nMx`, `nqx`, `Sx`, `Tx`. The dotted lines for `ndx` and `lx` correspond to `0.25`, `0.5`, and `0.75` quartiles of the distribution and the solid line is `e0.` Graphs will be created for each group identified by `.id` argument.
#' @importFrom ggplot2 ggplot geom_line facet_wrap scale_x_continuous scale_y_continuous theme_light geom_vline theme element_text element_text annotate
#' @importFrom scales pretty_breaks
#' @importFrom tidyr pivot_longer
#' @importFrom LifeIneq ineq_quantile_lower
#' @importFrom grDevices gray
#' @importFrom rlang .data
#' @export
#' @examples
#' library(tibble)
#' Exposures <- c(
#'   100958, 466275, 624134, 559559, 446736, 370653, 301862, 249409,
#'   247473, 223014, 172260, 149338, 127242, 105715, 79614, 53660,
#'   31021, 16805, 8000, 4000, 2000, 1000
#' )
#'
#' Deaths <- c(
#'   8674, 1592, 618, 411, 755, 1098, 1100, 1357,
#'   1335, 3257, 2200, 4023, 2167, 4578, 2956, 4212,
#'   2887, 2351, 1500, 900, 500, 300
#' )
#'
#' Age <- c(0, 1, seq(5, 100, by = 5))
#' data_in <- tibble(Age, Deaths, Exposures)
#' data_out <-
#'   lt_flexible(data_in,
#'     OAnew = 100,
#'     age_out = "single",
#'     extrapFrom = 80,
#'     extrapFit = Age[Age >= 60],
#'     radix = 1e+05,
#'     extrapLaw = NULL,
#'     SRB = 1.05,
#'     a0rule = "ak",
#'     axmethod = "un",
#'     Sex = "m"
#'   )$data_out
#' result <- plot_lifetable(data_out)
#' result$nMx$nMx_plot
#' result$lx$lx_plot
#' result$ndx$ndx_plot
#' result$nqx$nqx_plot
#' 
# TODO: add plot titles
plot_lifetable <- function(data_out, i18n = NULL) {

  
  ages_between_text <- translate_text("Ages between", i18n)
  age_text_value <- translate_text("Age_value", i18n)
  age_text <- translate_text("Age", i18n)
  nMx_text <- translate_text("nMx", i18n)
  log_nMx_text <- translate_text("nMx (log scale)", i18n)
  log_nMx_text_title <- translate_text("Log transformed Age-specific mortality rate", i18n)
  lx_text <- translate_text("lx", i18n)
  lx_text_title <- translate_text("Survival curve (lx) generated from lifetable nmx values", i18n)
  lx_text_subtitle <- translate_text("Marked locations on the curve indicate survival quartiles", i18n)
  life_expectancy_text <- translate_text("Life Expectancy", i18n)
  dx_text <- translate_text("dx", i18n)
  dx_text_title <- translate_text("Death distribution (dx) generated from lifetable nmx values", i18n)
  dx_text_subtitle <- translate_text("Marked locations on the distribution indicate age-at-death quartiles (grey)\nand life expectancy (red)", i18n)
  nqx_text <- translate_text("nqx", i18n)
  nqx_text_log <- translate_text("nqx (log scale)", i18n)
  nqx_text_title <- translate_text("Conditional Death Probabilities", i18n)

  if (!(".id" %in% colnames(data_out))){
    data_out <- data_out |>
      mutate(.id = "all")
  }

  lx25 <- ineq_quantile_lower(age = data_out$Age, lx = data_out$lx, quantile = 0.25)
  lx50 <- ineq_quantile_lower(age = data_out$Age, lx = data_out$lx, quantile = 0.5)
  lx75 <- ineq_quantile_lower(age = data_out$Age, lx = data_out$lx, quantile = 0.75)
  e0   <- data_out$ex[data_out$Age == 0]
  
  dt   <- data_out |>
    mutate(
      AgeInt    = age2int(.data$Age, OAG = FALSE),
      single    = is_single(.data$Age),
      age_plot  = if_else(.data$single, .data$Age, .data$Age + (.data$AgeInt / 2)),
      age_label = case_when(
        Age == max(.data$Age) ~ paste0(max(.data$Age), "+"),
        TRUE ~ paste0(ages_between_text, " [", .data$Age, ",", .data$Age + .data$AgeInt, ")")
      )
    )
  
  id   <- unique(dt$.id)

  # Rename columns 
  names(dt)[names(dt) == "age_plot"] <- age_text_value
  names(dt)[names(dt) == "nMx"] <- nMx_text

  sym_age_value <- sym(age_text_value)
  sym_nMx <- sym(nMx_text)

  nMx_plot <- 
    dt |>
    ggplot(aes(x = !!sym_age_value, y = !!sym_nMx), col = "black") +
    geom_line() +
    scale_y_log10() +
    theme_light() +
    theme(
      axis.text     = element_text(color = "black"),
      plot.title = element_text(size = 14, color = "black"),
      axis.title.y  = element_blank()
    ) + 
    labs(
      x = age_text,
      y = log_nMx_text,
      title = paste(log_nMx_text_title, id, sep = "; .id = ")
    )

  # --------- #
  # lx  plot  #
  # --------- #
  radix <- dt$lx[1]

  dt$lx <- round(dt$lx, 8)

  # Rename columns 
  names(dt)[names(dt) == "lx"] <- lx_text
  sym_lx <- sym(lx_text)
  
  lx_plot <- 
    dt |>
    ggplot(aes(x = !!sym_age_value, y = !!sym_lx), col = "black") +
    geom_line() +
    theme_light() +
    theme(
      axis.text = element_text(color = "black"),
      plot.title = element_text(size = 10, color = "black"),
      plot.subtitle = element_text(size = 8, color = "black")
    ) +
    geom_vline(xintercept = data_out$ex[1], color = "red") +
    annotate("text",
             x = data_out$ex[1] + 2, y = .25 * radix,
             label = life_expectancy_text, color = "red", angle = -90
    ) +
    annotate("segment", x = lx75 - 2, xend = lx75 + 2, y = .75 * radix, yend = .75 * radix, color = gray(.5)) +
    annotate("segment", x = lx50 - 2, xend = lx50 + 2, y = .50 * radix, yend = .50 * radix, color = gray(.5)) +
    annotate("segment", x = lx25 - 2, xend = lx25 + 2, y = .25 * radix, yend = .25 * radix, color = gray(.5)) +
    annotate("segment", x = lx75, xend = lx75, y = .73 * radix, yend = .77 * radix, color = gray(.5)) +
    annotate("segment", x = lx50, xend = lx50, y = .48 * radix, yend = .52 * radix, color = gray(.5)) +
    annotate("segment", x = lx25, xend = lx25, y = .23 * radix, yend = .27 * radix, color = gray(.5)) +
    annotate("text", x = lx75 + 4, y = .77 * radix, color = gray(.5), label = "75%") +
    annotate("text", x = lx50 + 4, y = .52 * radix, color = gray(.5), label = "50%") +
    annotate("text", x = lx25 + 4, y = .27 * radix, color = gray(.5), label = "25%") +
    labs(
      x = age_text,
      y = lx_text,
      title = paste(lx_text_title, id, sep = "; .id = "),
      subtitle = lx_text_subtitle
    )
  
  # --------- #
  # ndx plot  #
  # --------- #
  R <- dt$ndx |>
    max() |>
    pretty() |>
    max()
  
  dx_ages <- c(lx75, lx50, lx25) |> floor()
  
  R <- (dt$ndx /dt$AgeInt) |> max() |> pretty() |> max()
  dx_ages <- c(lx75, lx50, lx25) |> floor()
  
  if(is_abridged(dt$Age)) {
    dx_ages <- dx_ages - dx_ages %% 5
  }
  
  dx_mark <-
    dt |>
    filter(.data$Age %in% dx_ages) |>
    mutate(ndx = .data$ndx / .data$AgeInt) |>
    pull(.data$ndx)
  

  dt <-
    dt |>
    mutate(dx = round(.data$ndx / .data$AgeInt, 8))

  names(dt)[names(dt) == "Age"] <- age_text
  names(dt)[names(dt) == "dx"] <- dx_text

  sym_dx <- sym(dx_text)
  sym_age <- sym(age_text)

  ndx_plot <-
    dt |>
    ggplot(aes(x = !!sym_age, y = !!sym_dx), col = "black") +
    geom_line() +
    theme_light() +
    theme(
      axis.text = element_text(color = "black"),
      plot.title = element_text(size = 10, color = "black"),
      plot.subtitle = element_text(size = 8, color = "black")
    ) +
    # vertical ticks
    annotate("segment",
             x = lx75, xend = lx75, y = dx_mark[1] + R * .02, yend = dx_mark[1] - R * .02,
             color = gray(.5)
    ) +
    annotate("segment",
             x = lx50, xend = lx50, y = dx_mark[2] + R * .02, yend = dx_mark[2] - R * .02,
             color = gray(.5)
    ) +
    annotate("segment",
             x = lx25, xend = lx25, y = dx_mark[3] + R * .02, yend = dx_mark[3] - R * .02,
             color = gray(.5)
    ) +
    # horizontal ticks
    annotate("segment",
             x = lx75 - 2, xend = lx75 + 2, y = dx_mark[1], yend = dx_mark[1],
             color = gray(.5)
    ) +
    annotate("segment",
             x = lx50 - 2, xend = lx50 + 2, y = dx_mark[2], yend = dx_mark[2],
             color = gray(.5)
    ) +
    annotate("segment",
             x = lx25 - 2, xend = lx25 + 2, y = dx_mark[3], yend = dx_mark[3],
             color = gray(.5)
    ) +
    # labels
    annotate("text", x = lx75 - 5, y = dx_mark[1] + R * .03, color = gray(.5), label = "25%") +
    annotate("text", x = lx50 + 4, y = dx_mark[2] + R * .03, color = gray(.5), label = "50%") +
    annotate("text", x = lx25 + 4, y = dx_mark[3] + R * .03, color = gray(.5), label = "75%") +
    
    # geom_vline(xintercept = c(lx25,lx50,lx75),
    #            linewidth  = .5, color = gray(.5)) +
    geom_vline(xintercept = data_out$ex[1], color = "red") +
    labs(
      x = age_text,
      y = dx_text,
      title = str_c(dx_text_title, id, sep = "; .id = "),
      subtitle = dx_text_subtitle
    )


  dt <-
    dt |>
    mutate(nqx = round(.data$nqx, 2))

  names(dt)[names(dt) == "nqx"] <- nqx_text
  sym_nqx <- sym(nqx_text)
  
  nqx_plot <- 
    dt |>
    ggplot(aes(x = !!sym_age, y = !!sym_nqx), col = "black") +
    geom_line() +
    scale_y_log10() +
    theme_light() +
    theme(
      axis.text = element_text(color = "black")
    ) +
    labs(
      x = age_text,
      y = nqx_text_log,
      title = str_c(nqx_text_title, id, sep = "; .id = ")
    )

  return(lst(
    nMx = lst(
      nMx_plot, 
      nMx_plot_data = dt |> select(age_text, age_text_value, "age_label", nMx_text, ".id")
      ),
    lx = lst(
      lx_plot, 
      lx_plot_data = dt |> select(age_text, age_text_value, "age_label", lx_text, ".id")
      ),
    ndx = lst(
      ndx_plot, 
      ndx_plot_data = dt |> select(age_text, age_text_value, "age_label", dx_text, ".id")
      ),
    nqx = lst(
      nqx_plot, 
      nqx_plot_data = dt |> select(age_text, age_text_value, "age_label", nqx_text, ".id")
    )
  )
  )
  
}


# helper function for better axis lables
abs_and_comma <- function(x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' @title `pyramid`
#' @description Generate a population pyramid from the user data. 
#' @param data tibble. Empirical data was downloaded with the `read_data` function. Should contain the Exposures, Deaths
#' @param y  character. This argument indicates weather the `Exposures` or `Deaths` should be plotted.
#' @return A pyramid for either Deaths or Exposures
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous coord_flip theme_light scale_fill_brewer theme geom_bar element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr filter mutate pull
#' @importFrom rlang sym .data := !!
#' @export
#' @examples
#' Exposures <- c(100958, 466275, 624134, 559559, 446736, 370653, 301862,
#'                249409, 247473, 223014, 172260, 149338, 127242, 105715,
#'                79614,  53660,  31021,  16805,  8000,   4000,   2000,
#'                1000)
#' 
#' Deaths <- c(8674, 1592, 618,  411,  755,  1098, 1100, 1357,
#'             1335, 3257, 2200, 4023, 2167, 4578, 2956, 4212,
#'             2887, 2351, 1500, 900,  500,  300)
#' 
#' sex     <- c("Female")
#' Age     <- c(0, 1, seq(5, 100, by = 5))
#' data_in <- data.frame(Age, 
#'                       Deaths, 
#'                      Exposures,
#'                      AgeInt = c(diff(Age), NA),
#'                      Sex = sex)
#' pyramid(data_in, "Deaths")
#' 

pyramid <- function(data, y) {
  
  data |>
    mutate(Sex = tolower(.data$Sex)) |>
    filter(.data$Sex %in% c("male", "female")) |>
    mutate(Sex = factor(.data$Sex, levels = c("male", "female"))) |>
    mutate(!!y := ifelse(.data$Sex == "male", -(!!sym(y)), !!sym(y))) |> # done
    ggplot(aes(x = .data$Age, y = (.data[[y]] / .data$AgeInt), fill = .data$Sex, width = .data$AgeInt)) +
    
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(
      labels = abs_and_comma,
      limits = max(pull(mutate(data, "new" := .data[[y]] / .data$AgeInt), "new")) * c(-1, 1)) +
    theme_light() +
    scale_fill_brewer(palette = "Dark2", guide = guide_legend(title = "Sex")) +
    theme(
      legend.position = "bottom",
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 12),
      legend.text = element_text(color = "black", size = 12),
      legend.title = element_text(color = "black", size = 14),
      plot.subtitle = element_text(color = "black", size = 14)
    ) +
    labs(
      subtitle = str_c("Pyramid of ", y, "."),
      y = y
    )
}

# plot pyramid in case there are many .id For future
# Same for other initial plots
# plot_pyramid <- function(data, y) {
#   
#   data %>%
#     mutate(.id = "f") %>%
#     group_nest(.id) %>%
#     mutate(figure = map(data, ~ pyramid(.x, y)))
#   
# }
# 
# show_input_rates <- function(data) {
#   
#   data %>%
#     mutate(.id = "f") %>%
#     group_nest(.id) %>%
#     mutate(figure = map(data, ~ plot_input_rates(.x)))
#   
# }
# 
# show_histogram <- function(data, y) {
#   
#   data %>%
#     mutate(.id = "f") %>%
#     group_nest(.id) %>%
#     mutate(figure = map(data, ~ plot_histogram(.x, y)))
#   
# }

# Usage: plot_pyramid(data_in, "Exposures")$figure
# show_input_rates(data_in)$figure
# plot_histogram(data_in, "Deaths")
# show_histogram(data_in, "Deaths")$figure

#' @title `plot_input_rates`
#' @description Plots a line graph of the log10 transformed empirical mortality rate (Mx).
#' @param data tibble. Empirical data downloaded  with the `read_data` function
#' @param i18n An optional i18n object for translation.
#' @return A linechart of log 10 scaled empirical `M(x)` values
#' @importFrom ggplot2 ggplot scale_y_log10 scale_y_continuous coord_flip theme_bw scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
#' @examples
#' Exposures <- c(100958, 466275, 624134, 559559, 446736, 370653, 301862,
#'                249409, 247473, 223014, 172260, 149338, 127242, 105715,
#'                79614,  53660,  31021,  16805,  8000,   4000,   2000,
#'                1000)
#' 
#' Deaths <- c(8674, 1592, 618,  411,  755,  1098, 1100, 1357,
#'             1335, 3257, 2200, 4023, 2167, 4578, 2956, 4212,
#'             2887, 2351, 1500, 900,  500,  300)
#' 
#' Age     <- c(0, 1, seq(5, 100, by = 5))
#' data_in <- data.frame(Age, Deaths, Exposures, AgeInt = c(diff(Age), NA))
#' plot_input_rates(data_in)
#'

plot_input_rates <- function(data, i18n = NULL) {
  
  data <- data |>
    mutate(nMx = round(.data$Deaths / .data$Exposures,8),
           AgeInt = age2int(.data$Age, OAG = FALSE),
           single = is_single(.data$Age),
           age_plot = if_else(.data$single, .data$Age, .data$Age + (.data$AgeInt / 2)),
           age_label = case_when(Age == max(.data$Age) ~ paste0(max(.data$Age),"+"),
                                 TRUE ~ paste0("[", .data$Age, ",", .data$Age + .data$AgeInt, ")")))
  
  if(any(colnames(data) == "Sex")){
    
    p <- data |>
      ggplot(aes(x = .data$age_plot, y = .data$nMx), linewidth = 0.8)
    
  } else {
    
    p <- data |>
      ggplot(aes(x = .data$age_plot, y = .data$nMx), linewidth = 0.8)
    
  }
  
  figure <- p + 
    geom_line(color = "black") + 
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_log10() +
    theme_light() +
    labs(
      x = translate_text("Age", i18n),
      y = translate_text("nMx (log10 scale)", i18n),
      subtitle = translate_text("Empirical Mx for a given age range on a log10 scale.", i18n)
    ) +
    theme(
      axis.text = element_text(size = 10, color = "black"),
      plot.subtitle = element_text(size = 12, color = "black")
    )
  
  return(
    lst(
      figure,
      data = data |> select(.data$Age, .data$age_label, .data$nMx)
    )
  )
}

# Helper function for translation with fallback
translate_text <- function(text, i18n = NULL) {
  if (!is.null(i18n) && !is.null(text)) {
    tryCatch({
      return(i18n$t(text))
    }, error = function(e) {
      return(text)  # Fallback to original if translation fails
    })
  }
  return(text)
}

#' @title `plot_histogram`
#' @description Plots a histogram of population or death, depending on the user's choice.
#' @param data tibble. Empirical data downloaded  with the `read_data` function
#' @param y character. This argument indicates whether the `Exposures` or `Deaths` should be plotted.
#' @param i18n An optional i18n object for translation.
#' @return A histogram for either Deaths or Exposures
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous coord_flip theme_light scale_fill_brewer theme theme element_text guide_legend element_blank
#' @importFrom scales label_log pretty_breaks
#' @importFrom rlang .data !! sym
#' @export
#' @examples
#' Exposures <- c(100958, 466275, 624134, 559559, 446736, 370653, 301862,
#'                249409, 247473, 223014, 172260, 149338, 127242, 105715,
#'                79614,  53660,  31021,  16805,  8000,   4000,   2000,
#'                1000)
#' 
#' Deaths <- c(8674, 1592, 618,  411,  755,  1098, 1100, 1357,
#'             1335, 3257, 2200, 4023, 2167, 4578, 2956, 4212,
#'             2887, 2351, 1500, 900,  500,  300)
#' 
#' Age     <- c(0, 1, seq(5, 100, by = 5))
#' data_in <- data.frame(Age, Deaths, Exposures, AgeInt = c(diff(Age), NA))
#' plot_histogram(data_in, "Deaths")
#' 

plot_histogram <- function(data, y, i18n = NULL) {
  # Define key translations for user-facing elements
  age_text <- translate_text("Age", i18n)
  y_text <- translate_text(y, i18n)
  subtitle_prefix <- translate_text("Age histogram of ", i18n)
  
  if (! "AgeInt" %in% colnames(data)){
    data <- 
    data |> 
      mutate(AgeInt = age2int(.data$Age, OAvalue = 1))
  }
  
  # Note y doesn't get translated before calling plot_histogram
  # so this referring to the english y name
  y_sym <- sym(y)
  
  data <- 
  data |> 
    mutate(
      y_plot = !!y_sym / .data$AgeInt,
      age_label = case_when(
        Age == max(Age) ~ paste0(max(Age), "+"),
        TRUE ~ paste0("[", Age, ",", Age + AgeInt, ")")
        )
    ) |> 
    dplyr::select(.data$Age, .data$AgeInt, .data$age_label, !!y_sym, .data$y_plot)

  # Translate column names that will be visible in tooltips/hover
  if (!is.null(i18n)) {
    if (y == "Deaths" || y == "Exposures") {
      # Rename the column in data_display for tooltip visualization
      names(data)[names(data) == y] <- y_text
    }
    names(data)[names(data) == "Age"] <- age_text
  }

  y_sym <- sym(y_text)
  
  figure <- 
  data |> 
    ggplot(
      aes(
        x = .data[[age_text]] + .data$AgeInt / 2, 
        y = .data$y_plot, width = .data$AgeInt
      ), 
      color = "black"
    ) +
    geom_col() +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(
      labels = abs_and_comma,
      limits = c(0, max(data$y_plot, na.rm = TRUE))) +
    theme_light() +
    scale_fill_brewer(palette = "Dark2") +
    theme(
      legend.position = "bottom",
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 12),
      legend.text = element_blank(),
      legend.title = element_blank(),
      plot.subtitle = element_text(color = "black", size = 14)
    ) +
    labs(
      subtitle = str_c(subtitle_prefix, y_text, "."),
      y = y_text,
      x = age_text
    )

  res <- lst(
      figure,
      data = data |> select(.data[[age_text]], .data$age_label, !!y_sym)
  )

  res
}

# temporary exclusion

##' @title `plot_initial_two_sex`
##' @description Plots a line graph of the log10 transformed empirical mortality rate `M(x)`, population #pyramid and death pyramid if the data contains information on two sex.
##' @param data tibble. Empirical data downloaded  with the `read_data` function
##' @return A named list with 3 elements: `Exposures` - population pyramid, `Deaths` - death pyramid and #`Empirical Mx` - log 10 transformed empirical `M(x)` value
##' @importFrom ggplot2 ggplot scale_y_log10 scale_y_continuous coord_flip theme_bw scale_fill_brewer theme element_text guide_legend
##' @importFrom scales label_log pretty_breaks
##' @importFrom dplyr mutate
##' @export
##' @examples
##' \dontrun{
##' data1           <- data
##' data1$Sex       <- "Female"
##' data1$Exposures <- data1$Exposures
##' data1$Deaths    <- data1$Deaths
##' data <- data |> 
##'  full_join(data1) |> 
##'  mutate(Deaths = ifelse(Sex == "Female", Deaths + rpois(22, lambda = 50), Deaths))
##'
##' plot_initial_two_sex(data = data)
##' }
#
## plot_initial_two_sex <- function(data) {
##
##   if(plot_exposures) {
##
##     Exposures <- pyramid(data = data, y = "Exposures")
##
##   }
##
##   if(plot_deaths) {
##
##     Deaths <- pyramid(data = data, y = "Deaths")
##
##   }
##
##   if(plot_rates) {
##
##     `Empirical Mx` <- plot_input_rates(data = data)
##
##     }
##
##   return(lst(Exposures, Deaths, `Empirical Mx`))
##
## }

#' @title `plot_initial_single_sex`
#' @description Plots a line graph of the log10 transformed empirical mortality rate `M(x)`, population histogram and death histogram if the data contains information on only one sex.
#' @param data tibble. Empirical data that was downloaded with the `read_data` function.
#' @param i18n An optional i18n object for translation.
#' @return A named list with 3 elements: `Exposures` - population pyramid, `Deaths` - death pyramid and `Empirical Mx` - log 10 transformed empirical `M(x)` value.
#' @importFrom ggplot2 ggplot scale_y_log10 scale_y_continuous coord_flip theme_bw scale_fill_brewer theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr mutate if_else
#' @importFrom rlang .data !! sym
#' @export
#' @examples
#' Exposures <- c(100958, 466275, 624134, 559559, 446736, 370653, 301862,
#'                249409, 247473, 223014, 172260, 149338, 127242, 105715,
#'                79614,  53660,  31021,  16805,  8000,   4000,   2000,
#'                1000)
#' 
#' Deaths <- c(8674, 1592, 618,  411,  755,  1098, 1100, 1357,
#'             1335, 3257, 2200, 4023, 2167, 4578, 2956, 4212,
#'             2887, 2351, 1500, 900,  500,  300)
#' 
#' sex     <- c("Female")
#' Age     <- c(0, 1, seq(5, 100, by = 5))
#' data_in <- data.frame(Age, 
#'                       Deaths, 
#'                       Exposures, 
#'                       AgeInt = c(diff(Age), NA),
#'                       Sex = sex)
#' ex1 <- plot_initial_single_sex(data_in)
#' 
#' ex1$Exposures
#' ex1$Deaths
#' ex1$`Empirical Mx`
#' 

plot_initial_single_sex <- function(data, i18n = NULL) {
  # Define key translations for user-facing elements
  ages_between_text <- translate_text("Ages between: ", i18n)
  age_text <- translate_text("Age", i18n)
  exposures_text <- translate_text("Exposures", i18n)
  deaths_text <- translate_text("Deaths", i18n)
  
  # Prepare the data
  data <- 
    data |> 
    mutate(Mx_emp    = Deaths / Exposures,
           AgeInt    = age2int(.data$Age, OAG = FALSE),
           single    = is_single(.data$Age),
           `Age Mid` = if_else(single, .data$Age, .data$Age + (.data$AgeInt / 2)),
           age_label = case_when(Age == max(.data$Age) ~ paste0(max(.data$Age), "+"),
                                 TRUE ~ paste0("[", .data$Age, ",", .data$Age + .data$AgeInt, ")"))) |> 
    select(-"single")
  
  # ------------------ #
  # exposures bar plot #
  # ------------------ #
  Exposures <- plot_histogram(data = data, y = "Exposures", i18n = i18n)
  dt <- data
  dt$Exposures <- round(dt$Exposures / dt$AgeInt, 8)
  dt$age_label <- paste0(ages_between_text, dt$age_label)

  names(dt)[names(dt) == "Exposures"] <- exposures_text      
  names(dt)[names(dt) == "Age"] <- age_text

  Exposures$figure <-
    Exposures$figure +
    geom_col(
      data = dt,
      aes(
        y = !!sym(exposures_text),
        text = .data$age_label
      )
    )
  
  # ------------------ #
  #    deaths bar plot #
  # ------------------ #
  Deaths <- plot_histogram(data = data, y = "Deaths", i18n = i18n)
  
  dt <- data
  dt$Deaths <- round(dt$Deaths / dt$AgeInt, 8)
  dt$age_label <- paste0(ages_between_text, dt$age_label)

  names(dt)[names(dt) == "Age"] <- age_text
  names(dt)[names(dt) == "Deaths"] <- deaths_text
  
  Deaths$figure <-
    Deaths$figure +
    geom_col(
      data = dt,
      aes(
        y = !!sym(deaths_text),
        text = .data$age_label
      )
    )
  
  # ------------------ #
  # rates plot         #
  # ------------------ #
  # Use the original function for now, as requested to only modify specific functions
  `Empirical Mx` <- plot_input_rates(data = data, i18n = i18n)
  
  dt <- data
  dt$age_plot <- dt$`Age Mid`
  dt$age_label <- paste0(ages_between_text, dt$age_label)
  dt$nMx <- round(dt$Deaths / dt$Exposures, 8)
  
  `Empirical Mx`$figure <-
    `Empirical Mx`$figure +
    geom_line(
      data = dt,
      aes(
        text = .data$age_label
      )
    )
  
  # Return the original list structure with original keys
  return(lst(Exposures, Deaths, `Empirical Mx`))
}

#' @title `plot_initial_data`
#' @description Plots the corresponding 3 graphics for single sex or both sexes depending on data provided by the user.
#' @param data tibble. Empirical data that was downloaded with the `read_data` function.
#' @param i18n An optional i18n object for translation.
#' @return A list with 3 corresponding plots for either one or two sex.
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous coord_flip theme_light scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom rlang .data !! sym
#' @export
#' @examples
#' Exposures <- c(100958, 466275, 624134, 559559, 446736, 370653, 301862,
#'                249409, 247473, 223014, 172260, 149338, 127242, 105715,
#'                79614,  53660,  31021,  16805,  8000,   4000,   2000,
#'                1000)
#' 
#' Deaths <- c(8674, 1592, 618,  411,  755,  1098, 1100, 1357,
#'             1335, 3257, 2200, 4023, 2167, 4578, 2956, 4212,
#'             2887, 2351, 1500, 900,  500,  300)
#' 
#' Age     <- c(0, 1, seq(5, 100, by = 5))
#' data_in <- data.frame(Age, Deaths, Exposures, AgeInt = c(diff(Age), NA))
#' result <-plot_initial_data(data_in)
#' 
#' result$Exposures$figure
#' result$Deaths$figure
#' result$`Empirical Mx`$figure
#' 
plot_initial_data <- function(data, i18n = NULL) {
  
  if("Sex" %in% colnames(data)) {
    
    sexes <- unique(data$Sex)
    
  } else {
    
    sexes <- 1
    
  }
  
  if (length(sexes) > 1) {
    
    data <- data |>
      mutate(Sex = tolower(.data$Sex)) |>
      filter(.data$Sex %in% c("male", "female"))
    
    warning("Currently plots only handle single-sex data")
    
  } else {
    
    result <- plot_initial_single_sex(data, i18n = i18n)
    
  }
  
  return(result)
}
