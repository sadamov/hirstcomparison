#' Create a Comparison Plot of Pollen Concentrations for different traps and lines
#'
#' @param species The selected Pollen Type
#' @param resolution What temporal resolution should be plotted c("daily", "12hour", "6hour", "3hour", "hourly")
#' @param group Either compare traps or lines within traps c("line", "trap")
#' @param traps Which traps should be plotted c(2, 4, 6)
#' @param rm_zeros Should zero pollen measurements be removed from the plot
#'
#' @return A data frame with assets allocated to 5 main categories


plot_hirst <- function(species, resolution, group, traps, rm_zeros){

  # If needed one can add ifelse clauses here to make function more robust
  title <- tools::toTitleCase(paste0(resolution, " average concentrations of ", species, " per ", group, " for trap(s) number ", paste(traps, collapse = ", ")))
  alpha <- 0.5

  # The first plot needs actual datetimes for the x-axis, hence we need some complicated if statements
  if (resolution == "daily"){
    data_plot <- data_daily
  } else if (resolution == "12hour"){
    data_plot <- data_hours12
  } else if (resolution == "6hour"){
    data_plot <- data_hours6
  } else if (resolution == "3hour"){
    data_plot <- data_hours3
  } else if (resolution == "hourly"){
    data_plot <-data_hourly
  }

  data_plot <- map(data_plot, ~.x %>%
    mutate(timestamp = ymd_hm(paste0(date, hour))))

  data_plot <- data_plot %>%
    bind_rows %>%
    filter(trap %in% traps) %>%
    mutate_at(vars("trap", "line"), ~ as.factor(.)) %>%
    {if(rm_zeros) filter(., !!(sym(species)) > 0) else .}

  gg1 <- data_plot %>%
    ggplot(aes(x = timestamp)) +
    geom_line(aes(y = !!sym(species), col = !!sym(group)), alpha = alpha) +
    theme(legend.position = "none") +
    labs(y = "Mean Conc. [#Pollen/m³]", x = "")

  gg2 <- data_plot %>%
    ggplot() +
    geom_boxplot(aes(y = !!sym(species), fill = !!sym(group)), alpha = alpha) +
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    coord_cartesian(ylim = c(0, 600)) +
    labs(y = "Mean Conc. [#Pollen/m³]", x = "")

  gg3 <- data_plot %>%
    ggplot() +
    geom_histogram(aes(y = !!sym(species), fill = !!sym(group)), binwidth = 10, alpha = alpha) +
    facet_wrap(vars(!!sym(group)), ncol = 1) +
    theme(legend.position = "bottom") +
    coord_flip(ylim = c(0, 500)) +
    labs(x = "Occurence of Pollen Concentrations", y = "Mean Conc. [#Pollen/m³]")

  ggarrange(ggarrange(gg1, gg2, nrow = 2), gg3) %>%
    annotate_figure(top = title)

}

#' Create a Comparison Plot of Pollen Concentrations for different traps and lines
#'
#' @param resolution What temporal resolution should be plotted c("daily", "12hour", "6hour", "3hour", "hourly")
#' @param traps Which traps should be plotted c(poleno2, poleno4, poleno5, hirst)
#' @param rm_zeros Should zero pollen measurements be removed from the plot
#'
#' @return A data frame with assets allocated to 5 main categories


plot_comb <- function(resolution, traps, rm_zeros){

  # If needed one can add ifelse clauses here to make function more robust
  title <- tools::toTitleCase(paste0(resolution, " average concentrations of total pollen for trap(s): ", paste(traps, collapse = ", ")))
  alpha <- 0.5

  # The first plot needs actual datetimes for the x-axis, hence we need some complicated if statements
  if (resolution == "daily"){
    data_plot <- data_daily_comb
  } else if (resolution == "12hour"){
    data_plot <- data_hours12_comb
  } else if (resolution == "6hour"){
    data_plot <- data_hours6_comb
  } else if (resolution == "3hour"){
    data_plot <- data_hours3_comb
  } else if (resolution == "hourly"){
    data_plot <-data_hourly_comb
  }

  # data_plot <- map(data_plot, ~.x %>%
  #                    mutate(timestamp = ymd_hm(paste0(date, hour))))

  data_plot <- data_plot %>%
    filter(trap %in% traps) %>%
    mutate(trap = as.factor(trap)) %>%
    {if(rm_zeros) filter(., total > 0) else .}

  gg1 <- data_plot %>%
    ggplot(aes(x = timestamp)) +
    geom_line(aes(y = total, col = trap), alpha = alpha) +
    theme(legend.position = "none") +
    labs(y = "Mean Conc. [#Pollen/m³]", x = "")

  gg2 <- data_plot %>%
    ggplot() +
    geom_boxplot(aes(y = total, fill = trap), alpha = alpha) +
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    coord_cartesian(ylim = c(0, 600)) +
    labs(y = "Mean Conc. [#Pollen/m³]", x = "")

  gg3 <- data_plot %>%
    ggplot() +
    geom_histogram(aes(y = total, fill = trap), binwidth = 10, alpha = alpha) +
    facet_wrap(vars(trap), ncol = 1) +
    theme(legend.position = "bottom") +
    coord_flip(ylim = c(0, 500)) +
    labs(x = "Occurence of Pollen Concentrations", y = "Mean Conc. [#Pollen/m³]")

  ggarrange(ggarrange(gg1, gg2, nrow = 2), gg3) %>%
    annotate_figure(top = title)

}

