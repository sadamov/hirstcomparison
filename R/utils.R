#' Create a Comparison Plot of Pollen Concentrations for different traps and lines
#'
#' @param species The selected Pollen Type
#' @param resolution What temporal resolution should be plotted c("daily", "12hour", "6hour", "3hour", "hourly")
#' @param group Either compare traps or lines within traps c("line", "trap")
#' @param traps Which traps should be plotted c(2, 4, 6)
#'
#' @return A data frame with assets allocated to 5 main categories


plot_pollen <- function(species, resolution, group, traps){

  title <- tools::toTitleCase(paste0(resolution, " average concentrations of ", species, " per ", group, " for trap(s) number ", paste(traps, collapse = ", ")))
  alpha <- 0.5

  if (resolution == "daily"){
    data_plot <- map(data_daily, ~.x %>%
                       mutate(timestamp = date))
  } else if (resolution == "12hour"){
    data_plot <- map(data_hours12, ~.x %>%
                       mutate(timestamp = ymd_hm(paste0(date, hours12))))
  } else if (resolution == "6hour"){
    data_plot <- map(data_hours6, ~.x %>%
                       mutate(timestamp = ymd_hm(paste0(date, hours6))))
  } else if (resolution == "3hour"){
    data_plot <- map(data_hours3, ~.x %>%
                       mutate(timestamp = ymd_hm(paste0(date, hours3))))
  } else if (resolution == "hourly"){
    data_plot <- map(data_hourly, ~.x %>%
                       mutate(timestamp = ymd_hm(paste0(date, paste0(sprintf("%02d", hour), ":00")))))
  }

  data_plot <- data_plot %>%
    bind_rows %>%
    filter(!!sym(species) > 0,
           trap %in% traps)

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
    coord_cartesian(ylim = c(0, 300)) +
    labs(y = "Mean Conc. [#Pollen/m³]", x = "")

  gg3 <- data_plot %>%
    ggplot() +
    geom_histogram(aes(y = !!sym(species), fill = !!sym(group)), binwidth = 10, alpha = alpha) +
    facet_wrap(vars(!!sym(group)), ncol = 1) +
    coord_flip(ylim = c(0,450)) +
    theme(legend.position = "bottom") +
    labs(x = "Occurence of Pollen Concentrations", y = "Mean Conc. [#Pollen/m³]")

  ggarrange(ggarrange(gg1, gg2, nrow = 2), gg3) %>%
    annotate_figure(top = title)

}
