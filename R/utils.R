#' Sets measurements to NA for all times, where no trap measured anything
#'
#' @param data A list of dataframes for the measurements
#'
#' @return A list of dataframes with NA values

set_na <- function(data) {
  data_raw <- map(data, ~.x %>%
                          group_by(date, trap, line, hour) %>%
                          replace(is.na(.), 0) %>% # For the means we should include the NA's and for simplicity we assume that they are zero measurements (which is true for almost all of them).
                          summarise_at(vars(all_of(species_selection), Total, Group1, Group2, Group3, Group4, Group5), ~mean(., na.rm = TRUE)) %>%
                          ungroup())

  data_raw <- map(data_raw, ~.x %>%
                    mutate(hour = paste0(sprintf("%02d", hour), ":00"),
                           timestamp = ymd_hm(paste0(as.character(date), hour))))

  # Now we have to set some of those zeros back to NA, to define the blooming season as mentioned above

  measurements_to_exclude <- data_raw %>%
    bind_rows %>%
    group_by(trap, timestamp) %>%
    replace(is.na(.), 0) %>% # This should do nothing :-)
    mutate_at(vars(all_of(species_selection), Total, Group1, Group2, Group3, Group4, Group5), .funs = list(exclude = ~mean(.))) %>%
    ungroup() %>%
    group_by(timestamp) %>%
    mutate_at(vars(all_of(paste0(c(species_selection, "Total", "Group1", "Group2", "Group3", "Group4", "Group5"), "_exclude"))), function(x){
      if(all(as.integer(x) == 0))
        return(NA) # This will set all measurements to NA if no trap measured any pollen
      else
        return(0)
    }) %>%
    ungroup()


  measurements_after_exclusion <- map2_df(data_raw %>% bind_rows %>% select(all_of(species_selection), Total, Group1, Group2, Group3, Group4, Group5), measurements_to_exclude %>% select(contains("exclude")), ~.x + as.integer(.y))

  measurements_after_exclusion %>%
    bind_cols(measurements_to_exclude %>%
                select(timestamp, date, hour, trap, line)) %>%
    group_split(trap,line)
}


#' Create a Comparison Plot of Pollen Concentrations for different traps and lines
#'
#' @param species The selected Pollen Type
#' @param resolution What temporal resolution should be plotted c("daily", "12hour", "6hour", "3hour", "hourly")
#' @param group Either compare traps or lines within traps c("line", "trap")
#' @param traps Which traps should be plotted c(2, 4, 6)
#' @param rm_zeros Should zero pollen measurements be removed from the plot
#' @param combined Should the function return the combined plots with title or seperate plots
#'
#' @return A data frame with assets allocated to 5 main categories


plot_hirst <- function(species, resolution, group, traps, rm_zeros, combined){

  # If needed one can add ifelse clauses here to make function more robust
  title <- tools::toTitleCase(paste0(resolution, " average concentrations of ", species, " Pollen per ", group, " for trap(s) number ", paste(traps, collapse = ", ")))
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
  } else if (resolution == "2hour"){
    data_plot <- data_hours2
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
    geom_boxplot(aes(y = log10(!!sym(species)), fill = !!sym(group)), alpha = alpha) +
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(y = "Log Mean Conc. [#Pollen/m³]", x = "")

  gg3 <- data_plot %>%
    ggplot() +
    geom_histogram(aes(y = log10(!!sym(species)), fill = !!sym(group)), alpha = alpha) +
    facet_wrap(vars(!!sym(group)), ncol = 1) +
    theme(legend.position = "bottom") +
    coord_flip() +
    labs(x = "Occurence of Pollen Concentrations", y = "Log Mean Conc. [#Pollen/m³]")

  if (!combined) {
    list(gg1 + ggtitle(title), gg2 + ggtitle(title), gg3 + ggtitle(title))
  } else {
    ggarrange(ggarrange(gg1, gg2, nrow = 2), gg3) %>%
      annotate_figure(top = title)
  }
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
    coord_cartesian(ylim = c(0, 50)) +
    labs(y = "Mean Conc. [#Pollen/m³]", x = "")

  gg3 <- data_plot %>%
    ggplot() +
    geom_histogram(aes(y = total, fill = trap), binwidth = 1, alpha = alpha) +
    facet_wrap(vars(trap), ncol = 1) +
    theme(legend.position = "bottom") +
    coord_flip(ylim = c(0, 30)) +
    labs(x = "Occurence of Pollen Concentrations", y = "Mean Conc. [#Pollen/m³]")

  ggarrange(ggarrange(gg1, gg2, nrow = 2), gg3) %>%
    annotate_figure(top = title)

}


#' Plot boxplots for n spi samples of one pollen species
#'
#' @param data The dataframe containing the measurements
#' @param species The selected Pollen Type
#' @param n Number of samples drawn for each day
#' @param xlim Limits of the x-axis in the boxplots
#' @param samples The sampled daily values (created by plot_hist_dt())
#'
#' @return A list of ggplots

plot_spi <- function(data, species, samples, n = 1000, xlim = c(0, 3.5e4)){

  sampled_days_random <- list()
  sampled_spi <- list()

  # In the following we will be working with the mean values from the three traps.

    mean_traps <- data %>%
      select(!!sym(species), trap, timestamp, type) %>%
      pivot_wider(names_from = trap, values_from = !!sym(species)) %>%
      setNames(c("timestamp", "type", paste0("trap", c(2, 4, 8)))) %>%
      mutate(measuring_traps = as.integer(!is.na(trap2)) + as.integer(!is.na(trap4)) + as.integer(!is.na(trap8)),
             mean = if_else(!is.na(trap2) & !is.na(trap4) & !is.na(trap8),
                            rowSums(.[3:5], na.rm = TRUE) / measuring_traps, # This is only necessary if the condition one line above is less strict
                            NA_real_)) %>%
      pull(mean) %>%
      na.omit()

    # Now for each of these daily averages we want to estimate a spread, according to the fitted t-distribution above. This reflects the uncertainty between the three measurers.
    relative_differences <- pmax(0, samples[[species]]) # Samples were generated in step above, make sure that same species were applied. Very few are negative and that makes no sense for measurements. Sensitivity was checked and capping at zero poses no issues.

    # These relative differences from the mean are sampled fractions. Basically 10000 options of what a trap could have measured for that day. Most samples should be close to the empirical common mean, but some will stray further apart. As these are relative differences from the mean, the absolute measurement error depends on the measured pollen concentration of any specific day.

    sampled_days <- map(mean_traps, ~ .x * (relative_differences))

    # Now we will pick random measurements for each of the 89 days, and see how that affects the SPI

    for (i in 1:n){
      sampled_days_random[[i]] <- map(sampled_days, ~sample(.x, 1)) %>% flatten_dbl() %>% sum
    }

    sampled_spi[[species]] <- sampled_days_random %>% flatten_dbl %>% as_tibble()

    sampled_spi[[species]] %>%
      ggplot(aes(x = value)) +
      geom_boxplot() +
      coord_cartesian(xlim = xlim) +#quantile(sampled_spi[[j]]$value, probs = c(0.15, 0.85)) %>% as.numeric) +
      labs(x = species)
}

#' Fit t distribution to relative differences from the mean and then create daily samples of of it and return a ggplot with the fitted densitiy function
#'
#' @param data The dataframe containing the measurements
#' @param species The selected Pollen Type
#' @param n_test Number of times the cvm and ad tests should be carried out
#' @param samples Number of samples created for each day
#' @param plots If plots is TRUE, histogram and density plots are returned, if FALSE the fct returns sampled daily values
#' @param tdist Should a t distribution be fitted to the data?
#'
#' @return A list of ggplots

plot_hist_dt <- function(data, species, n_test = 1, samples = 10000, plots = FALSE, tdist = TRUE){

  ggthemr("fresh")

  cvm <- numeric()
  ad <- numeric()
  errors <- data %>%
    select(!!sym(species), trap, timestamp, type) %>%
    pivot_wider(names_from = trap, values_from = !!sym(species)) %>%
    setNames(c("timestamp", "type", paste0("trap", c(2, 4, 8)))) %>%
    mutate(measuring_traps = as.integer(!is.na(trap2)) + as.integer(!is.na(trap4)) + as.integer(!is.na(trap8)),
           mean = if_else(!is.na(trap2) & !is.na(trap4) & !is.na(trap8),
                          rowSums(.[3:5], na.rm = TRUE) / measuring_traps, # This is only necessary if the condition one line above is less strict
                          NA_real_)) %>%
    mutate_at(vars(paste0("trap", c(2, 4, 8))), ~(. / mean)) %>%
    pivot_longer(cols = trap2:trap8, values_to = "error", names_to = "trap")

  if (tdist){
    sd <- sd(errors$error, na.rm = TRUE)
    e <- simpleError("test error")
    t_shape <- try(QRM::fit.st(errors$error[!is.na(errors$error)])) # Actually lead to the same result as the bootstrapped version below
    # t_shape <- QRM::fit.mst(errors$error, method = "Brent", upper = 2, lower = 1, nit = 2000, tol = 1e-10) # https://magesblog.com/post/2013-03-12-how-to-use-optim-in-r/ or here https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html
    if (class(t_shape) != "try-error"){
      m <- t_shape$par.ests[2]
      s <- t_shape$par.ests[3]
      df <- t_shape$par.ests[1]
      t_errors <- tibble(x = x, y = dt((x - m) / s, df = df) / s) # See here for supplying location and scale parameteres to t-distribution in Base-R: https://en.wikipedia.org/wiki/Location%E2%80%93scale_family

      # If the parameteres are estimated from the data then this test applies the method from Braun and splits the data into two equally sized subsets. Therefore the test results are not stable, especially for smaller datasets (coarse temporal resolution). I will try to bootstrap the testing and the use the  mean (not 100% if this is valid, tbd).

      for (i in 1:n_test) {
        cvm_test <- cvm.test((errors$error[!is.na(errors$error)] - m)/s, "pt", df = df, estimated = TRUE)
        ad_test <- ad.test((errors$error[!is.na(errors$error)] - m)/s, "pt", df = df, estimated = TRUE)

        cvm[i] <- cvm_test$p.value
        ad[i] <- ad_test$p.value
      }

      cvm <- mean(cvm)
      ad <- mean(ad)

      samples <- rt(samples, df = df) * s + m # This gives us 10k random samples for the absolute log-differences from the mean. See here again for supplying location and scale parameteres to t-distribution in Base-R: https://en.wikipedia.org/wiki/Location%E2%80%93scale_family
    }
  }



  gg <- errors %>%
    ggplot(aes(x=error, y = ..density..)) +
    geom_histogram(binwidth = 0.05) +
    geom_density(col = swatch()[4]) +
    geom_rug(aes(y = 0), position = position_jitter(height = 0), col = swatch()[5]) +
    coord_cartesian(xlim = c(0, 2)) +
    labs(x = paste("Error", species))
  if (plots){
    if (tdist){
      if (class(t_shape) != "try-error"){
      gg +
        geom_line(data = t_errors, aes(x = x, y = y), col = swatch()[6]) +
        geom_label(label = paste("P-Values \n CVM Test:", round(cvm, 2), "\n AD-Test", round(ad, 2)), aes(x = 0.2, y = 1), size = 3) +
        geom_label(label = paste("SD:", round(sd, 3), "\n DF:", round(df, 2)), aes(x = 1.8, y = 1), size = 3)
      }
    } else gg
  } else
    samples
}

