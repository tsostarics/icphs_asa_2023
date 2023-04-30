# library(rlang)      # tidyeval helpers
# library(ggplot2)    # Plotting
# library(dplyr)      # Data wrangling
# library(purrr)      # Data wrangling
# library(modelr)     # Data grid
# library(tidybayes)  # Draw predictions from bayesian models
# library(ggdist)     # Plot bayesian draws
# library(svglite)    # Svg output

plot_experiment_heatmap <- function(exp_data, 
                                    schematic_tunes = NULL, 
                                    .show_percentage = TRUE,
                                    .show_type = TRUE,
                                    .family = "Roboto") {
  # Calculate proportions foreach cell
  proportion_data <- 
    exp_data |> 
    dplyr::group_by(pa_step, bt_step, 
                    start_erb_centered, end_erb_centered, 
                    erb_diff, st_diff) %>% 
    dplyr::summarize(prop_telling = mean(response_binary)) |> 
    dplyr::mutate(type = ifelse(sign(erb_diff)==1, "rising","falling")) |> 
    ungroup()
  
  ## Default values
  # Default gray box around heatmap
  type_annotation    <- 
    list(
      annotate(geom = 'rect', 
               xmax = 5.5, 
               xmin = .5, 
               ymax = 5.5, 
               ymin = .5, 
               linewidth = 1, 
               color = 'gray15', 
               fill = NA)
    )
  heatmap_tiles <- list(geom_tile(linewidth = .5, color = 'white'))
  type_var           <- NULL
  type_color <- "white"
  percent_annotation <- NULL
  tune_annotation    <- NULL
  
  # Whether to add falling/rising labels
  if (.show_type) {
    heatmap_tiles <-list(geom_tile(linewidth = 1))
    type_annotation <- 
      list(
        ggplot2::annotate(geom = 'text',
                          x = 5,
                          y = .25,
                          label = "Falling",
                          size = 6,
                          color = RColorBrewer::brewer.pal(5,"Set1")[1]),
        ggplot2::annotate(geom = 'text',
                          x = 1,
                          y = 5.7,
                          label = "Rising",
                          size = 6,
                          color = RColorBrewer::brewer.pal(5,"Set1")[2])
      )
    type_var <-  sym("type")
  }
  
  # Whether to show numeric proportions in each cell
  if (.show_percentage) {
    percent_annotation <- 
      list(
        ggplot2::geom_label(aes(x = pa_step - .15,
                                y = bt_step + .3,
                                family = .family,
                                label = round(prop_telling,2)*100),
                            fill = 'white',
                            color ='black',
                            size = 4) 
      )
  }
  
  # Whether to plot schematic tunes if passed
  if (!is.null(schematic_tunes)) {
    tune_annotation <- 
      list(
        ggplot2::geom_line(data = schematic_tunes,
                           aes(x = x_position, y = y_position, group = step),
                           color = 'white',
                           linewidth = 2,
                           inherit.aes = FALSE),
        ggplot2::geom_line(data = schematic_tunes,
                           aes(x = x_position, y = y_position, group = step),
                           inherit.aes = FALSE,
                           color = "black",
                           linewidth = 1)
      )
  }
  
  
  proportion_data |> 
    ggplot2::ggplot(aes(x = pa_step, 
                        y = bt_step, 
                        color = !!type_var, 
                        fill = prop_telling)) +
    heatmap_tiles +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::scale_x_continuous(breaks = 1:5, labels = 1:5) +
    ggplot2::scale_y_continuous(breaks = 1:5, labels = 1:5) +
    ggplot2::scale_color_brewer(palette = 'Set1', guide='none') +
    ggplot2::ylab("Boundary Tone Step (Low to High)") +
    ggplot2::xlab("Pitch Accent Step (Low to High)") +
    ggplot2::scale_fill_viridis_c(limits = c(0,1), 
                                  option = 'magma',
                                  direction = 1,
                                  guide = guide_colorbar(title = "% Telling",
                                                         frame.colour = 'black',
                                                         title.position = 'top',
                                                         label.position = 'top'),
                                  breaks = c(0,1),
                                  labels = c("0", "100"))+
    # ggplot2::scale_fill_continuous(limits = c(0,1),
    #                                high = "#132B43", low = "#56B1F7") +
    
    # Plot tunes
    tune_annotation +
    percent_annotation +
    type_annotation +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "top", 
                   panel.grid = element_blank(),
                   legend.box.margin = margin(b = -15),
                   legend.box.spacing = unit(0,'pt'),
                   legend.key.width = unit(50,'pt'),
                   legend.key.height = unit(.4,'cm'),
                   legend.title = element_text(margin = margin(b = -20)),
                   axis.text.x = element_text(margin = margin(t = -12)),
                   axis.text.y = element_text(margin = margin(r = -12)),
                   text = element_text(family = .family),
                   legend.title.align = 0.5,
                   plot.margin = margin())
}


plot_experiment_heatmap_logrt <- function(exp_data, 
                                          schematic_tunes = NULL, 
                                          .show_percentage = TRUE,
                                          .show_type = TRUE,
                                          .family = "Roboto") {
  # Calculate proportions foreach cell
  proportion_data <- 
    exp_data |> 
    dplyr::group_by(pa_step, bt_step,
                    start_erb_centered, end_erb_centered, 
                    erb_diff, st_diff) %>% 
    dplyr::summarize(prop_telling = mean(response_binary)) |> 
    dplyr::mutate(type = ifelse(sign(erb_diff)==1, "rising","falling")) |> 
    ungroup()
  
  ## Default values
  # Default gray box around heatmap
  type_annotation    <- 
    list(
      annotate(geom = 'rect', 
               xmax = 5.5, 
               xmin = .5, 
               ymax = 5.5, 
               ymin = .5, 
               linewidth = 1, 
               color = 'gray15', 
               fill = NA)
    )
  heatmap_tiles <- list(geom_tile(linewidth = .5, color = 'white'))
  type_var           <- NULL
  type_color <- "white"
    percent_annotation <- NULL
    tune_annotation    <- NULL
    
    # Whether to add falling/rising labels
    if (.show_type) {
      heatmap_tiles <-list(geom_tile(linewidth = 1))
      type_annotation <- 
        list(
          ggplot2::annotate(geom = 'text',
                            x = 5,
                            y = .25,
                            label = "Falling",
                            size = 6,
                            color = RColorBrewer::brewer.pal(5,"Set1")[1]),
          ggplot2::annotate(geom = 'text',
                            x = 1,
                            y = 5.7,
                            label = "Rising",
                            size = 6,
                            color = RColorBrewer::brewer.pal(5,"Set1")[2])
        )
      type_var <-  sym("type")
    }
    
    # Whether to show numeric proportions in each cell
    if (.show_percentage) {
      percent_annotation <- 
        list(
          ggplot2::geom_label(aes(x = pa_step - .15,
                                  y = bt_step + .3,
                                  family = .family,
                                  label = round(prop_telling,2)*100),
                              fill = 'white',
                              color ='black',
                              size = 4) 
        )
    }
    
    # Whether to plot schematic tunes if passed
    if (!is.null(schematic_tunes)) {
      tune_annotation <- 
        list(
          ggplot2::geom_line(data = schematic_tunes,
                             aes(x = x_position, y = y_position, group = step),
                             color = 'white',
                             linewidth = 2,
                             inherit.aes = FALSE),
          ggplot2::geom_line(data = schematic_tunes,
                             aes(x = x_position, y = y_position, group = step),
                             inherit.aes = FALSE,
                             color = "black",
                             linewidth = 1)
        )
    }
    
    
    proportion_data |> 
      ggplot2::ggplot(aes(x = pa_step, 
                          y = bt_step, 
                          color = !!type_var, 
                          fill = prop_telling)) +
      heatmap_tiles +
      ggplot2::theme_minimal(base_size = 16) +
      ggplot2::scale_x_continuous(breaks = 1:5, labels = 1:5) +
      ggplot2::scale_y_continuous(breaks = 1:5, labels = 1:5) +
      ggplot2::scale_color_brewer(palette = 'Set1', guide='none') +
      ggplot2::ylab("Boundary Tone Step (Low to High)") +
      ggplot2::xlab("Pitch Accent Step (Low to High)") +
      ggplot2::scale_fill_viridis_c(limits = c(0,1), 
                                    option = 'magma',
                                    direction = 1,
                                    guide = guide_colorbar(title = "% Telling",
                                                           frame.colour = 'black',
                                                           title.position = 'top',
                                                           label.position = 'top'),
                                    breaks = c(0,1),
                                    labels = c("0", "100"))+
      # ggplot2::scale_fill_continuous(limits = c(0,1),
      #                                high = "#132B43", low = "#56B1F7") +
      
      # Plot tunes
      tune_annotation +
      percent_annotation +
      type_annotation +
      ggplot2::coord_fixed() +
      ggplot2::theme(legend.position = "top", 
                     panel.grid = element_blank(),
                     legend.box.margin = margin(b = -15),
                     legend.box.spacing = unit(0,'pt'),
                     legend.key.width = unit(50,'pt'),
                     legend.key.height = unit(.4,'cm'),
                     legend.title = element_text(margin = margin(b = -20)),
                     axis.text.x = element_text(margin = margin(t = -12)),
                     axis.text.y = element_text(margin = margin(r = -12)),
                     text = element_text(family = .family),
                     legend.title.align = 0.5,
                     plot.margin = margin()) +
      facet_wrap(~utterance)
}

plot_experiment_heatmap_byutt <- function(exp_data, 
                                    schematic_tunes = NULL, 
                                    .show_percentage = TRUE,
                                    .show_type = TRUE,
                                    .family = "Roboto") {
  # Calculate proportions foreach cell
  proportion_data <- 
    exp_data |> 
    dplyr::group_by(pa_step, bt_step, utterance,
                    start_erb_centered, end_erb_centered, 
                    erb_diff, st_diff) %>% 
    dplyr::summarize(prop_telling = mean(response_binary)) |> 
    dplyr::mutate(type = ifelse(sign(erb_diff)==1, "rising","falling")) |> 
    ungroup()
  
  ## Default values
  # Default gray box around heatmap
  type_annotation    <- 
    list(
      annotate(geom = 'rect', 
               xmax = 5.5, 
               xmin = .5, 
               ymax = 5.5, 
               ymin = .5, 
               linewidth = 1, 
               color = 'gray15', 
               fill = NA)
    )
  heatmap_tiles <- list(geom_tile(linewidth = .5, color = 'white'))
  type_var           <- NULL
  type_color <- "white"
    percent_annotation <- NULL
    tune_annotation    <- NULL
    
    # Whether to add falling/rising labels
    if (.show_type) {
      heatmap_tiles <-list(geom_tile(linewidth = 1))
      type_annotation <- 
        list(
          ggplot2::annotate(geom = 'text',
                            x = 5,
                            y = .25,
                            label = "Falling",
                            size = 6,
                            color = RColorBrewer::brewer.pal(5,"Set1")[1]),
          ggplot2::annotate(geom = 'text',
                            x = 1,
                            y = 5.7,
                            label = "Rising",
                            size = 6,
                            color = RColorBrewer::brewer.pal(5,"Set1")[2])
        )
      type_var <-  sym("type")
    }
    
    # Whether to show numeric proportions in each cell
    if (.show_percentage) {
      percent_annotation <- 
        list(
          ggplot2::geom_label(aes(x = pa_step - .15,
                                  y = bt_step + .3,
                                  family = .family,
                                  label = round(prop_telling,2)*100),
                              fill = 'white',
                              color ='black',
                              size = 4) 
        )
    }
    
    # Whether to plot schematic tunes if passed
    if (!is.null(schematic_tunes)) {
      tune_annotation <- 
        list(
          ggplot2::geom_line(data = schematic_tunes,
                             aes(x = x_position, y = y_position, group = step),
                             color = 'white',
                             linewidth = 2,
                             inherit.aes = FALSE),
          ggplot2::geom_line(data = schematic_tunes,
                             aes(x = x_position, y = y_position, group = step),
                             inherit.aes = FALSE,
                             color = "black",
                             linewidth = 1)
        )
    }
    
    
    proportion_data |> 
      ggplot2::ggplot(aes(x = pa_step, 
                          y = bt_step, 
                          color = !!type_var, 
                          fill = prop_telling)) +
      heatmap_tiles +
      ggplot2::theme_minimal(base_size = 16) +
      ggplot2::scale_x_continuous(breaks = 1:5, labels = 1:5) +
      ggplot2::scale_y_continuous(breaks = 1:5, labels = 1:5) +
      ggplot2::scale_color_brewer(palette = 'Set1', guide='none') +
      ggplot2::ylab("Boundary Tone Step (Low to High)") +
      ggplot2::xlab("Pitch Accent Step (Low to High)") +
      ggplot2::scale_fill_viridis_c(limits = c(0,1), 
                                    option = 'magma',
                                    direction = 1,
                                    guide = guide_colorbar(title = "% Telling",
                                                           frame.colour = 'black',
                                                           title.position = 'top',
                                                           label.position = 'top'),
                                    breaks = c(0,1),
                                    labels = c("0", "100"))+
      # ggplot2::scale_fill_continuous(limits = c(0,1),
      #                                high = "#132B43", low = "#56B1F7") +
      
      # Plot tunes
      tune_annotation +
      percent_annotation +
      type_annotation +
      ggplot2::coord_fixed() +
      ggplot2::theme(legend.position = "top", 
                     panel.grid = element_blank(),
                     legend.box.margin = margin(b = -15),
                     legend.box.spacing = unit(0,'pt'),
                     legend.key.width = unit(50,'pt'),
                     legend.key.height = unit(.4,'cm'),
                     legend.title = element_text(margin = margin(b = -20)),
                     axis.text.x = element_text(margin = margin(t = -12)),
                     axis.text.y = element_text(margin = margin(r = -12)),
                     text = element_text(family = .family),
                     legend.title.align = 0.5,
                     plot.margin = margin()) +
      facet_wrap(~utterance)
}

plot_experiment_heatmap_small <- function(exp_data, 
                                          schematic_tunes = NULL, 
                                          .show_percentage = TRUE,
                                          .show_type = TRUE,
                                          .family = "Roboto") {
  
  proportion_data <- 
    exp_data |> 
    dplyr::group_by(pa_step, bt_step, 
                    start_erb_centered, end_erb_centered, 
                    erb_diff, st_diff) %>% 
    dplyr::summarize(prop_telling = mean(response_binary)) |> 
    dplyr::mutate(type = ifelse(sign(erb_diff)==1, "rising","falling")) |> 
    ungroup()
  
  type_annotation    <- 
    list(
      annotate(geom = 'rect', 
               xmax = 5.5, 
               xmin = .5, 
               ymax = 5.5, 
               ymin = .5, 
               linewidth = .5,
               color = 'gray15', 
               fill = NA)
    )
  heatmap_tiles      <- list(geom_tile(linewidth = .3, color = 'white'))
  type_var           <- NULL
  type_color         <- "white"
  percent_annotation <- NULL
  tune_annotation    <- NULL
  
  if (.show_type) {
    heatmap_tiles <-list(geom_tile(linewidth = 1))
    type_annotation <- 
      list(
        ggplot2::annotate(geom = 'text',
                          x = 5,
                          y = .25,
                          label = "Falling",
                          size = 6,
                          color = RColorBrewer::brewer.pal(5,"Set1")[1]),
        ggplot2::annotate(geom = 'text',
                          x = 1,
                          y = 5.7,
                          label = "Rising",
                          size = 6,
                          color = RColorBrewer::brewer.pal(5,"Set1")[2])
      )
    type_var <-  sym("type")
  }
  
  if (.show_percentage) {
    percent_annotation <- 
      list(
        ggplot2::geom_label(aes(x = pa_step - .15,
                                y = bt_step + .3,
                                family = .family,
                                label = round(prop_telling,2)*100),
                            fill = 'white',
                            color ='black',
                            label.padding = unit(0.15, 'lines'),
                            size = 2.5) 
      )
  }
  
  if (!is.null(schematic_tunes)) {
    tune_annotation <- 
      list(
        ggplot2::geom_line(data = schematic_tunes,
                           aes(x = x_position, y = y_position, group = step),
                           color = 'white',
                           linewidth = 2,
                           inherit.aes = FALSE),
        ggplot2::geom_line(data = schematic_tunes,
                           aes(x = x_position, y = y_position, group = step),
                           inherit.aes = FALSE,
                           color = "black",
                           linewidth = 1)
      )
  }
  
  
  proportion_data |> 
    ggplot2::ggplot(aes(x = pa_step, 
                        y = bt_step, 
                        color = !!type_var, 
                        fill = prop_telling)) +
    heatmap_tiles +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::scale_x_continuous(breaks = 1:5, labels = 1:5) +
    ggplot2::scale_y_continuous(breaks = 1:5, labels = 1:5) +
    ggplot2::scale_color_brewer(palette = 'Set1', guide='none') +
    ggplot2::ylab("Boundary Tone Step (Low to High)") +
    ggplot2::xlab("Pitch Accent Step (Low to High)") +
    ggplot2::scale_fill_viridis_c(limits = c(0,1), 
                                  option = 'magma',
                                  direction = 1,
                                  guide = guide_colorbar(title = "% Telling",
                                                         frame.colour = 'black',
                                                         title.position = 'top',
                                                         label.position = 'top'),
                                  breaks = c(0,1),
                                  labels = c("0", "100"))+
    # ggplot2::scale_fill_continuous(limits = c(0,1),
    #                                high = "#132B43", low = "#56B1F7") +
    
    # Plot tunes
    tune_annotation +
    percent_annotation +
    type_annotation +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "top", 
                   panel.grid = element_blank(),
                   legend.box.margin = margin(b = -10),
                   legend.box.spacing = unit(0,'pt'),
                   legend.key.width = unit(.45,'in'),
                   legend.key.height = unit(1/8,'in'),
                   legend.title = element_text(margin = margin(b = -10)),
                   axis.text.x = element_text(margin = margin(t = -6)),
                   axis.text.y = element_text(margin = margin(r = -6)),
                   text = element_text(family = .family),
                   legend.title.align = 0.5,
                   plot.margin = margin())
}

plot_tcog_idx_curve <- function(exp_data, 
                                exp_models,
                                .which_tcog = "tcog_f_semitone",
                                .family = "Roboto") {
  facet_by_exp <- NULL
  # If two experiments are passed, facet by the experiment and set the labels
  if ('experiment' %in% colnames(exp_data) && 
      length(unique(exp_data$experiment)) > 1)
    facet_by_exp <- 
      list(
        ggplot2::facet_grid(~experiment,
                            labeller = 
                              labeller(
                                experiment = c(exp1 = "Exp.1 (Gradual Falls)",
                                               exp2 = "Exp.2 (Early Falls)"))
        )
      )
  
  # Compute the average proportions for each utterance (/5) 
  #  at each continuum step (/25), 125 points total for each experiment
  avg_props <- 
    exp_data |> 
    dplyr::group_by(pa_step, bt_step, utterance, experiment,
                    tcog_f_raw, tcog_t_raw, 
                    tcog_f_centered, tcog_t_centered,
                    tcog_f_semitone, tcog_t_semitone,
                    st_tcog,
                    st_diff, erb_diff) |> 
    dplyr::summarize(prop_telling = mean(response_binary),
                     global_pattern = ifelse(first(erb_diff) < 0, 'falling', 'rising'),
                     shp = ifelse(global_pattern == 'falling', 19, 2))  
  
  # Compute model predictions for each experiment
  model_preds <- get_model_predictions(exp_models)
  
  # Create the curves for the marginal effect of tcog
  marginal_preds <- rbind(model_preds$exp1$marginal,
                          model_preds$exp2$marginal)
  
  marginal_smooth <- list(
    geom_ribbon(data = marginal_preds,
                aes(group = experiment,
                    ymax = conf.high,
                    ymin = conf.low,
                    x = !!sym(.which_tcog)),
                inherit.aes = FALSE,
                fill = 'gray45',
                color = NA,
                alpha = .15),
    geom_line(data = marginal_preds,
              aes(group = experiment,
                  y = predicted,
                  x = !!sym(.which_tcog)),
              inherit.aes = FALSE,
              linetype = 'dashed',
              color = 'gray30')
  )
  
  # Create the curves for the conditional effect of tcog given global pattern
  structural_preds <- rbind(model_preds$exp1$structural,
                            model_preds$exp2$structural)
  
  structural_smooth <- list(
    geom_ribbon(data = structural_preds,
                aes(group = global_pattern,
                    ymax = conf.high,
                    ymin = conf.low,
                    fill = factor(global_pattern),
                    x = !!sym(.which_tcog)),
                inherit.aes = FALSE,
                color = NA,
                alpha = .15),
    geom_line(data = structural_preds,
              aes(group = global_pattern,
                  y = predicted,
                  x = !!sym(.which_tcog),
                  color = global_pattern),
              inherit.aes = FALSE)
  )
  
  # Plot points and curves
  exp_data |> 
    dplyr::mutate(shp = ifelse(global_pattern == 'falling', 19, 2)) |>
    ggplot(aes(x = !!sym(.which_tcog), 
               y = response_binary, 
               fill = global_pattern, 
               color = global_pattern,
               shape = I(shp),
               group = experiment)) +
    scale_y_continuous(labels = seq(0, 100, 25)) +
    scale_color_manual(values = c('firebrick4','steelblue4')) +
    scale_fill_manual(values = c('firebrick2','dodgerblue3')) +
    marginal_smooth+
    structural_smooth+
    geom_point(data = avg_props,
               aes(y = prop_telling),
               size = .9)+
    facet_by_exp +
    theme_bw(base_size = 16) +
    ylab("% Telling") +
    theme(legend.position = 'none',
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = .15),
          strip.background = element_blank(),
          text = element_text(size = 16, family = .family)) +
    coord_fixed(ratio = 12)
}

get_model_predictions <- function(exp_models, 
                                  .which_tcog = "tcog_f_semitone") {
  # Extract the experiment name from the model call
  experiments <- 
    lapply(exp_models,
           \(mdl) {
             mdl@call$data |> 
               as.character() |> 
               gsub("_data", "",x = _)
           })
  
  names(exp_models) <- experiments
  names(experiments) <- experiments
  lapply(experiments,
         \(exp_name) {
           all_tcog <- paste0(.which_tcog, " [all]")
           
           # Returns 95% confidence interval predictions
           # Compute marginal effect of tcog
           marginal_tcog   <- 
             ggeffects::ggemmeans(exp_models[[exp_name]], 
                                  terms = c(all_tcog)) |> 
             cbind(experiment = exp_name)
           
           # Compute conditional effect of tcog for rising/falling
           structural_tcog <- 
             ggeffects::ggpredict(exp_models[[exp_name]], 
                                  terms = c(all_tcog, 'global_pattern')) |> 
             cbind(experiment = exp_name)
           
           colnames(marginal_tcog)[1L]   <- .which_tcog
           colnames(structural_tcog)[1L] <- .which_tcog
           colnames(structural_tcog)[6L] <- "global_pattern"
           list(marginal_tcog, structural_tcog) |> 
             setNames(c("marginal","structural"))
         })
}

plot_tcog_idx_curve_bayes <- function(exp_data, 
                                      exp_models,
                                      .which_tcog = "tcog_f_semitone",
                                      .family = "Roboto",
                                      .experiments = c('exp2','exp3'),
                                      .labeller = c(exp1 = "exp1",
                                                    exp2 = "exp2"),
                                      .xrange = c(-5,5)) {
  facet_by_exp <- NULL
  
  # If two experiments are passed, facet by the experiment and set the labels
  if (all('experiment' %in% colnames(exp_data)) && 
      length(unique(exp_data$experiment)) > 1)
    facet_by_exp <- 
      list(
        facet_grid(~experiment,
                   labeller = 
                     labeller(
                       experiment = .labeller)
        )
      )
  
  # Compute the average proportions for each utterance (/5) 
  #  at each continuum step (/25), 125 points total for each experiment
  avg_props <- 
    exp_data |> 
    dplyr::group_by(pa_step, bt_step, utterance, experiment,
             tcog_f_raw, tcog_t_raw, 
             tcog_f_centered, tcog_t_centered,
             tcog_f_semitone, tcog_t_semitone,
             st_tcog,
             st_diff, erb_diff) |> 
    dplyr::summarize(prop_telling = mean(response_binary),
              global_pattern = ifelse(first(erb_diff) < 0, 'falling', 'rising'),
              shp = ifelse(global_pattern == 'falling', 19, 2))  
  
  # Compute model predictions for each experiment
  model_preds <- get_model_predictions_bayes(exp_models, 
                                             exp_data, 
                                             experiments = .experiments,
                                             .xrange = .xrange)
  
  structural_smooth <- list(
    ggdist::stat_lineribbon(data = model_preds,
                    aes(x = tcog_f_semitone, 
                        y = .epred, 
                        fill = global_pattern,
                        group = global_pattern),
                    inherit.aes = FALSE,
                    color = NA,
                    geom = 'ribbon',
                    alpha = .15,
                    .width = .95),
    ggdist::stat_lineribbon(data = model_preds,
                    aes(x = tcog_f_semitone, 
                        y = .epred, 
                        color = global_pattern,
                        fill = global_pattern,
                        group = global_pattern),
                    inherit.aes = FALSE,
                    geom = 'line',
                    .width = .95)
  )
  
  # Plot points and curves
  exp_data |> 
    mutate(shp = ifelse(global_pattern == 'falling', 19, 2)) |>
    ggplot(aes(x = !!sym(.which_tcog), 
               y = response_binary, 
               fill = global_pattern, 
               color = global_pattern,
               shape = I(shp),
               group = experiment)) +
    scale_y_continuous(labels = seq(0, 100, 25)) +
    scale_color_manual(values = c('black','#0099cc')) +
    scale_fill_manual(values = c('black','#0099cc')) +
    structural_smooth +
    geom_point(data = avg_props,
               aes(y = prop_telling),
               size = 1.5)+
    # geom_label(data = avg_props,
    #            aes(y = prop_telling, label = paste0(pa_step, bt_step)),
    #            color = 'white')+
    facet_by_exp +
    theme_bw(base_size = 12) +
    ylab("% Telling") +
    theme(legend.position = 'none',
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = .15),
          strip.background = element_blank(),
          text = element_text(family = .family)) +
    coord_fixed(ratio = 12)
}



get_model_predictions_bayes <- function(exp_models,
                                        fulldata,
                                        experiments = c('exp1','exp2'),
                                        .which_tcog = "tcog_f_semitone",
                                        .xrange = c(-5,5)) {
  
  exp_data <- split(fulldata, ~ experiment)
  
  # Extract the experiment name from the model call
  
  names(experiments) <- experiments
  names(exp_models) <- experiments
  
  # Get predictions for each experiment
  purrr::map_dfr(experiments,
                 \(exp_name) {
                   draws <- 
                     # Extract experiment data
                     exp_data[[exp_name]] |> 
                     group_by(global_pattern) |>
                     # Create a data grid for draws
                     modelr::data_grid(tcog_f_semitone = seq(.xrange[1],.xrange[2], length.out = 51)) |>
                     # Draw prediction samples for credible intervals
                     tidybayes::add_epred_draws(exp_models[[exp_name]], re_formula = NA)
                   
                 }, .id = 'experiment')
}
