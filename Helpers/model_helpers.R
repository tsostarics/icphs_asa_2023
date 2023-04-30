# Creates data grids more easily
marginal_data_grid <- function(model_data, model, ...) {
  requireNamespace('modelr', quietly = TRUE)
  # Extract set arguments passed to ...
  dots <- rlang::enexprs(...)
  user_args <- list()
  
  # Wrangle the dots into a named list
  for (i in seq_along(dots)) {
    name_i <- names(dots[i])
    should_expand <- name_i == ""
    
    # If no value is passed for a variable, use all of its unique levels
    if (should_expand) {
      varname <- as.character(dots[i])
      varval <- unique(model_data[[varname]])
    } else {
      # Otherwise, use the passed value by evaluating it
      varname <- name_i
      varval <- eval(dots[[varname]])
    }
    
    user_args[[varname]] <- varval
  }
  
  # Determine which variables were not passed but are expected by the model
  set_vars <- names(user_args)
  mdl_vars <- colnames(model$data)
  is_set <- mdl_vars %in% set_vars
  names(is_set) <- mdl_vars
  # clusters <- unique(mdl$ranef$group)
  grid_vars <- list()
  
  # If an expected variable was not passed, set it to NA instead, which will
  # marginalize over that predictor. Note that for group-level random effects,
  # you will need to set re_formula = NA in tidybayes::linpred_draws
  for (varname in mdl_vars) {
    grid_vars[[varname]] <- NA
    if (is_set[varname])
      grid_vars[[varname]] <- user_args[[varname]]
  }
  
  # Remove the dependent variable from the grid
  grid_vars[[model$formula$resp]] <- NULL
  
  # Pass the composed grid variables to modelr::data_grid
  # Note: data_grid is supposed to do all this if a model is provided to .model,
  #       but it does not appear to work as expected, hence I wrote this fx.
  modelr::data_grid(model_data, ... = !!!grid_vars)
}

# AUC boilerplate code:
# https://psyc-bayes-notes.netlify.app/generalized-linear-models.html

# Wrapper for getting predictions,excluding random effects
nore_predict <- function(mdl, testdata) {
  predict(mdl, newdata = testdata, re_formula = NA, type = 'response')[,'Estimate']
}

# Wrapper for making ROC curves and saving the model and test data information
# for use in later tables and plots
roc_curve <- function(mdl, test_data, .plot = TRUE) {
  set.seed = 111
  result <- 
    pROC::roc(response = test_data$response_binary,
              predictor = nore_predict(mdl, 
                                       dplyr::select(test_data, 
                                                     -response_binary)),
              plot = .plot, print.auc = TRUE, ci = TRUE)
  
  roc_call <- match.call()
  model_name <- as.character(roc_call[[2]])
  test_data <- as.character(roc_call[[3]])
  
  attr(result, ".model_name") <- model_name
  attr(result, ".test_data") <- test_data
  
  result
}


# Helper for making a table of AUC vals
entable_aucs <- function(...) {
  ROCs <- rlang::dots_list(...) # Allow and extract arbitrary number of args
  
  map_dfr(ROCs, 
          \(x) {
            modelname <- attr(x, ".model_name") # x$call$predictor[[2]]
            testset <- attr(x, ".test_data") #x$call$predictor[[3]]
            aucval <- x$auc[[1]]
            errorval <- (x$ci[[2]] - x$ci[[1]]) /2
            data.frame(TrainModel = as.character(modelname), # Convert symbols
                       TestData  = as.character(testset),    #
                       AUC        = round(aucval,3),
                       Error = round(errorval, 4))
          })
}

# Helper for getting a dataframe of roc curve results
enframe_rocs <- function(...) {
  roc_curves <- rlang::dots_list(...)
  
  map_dfr(roc_curves,
          \(rc) {
            data.frame(TrainModel = attr(rc, '.model_name'),
                       TestData  = attr(rc, '.test_data'),
                       Sensitivity = rc$sensitivities,
                       Specificity = rc$specificities,
                       AUC = rc$auc[[1]],
                       AUC.low = rc$ci[[1]],
                       AUC.high = rc$ci[[2]]
            )
          })
}

# Computes model vs empirical differentials for tcog models
get_cell_differences <- function(mdl, test_data, xvar = 'tcog_f_semitone', ndraws = 1000) {
  grps <- c('pa_step','bt_step')
  
  mdl_formula <- as.character(mdl$formula$formula)
  
  if (grepl("pa_st", mdl_formula[3]))
    return(get_cell_differences.tonal(mdl, test_data,ndraws = ndraws))
  
  use_gp <- grepl('global_pattern', mdl_formula[3])
  marginal_grid <- marginal_data_grid(test_data, mdl)
  
  if (use_gp){
    grps <- c(grps, "global_pattern")
    marginal_grid <- dplyr::select(marginal_grid, -global_pattern)
  }
  
  
  newdata_grid <- 
    test_data |> 
    group_by(across(all_of(grps))) |> 
    summarize(!!sym(xvar) := mean(.data[[xvar]]),
              .groups = 'drop') |> 
    left_join(marginal_grid, by = xvar)
  
  
  pred_draws <- tidybayes::add_epred_draws(newdata = newdata_grid,
                                           object  = mdl,
                                           re.form = NA,
                                           seed = 111,
                                           ndraws = ndraws)
  
  pred_props <- 
    pred_draws |> 
    group_by(across(all_of(grps))) |> 
    summarize(mdl_prop = mean(.epred),
              .groups = 'drop')
  
  empirical_props <-  
    test_data |> 
    group_by(across(all_of(grps))) |> 
    summarize(emp_prop = mean(response_binary),
              .groups = 'drop')
  
  seg_data <- 
    empirical_props |> 
    left_join(pred_props, by = c('pa_step', 'bt_step')) |> 
    mutate(prop_diff = mdl_prop - emp_prop,
           across(c(emp_prop, mdl_prop), \(x) pa_step - .5 + x))
  
  ungroup(seg_data)
  
}

# Computes model vs empirical differentials for the scaling model
get_cell_differences.tonal <- function(mdl, test_data,ndraws = 1000) {
  marginal_grid <- marginal_data_grid(test_data, mdl)
  
  newdata_grid <- 
    test_data |> 
    group_by(pa_step, bt_step) |> 
    summarize(pa_st = mean(pa_st),
              bt_st = mean(bt_st),
              .groups = 'drop') |> 
    left_join(marginal_grid, by = c('pa_st', 'bt_st'))
  
  
  pred_draws <- tidybayes::add_epred_draws(newdata = newdata_grid,
                                           object  = mdl,
                                           re.form = NA,
                                           seed = 111,
                                           ndraws = ndraws)
  
  pred_props <- 
    pred_draws |> 
    group_by(pa_step, bt_step) |> 
    summarize(mdl_prop = mean(.epred))
  
  empirical_props <-  
    test_data |> 
    group_by(pa_step, bt_step) |> 
    summarize(emp_prop = mean(response_binary),
              .groups = 'drop')
  
  seg_data <- 
    empirical_props |> 
    left_join(pred_props, by = c('pa_step', 'bt_step')) |> 
    mutate(prop_diff = mdl_prop - emp_prop,
           across(c(emp_prop, mdl_prop), \(x) pa_step - .5 + x))
  
  ungroup(seg_data)
}

# Plot differential as a 2D heatmap
plot_differential_heatmap <- function(seg_data) {
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
  
  seg_data |> 
    ggplot(aes(x = pa_step, y = bt_step, fill = prop_diff)) +
    type_annotation +
    geom_tile(linewidth = .5,
              color = 'white') +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(11,"RdBu"),
                         guide = guide_colorbar(title = "▽Predicted % - △Empirical %",
                                                frame.color = 'black',
                                                barwidth = unit(4,'in'),
                                                title.position = 'top',
                                                title.hjust = 0.5,
                                                title.theme =  element_text(family = "Segoe UI Symbol",
                                                                            size = 14)),
                         limits = c(-.5,.5), 
                         breaks=c(-.5,-.3,-.1,0, .1, .3, .5),
                         labels=c(-.5,-.3,-.1,'0', .1, .3, .5)) +
    geom_segment(data = seg_data,
                 aes(x = emp_prop, 
                     xend = mdl_prop, 
                     y = bt_step,
                     yend = bt_step),
                 inherit.aes = FALSE,
                 color = 'black',
                 linewidth = 1.5) +
    geom_segment(data = seg_data,
                 aes(x = emp_prop, 
                     xend = mdl_prop, 
                     y = bt_step,
                     yend = bt_step),
                 inherit.aes = FALSE,
                 color = 'white',
                 linewidth = .75) +
    geom_point(data = seg_data,
               aes(x = emp_prop,
                   y = bt_step),
               color = 'black',
               fill = 'white',
               size = 2,
               shape = 24, #△
               inherit.aes = FALSE) +
    geom_point(data = seg_data,
               aes(x = mdl_prop,
                   y = bt_step),
               color = 'black',
               fill = 'white',
               size = 2,
               shape = 25, #▽
               inherit.aes = FALSE) +
    theme_minimal(base_size = 16) +
    xlab("Pitch Accent Step") +
    ylab("Boundary Tone Step") +
    coord_fixed() +
    theme(legend.position = "top", 
          panel.grid = element_blank(),
          legend.box.margin = margin(b = -15),
          legend.box.spacing = unit(0,'pt'),
          legend.key.width = unit(50,'pt'),
          legend.key.height = unit(.4,'cm'),
          axis.text.x = element_text(margin = margin(t = -12)),
          axis.text.y = element_text(margin = margin(r = -12)))
}


# These are just wrapper functions to reduce repetitive typing, suppressWarnings
# is added to get rid of a warning that re.form and seed aren't used (they
# actually are in some of the downstream functions)
pp_check_exp <- function(mdl, title = "", ndraws = 300) {
  suppressWarnings(pp_check(object = mdl,
                            ndraws = ndraws,
                            type = "bars",
                            seed = 111)) +
    ggplot2::ggtitle(title)
}

pp_check_exp_gp <- function(mdl, title = "", ndraws = 300) {
  suppressWarnings(pp_check(object = mdl,
                            ndraws = ndraws,
                            type = "bars_grouped",
                            group = "global_pattern",
                            seed = 111)) +
    ggplot2::ggtitle(title)
}

pp_check_gp <- function(mdl, newdata, title = "", ndraws = 300) {
  suppressWarnings(pp_check(object = mdl,
                            newdata = newdata, 
                            ndraws = ndraws, 
                            type = "bars_grouped",
                            group = 'global_pattern',
                            re.form = NA,
                            seed = 111)) + 
    ggplot2::ggtitle(title)
}

pp_check_only <- function(mdl, newdata, title = "", ndraws = 300) {
  suppressWarnings(pp_check(object = mdl,
                            newdata = newdata, 
                            ndraws = ndraws, 
                            type = "bars",
                            re.form = NA,
                            seed = 111)) + 
    ggplot2::ggtitle(title)
}


map_to_discrete <- function(vals, steps = 11, minx = -.5, maxx=.5) {
  breaks <- seq(minx, maxx, length.out = steps)
  mappings <- vals[NA]
  
  vapply(vals, \(x) sum(x>breaks), 1L)
  
}

#' Draw 3d differential heatmap
#' 
#' Adapted from https://stackoverflow.com/questions/25033419/plot-a-heatmap-with-a-third-dimension
#' 
#' This function takes a dataframe of model-predicted differentials and plots
#' them as a 3D heatmap mapping x=Acc Pitch step, y=End pitch step, z=difference.
#' The color is mapped to the direction of the difference.
#'
#' @param which_data Differential dataframe
#' @param theta Perspective var
#' @param phi Perspective var
#' @param ncols Number of colors to use in the gradient
#'
#' @return Draws a heatmap to the current graphics device
draw_3d_heatmap <- function(which_data, theta = 30, phi = 30, ncols = 21) {

  data = matrix(data = runif(n = 25, min = 0, max = 1), nrow=5, ncol = 5, dimnames=list(paste0('x',1:5),paste0('y',1:5)))
  data = sweep(x = data, MARGIN = 1, 5:1, FUN = '+')
  data = sweep(x = data, MARGIN = 2, 1:5, FUN = '+')
  
  # Draw perspective starter
  pmat = persp(x=c(0,5), y=c(0,5), z=matrix(c(0,0,0,0), nrow=2), 
               xlim=c(0,5), ylim=c(0,5), zlim=c(-.5,.5), 
               xlab='Accentual Pitch Step', ylab='Ending Pitch Step', zlab='|Error|', 
               theta=theta, 
               phi=phi, d=2, 
               box=F) 
  
  
  # Draw far side of the cube
  polygon(trans3d(c(0,0,0,0),
                  c(0,5,5,0),
                  c(.5,.5,0,.0), pmat), col=NA, border=1,lty = 'dashed')
  
  {
    # generate color matrix (values between 1 and 5, corresponding to 5 values my_cols
    {
      exp_data_mat <- 
        which_data |> 
        dplyr::select(pa_step, bt_step, prop_diff) |> 
        pivot_wider(names_from = 'bt_step', 
                    id_cols = 'pa_step', 
                    values_from = 'prop_diff') |> 
        dplyr::select(-pa_step) |>
        as.matrix()
      
      datahold <- data
      dimnames(exp_data_mat) <- list(1:5, 1:5)
      data <- abs(exp_data_mat)
      
      my_cols <- colorRampPalette(RColorBrewer::brewer.pal(11, 'RdBu'))(ncols)
      colmat <- matrix(map_to_discrete(which_data$prop_diff,steps = ncols),
                       ncol = 5,
                       nrow = 5,byrow = T)
    }

    # draw each bar: from left to right ...
    for (i in 1:nrow(data)){
      
      # ... and back to front 
      for (j in ncol(data):1){
        
        xy = as.array(c('row'=i, 'col'=j))#which(data == data[i,j], arr.ind=TRUE)
        
        # side facing y
        A = ifelse(data[i,j] > 0, -1, -1)
        Ax <- ifelse(data[i,j] < 0, -1, 0)
        Ay <- ifelse(data[i,j] < 0, 1, 0)
        # Az <- ifelse(data[i,j] < 0, 1, -1)
        

        x = rep(xy[1]+Ax,4)
        y = c(xy[2]-1,xy[2],xy[2],xy[2]-1)
        z = c(min(c(data[i-1,j], 0)),
              min(c(data[i-1,j], 0)),
              data[i,j],
              data[i,j])
        polygon(trans3d(x, y, z, pmat), col=my_cols[colmat[i,j]], border=1)
        
        #  side facing x
        x = c(xy[1]-1,xy[1],xy[1],xy[1]-1)
        y = rep(xy[2]-1+Ay,4)
        z = c(0,0,data[i,j],data[i,j])
        polygon(trans3d(x, y, z, pmat), col=my_cols[colmat[i,j]], border=1)
        
        # top side
        x = c(xy[1]-1,xy[1],xy[1],xy[1]-1)
        y = c(xy[2]-1,xy[2]-1,xy[2],xy[2])
        z = rep(data[i,j],4)
        polygon(trans3d(x, y, z, pmat), col=my_cols[colmat[i,j]], border=1)
        
      }
    }
    
    {
      # define axis ranges etc
      x.axis <- 1:ncol(data) - 0.5
      min.x <- 0
      max.x <- 5
      y.axis <- 1:nrow(data) - 0.5 
      min.y <- 0
      max.y <- 5
      z.axis <- seq(0, .5, length.out = 5)
      min.z <- 0
      max.z <- .5
      
      # add some distance between tick labels and the axis
      xoffset = .5
      yoffset = 0.5
      zoffset = 0.5
      ticklength = 0.2
      
      # x axis ticks
      tick.start <- trans3d(x.axis, min.y, min.z, pmat)
      tick.end <- trans3d(x.axis, (min.y - ticklength), min.z, pmat)
      segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
      
      # y axis ticks
      tick.start <- trans3d(max.x, y.axis, min.z, pmat)
      tick.end <- trans3d(max.x + ticklength, y.axis, min.z, pmat)
      segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
      
      # z axis ticks
      tick.start <- trans3d(min.x, min.y, z.axis, pmat)
      tick.end <- trans3d(min.x, (min.y - ticklength), z.axis, pmat)
      segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
      
      # x labels
      labels <- rownames(data)
      label.pos <- trans3d(x.axis, (min.y - xoffset), min.z, pmat)
      text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), srt=0, cex=0.6)
      
      # y labels
      labels <- colnames(data)
      label.pos <- trans3d((max.x + yoffset), y.axis, min.z, pmat)
      text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), srt=0, cex=0.6)
      
      # z labels
      labels <- paste0("±",as.character(z.axis*100),"pp")
      label.pos <- trans3d(min.x, (min.y - zoffset), z.axis, pmat)
      text(label.pos$x, label.pos$y, labels=labels, adj=c(1, NA), srt=0, cex=0.6)
      
      
    }
  }
  
  # Draw rest of prism
  polygon(trans3d(c(0,0,5,5),
                  c(0,5,5,0),
                  c(.5,.5,.5,.5), pmat), col=NA, border=1,lty = 'dashed')




  polygon(trans3d(c(5,5,5,5),
                  c(0,5,5,0),
                  c(.5,.5,0,.0), pmat), col=NA, border=1,
          lty = 'dashed')

  # Return the perspective matrix for other annotations as needed
  invisible(pmat)  
}

# Save 3d heatmap figure
save_3d_heatmap <- function(data) {
  plotname <- gsub("diffs", "plot", as.character(ensym(data)))
  
  cairo_pdf(paste0("Figures/", plotname, ".pdf"),width = 5, height = 5)
  draw_3d_heatmap(data)
  dev.off()
  png(paste0("Figures/", plotname, ".png"), width = 5000, height = 5000, res = 300)
  draw_3d_heatmap(data)
  dev.off()
  
  invisible(NULL)
}