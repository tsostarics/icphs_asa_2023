# library(tidyverse) # Tidy utils
# library(lme4)      # Modeling
# library(ggeffects) # Predictions
# library(pROC)      # ROC curves
# library(furrr)     # Parallelization

# response ~ pa + boundary + pa:boundary + (1|subj)
# or, response ~ erb_difference

# Convert a given Hz value to erbs
hz_to_erb <- function(freq) { 
  # Equation from https://www2.ling.su.se/staff/hartmut/bark.htm
  vapply(freq, 
         function(f)
           11.17*log((f + 312)/(f + 14675))+43.0,
         1.0)
}

# Convert a given erb value to Hz
erb_to_hz <- function(erb) {
  # Back transformed from the equation in 
  # https://www2.ling.su.se/staff/hartmut/bark.htm
  E <- exp((erb - 43)/11.17)
  (312 - 14675*E) / (E - 1)
}

# Add some number of erbs to given frequencies, returns a matrix
add_erb <-  function(freqs, erbs) {
  sapply(freqs,
         function(f) {
           vapply(erbs, 
                  function(erb)
                    erb_to_hz(hz_to_erb(f) + erb),
                  1.0)
         })
}

# Calculate the difference in erbs between two frequencies in Hz
erb_difference <- function(f1, f2) {
  stopifnot(length(f1) == length(f2))
  
  vapply(seq_along(f1),
         function(i)
           hz_to_erb(f1[i]) - hz_to_erb(f2[i]),
         1.0
  )
}

# Calculate difference in semitones between two frequencies
# Vectorized to calculate differences of pairs
semitone_difference <- function(f1, f2) { 
  stopifnot(length(f1) == length(f2))
  
  vapply(seq_along(f1),
         function(i)
           log(f1[i] / f2[i]) * (12/log(2)),
         1.0
  )
}

# Add some number of semitones to given frequencies
# Returns a matrix 
add_semitone <- function(freqs, sts = 1) {
  sapply(freqs,
         function(f) {
           vapply(sts, 
                  function(st)
                    f * 2^(st/12),
                  1.0)
         })
  
}

generate_response_accents <- function(pa_values, 
                                      bt_values, 
                                      intercept = 0,
                                      pa_weight = 1, 
                                      bt_weight = -1,
                                      int_weight = .25,
                                      subj_mean,
                                      pa_slope_sd = .8,
                                      bt_slope_sd = .8) {
  stopifnot(length(pa_values) == length(bt_values))
  
  indices <- seq_along(pa_values)
  
  pa_slope <- rnorm(1, 0, pa_slope_sd)
  bt_slope <- rnorm(1, 0, bt_slope_sd)
  
  vapply(indices,
         function(i) {
           bt_val <- bt_values[[i]]
           pa_val    <- pa_values[[i]]
           xb <- 
             intercept + 
             (pa_slope + pa_weight)*pa_val + 
             (bt_slope + bt_weight)*bt_val + 
             int_weight*bt_val*pa_val + 
             subj_mean[[i]]
           
           p <- 1 / (1 + exp(-xb))
           
           response <- rbinom(n = 1, size = 1, prob = p)
           response
         },
         1L)
  
}


generate_response_erbdiff <- function(erb_diff_values,
                                      intercept = 0,
                                      erb_diff_weight = -2,
                                      subj_mean) {
  indices <- seq_along(erb_diff_values)
  
  vapply(indices,
         function(i) {
           erb_val    <- erb_diff_values[[i]]
           xb <- intercept + erb_diff_weight*erb_val + subj_mean[[i]]
           p <- 1 / (1 + exp(-xb))
           
           response <- rbinom(n = 1, size = 1, prob = p)
           response
         },
         1L)
  
}


do_simulations <- function(steps = 5, 
                           n_reps,
                           n_subj,
                           start_endpoints = c(140, 220),
                           erb_endpoints = c(.5, 3.5),
                           intercept = 0,
                           pa_weight = 1, 
                           bt_weight = -1, 
                           int_weight = .25,
                           erb_diff_weight = -2,
                           pa_slope_sd = .8,
                           bt_slope_sd = .8,
                           alpha = .01, 
                           simulation_i) {
  future::plan(multisession)
  furrr::future_map_dfr(seq_len(simulation_i),
         function(i) {
           run_simulation(steps, 
                          n_reps, 
                          n_subj, 
                          start_endpoints,
                          erb_endpoints,
                          intercept,
                          pa_weight, 
                          bt_weight,
                          int_weight,
                          erb_diff_weight,
                          pa_slope_sd,
                          bt_slope_sd,
                          alpha, 
                          simulation_i = i)
         },
         .options = furrr_options(seed = TRUE))
}

make_continuum_data <- function(start_endpoints = c(70, 110),
                                erb_endpoints = c(-.25, 2),
                                steps = 5) {
  
  # Create the pitch target continuua
  pa_continuum <-  seq(start_endpoints[[1]],
                       start_endpoints[[2]],
                       length.out = steps)
  erb_continuum  <- seq(erb_endpoints[[1]],
                        erb_endpoints[[2]],
                        length.out = steps)
  boundary_continuum <- as.vector(add_erb(pa_continuum[[1L]], 
                                          erb_continuum))
  # Extract the midpoint value for centering purposes
  midpoint_start <- hz_to_erb(median(pa_continuum))
  
  # Extract the midpoint value for centering purposes
  midpoint_start <- hz_to_erb(median(pa_continuum))
  
  # Create a frame for all the experimental manipulations (crossing stimuli etc.)  
  continuum_df <- 
    expand.grid(start = pa_continuum,  # Interpolate between two pitch accent targets
                end = boundary_continuum) %>% # Interpolate between different slopes (erb differences) and add to the bottom point 
    cbind(expand.grid(pa_step = seq_len(steps), 
                      bt_step = seq_len(steps))
    ) %>% # Apply step labels
    mutate(erb_diff = erb_difference(end, start),    # Calculate erb difference
           st_diff = semitone_difference(end, start),# Calculate st difference
           start_erb = hz_to_erb(start),             # Convert hz to erb
           end_erb = hz_to_erb(end),                 #
           start_erb_centered = start_erb - midpoint_start,    # Center BY ERB
           end_erb_centered = start_erb_centered + erb_diff) #
  
  continuum_df
}

make_sample_data <- function(steps = 5, 
                             n_reps,
                             n_subj,
                             start_endpoints = c(140, 220),
                             erb_endpoints = c(.5, 3.5),
                             intercept = 0,
                             pa_weight = 1, 
                             bt_weight = -1, 
                             int_weight = .25,
                             erb_diff_weight = -2,
                             pa_slope_sd = .8,
                             bt_slope_sd = .8) {
  # Create the pitch target continuua
  pa_continuum <-  seq(start_endpoints[[1]],
                       start_endpoints[[2]],
                       length.out = steps)
  erb_continuum  <- seq(erb_endpoints[[1]],
                        erb_endpoints[[2]],
                        length.out = steps)
  boundary_continuum <- as.vector(add_erb(pa_continuum[[1L]], 
                                          erb_continuum))
  # Extract the midpoint value for centering purposes
  midpoint_start <- hz_to_erb(median(pa_continuum))

  # Create a frame for all the experimental manipulations (crossing stimuli etc.)  
  continuum_df <- 
    expand.grid(start = pa_continuum,  # Interpolate between two pitch accent targets
                end = boundary_continuum) %>% # Interpolate between different slopes (erb differences) and add to the bottom point 
    cbind(expand.grid(start_step = seq_len(steps), 
                      end_step = seq_len(steps))
    ) %>% # Apply step labels
    mutate(erb_diff = erb_difference(end, start),    # Calculate erb difference
           st_diff = semitone_difference(end, start),# Calculate st difference
           start_erb = hz_to_erb(start),             # Convert hz to erb
           end_erb = hz_to_erb(end),                 #
           start_erb_centered = start_erb - midpoint_start,    # Center BY ERB
           end_erb_centered = start_erb_centered + erb_diff) #
  
  # Create the frame for the number of participants and reps, then join
  # it to the stimuli frame to get an experiment's worth of results
  expand.grid(start_step    = seq_len(steps), 
              end_step = seq_len(steps), 
              repetition = seq_len(n_reps),
              subject = seq_len(n_subj)) %>% 
    left_join(continuum_df, by = c('start_step','end_step')) %>% 
    group_by(subject) %>% 
    # Add random intercepts (normally distributed)
    mutate(subj_mean = rnorm(1, 0, .25)) %>% 
    # Create responses using 2 methods: taking PA/BT into consideration vs just slope
    mutate(response_accents = generate_response_accents(start_erb_centered,
                                                        end_erb_centered,
                                                        intercept, 
                                                        pa_weight,
                                                        bt_weight, 
                                                        int_weight, 
                                                        subj_mean,
                                                        pa_slope_sd,
                                                        bt_slope_sd),
           response_erbdiff = generate_response_erbdiff(erb_diff, intercept, erb_diff_weight, subj_mean))
}

run_simulation <- function(steps = 5,
                           n_reps = 3,
                           n_subj = 40,
                           start_endpoints = c(140, 220),
                           erb_endpoints = c(.5, 3.5),
                           intercept = 0,
                           pa_weight = 1, 
                           bt_weight = -.5, 
                           int_weight = -1,
                           erb_diff_weight = -2,
                           pa_slope_sd = .8,
                           bt_slope_sd = .8,
                           alpha = .01,
                           simulation_i) {
  
  sample_data <- make_sample_data(steps,
                                  n_reps,
                                  n_subj,
                                  start_endpoints,
                                  erb_endpoints,
                                  intercept,
                                  pa_weight, 
                                  bt_weight , 
                                  int_weight,
                                  erb_diff_weight,
                                  pa_slope_sd,
                                  bt_slope_sd)
  
  
  mdl <- glmer(response_accents ~ start_erb_centered * end_erb_centered + (1+start_erb_centered|subject) + (0+end_erb_centered|subject), 
               data = sample_data, family = 'binomial',
               control = glmerControl('bobyqa')) 
  mdlsum <- summary(mdl)
  
  message(glue::glue("Simulation #{simulation_i} of size {n_reps} reps & {n_subj} subjects @ {Sys.time()}"))
  
  data.frame(sim_i = simulation_i,
             steps = steps,
             n_reps = n_reps,
             n_subj = n_subj,
             converged = mdl@optinfo$conv$opt,
             singular  = length(mdl@optinfo$conv$lme4),
             term = rownames(mdlsum$coefficients),
             estimate = mdlsum$coefficients[,1],
             true_value = c(intercept, pa_weight, bt_weight, int_weight),
             parameter_error = mdlsum$coefficients[,1] - c(intercept, pa_weight, bt_weight, int_weight),
             p = vapply(mdlsum$coefficients[,4], \(p) p < alpha, TRUE))
}

plot_roc_curves <- function(models, 
                            dataset, 
                            colors = c('red', 'blue'), 
                            legend.names = NA, 
                            is_test_set = FALSE,
                            ...) {
  # Dataset can either be the original dataset or a new dataset
  col_i = 1
  for (model_i in seq_along(models)) {
    roc_data <- dataset
    if (is_test_set){
      roc_data$probs <- predict(models[[model_i]], dataset, type = 'response',re.form=~0)
      
    } else {
      roc_data$probs <- predict(models[[model_i]], dataset, type = 'response')
    }
    if (model_i == 1){ 
      plot(roc(response_accents ~ probs, roc_data), ...)
    } else {
      lines(roc(response_accents ~ probs, roc_data), col = colors[[col_i]])
      col_i = col_i + 1
    }
    
  }
  if(!any(is.na(legend.names)))
    legend(.4, .4, legend = legend.names,
           col = c('black', colors),lwd = 1)
}
