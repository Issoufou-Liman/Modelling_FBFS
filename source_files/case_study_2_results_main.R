## simulation ####

ficher <- "output_files/Modelling_FBFS_model_RAW_predictions_Case_study_2.rds"

# bn nodes estimates
Local_constraints_at_initial_stage_estimates <- make_node_states_estimates(bn=network_bn_fit, node='Local_constraints_at_initial_stage', op = 'proba', distr = c('beta', 'unif', 'unif'), state_effects = c(0.9, 0.6, 0.1)) # , state_effects = c(0.9, 0.6, 0.1) # c(0.6, 0.3, 0.1)
Local_constraints_at_development_stage_estimates <- make_node_states_estimates(bn=network_bn_fit, node='Local_constraints_at_development_stage', op = 'proba', distr = c('beta', 'unif', 'unif'), state_effects = c(0.9, 0.6, 0.1))

Local_constraints_at_mid_stage_estimates <- make_node_states_estimates(bn=network_bn_fit, node='Local_constraints_at_mid_stage', op = 'proba', distr = 'beta', state_effects = c(0.9, 0.6, 0.1))
Local_constraints_at_late_stage_estimates <- make_node_states_estimates(bn=network_bn_fit, node='Local_constraints_at_late_stage', op = 'proba', distr = 'beta', state_effects = c(0.9, 0.6, 0.1))
rm(network_bn_fit); gc(verbose=FALSE)

if(!file.exists(ficher)){
  ## Obversed and potential grain yield for various rainfed crops near kisumu county and tigray region
  source("data_files/yield.R")
  
  ## relative biomass accumulation accross development stages
  stage_ratio = c(initial_stage = 0.10,  development_stage = 0.80, mid_stage = 0.95, late_stage = 1)
  
  ## proportion of grain yield relative to biomass yield
  harvest_index = c(0.1, 0.3)
  
  ## A function for making an informed guess of biomass yield at each stage
  ## This can also be used as checklist for the final prediction
  guess_biomass_yield <- function(grain_yield_pot, 
                                  # harvest_index = c(0.1, 0.3),
                                  stage_ratio = c(initial_stage = 0.10,  development_stage = 0.80, mid_stage = 0.95, late_stage = 1)){
    # harvest_index <- runif(1000, harvest_index[1], harvest_index[2]) #
    # total_biomass_pot <- sapply(harvest_index, function(i){
    #   grain_yield_pot/i
    # })
    
    total_biomass_pot <- sapply(stage_ratio, function (i){
      # total_biomass_pot*i
      grain_yield_pot*i
    }, simplify = TRUE, USE.NAMES = TRUE)
    as.data.frame(na.omit(total_biomass_pot))
  }
  
  ## yield potential estimate
  mc_nodes_estimates_biomass_yield_pot <- guess_decisionSupport_estimates (data = list(potential_grain_yield, 
                                                                                       # harvest_index = harvest_index, 
                                                                                       stage_ratio = stage_ratio), 
                                                                           fun = guess_biomass_yield,
                                                                           distr = rep('gamma', 4), 
                                                                           percentiles = c(0.025, 0.5, 0.975), 
                                                                           plot = FALSE, show.output = FALSE)
  rownames(mc_nodes_estimates_biomass_yield_pot$marginal) <- paste0('biomass_Yield_Pot_', rownames(mc_nodes_estimates_biomass_yield_pot$marginal))
  
  ## Attainable yield estimates : Median, Lowest and Highest
  
  mc_nodes_lower_exploitable_biomass_yield_pot <- data.frame(lower = c(initial_stage = 0.85-0.35, mid_stage = 0.80-0.35, development_stage = 0.75-0.35, late_stage = 0.70-0.35),
                                                             median = c(initial_stage = 0.89-0.35, mid_stage = 0.87-0.35, development_stage = 0.80-0.35, late_stage = 0.71-0.35),
                                                             upper = c(initial_stage = 0.90-0.35, mid_stage = 0.90-0.35, development_stage = 0.90-0.35, late_stage = 0.90-0.35),
                                                             distribution = c(initial_stage = 'unif', mid_stage = 'unif', development_stage = 'unif', late_stage = 'unif'),
                                                             method = c(initial_stage = 'fit', mid_stage = 'fit', development_stage = 'fit', late_stage = 'fit'),
                                                             stringsAsFactors = FALSE)
  
  rownames(mc_nodes_lower_exploitable_biomass_yield_pot) <- paste0('lower_exploitable_biomass_yield_pot_', rownames(mc_nodes_lower_exploitable_biomass_yield_pot))
  mc_nodes_lower_exploitable_biomass_yield_pot <- as.estimate(mc_nodes_lower_exploitable_biomass_yield_pot)
  
  mc_nodes_mid_exploitable_biomass_yield_pot <-  data.frame(lower = c(initial_stage = 0.85-0.15, mid_stage = 0.80-0.15, development_stage = 0.75-0.15, late_stage = 0.70-0.15),
                                                            median = c(initial_stage = 0.89-0.15, mid_stage = 0.87-0.15, development_stage = 0.80-0.15, late_stage = 0.71-0.15),
                                                            upper = c(initial_stage = 0.90-0.15, mid_stage = 0.90-0.15, development_stage = 0.90-0.15, late_stage = 0.90-0.15),
                                                            distribution = c(initial_stage = 'unif', mid_stage = 'unif', development_stage = 'unif', late_stage = 'unif'),
                                                            method = c(initial_stage = 'fit', mid_stage = 'fit', development_stage = 'fit', late_stage = 'fit'),
                                                            stringsAsFactors = FALSE)
  
  rownames(mc_nodes_mid_exploitable_biomass_yield_pot) <- paste0('mid_exploitable_biomass_yield_pot_', rownames(mc_nodes_mid_exploitable_biomass_yield_pot))
  mc_nodes_mid_exploitable_biomass_yield_pot <- as.estimate(mc_nodes_mid_exploitable_biomass_yield_pot)
  
  
  mc_nodes_upper_exploitable_biomass_yield_pot <- data.frame(lower = c(initial_stage = 0.85, mid_stage = 0.80, development_stage = 0.75, late_stage = 0.70),
                                                             median = c(initial_stage = 0.89, mid_stage = 0.87, development_stage = 0.80, late_stage = 0.71),
                                                             upper = c(initial_stage = 0.90, mid_stage = 0.90, development_stage = 0.90, late_stage = 0.90),
                                                             distribution = c(initial_stage = 'unif', mid_stage = 'unif', development_stage = 'unif', late_stage = 'unif'),
                                                             method = c(initial_stage = 'fit', mid_stage = 'fit', development_stage = 'fit', late_stage = 'fit'),
                                                             stringsAsFactors = FALSE)
  rownames(mc_nodes_upper_exploitable_biomass_yield_pot) <- paste0('upper_exploitable_biomass_yield_pot_', rownames(mc_nodes_upper_exploitable_biomass_yield_pot))
  mc_nodes_upper_exploitable_biomass_yield_pot <- as.estimate(mc_nodes_upper_exploitable_biomass_yield_pot)
  
  mc_nodes_exploitable_biomass_yield_pot <- list(mc_nodes_lower_exploitable_biomass_yield_pot, mc_nodes_mid_exploitable_biomass_yield_pot, mc_nodes_upper_exploitable_biomass_yield_pot)
  
  ## Average yield estimates: Median, Lowest and Highest
  
  cond <- get_boxplot_range_1d (observed_actual_grain_yield)
  mc_nodes_lower_average_actu_biomass_yield <- observed_actual_grain_yield[observed_actual_grain_yield >= cond[1] & observed_actual_grain_yield < cond[3]]
  mc_nodes_mid_average_actu_biomass_yield <- observed_actual_grain_yield[observed_actual_grain_yield >= cond[3] & observed_actual_grain_yield < cond[5]]
  mc_nodes_upper_average_actu_biomass_yield <- observed_actual_grain_yield[observed_actual_grain_yield >= cond[5] & observed_actual_grain_yield < cond[7]]
  
  mc_nodes_lower_average_actu_biomass_yield <- guess_decisionSupport_estimates (data = list(mc_nodes_lower_average_actu_biomass_yield, 
                                                                                            # harvest_index = harvest_index,
                                                                                            stage_ratio = stage_ratio), fun=guess_biomass_yield, distr = rep('gamma', 4),
                                                                                percentiles = c(0.025, 0.5, 0.975), plot = FALSE, show.output = FALSE)
  rownames(mc_nodes_lower_average_actu_biomass_yield$marginal) <- paste0('lower_average_Actual_yield_', rownames(mc_nodes_lower_average_actu_biomass_yield$marginal))
  
  
  mc_nodes_mid_average_actu_biomass_yield <- guess_decisionSupport_estimates (data = list(mc_nodes_mid_average_actu_biomass_yield, 
                                                                                          # harvest_index = harvest_index,
                                                                                          stage_ratio = stage_ratio), fun=guess_biomass_yield, distr = rep('gamma', 4),
                                                                              percentiles = c(0.025, 0.5, 0.975), plot = FALSE, show.output = FALSE)
  rownames(mc_nodes_mid_average_actu_biomass_yield$marginal) <- paste0('mid_average_Actual_yield_', rownames(mc_nodes_mid_average_actu_biomass_yield$marginal))
  
  
  mc_nodes_upper_average_actu_biomass_yield <- guess_decisionSupport_estimates (data = list(mc_nodes_upper_average_actu_biomass_yield, 
                                                                                            # harvest_index = harvest_index,
                                                                                            stage_ratio = stage_ratio), fun=guess_biomass_yield, distr = rep('gamma', 4),
                                                                                percentiles = c(0.025, 0.5, 0.975), plot = FALSE, show.output = FALSE)
  rownames(mc_nodes_upper_average_actu_biomass_yield$marginal) <- paste0('upper_average_Actual_yield_', rownames(mc_nodes_upper_average_actu_biomass_yield$marginal))
  mc_nodes_average_actu_biomass_yield <- list(mc_nodes_lower_average_actu_biomass_yield, mc_nodes_mid_average_actu_biomass_yield, mc_nodes_upper_average_actu_biomass_yield)
  
  
  ## associate average yield and the exploitable yield potential to their corresponding BNs nodes
  
  tmp <- 1:length(Local_constraints_at_initial_stage_estimates)
  names(tmp) <- names(Local_constraints_at_initial_stage_estimates)
  est_int <- sapply(X=tmp, function(i){
    rbind (Local_constraints_at_initial_stage_estimates [[i]],
           mc_nodes_estimates_biomass_yield_pot, 
           mc_nodes_exploitable_biomass_yield_pot[[i]],
           mc_nodes_average_actu_biomass_yield[[i]]
    )
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  ## Biomass yield at dev stage
  
  mc_nodes_biomass_exp_factor_dev <- estimate(distribution = 'gamma', lower = 0, median = 0.725, upper = 0.850, variable= 'biomass_exp_factor_dev', method = 'fit')
  
  est_dev <- lapply(X=Local_constraints_at_development_stage_estimates, FUN = function(i){
    lapply(est_int, FUN = rbind,
           i,
           mc_nodes_biomass_exp_factor_dev)
  })
  
  ## Biomass yield at mid stage
  
  mc_nodes_biomass_exp_factor_mid <- estimate(distribution = 'gamma', lower = 0, median = 0.900, upper = 0.950, variable= 'biomass_exp_factor_mid', method = 'fit')
  
  est_mid <- lapply(X=Local_constraints_at_mid_stage_estimates, FUN = function(i){
    lapply(est_dev, function(j) {
      lapply(j, function (k){
        rbind(k, i, mc_nodes_biomass_exp_factor_mid)
      })
    })
  })
  
  ## Biomass yield at late stage
  
  mc_nodes_biomass_exp_factor_late <- estimate(distribution = 'gamma', lower = 0, median = 0.975, upper = 1, variable= 'biomass_exp_factor_late', method = 'fit')
  mc_nodes_harvest_index <- estimate(distribution = 'unif', lower = 0.1, median=0.2, upper = 0.3, variable= 'harvest_index', method = 'fit')
  mc_nodes_biomass_exp_factor_init <- estimate(distribution = 'gamma', lower = 0, median = 0.090, upper = 0.100, variable= 'biomass_exp_factor_init', method = 'fit')
  
  est_late <- lapply(X=Local_constraints_at_late_stage_estimates, FUN = function(i){
    lapply(est_mid, function(j) {
      lapply(j, function(k){
        lapply(k, function (l){
          rbind(l, i, 
                mc_nodes_biomass_exp_factor_late, 
                mc_nodes_harvest_index, 
                mc_nodes_biomass_exp_factor_init)
        })
      })
    })
  })
  
  
  ## Running the MC similation
  
  model_function <- function(x){
    # initial stage 
    attainable_yield_at_initial_stage <- x[,2] * x[, 6] 
    exploitable_yield_gap_at_initial_stage <- attainable_yield_at_initial_stage - x[,10]

    exploitable_yield_gap_at_initial_stage <- ifelse(exploitable_yield_gap_at_initial_stage < 0, 0, exploitable_yield_gap_at_initial_stage) # because average yield cannot go beyond attainable. 

    exploitable_yield_gap_loss_due_to_constraints_at_initial_stage <- exploitable_yield_gap_at_initial_stage * x[, 1]

    exploitable_yield_gap_loss_due_to_constraints_at_initial_stage <- ifelse(exploitable_yield_gap_loss_due_to_constraints_at_initial_stage <= exploitable_yield_gap_at_initial_stage, exploitable_yield_gap_loss_due_to_constraints_at_initial_stage, exploitable_yield_gap_at_initial_stage) # because exploitable_yield_gap_loss_due_to_constraints_at_initial_stage cannot exceed the exploitable_yield_gap_at_initial_stage itself.

    actually_exploited_yield_gap_at_initial_stage <- exploitable_yield_gap_at_initial_stage - exploitable_yield_gap_loss_due_to_constraints_at_initial_stage
    expected_actual_yield_at_initial_stage <- actually_exploited_yield_gap_at_initial_stage + x[, 10]
    

    expected_actual_yield_at_initial_stage <- ifelse(expected_actual_yield_at_initial_stage > attainable_yield_at_initial_stage, attainable_yield_at_initial_stage, expected_actual_yield_at_initial_stage) # becuase yield expectation cannot go beyond the attainable limit.

    initial_stage = list(
      # attainable_yield_at_initial_stage = attainable_yield_at_initial_stage,
      exploitable_yield_gap_at_initial_stage = exploitable_yield_gap_at_initial_stage,
      exploitable_yield_gap_loss_due_to_constraints_at_initial_stage = exploitable_yield_gap_loss_due_to_constraints_at_initial_stage,
      actually_exploited_yield_gap_at_initial_stage = actually_exploited_yield_gap_at_initial_stage,
      expected_actual_yield_at_initial_stage=expected_actual_yield_at_initial_stage)
    
    # dev stage
    exploitable_yield_gap_at_development_stage <- exploitable_yield_gap_at_initial_stage + (exploitable_yield_gap_at_initial_stage * x[,15])
    # exploitable_yield_gap_at_development_stage <- (exploitable_yield_gap_at_initial_stage * x[,15])/x[, 21]
    exploitable_yield_gap_loss_due_to_constraints_at_development_stage <- exploitable_yield_gap_at_development_stage * x[, 14]

    exploitable_yield_gap_loss_due_to_constraints_at_development_stage <- ifelse(exploitable_yield_gap_loss_due_to_constraints_at_development_stage <= exploitable_yield_gap_at_development_stage, exploitable_yield_gap_loss_due_to_constraints_at_development_stage, exploitable_yield_gap_at_development_stage) # because exploitable_yield_gap_loss_due_to_constraints_at_development_stage cannot exceed the exploitable_yield_gap_at_development_stage itself.

    actually_exploited_yield_gap_at_development_stage <- exploitable_yield_gap_at_development_stage - exploitable_yield_gap_loss_due_to_constraints_at_development_stage
    # expected_actual_yield_at_development_stage <- actually_exploited_yield_gap_at_development_stage + (expected_actual_yield_at_initial_stage * x[, 15])
    expected_actual_yield_at_development_stage <- actually_exploited_yield_gap_at_development_stage + (expected_actual_yield_at_initial_stage * x[, 15])
    
    dev_stage = list(exploitable_yield_gap_at_development_stage = exploitable_yield_gap_at_development_stage,
                     exploitable_yield_gap_loss_due_to_constraints_at_development_stage = exploitable_yield_gap_loss_due_to_constraints_at_development_stage,
                     actually_exploited_yield_gap_at_development_stage = actually_exploited_yield_gap_at_development_stage,
                     expected_actual_yield_at_development_stage = expected_actual_yield_at_development_stage)
    
    # mid stage
    exploitable_yield_gap_at_mid_stage <- exploitable_yield_gap_at_development_stage + exploitable_yield_gap_at_development_stage * x[,17]
    exploitable_yield_gap_loss_due_to_constraints_at_mid_stage <- exploitable_yield_gap_at_mid_stage * x[, 16]

    exploitable_yield_gap_loss_due_to_constraints_at_mid_stage <- ifelse(exploitable_yield_gap_loss_due_to_constraints_at_mid_stage <= exploitable_yield_gap_at_mid_stage, exploitable_yield_gap_loss_due_to_constraints_at_mid_stage, exploitable_yield_gap_at_mid_stage) # because exploitable_yield_gap_loss_due_to_constraints_at_mid_stage cannot exceed the exploitable_yield_gap_at_mid_stage itself.

    actually_exploited_yield_gap_at_mid_stage <- exploitable_yield_gap_at_mid_stage - exploitable_yield_gap_loss_due_to_constraints_at_mid_stage
    expected_actual_yield_at_mid_stage <-  actually_exploited_yield_gap_at_mid_stage + (expected_actual_yield_at_development_stage * x[, 17])
    
    mid_stage = list(exploitable_yield_gap_at_mid_stage = exploitable_yield_gap_at_mid_stage,
                     exploitable_yield_gap_loss_due_to_constraints_at_mid_stage = exploitable_yield_gap_loss_due_to_constraints_at_mid_stage,
                     actually_exploited_yield_gap_at_mid_stage = actually_exploited_yield_gap_at_mid_stage,
                     expected_actual_yield_at_mid_stage = expected_actual_yield_at_mid_stage)
    
    # late stage
    exploitable_yield_gap_at_late_stage <- exploitable_yield_gap_at_mid_stage + exploitable_yield_gap_at_mid_stage * x[,19]
    exploitable_yield_gap_loss_due_to_constraints_at_late_stage <- exploitable_yield_gap_at_late_stage * x[, 18]

    exploitable_yield_gap_loss_due_to_constraints_at_late_stage <- ifelse(exploitable_yield_gap_loss_due_to_constraints_at_late_stage <= exploitable_yield_gap_at_late_stage, exploitable_yield_gap_loss_due_to_constraints_at_late_stage, exploitable_yield_gap_at_late_stage) # because exploitable_yield_gap_loss_due_to_constraints_at_late_stage cannot exceed the exploitable_yield_gap_at_late_stage itself.

    actually_exploited_yield_gap_at_late_stage <- exploitable_yield_gap_at_late_stage - exploitable_yield_gap_loss_due_to_constraints_at_late_stage
    expected_actual_yield_at_late_stage <-  actually_exploited_yield_gap_at_late_stage + (expected_actual_yield_at_mid_stage * x[,19])
    
    late_stage = list(exploitable_yield_gap_at_late_stage = exploitable_yield_gap_at_late_stage,
                      exploitable_yield_gap_loss_due_to_constraints_at_late_stage = exploitable_yield_gap_loss_due_to_constraints_at_late_stage,
                      actually_exploited_yield_gap_at_late_stage = actually_exploited_yield_gap_at_late_stage,
                      expected_actual_yield_at_late_stage = expected_actual_yield_at_late_stage)
    
    # grain yield
    actually_exploited_yield_gap = actually_exploited_yield_gap_at_late_stage * x[, 20]
    exploitable_yield_gap = exploitable_yield_gap_at_late_stage * x[, 20]
    expected_actual_yield = expected_actual_yield_at_late_stage * x[, 20]
    exploitable_yield_gap_loss_due_to_constraints = exploitable_yield_gap_loss_due_to_constraints_at_mid_stage * x[, 20]
    
    grain_yield = list(exploitable_yield_gap = exploitable_yield_gap, 
                       exploitable_yield_gap_loss_due_to_constraints = exploitable_yield_gap_loss_due_to_constraints,
                       actually_exploited_yield_gap = actually_exploited_yield_gap,
                       expected_actual_yield = expected_actual_yield)
    
    
    ## Outputs
    out <- list(initial_stage = initial_stage, dev_stage = dev_stage, mid_stage = mid_stage, 
                late_stage = late_stage, grain_yield = grain_yield)
    return(out)
  }
  
  
  state_ids = 1:length(est_late)
  names(state_ids) <- names(est_late)
  
  ssp <- sapply(X=state_ids, function (i) {
    sapply(est_late[[i]], function (j){
      sapply(j, function(k){
        sapply(k, function(l){
          # mcSimulation(estimate = l, model_function = model_function, state_effects = state_effects [i], numberOfModelRuns=10000, functionSyntax="matrixNames")
          mcSimulation(estimate = l, model_function = model_function, numberOfModelRuns=10000, functionSyntax="matrixNames")
          
        }, simplify = FALSE, USE.NAMES = TRUE)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)
  saveRDS(ssp, ficher)
} else {
  ssp <- readRDS(ficher)
}

## General settings for all plotting options

p = ggplot_mc_dens(ssp, 
                   colorQuantile = c("red", "green", "green", "green", "green", "green", "red1"),
                   
                   # colorQuantile = c("GRAY46", "lavender", "lavender", "lavender", "lavender", "lavender", "GRAY47"),
                   colorProbability = c(1.00,    0.95,     0.75,     0.55,         0.45,     0.25,     0.05))

gg_data <- p$data

saveRDS(gg_data, paste0("output_files", "/", "Modelling_FBFS_model_predictions_Case_study_2.rds"))

y_scaleFUN <- function(x) sprintf("%.2f", x)
x_scaleFUN <- function(x) sprintf("%.0f", x)

scale_fun <- function(gg_obj, new_data, split_column, facet_column) {
  if (!is.null(new_data)){
    p <- gg_obj %+% new_data
  } else {
    p <- gg_obj
  }
  sapply(unique(p$data[[split_column]]), function(i){
    # p$data[[facet_column]] <- factor(p$data[[facet_column]], levels = unique(p$data[[facet_column]]))
    p_data <- p$data[grepl(i, p$data[[split_column]]), ]
    data_list <- split(p_data, p_data[[facet_column]])
    my_fill <- lapply(data_list, function (x){
      scale_fill_identity(NULL, labels = x$color_equiv, breaks = x$color,
                          guide = 'none', drop = FALSE)
    })
    p_i = p %+% p_data +
      scale_x_continuous(
        labels = y_scaleFUN, 
        expand = c(0,0)
      )+
      scale_y_continuous(
        labels = x_scaleFUN, 
        expand = c(0,0)
      )+
      my_fill+
      my_theme + 
      theme(axis.title = element_blank(),
            plot.subtitle=element_text(color='darkgray'))+
      labs(subtitle = gsub("_", " ", gsub("initial_stage.|at_initial_stage|dev_stage.|at_development_stage|mid_stage.|at_mid_stage|late_stage.|at_late_stage|grain_yield.", "", i)))
    p_i +     scale_x_continuous(
      labels = x_scaleFUN, 
      expand = c(0,0)
    )+
      scale_y_continuous(
        labels = y_scaleFUN, 
        expand = c(0,0))
  }, simplify = FALSE, USE.NAMES = TRUE)
}

grid_fun <- function(gg_list) {
  tmp <- 1:length(gg_list)
  names(tmp) <- names(gg_list)
  sapply(tmp, function(i){
    if (i == 1){
      gg_list[[i]] +
        my_theme+
        theme(
          # strip.background = element_rect(fill='lightgoldenrodyellow'),
          plot.title = element_blank(),
          axis.title = element_blank(),
          plot.margin=unit(c(1, 1, 0, 0), "pt"),
          strip.text = element_text(colour = 'black', size = 7),
          axis.text = element_text(size = plot_font_size-1),
          strip.text.x = element_text(margin = margin(t=1, b = 1))
          
        )
    } else {
      gg_list[[i]]+ guides(fill = FALSE)+
        my_theme +
        theme(
          plot.title = element_blank(),
          strip.text = element_blank(),
          strip.background = element_blank(),
          plot.margin=unit(c(5, 1, 0, 0), "pt"),
          # panel.border = element_rect(fill=NA, size = 0.05),
          # panel.grid.major = element_line(size = 0.05),
          # panel.grid.minor = element_line(size = 0.02),
          axis.title = element_blank(),
          axis.text = element_text(size = plot_font_size-1),
          strip.text.x = element_text(margin = margin(t=1, b = 1))
        )
    }
  }, simplify = FALSE, USE.NAMES = TRUE)
}

limits_fun <- function(gg_list, facet_column){
  sapply(gg_list, function(i){
    gg_data <- i$data
    ids <- unique(gg_data[[facet_column]])
    names(ids) <- ids
    sapply(ids, function(j){
      tmp <- gg_data[gg_data[[facet_column]] == j, 'y']
      range(tmp)
    })
  }, simplify = F)
}
## Biomass yield at initial stage ####
dat <- gg_data[grepl("initial_stage.", gg_data$L5), ]
dat$L5 <- factor(dat$L5, levels = unique(dat$L5))
dat <- split(dat, dat$L5)
tmp <- 1:length(dat)
names(tmp) <- names(dat)
# min_y <- c(0.003, 0.04, 0.003, 0.003)
# dat <- sapply(tmp, function(i){
#   dat[[i]] [dat[[i]]$y > min_y[i], ]
# }, simplify = FALSE, USE.NAMES = TRUE)
dat <- do.call(rbind, dat)

# dat$L4 <- factor(dat$L4, levels = unique(dat$L4))
# p1 <- grid_fun(scale_fun(gg_obj = p, new_data = dat, split_column = 'L5', facet_column = 'L4'))

# labeller0 <- unique(gg_data$L4)
# labeller1 <- gsub(pattern = 'at_initial_stage', replacement = '', labeller0)
# labeller1 <- gsub(pattern = 'Local', replacement = 'Farming', labeller1)
# labeller1 <- gsub(pattern = '_', replacement = ' ', labeller1)
# labeller1 <- gsub(pattern = '=', replacement = ' = ', labeller1)
# names(labeller1) <- labeller0

dat$L4 <- gsub(pattern = 'Local_constraints_', replacement = "{bold('FC'", dat$L4)
dat$L4 <- gsub(pattern = 'at_late_stage', replacement = "[italic(iiii)]", dat$L4)

dat$L4 <- gsub(pattern = 'at_mid_stage', replacement = "[italic(iii)]", dat$L4)
dat$L4 <- gsub(pattern = 'at_development_stage', replacement = "[italic(ii)]", dat$L4)
dat$L4 <- gsub(pattern = 'at_initial_stage', replacement = "[italic(i)]", dat$L4)
dat$L4 <- gsub(pattern = '=', replacement = "==", dat$L4)
dat$L4 <- gsub(pattern = '/', replacement = ")} / ", dat$L4)
dat$L4 <- paste0(dat$L4, ")}")
dat$L4 <- gsub(pattern = 'Medium', replacement = 'Mid', dat$L4)

dat$L4 <- factor(dat$L4, levels = unique(dat$L4))
levels(dat$L4) <- c('High', 'Medium', 'Low')

dat$facet_twick <- rep("{bold('Farming constraints during the initial stage')}", nrow(dat))

p1 <- grid_fun(scale_fun(gg_obj = p, new_data = dat, split_column = 'L5', facet_column = 'L4'))

## facet_wrap with ribbon

x_limits <- list(c(-0.50, 4.00), 
                 c(-0.10, 10.00), 
                 c(-0.50, 4.00),
                 c(-0.75, 4.00))

x_breaks <- list(seq(0.00, 4.00, by=1.00),
                 seq(0.00, 10.00, by=5.00),
                 seq(0.00, 4.00, by=1.00),
                 seq(0.00, 4.00, by=1.00))

tmp <- 1:length(p1)
names(tmp) <- names(p1)
p1 <- sapply(X = tmp, function(i) {
  p_i <- p1[[i]] + 
    # geom_line(aes(y = y))+
    # geom_ribbon(aes(ymin = 0, ymax = y, fill = color), alpha = 0.5) +
    aes(x, y, fill = color, color = color)+
    # scale_y_continuous(position = "right")+
    scale_fill_identity(NULL, labels = p1[[1]]$data$color_equiv, 
                        breaks = p1[[1]]$data$color, guide = "legend", drop = FALSE)+
    scale_color_identity(NULL, labels = p1[[1]]$data$color_equiv, 
                         breaks = p1[[1]]$data$color, guide = "legend", drop = FALSE)+
    facet_wrap(.~facet_twick+L4, nrow= 1, labeller = label_parsed, scales = "free_x")+
    
    theme(legend.position = "none")
  p_i_subtitle <- ggplot_build(p_i)$plot$labels$subtitle
  p_i_subtitle <- strwrap(p_i_subtitle, width=18)
  p_i_subtitle <- paste0(p_i_subtitle, collapse = '\n')
  p_i_subtitle <- grid::textGrob(p_i_subtitle, rot = 0,
                                 gp = gpar(fontfamily=plots_font_family,fontsize=plot_font_size, col="black"))
  
  if (i==2){
    p_i <- p_i +
      scale_x_continuous(breaks = x_breaks[[i]], limits = x_limits[[i]], labels = x_scaleFUN)+
      # scale_y_continuous(labels = x_scaleFUN) # use only 1 digit for this one to fit the figures on grid.
      scale_y_continuous(position = "right", labels = function(x) sprintf("%.1f", x))+ # use only 1 digit for this one to fit the figures on grid.
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
        )
    
    p_i
  } else {
    if (i == 1){
      p_i <- p_i +
        ggh4x::facet_nested(.~facet_twick+L4,
                               # nrow= 1,
                               labeller = label_parsed, scales = "free_x")+
        
        scale_x_continuous(breaks = x_breaks[[i]], limits = x_limits[[i]], labels = x_scaleFUN)+
        scale_y_continuous(position = "right", breaks = c(0, 0.5, 1), labels = y_scaleFUN)+ # making some space for this one as labels overlap.
        theme(strip.text = element_text(size = plot_font_size, face = "italic"))
    } else {
      p_i <- p_i +
        scale_x_continuous(breaks = x_breaks[[i]], limits = x_limits[[i]], labels = x_scaleFUN)+
        scale_y_continuous(position = "right", labels = y_scaleFUN)+
        theme(
          strip.background = element_blank(),
          strip.text.x = element_blank()
          )
    }
    p_i
  }
  return(grid.arrange(p_i_subtitle, p_i+
                        theme(plot.subtitle = element_blank(),
                              panel.spacing = unit(0.2,"line")
                        ), 
                      layout_matrix = cbind(1, 2, 2, 2, 2, 2)))
}, simplify = FALSE, USE.NAMES = T)


p1$ncol=1

p1$top = grid::textGrob("Initial crop development stage",
                        x=0, hjust=0, vjust=0.5,
                        gp = gpar(fontfamily=plots_font_family,fontsize=plot_font_size+4,fontface="bold", col="black"))

p1$right = grid::textGrob("Density", rot = 90,  
                          gp = gpar(fontfamily=plots_font_family,fontsize=plot_font_size, col="black"))

p1$bottom <-  grid::textGrob(expression('Total biomass (Mg ha'^-1*')'), 
                             gp = gpar(fontfamily=plots_font_family,fontsize=plot_font_size,  col="black"))
p1$layout_matrix = rbind(1,1,1,1,2,2,2,3,3,3,4,4,4)

grid.newpage()
ga = do.call("grid.arrange", p1)
gb = grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
               gp=gpar(lwd=1, fill=NA, col="lightgray"))
g1 <- gTree(children = gList(ga, gb))
ggsave(plot=g1, device = 'png', filename = "figures/Modelling_FBFS_biomass_yield_initial_stage.png", width = max_plots_width_in, 
       height =max_plots_height_in/2.25, units = "in", dpi = min_plots_res)

## Biomass yield at late stage ####

dat <- gg_data[grepl("late_stage.", gg_data$L5), ]
dat$L5 <- factor(dat$L5, levels = unique(dat$L5))

dat$facet_query <- paste(dat$L1, dat$L2, dat$L3, dat$L4, sep = " / ")
dat$facet_query <- gsub(pattern = 'Local_constraints_', replacement = "{bold('FC'", dat$facet_query)
dat$facet_query <- gsub(pattern = 'at_late_stage', replacement = "[italic(iiii)]", dat$facet_query)

dat$facet_query <- gsub(pattern = 'at_mid_stage', replacement = "[italic(iii)]", dat$facet_query)
dat$facet_query <- gsub(pattern = 'at_development_stage', replacement = "[italic(ii)]", dat$facet_query)
dat$facet_query <- gsub(pattern = 'at_initial_stage', replacement = "[italic(i)]", dat$facet_query)
dat$facet_query <- gsub(pattern = '=', replacement = "==", dat$facet_query)
dat$facet_query <- gsub(pattern = '/', replacement = ")} / ", dat$facet_query)
dat$facet_query <- paste0(dat$facet_query, ")}")
cond <- (dat$L1 == "Local_constraints_at_late_stage=High" & dat$L2 == "Local_constraints_at_mid_stage=High" & dat$L3 == "Local_constraints_at_development_stage=High" & dat$L4 == "Local_constraints_at_initial_stage=High")|
  (dat$L1 == "Local_constraints_at_late_stage=Medium" & dat$L2 == "Local_constraints_at_mid_stage=Medium" & dat$L3 == "Local_constraints_at_development_stage=Medium" & dat$L4 == "Local_constraints_at_initial_stage=Medium")|
  (dat$L1 == "Local_constraints_at_late_stage=Low" & dat$L2 == "Local_constraints_at_mid_stage=Low" & dat$L3 == "Local_constraints_at_development_stage=Low" & dat$L4 == "Local_constraints_at_initial_stage=Low")

dat <- dat[cond, ]

dat$facet_query <- gsub(pattern = 'Medium', replacement = 'Mid', dat$facet_query)


dat$facet_query <- factor(dat$facet_query, levels = unique(dat$facet_query)[c(3, 1, 2)])
levels(dat$facet_query) <- c('High', 'Medium', 'Low')

dat <- split(dat, dat$L5)
tmp <- 1:length(dat)
names(tmp) <- names(dat)

dat <- do.call(rbind, dat)

## facet_wrap with ribbon

x_limits <- list(c(-2.50, 20.00),
                 c(-0.50, 15.00),
                 c(-2.00, 20.00),
                 c(-3.00, 30.00))

x_breaks <- list(seq(0.00, 20.00, by=10.00),
                 seq(0.00, 15.00, by=5.00),
                 seq(0.00, 20.00, by=10.00),
                 seq(0.00, 30.00, by=10.00))

dat$facet_twick <- rep("{bold('Farming constraints during the initial, development, mid, and late stages')}", nrow(dat))
p1 <- grid_fun(scale_fun(gg_obj = p, new_data = dat, split_column = 'L5', facet_column = 'facet_query'))

tmp <- 1:length(p1)
names(tmp) <- names(p1)


p1 <- sapply(X = tmp, function(i) {
  p_i <- p1[[i]] + 
    # geom_line(aes(y = y))+
    # geom_ribbon(aes(ymin = 0, ymax = y, fill = color), alpha = 0.5) +
    aes(x, y, fill = color, color = color)+
    # scale_y_continuous(position = "right")+
    scale_fill_identity(NULL, labels = p1[[1]]$data$color_equiv, 
                        breaks = p1[[1]]$data$color, guide = "legend", drop = FALSE)+
    scale_color_identity(NULL, labels = p1[[1]]$data$color_equiv, 
                         breaks = p1[[1]]$data$color, guide = "legend", drop = FALSE)+
    facet_wrap(.~facet_twick+facet_query, nrow= 1, labeller = label_parsed, scales = "free_x")+
    
    theme(legend.position = "none")
  p_i_subtitle <- ggplot_build(p_i)$plot$labels$subtitle
  p_i_subtitle <- strwrap(p_i_subtitle, width=18)
  p_i_subtitle <- paste0(p_i_subtitle, collapse = '\n')
  p_i_subtitle <- grid::textGrob(p_i_subtitle, rot = 0,hjust = 0.5, vjust = 0.5,
                                 gp = gpar(fontfamily=plots_font_family,fontsize=plot_font_size, col="black"))
  
  if (i==2){
    p_i <- p_i +
      scale_x_continuous(breaks = x_breaks[[i]], limits = x_limits[[i]], labels = x_scaleFUN)+
      # scale_y_continuous(labels = x_scaleFUN) # use only 1 digit for this one to fit the figures on grid.
      scale_y_continuous(position = "right", labels = y_scaleFUN)+ # use only 1 digit for this one to fit the figures on grid.
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
      )
    p_i
  } else {
    if (i == 1){
      p_i <- p_i +
        ggh4x::facet_nested(.~facet_twick+facet_query,
                               # nrow= 1,
                               labeller = label_parsed, scales = "free_x")+
        
        scale_x_continuous(breaks = x_breaks[[i]], limits = x_limits[[i]], labels = x_scaleFUN)+
        scale_y_continuous(breaks = c(0, 0.10, 0.20), position = "right", labels = y_scaleFUN)+ # making some space for this one as labels overlap.
        theme(strip.text = element_text(size = plot_font_size, face = "italic"))
    } else {
      p_i <- p_i +
        scale_x_continuous(breaks = x_breaks[[i]], limits = x_limits[[i]], labels = x_scaleFUN)+
        scale_y_continuous(position = "right", labels = y_scaleFUN)+
        theme(
          strip.background = element_blank(),
          strip.text.x = element_blank()
        )
    }
    p_i
  }
  return(grid.arrange(p_i_subtitle, p_i+
                        theme(plot.subtitle = element_blank(),
                              panel.spacing = unit(0.2,"line")
                        ), 
                      layout_matrix = cbind(1, 2, 2, 2, 2, 2)))
}, simplify = FALSE, USE.NAMES = T)



p1$ncol=1

p1$top = grid::textGrob("Late crop development stage",
                        x=0, hjust=0, vjust=0.5,
                        gp = gpar(fontfamily=plots_font_family, fontsize=plot_font_size+4, fontface = "bold", col="black"))

p1$right = grid::textGrob("Density", rot = 90,
                          gp = gpar(fontfamily=plots_font_family,fontsize=plot_font_size,  col="black"))

p1$bottom <-  grid::textGrob(expression('Total biomass (Mg ha'^-1*')'),
                             gp = gpar(fontfamily=plots_font_family,fontsize=plot_font_size,  col="black"))
p1$layout_matrix = rbind(1,1,1,1,2,2,2,3,3,3,4,4,4)
grid.newpage()
ga = do.call("grid.arrange", p1)
gb = grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
               gp=gpar(lwd=1, fill=NA, col="lightgray"))
g2 <- gTree(children = gList(ga, gb))

ggsave(plot=g2, device = 'png', filename = "figures/Modelling_FBFS_biomass_yield_late_stage.png", width = max_plots_width_in, 
       height =max_plots_height_in/2.25, units = "in", dpi = min_plots_res)

## Biomass yield at initial and late stage ####

g <- list(g1, nullGrob(), g2)
g$heights = unit(c(1, 0.025, 1), 'null')
g$ncol=1

grid.newpage()
ga = do.call("grid.arrange", g)
gb = grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
               gp=gpar(lwd=1, fill=NA, col="lightgray"))
g1 <- gTree(children = gList(ga, gb))

ggsave(plot=g1, device = 'png', filename = "figures/Modelling_FBFS_biomass_yield_Initial_and_late_stage.png", width = max_plots_width_in,
       height =2*max_plots_height_in/2.15, units = "in", dpi = min_plots_res
)
ggsave(plot=g1, device = 'pdf', filename = "figures/Modelling_FBFS_biomass_yield_Initial_and_late_stage.pdf", width = max_plots_width_in,
       height =2*max_plots_height_in/2.15, units = "in", dpi = min_plots_res
)
