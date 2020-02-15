## constructing the evidence list ####

evidence <- c("Pest_and_disease_impacts_at_mid_stage", "Weed_impacts_at_mid_stage", "Crop_type", "Available_soil_water_at_mid_stage")

evidence <- sapply(evidence, function (i){
  out <- dimnames(net[[i]][['prob']])[[1]]
  # Consider the extremes only 
  out[c(1, length(out))]
}, simplify = FALSE)

evidence <- expand.grid(evidence)
evidence <- sapply(evidence, as.character)

make_evidence <- function(evidence_matrix){
  tmp <- 1:nrow(evidence_matrix)
  names(tmp) <- paste0('Traitment_', tmp)
  sapply(tmp, function(i){
    out <- evidence_matrix[i, ]
    names(out) <- colnames(evidence_matrix)
    out
  }, simplify = FALSE)
}

evidence_list <- make_evidence(evidence)
evidence_list_rice <- sapply(evidence_list, function(i) {
  if (i[['Crop_type']] == "Rice"){
    return(TRUE)
  } else {
    return(FALSE)
  }
})
evidence_list_sorghum <- evidence_list[!evidence_list_rice]
evidence_list_rice <- evidence_list[evidence_list_rice]

## Running the MC similation
model_function <- function(x){
  # initial stage
  
  attainable_yield_at_initial_stage <- x[, 2] * x[, 6]
  
  exploitable_yield_gap_at_initial_stage <- attainable_yield_at_initial_stage - x[, 10] # This holds in normal situation
  exploitable_yield_gap_at_initial_stage <- ifelse(exploitable_yield_gap_at_initial_stage < 0, 0, exploitable_yield_gap_at_initial_stage) # because average yield cannot go beyond attainable.
  bio_exp <- x[, 21] - (x[, 21] * x[, 1]) # Farming Constraints alters the normal situation assuming that crop growth depends on farming constraints
  exploitable_yield_gap_at_initial_stage <-  exploitable_yield_gap_at_initial_stage + (exploitable_yield_gap_at_initial_stage * bio_exp) #
  
  exploitable_yield_gap_loss_due_to_constraints_at_initial_stage <- exploitable_yield_gap_at_initial_stage - (exploitable_yield_gap_at_initial_stage * x[, 1])
  exploitable_yield_gap_loss_due_to_constraints_at_initial_stage <- ifelse(exploitable_yield_gap_loss_due_to_constraints_at_initial_stage <= exploitable_yield_gap_at_initial_stage, exploitable_yield_gap_loss_due_to_constraints_at_initial_stage, exploitable_yield_gap_at_initial_stage) # because exploitable_yield_gap_loss_due_to_constraints_at_initial_stage cannot exceed the exploitable_yield_gap_at_initial_stage itself.
  
  actually_exploited_yield_gap_at_initial_stage <- exploitable_yield_gap_at_initial_stage - exploitable_yield_gap_loss_due_to_constraints_at_initial_stage
  # expected_actual_yield_at_initial_stage <- actually_exploited_yield_gap_at_initial_stage + x[, 4]
  expected_actual_yield_at_initial_stage <- actually_exploited_yield_gap_at_initial_stage + (attainable_yield_at_initial_stage-exploitable_yield_gap_at_initial_stage) # the second term is the new translation of average yield ([,4]) accounting for farming constraints
  
  expected_actual_yield_at_initial_stage <- ifelse(expected_actual_yield_at_initial_stage > attainable_yield_at_initial_stage, attainable_yield_at_initial_stage, expected_actual_yield_at_initial_stage) # becuase yield expectation cannot go beyond the attainable limit.
  
  initial_stage = list(
    exploitable_yield_gap_at_initial_stage = exploitable_yield_gap_at_initial_stage,
    exploitable_yield_gap_loss_due_to_constraints_at_initial_stage = exploitable_yield_gap_loss_due_to_constraints_at_initial_stage,
    actually_exploited_yield_gap_at_initial_stage = actually_exploited_yield_gap_at_initial_stage,
    expected_actual_yield_at_initial_stage=expected_actual_yield_at_initial_stage)
  
  # dev stage
  bio_exp <- x[,15] - (x[, 15] * x[, 14])
  
  exploitable_yield_gap_at_development_stage <- exploitable_yield_gap_at_initial_stage + (exploitable_yield_gap_at_initial_stage * bio_exp)
  
  exploitable_yield_gap_loss_due_to_constraints_at_development_stage <- exploitable_yield_gap_at_development_stage * x[, 14]
  exploitable_yield_gap_loss_due_to_constraints_at_development_stage <- ifelse(exploitable_yield_gap_loss_due_to_constraints_at_development_stage <= exploitable_yield_gap_at_development_stage, exploitable_yield_gap_loss_due_to_constraints_at_development_stage, exploitable_yield_gap_at_development_stage) # because exploitable_yield_gap_loss_due_to_constraints_at_development_stage cannot exceed the exploitable_yield_gap_at_development_stage itself.
  
  actually_exploited_yield_gap_at_development_stage <- exploitable_yield_gap_at_development_stage - exploitable_yield_gap_loss_due_to_constraints_at_development_stage
  
  # expected_actual_yield_at_development_stage <- actually_exploited_yield_gap_at_development_stage + (expected_actual_yield_at_initial_stage * x[, 6])
  expected_actual_yield_at_development_stage <- actually_exploited_yield_gap_at_development_stage + (expected_actual_yield_at_initial_stage * bio_exp)
  
  
  dev_stage = list(exploitable_yield_gap_at_development_stage = exploitable_yield_gap_at_development_stage,
                   exploitable_yield_gap_loss_due_to_constraints_at_development_stage = exploitable_yield_gap_loss_due_to_constraints_at_development_stage,
                   actually_exploited_yield_gap_at_development_stage = actually_exploited_yield_gap_at_development_stage,
                   expected_actual_yield_at_development_stage = expected_actual_yield_at_development_stage)
  
  # mid stage
  bio_exp <- x[,17] - (x[, 17] * x[, 16])
  
  exploitable_yield_gap_at_mid_stage <- exploitable_yield_gap_at_development_stage + (exploitable_yield_gap_at_development_stage * bio_exp)
  
  exploitable_yield_gap_loss_due_to_constraints_at_mid_stage <- exploitable_yield_gap_at_mid_stage * x[, 16]
  exploitable_yield_gap_loss_due_to_constraints_at_mid_stage <- ifelse(exploitable_yield_gap_loss_due_to_constraints_at_mid_stage <= exploitable_yield_gap_at_mid_stage, exploitable_yield_gap_loss_due_to_constraints_at_mid_stage, exploitable_yield_gap_at_mid_stage) # because exploitable_yield_gap_loss_due_to_constraints_at_mid_stage cannot exceed the exploitable_yield_gap_at_mid_stage itself.
  
  actually_exploited_yield_gap_at_mid_stage <- exploitable_yield_gap_at_mid_stage - exploitable_yield_gap_loss_due_to_constraints_at_mid_stage
  
  # expected_actual_yield_at_mid_stage <-  actually_exploited_yield_gap_at_mid_stage + (expected_actual_yield_at_development_stage * x[, 8])
  expected_actual_yield_at_mid_stage <-  actually_exploited_yield_gap_at_mid_stage + (expected_actual_yield_at_development_stage * bio_exp)
  
  mid_stage = list(exploitable_yield_gap_at_mid_stage = exploitable_yield_gap_at_mid_stage,
                   exploitable_yield_gap_loss_due_to_constraints_at_mid_stage = exploitable_yield_gap_loss_due_to_constraints_at_mid_stage,
                   actually_exploited_yield_gap_at_mid_stage = actually_exploited_yield_gap_at_mid_stage,
                   expected_actual_yield_at_mid_stage = expected_actual_yield_at_mid_stage)
  
  # late stage
  bio_exp <- x[,19] - (x[, 19] * x[, 18])
  
  exploitable_yield_gap_at_late_stage <- exploitable_yield_gap_at_mid_stage + (exploitable_yield_gap_at_mid_stage * bio_exp)
  
  exploitable_yield_gap_loss_due_to_constraints_at_late_stage <- exploitable_yield_gap_at_late_stage * x[, 18]
  exploitable_yield_gap_loss_due_to_constraints_at_late_stage <- ifelse(exploitable_yield_gap_loss_due_to_constraints_at_late_stage <= exploitable_yield_gap_at_late_stage, exploitable_yield_gap_loss_due_to_constraints_at_late_stage, exploitable_yield_gap_at_late_stage) # because exploitable_yield_gap_loss_due_to_constraints_at_late_stage cannot exceed the exploitable_yield_gap_at_late_stage itself.
  
  actually_exploited_yield_gap_at_late_stage <- exploitable_yield_gap_at_late_stage - exploitable_yield_gap_loss_due_to_constraints_at_late_stage
  
  expected_actual_yield_at_late_stage <-  actually_exploited_yield_gap_at_late_stage + (expected_actual_yield_at_mid_stage * bio_exp)
  
  late_stage = list(exploitable_yield_gap_at_late_stage = exploitable_yield_gap_at_late_stage,
                    exploitable_yield_gap_loss_due_to_constraints_at_late_stage = exploitable_yield_gap_loss_due_to_constraints_at_late_stage,
                    actually_exploited_yield_gap_at_late_stage = actually_exploited_yield_gap_at_late_stage,
                    expected_actual_yield_at_late_stage = expected_actual_yield_at_late_stage)
  
  # grain yield
  actually_exploited_yield_gap = actually_exploited_yield_gap_at_late_stage * x[, 20]
  exploitable_yield_gap = exploitable_yield_gap_at_late_stage * x[, 20]
  expected_actual_yield = expected_actual_yield_at_late_stage * x[, 20]
  exploitable_yield_gap_loss_due_to_constraints = exploitable_yield_gap_loss_due_to_constraints_at_late_stage * x[, 20]
  
  grain_yield = list(exploitable_yield_gap = exploitable_yield_gap,
                     exploitable_yield_gap_loss_due_to_constraints = exploitable_yield_gap_loss_due_to_constraints,
                     actually_exploited_yield_gap = actually_exploited_yield_gap,
                     expected_actual_yield = expected_actual_yield)
  
  
  ## Outputs
  out <- list(initial_stage = initial_stage, dev_stage = dev_stage, mid_stage = mid_stage,
              late_stage = late_stage, grain_yield = grain_yield)
  
  return(out)
} # Enf of Model Function


### Rice ####
ficher <- "output_files/Modelling_FBFS_model_RICE_RAW_predictions_Case_study_3.rds"
# bn nodes estimates

stage_ratio = c(initial_stage = 0.27,  development_stage = 0.80, mid_stage = 0.95, late_stage = 1)

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

 mc_nodes_biomass_exp_factor_init <- list(mc_nodes_lower_biomass_exp_factor_init=estimate(distribution = 'unif', lower = .Machine$double.eps, median = 0.19-0.1, upper = 0.20-0.1, variable= 'biomass_exp_factor_init', method = 'fit'),
                                           mc_nodes_mid_biomass_exp_factor_init=estimate(distribution = 'unif', lower = 0.18, median = 0.19, upper = 0.20, variable= 'biomass_exp_factor_init', method = 'fit'),
                                           mc_nodes_upper_biomass_exp_factor_init=estimate(distribution = 'unif', lower = 0.19, median = 0.19+0.1, upper = 0.20+0.1, variable= 'biomass_exp_factor_init', method = 'fit'))
  
  
  mc_nodes_biomass_exp_factor_dev <- list(mc_nodes_lower_biomass_exp_factor_dev=estimate(distribution = 'unif', lower = .Machine$double.eps, median = 0.49-0.1, upper = 0.50-0.1, variable= 'biomass_exp_factor_dev', method = 'fit'),
                                          mc_nodes_mid_biomass_exp_factor_dev=estimate(distribution = 'unif', lower = 0.20, median = 0.49, upper = 0.50, variable= 'biomass_exp_factor_dev', method = 'fit'),
                                          mc_nodes_upper_biomass_exp_factor_dev=estimate(distribution = 'unif', lower = 0.40, median = 0.49+0.1, upper = 0.50+0.1, variable= 'biomass_exp_factor_dev', method = 'fit'))
  
  mc_nodes_biomass_exp_factor_mid <- list(mc_nodes_lower_biomass_exp_factor_mid=estimate(distribution = 'unif', lower = .Machine$double.eps, median = 0.19-0.1, upper = 0.20-0.1, variable= 'biomass_exp_factor_mid', method = 'fit'),
                                          mc_nodes_mid_biomass_exp_factor_mid=estimate(distribution = 'unif', lower = 0.18, median = 0.19, upper = 0.20, variable= 'biomass_exp_factor_mid', method = 'fit'),
                                          mc_nodes_upper_biomass_exp_factor_mid=estimate(distribution = 'unif', lower = 0.19, median = 0.19+0.1, upper = 0.20+0.1, variable= 'biomass_exp_factor_mid', method = 'fit'))
  
  mc_nodes_biomass_exp_factor_late <- list(mc_nodes_lower_biomass_exp_factor_late=estimate(distribution = 'unif', lower = .Machine$double.eps, median = 0.09-0.01, upper = 0.10-0.01, variable= 'biomass_exp_factor_late', method = 'fit'),
                                           mc_nodes_mid_biomass_exp_factor_late=estimate(distribution = 'unif', lower = 0.05, median = 0.09, upper = 0.10, variable= 'biomass_exp_factor_late', method = 'fit'),
                                           mc_nodes_upper_biomass_exp_factor_late=estimate(distribution = 'unif', lower = 0.08, median = 0.09+0.01, upper = 0.10+0.01, variable= 'biomass_exp_factor_late', method = 'fit'))

mc_nodes_harvest_index <- list(mc_nodes_lower_harvest_index=estimate(distribution = 'gamma', lower = .Machine$double.eps, median=0.1, upper = 0.3, variable= 'harvest_index', method = 'fit'),
                                 mc_nodes_mid_harvest_index=estimate(distribution = 'gamma', lower = 0.2, median=0.4, upper = 0.5, variable= 'harvest_index', method = 'fit'),
                                 mc_nodes_upper_harvest_index=estimate(distribution = 'gamma', lower = 0.3, median=0.6, upper = 0.7, variable= 'harvest_index', method = 'fit'))
  
  if(!file.exists(ficher)){
   
  
  source("data_files/rice.R")
  
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
 
    distr_mid <- list(
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'unif'),
    c('beta', 'unif', 'unif'),
    c('beta', 'unif', 'unif')
  )
  
  distr_late <- list(
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'norm', 'beta'),
    c('beta', 'norm', 'beta'),
    c('beta', 'norm', 'beta')
  )             
  tmp <- 1:length(evidence_list_rice); names(tmp) <- names(evidence_list_rice)
  
  ssp <- sapply(tmp, function(evid){
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
    tmp <- 1:length(Local_constraints_at_development_stage_estimates)
    names(tmp) <- names(Local_constraints_at_development_stage_estimates)
    est_dev <- lapply(X=tmp, FUN = function(i){
      lapply(est_int, function(j){
        rbind(j,
              Local_constraints_at_development_stage_estimates[[i]],
              mc_nodes_biomass_exp_factor_dev[[i]])
      })
    })
    
    
    ## Biomass yield at mid stage
    # Local_constraints_at_mid_stage_estimates <- make_node_states_estimates(bn=net, node='Local_constraints_at_mid_stage', op = 'proba', distr = 'beta', state_effects = c(0.9, 0.6, 0.1), evidence = evid, include_relatives = FALSE)
    Local_constraints_at_mid_stage_estimates <- make_node_states_estimates(bn = net, node='Local_constraints_at_mid_stage', distr = distr_mid[[evid]], evidence = evidence_list_rice[[evid]], state_effects = c(0.9, 0.6, 0.1))
    
    tmp <- 1:length(Local_constraints_at_mid_stage_estimates)
    names(tmp) <- names(Local_constraints_at_mid_stage_estimates)
    est_mid <- lapply(X=tmp, FUN = function(i){
      lapply(est_dev, function(j) {
        lapply(j, function (k){
          rbind(k,
                Local_constraints_at_mid_stage_estimates[[i]],
                mc_nodes_biomass_exp_factor_mid[[i]])
        })
      })
    })
    
    ## Biomass yield at late stage
    # Local_constraints_at_late_stage_estimates <- make_node_states_estimates(bn=net, node='Local_constraints_at_late_stage', op = 'proba', distr = 'beta', state_effects = c(0.9, 0.6, 0.1), evidence = evid, include_relatives = FALSE)
    Local_constraints_at_late_stage_estimates <- make_node_states_estimates(bn = net, node = 'Local_constraints_at_late_stage', distr = distr_late[[evid]], evidence = evidence_list_rice[[evid]], state_effects = c(0.9, 0.6, 0.1))
    
    
    tmp <- 1:length(Local_constraints_at_late_stage_estimates)
    names(tmp) <- names(Local_constraints_at_late_stage_estimates)
    est_late <- lapply(X=tmp, FUN = function(i){
      lapply(est_mid, function(j) {
        lapply(j, function(k){
          lapply(k, function (l){
            rbind(l,
                  Local_constraints_at_late_stage_estimates[[i]],
                  mc_nodes_biomass_exp_factor_late[[i]], 
                  mc_nodes_harvest_index[[i]],
                  mc_nodes_biomass_exp_factor_init[[i]])
          })
        })
      })
    })
    
    state_ids = 1:length(est_late)
    names(state_ids) <- names(est_late)
    
    sapply(X=state_ids, function (i) {
      sapply(est_late[[i]], function (j){
        sapply(j, function(k){
          sapply(k, function(l){
            mcSimulation(estimate = l, model_function = model_function, numberOfModelRuns=1000, functionSyntax="matrixNames")
          }, simplify = FALSE, USE.NAMES = TRUE)
        }, simplify = FALSE, USE.NAMES = TRUE)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  saveRDS(ssp, ficher)
  rm(ssp); gc(verbose=FALSE)
}

#### Sorghum #####
ficher <- "output_files/Modelling_FBFS_model_SORGHUM_RAW_predictions_Case_study_3.rds"
if(!file.exists(ficher)){
  source("data_files/sorghum.R")
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
  
  distr_mid <- list(
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'unif'),
    c('beta', 'norm', 'beta'),
    c('beta', 'norm', 'beta')
  )
  
  distr_late <- list(
    c('beta', 'unif', 'beta'),
    c('beta', 'norm', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'unif', 'beta'),
    c('beta', 'norm', 'beta'),
    c('unif', 'norm', 'unif'),
    c('beta', 'norm', 'beta'),
    c('beta', 'logis', 'unif')
  )   
  
  tmp <- 1:length(evidence_list_sorghum); names(tmp) <- names(evidence_list_sorghum)
  
  ssp <- sapply(tmp, function(evid){
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
    tmp <- 1:length(Local_constraints_at_development_stage_estimates)
    names(tmp) <- names(Local_constraints_at_development_stage_estimates)
    est_dev <- lapply(X=tmp, FUN = function(i){
      lapply(est_int, function(j){
        rbind(j,
              Local_constraints_at_development_stage_estimates[[i]],
              mc_nodes_biomass_exp_factor_dev[[i]])
      })
    })
    
    
    ## Biomass yield at mid stage
    Local_constraints_at_mid_stage_estimates <- make_node_states_estimates(bn = net, node='Local_constraints_at_mid_stage', distr = distr_mid[[evid]], evidence = evidence_list_sorghum[[evid]], state_effects = c(0.9, 0.6, 0.1))
    
    tmp <- 1:length(Local_constraints_at_mid_stage_estimates)
    names(tmp) <- names(Local_constraints_at_mid_stage_estimates)
    est_mid <- lapply(X=tmp, FUN = function(i){
      lapply(est_dev, function(j) {
        lapply(j, function (k){
          rbind(k,
                Local_constraints_at_mid_stage_estimates[[i]],
                mc_nodes_biomass_exp_factor_mid[[i]])
        })
      })
    })
    
    ## Biomass yield at late stage
    Local_constraints_at_late_stage_estimates <- make_node_states_estimates(bn = net, node = 'Local_constraints_at_late_stage', distr = distr_late[[evid]], evidence = evidence_list_sorghum[[evid]], state_effects = c(0.9, 0.6, 0.1))
    
    tmp <- 1:length(Local_constraints_at_late_stage_estimates)
    names(tmp) <- names(Local_constraints_at_late_stage_estimates)
    est_late <- lapply(X=tmp, FUN = function(i){
      lapply(est_mid, function(j) {
        lapply(j, function(k){
          lapply(k, function (l){
            rbind(l,
                  Local_constraints_at_late_stage_estimates[[i]],
                  mc_nodes_biomass_exp_factor_late[[i]], 
                  mc_nodes_harvest_index[[i]],
                  mc_nodes_biomass_exp_factor_init[[i]])
          })
        })
      })
    })
    
    state_ids = 1:length(est_late)
    names(state_ids) <- names(est_late)
    
    sapply(X=state_ids, function (i) {
      sapply(est_late[[i]], function (j){
        sapply(j, function(k){
          sapply(k, function(l){
            mcSimulation(estimate = l, model_function = model_function, numberOfModelRuns=1000, functionSyntax="matrixNames")
          }, simplify = FALSE, USE.NAMES = TRUE)
        }, simplify = FALSE, USE.NAMES = TRUE)
      }, simplify = FALSE, USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  saveRDS(ssp, ficher)
  rm(ssp); gc(verbose=FALSE)
}

## Merging Rice and Sorghum ####

## removing objects that are no longer needed
rm(list=setdiff(ls(), c("evidence", "my_theme", "plot_font_size"))); gc ()

## some common plots specifications
legend_bg <- adjustcolor( "yellow", alpha.f = 0.15)
plots_lwd <- 0.5
min_plots_width_in <- 2.63
max_plots_width_in <- 7.5
max_plots_height_in <- 8.75
min_plots_res <- 300
plots_compression <- "lzw"
plots_font_family <- 'serif' # 'sans'
plots_font <- 1 # 2
plot_font_size <- 10

ssp1 <- readRDS("output_files/Modelling_FBFS_model_RICE_RAW_predictions_Case_study_3.rds")
ssp2 <- readRDS("output_files/Modelling_FBFS_model_SORGHUM_RAW_predictions_Case_study_3.rds")

f <- function(x){
  if(inherits(x, 'mcSimulation')){
    out <- x$y
  } else {
    out <- sapply(x, f, simplify = FALSE)
  }
  
}

ssp1 <- f(ssp1)

ssp1 <- reshape2::melt(ssp1)
ssp1 <- ssp1[ssp1$variable=="grain_yield.expected_actual_yield", ]

ssp1$Crop_type <- rep('Rice', nrow(ssp1))

ssp2 <- f(ssp2)

ssp2 <- reshape2::melt(ssp2)
ssp2 <- ssp2[ssp2$variable=="grain_yield.expected_actual_yield", ]

ssp2$Crop_type <- rep('Sorghum', nrow(ssp2))

ssp <- rbind(ssp1, ssp2)

## plot ####

evidence <- data.frame(evidence, L1=paste0('Traitment_', 1:nrow(evidence)))
ssp <- merge(ssp, evidence, by = c("L1", 'Crop_type'), all.x=TRUE, all.y=TRUE)

ssp$Crop_type <- as.character(ssp$Crop_type)

ssp <- ssp[
  (ssp$L5 != "Local_constraints_at_initial_stage=Medium")&
    (ssp$L4 != "Local_constraints_at_development_stage=Medium")&
    (ssp$L3 != "Local_constraints_at_mid_stage=Medium")&
    (ssp$L2 != "Local_constraints_at_late_stage=Medium"), ]

ssp$L5 <- gsub("Local_constraints_at_initial_stage=High", "{bold('FC'[italic(i)]==High)}", ssp$L5)
ssp$L5 <- gsub("Local_constraints_at_initial_stage=Low", "{bold('FC'[italic(i)]==Low)}", ssp$L5)

ssp$L5 <- gsub(pattern = 'Low', replacement = "L", ssp$L5)
ssp$L5 <- gsub(pattern = 'Medium', replacement = "M", ssp$L5)
ssp$L5 <- gsub(pattern = 'High', replacement = "H", ssp$L5)

ssp$L4 <- gsub("Local_constraints_at_development_stage=High", "{bold('FC'[italic(ii)]==High)}", ssp$L4)
ssp$L4 <- gsub("Local_constraints_at_development_stage=Low", "{bold('FC'[italic(ii)]==Low)}", ssp$L4)

ssp$L4 <- gsub(pattern = 'Low', replacement = "L", ssp$L4)
ssp$L4 <- gsub(pattern = 'Medium', replacement = "M", ssp$L4)
ssp$L4 <- gsub(pattern = 'High', replacement = "H", ssp$L4)

ssp$L3 <- gsub("Local_constraints_at_mid_stage=High", "{bold('FC'[italic(iii)]==High)}", ssp$L3)
ssp$L3 <- gsub("Local_constraints_at_mid_stage=Low", "{bold('FC'[italic(iii)]==Low)}", ssp$L3)

ssp$L3 <- gsub(pattern = 'Low', replacement = "L", ssp$L3)
ssp$L3 <- gsub(pattern = 'Medium', replacement = "M", ssp$L3)
ssp$L3 <- gsub(pattern = 'High', replacement = "H", ssp$L3)

ssp$L2 <- gsub("Local_constraints_at_late_stage=High", "{bold('FC'[italic(iiii)]==High)}", ssp$L2)
ssp$L2 <- gsub("Local_constraints_at_late_stage=Low", "{bold('FC'[italic(iiii)]==Low)}", ssp$L2)

ssp$L2 <- gsub(pattern = 'Low', replacement = "L", ssp$L2)
ssp$L2 <- gsub(pattern = 'Medium', replacement = "M", ssp$L2)
ssp$L2 <- gsub(pattern = 'High', replacement = "H", ssp$L2)

ssp$Pest_and_disease_impacts_at_mid_stage <- factor(ssp$Pest_and_disease_impacts_at_mid_stage, levels = unique(ssp$Pest_and_disease_impacts_at_mid_stage))
ssp$Weed_impacts_at_mid_stage <- factor(ssp$Weed_impacts_at_mid_stage, levels = unique(ssp$Weed_impacts_at_mid_stage))
ssp$Available_soil_water_at_mid_stage <- factor(ssp$Available_soil_water_at_mid_stage, levels = unique(ssp$Available_soil_water_at_mid_stage))

ssp$L2 <- factor(ssp$L2, levels = unique(ssp$L2))
ssp$L3 <- factor(ssp$L3, levels = unique(ssp$L3))
ssp$L4 <- factor(ssp$L4, levels = unique(ssp$L4))
ssp$L5 <- factor(ssp$L5, levels = unique(ssp$L5))

labellers <- labeller(
  Pest_and_disease_impacts_at_mid_stage = c(Minor = "Pest and disease impact = Minor",
                                            Severe = "Pest and disease impact = Severe"),
  Weed_impacts_at_mid_stage = c(Negligible = "Weeds impact = Negligible",
                                Significant = "Weeds impact = Significant"),
  Available_soil_water_at_mid_stage = c(`Drought risk` = "Drought\nrisk",
                                        `Waterlogging risk` = "Waterlogging\nrisk"),
  L5 = label_parsed,
  L4 = label_parsed,
  L3 = label_parsed,
  L2 = label_parsed
)

p1 <- ggplot (data = ssp[ssp$L3 == "{bold('FC'[italic(iii)]==L)}", ], aes(x=Crop_type, y=value, fill=Crop_type, colour=Crop_type))+
  # geom_density_ridges(scale=1, size=0.05)+
  # geom_violin(draw_quantiles = c(0.05, 0.5, 0.95), size = 0.1)+
  geom_violin(size = 0.1, alpha=0.5)+
  geom_boxplot(width=0.1, position = 'dodge', outlier.colour=NA, size = 0.2, colour='black') +
  # geom_boxplot(width=0.1)+
  # stat_summary(fun.y=mean, geom="point", shape=23, size=1)+
  # stat_summary(fun.y=median, geom="point", size=1, color="red")+
  ggnomics::facet_nested(L2+L4+L5 ~ Pest_and_disease_impacts_at_mid_stage+Weed_impacts_at_mid_stage+Available_soil_water_at_mid_stage, 
                         space = 'free_x', scales = 'free_x', labeller = labellers)+
  scale_y_continuous(
    expand = c(0, 0),
    limits = function(x) c(min(x, na.rm = TRUE), 10),
    breaks = seq(2, 8, by = 2)
  )+
  
  my_theme+
  theme(
    panel.grid.major = element_line(size = 0.2),
    panel.grid.minor = element_line(size = 0.1),
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    legend.position=c(1,1),
    legend.justification = c("right", "top"),
    legend.text = element_text(size = plot_font_size+2, margin = margin(0,0,0,-4)),
    legend.margin=margin(0,0,0,0),
    legend.box = "vertical",
    legend.box.margin=margin(-40,-53,0, 0),
    legend.key.width = unit(0.25/1.5, "cm"),
    legend.key.height = unit(0.05, "cm"),
    strip.text = element_text(size = plot_font_size-0.5, face = 'bold'),
    strip.text.x = element_text(margin = margin(t=3, b = 3)),
    axis.text = element_text(size = plot_font_size+2),
    # axis.text.x = element_text(hjust = 0.5),
    axis.text.y = element_text(hjust = 0.5),
    axis.ticks.length = unit(0.25, "mm"),
    plot.margin=unit(c(4, 1, 1, 1), "pt")
  )+
  labs(y = 'Grain yield (tons/ha)')
# rm(ssp); gc(verbose=FALSE)

gb = grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
               gp=gpar(lwd=1, fill=NA, col="lightgray"))
p1 <- gTree(children = gList(ggplotGrob(p1), gb))

p1 <- list(p1)
p1$ncol=1
# p1$top = grid::textGrob(
# "FC = farming constraint, H = High, M = Medium, L = Low. The indices i, ii, iiii, respectively, indicate crop at initial, developement and late stages.",
#                         x=0.04, hjust=0, vjust = 0.5,
#                         gp = gpar(fontfamily=plots_font_family,fontsize=plot_font_size-2,fontface="italic", col="black"))
grid.newpage()
ga = do.call("grid.arrange", p1)

g <- gTree(children = gList(ga, gb))
grid.draw(g)

ggsave(plot=g, device='png', filename = "figures/Modelling_FBFS_grain_yield_violin_LOW.png", width = max_plots_width_in, 
       height=max_plots_height_in/1.25, units="in", dpi=min_plots_res
)
ggsave(plot=g, device='pdf', filename = "figures/Modelling_FBFS_grain_yield_violin_LOW.pdf", width = max_plots_width_in, 
       height=max_plots_height_in/1.25, units="in", dpi=min_plots_res
)

# p1 <- ggplot (data = ssp[ssp$L3 == "{bold('FC'[italic(iii)]==H)}", ], aes(x=Crop_type, y=value, fill=Crop_type))+
#   # geom_density_ridges(scale=1, size=0.05)+
#   # geom_violin(draw_quantiles = c(0.05, 0.5, 0.95), size = 0.1)+
#   geom_violin(size = 0.1)+
#   geom_boxplot(width=0.1, position = 'dodge', outlier.colour=NA, size = 0.1) +
#   # geom_boxplot(width=0.1)+
#   # stat_summary(fun.y=mean, geom="point", shape=23, size=1)+
#   # stat_summary(fun.y=median, geom="point", size=1, color="red")+
#   ggnomics::facet_nested(L2+L4+L5 ~ Pest_and_disease_impacts_at_mid_stage+Weed_impacts_at_mid_stage+Available_soil_water_at_mid_stage,
#                          space = 'free_x', scales = 'free_x', labeller = labellers)+
#   scale_y_continuous(
#     expand = c(0, 0),
#     limits = function(x) c(min(x, na.rm = TRUE), 10),
#     breaks = seq(2, 8, by = 2)
#   )+
#   
#   my_theme+
#   theme(
#     axis.title.x = element_blank(),
#     axis.text.x=element_blank(),
#     legend.position=c(1,1),
#     legend.justification = c("right", "top"),
#     legend.text = element_text(size = plot_font_size+2, margin = margin(0,0,0,-4)),
#     legend.margin=margin(0,0,0,0),
#     legend.box = "vertical",
#     legend.box.margin=margin(-40,-53,0, 0),
#     legend.key.width = unit(0.25/1.5, "cm"),
#     legend.key.height = unit(0.05, "cm"),
#     strip.text = element_text(size = plot_font_size-0.5, face = 'bold'),
#     strip.text.x = element_text(margin = margin(t=3, b = 3)),
#     axis.text = element_text(size = plot_font_size+2),
#     # axis.text.x = element_text(hjust = 0.5),
#     axis.text.y = element_text(hjust = 0.5),
#     axis.ticks.length = unit(0.25, "mm"),
#     plot.margin=unit(c(4, 1, 1, 1), "pt")
#   )+
#   labs(y = 'Grain yield (tons/ha)')
# rm(ssp); gc(verbose=FALSE)
# 
# p1 <- list(p1)
# p1$ncol=1
# p1$top = grid::textGrob(
#   "FC = farming constraint, H = High, M = Medium, L = Low, D = Drought risk, W = Waterlogging risk. The indices i, ii, iiii,
# respectively, indicate crop at initial, developement and late stages.",
#   x=0, hjust=0, vjust = 0.5,
#   gp = gpar(fontfamily=plots_font_family,fontsize=plot_font_size,fontface="italic", col="black"))
# grid.newpage()
# ga = do.call("grid.arrange", p1)
# gb = grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"),
#                gp=gpar(lwd=1, fill=NA, col="lightgray"))
# g <- gTree(children = gList(ga, gb))
# grid.draw(g)
# 
# ggsave(plot=g, device='png', filename = "figures/Modelling_FBFS_grain_yield_violin_HIGH.png", width = max_plots_width_in,
#        height=max_plots_height_in/1.5, units="in", dpi=min_plots_res
# )
