## INITIAL STAGE ####
#------------------------------------------------------------------------------------#
# amount of water reaching the plot #
#------------------------------------------------------------------------------------#

## Type of water diversion ####
Type_of_water_diversion <- cptable(~Type_of_water_diversion, values = c(0.3, 0.4, 0.3),
                                   levels = c('Modern', 'Hybrid', 'Traditional'))


## Sediment_load_at_initial_stage ####
Sediment_load_at_initial_stage <- cptable (~Sediment_load_at_initial_stage|Type_of_water_diversion, values =   c(0.1, 0.3, 0.6,
                                                                                                                 0.1, 0.6, 0.3,
                                                                                                                 0.7, 0.2, 0.1),
                                           # values = c(0.6, 0.2, 0.1,
                                           #            0.3, 0.6, 0.2,
                                           #            0.1, 0.2, 0.7),
                                           
                                           levels = c("High", 'Medium', 'Low'))


## Rain_event_occurence_at_initial_stage ####
Rain_event_occurence_at_initial_stage <- cptable (~Rain_event_occurence_at_initial_stage, values = c(0.65, 0.35),levels = c('Off-site', 'On-site'))

## Slope ####
Slope <- cptable (~Slope, values = c(0.1, 0.2, 0.7),levels = c('Steep', 'Moderate', 'Gentil'))

## Socio-institutional arrangements ####
Social_arrangements <- cptable (~Social_arrangements, values = c(0.3, 0.7),levels = c('Inadequate', 'Adequate'))

## Main_Canals_Maitnenance_at_initial_stage ####
Main_Canals_Maitnenance_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(4,3,2),
                                                                                     c(0,1)),
                                                               parent_weights = c(1,2),
                                                               b = 1.5,
                                                               child_prior = c(0.5,0.5),
                                                               child_states = c('Poor', 'Good'),
                                                               parent_states = list(c('Modern', 'Hybrid', 'Traditional'),
                                                                                    c('Inadequate', 'Adequate')))

# Main_Canals_Maitnenance_at_initial_stage_values <- Main_Canals_Maitnenance_at_initial_stage_tmp$values
Main_Canals_Maitnenance_at_initial_stage_values <- c(0.919293820933165, 0.0807061790668348,
                                                     0.835051546391752, 0.164948453608247, 
                                                     0.692307692307692, 0.307692307692308, 
                                                     0.0807061790668348, 0.919293820933165, 
                                                     0.164948453608247, 0.835051546391752, 
                                                     0.307692307692308, 0.692307692307692)
Main_Canals_Maitnenance_at_initial_stage_levels <- Main_Canals_Maitnenance_at_initial_stage_tmp$levels
Main_Canals_Maitnenance_at_initial_stage <- cptable (~Main_Canals_Maitnenance_at_initial_stage|Type_of_water_diversion:Social_arrangements, values = Main_Canals_Maitnenance_at_initial_stage_values,levels = Main_Canals_Maitnenance_at_initial_stage_levels)


## Amount_of_shared_flood_at_initial_stage ####
Amount_of_shared_flood_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2), 
                                                                                    c(1,2), 
                                                                                    c(1,2,4),
                                                                                    c(1,2,3.5)),
                                                              parent_weights = c(1,2,3,2),
                                                              b = 2.5,
                                                              child_prior = c(0.3, 0.6, 0.1),
                                                              child_states = c('< Enough', 'Enough', '> Enough'),
                                                              parent_states = list(c('Off-site', 'On-site'),
                                                                                   c('Poor', 'Good'),
                                                                                   c('High', 'Medium',  'Low'), 
                                                                                   c('Steep','Moderate', 'Gentil')))

Amount_of_shared_flood_at_initial_stage_values <- Amount_of_shared_flood_at_initial_stage_tmp$values
Amount_of_shared_flood_at_initial_stage_levels <- Amount_of_shared_flood_at_initial_stage_tmp$levels
Amount_of_shared_flood_at_initial_stage <- cptable (~Amount_of_shared_flood_at_initial_stage|
                                                      Rain_event_occurence_at_initial_stage:
                                                      Main_Canals_Maitnenance_at_initial_stage:
                                                      Sediment_load_at_initial_stage:
                                                      Slope, values = Amount_of_shared_flood_at_initial_stage_values,levels = Amount_of_shared_flood_at_initial_stage_levels)

## Location_of_the_plot ####
Location_of_the_plot <- cptable (~Location_of_the_plot, values = c(0.6, 0.3, 0.1),levels = c('Lowlands', 'Midlands', 'Highlands'))
## Upstream_abstraction_at_initial_stage ####
Upstream_abstraction_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                                  c(0,1,2)),
                                                            parent_weights = c(1,2),
                                                            b = 3,
                                                            child_prior = c(0.6,0.4),
                                                            child_states = c('Unfair', 'Fair'),
                                                            parent_states = list(c('Inadequate', 'Adequate'),
                                                                                 c('Lowlands', 'Midlands', 'Highlands')))



# Upstream_abstraction_at_initial_stage_values <- Upstream_abstraction_at_initial_stage_tmp$values
Upstream_abstraction_at_initial_stage_values <- c(0.999086340794883, 0.000913659205116492,
                                                  0.242857142857143, 0.757142857142857,
                                                  0.931034482758621, 0.0689655172413793, 
                                                  0.142857142857143, 0.857142857142857,
                                                  0.142857142857143, 0.857142857142857, 
                                                  0.00205338809034908, 0.997946611909651)
Upstream_abstraction_at_initial_stage_levels <- Upstream_abstraction_at_initial_stage_tmp$levels
Upstream_abstraction_at_initial_stage <- cptable (~Upstream_abstraction_at_initial_stage|Social_arrangements:Location_of_the_plot, values = Upstream_abstraction_at_initial_stage_values,levels = Upstream_abstraction_at_initial_stage_levels)

## Field_canal_maintenance_at_initial_stage####
Field_canal_maintenance_at_initial_stage<- cptable (~Field_canal_maintenance_at_initial_stage, values = c(0.2, 0.8),levels = c('Poor', 'Good'))

## Presence_of_the_farmer_during_flood_event_at_initial_stage ###
Presence_of_the_farmer_during_flood_event_at_initial_stage <- cptable (~Presence_of_the_farmer_during_flood_event_at_initial_stage, values = c(0.2, 0.8),levels = c('False', 'True'))

## Amount_of_flood_reaching_the_plot_at_initial_stage ####
Amount_of_flood_reaching_the_plot_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2), 
                                                                                               c(0,2), 
                                                                                               c(1,2,3), 
                                                                                               c(1,2)),
                                                                         parent_weights = c(1,1,3,3),
                                                                         b = 2,
                                                                         child_prior = c(0.2,0.7,0.1),
                                                                         child_states = c('Too little', 'Desired', 'Too much'),
                                                                         parent_states = list(c('Poor', 'Good'),
                                                                                              c('False', 'True'),
                                                                                              c('< Enough', 'Enough', '> Enough'),
                                                                                              c('Unfair', 'Fair')))



Amount_of_flood_reaching_the_plot_at_initial_stage_values <- Amount_of_flood_reaching_the_plot_at_initial_stage_tmp$values
Amount_of_flood_reaching_the_plot_at_initial_stage_levels <- Amount_of_flood_reaching_the_plot_at_initial_stage_tmp$levels
Amount_of_flood_reaching_the_plot_at_initial_stage <- cptable (~Amount_of_flood_reaching_the_plot_at_initial_stage|Field_canal_maintenance_at_initial_stage:Presence_of_the_farmer_during_flood_event_at_initial_stage:Amount_of_shared_flood_at_initial_stage:Upstream_abstraction_at_initial_stage, values = Amount_of_flood_reaching_the_plot_at_initial_stage_values,levels = Amount_of_flood_reaching_the_plot_at_initial_stage_levels)


#------------------------------------------------------------------------------------#
# Available soil water #
#------------------------------------------------------------------------------------#

## Soil_type ####

Soil_type <- cptable (~Soil_type, values = c(0.05, 0.55, 0.4),levels = c('Sandy', 'Loamy', 'Clayey'))

## Manure_application ####

Manure_application <- cptable(~Manure_application, values = c(0.3, 0.7), levels = c('False', 'True'))

## Soil_water_holding_capacity ####

Soil_water_holding_capacity_tmp <- make_gRain_CPT(parent_effects = list(c(0, 2.5, 3),
                                                                        c(0, 2)),
                                                  parent_weights = c(2,1),
                                                  b = 3,
                                                  child_prior = c(0.2,0.5,0.3),
                                                  child_states = c('Low', 'Medium', 'High'),
                                                  parent_states = list(c('Sandy', 'Loamy', 'Clayey'),
                                                                       c('False', 'True')))
Soil_water_holding_capacity_values <- Soil_water_holding_capacity_tmp$values
Soil_water_holding_capacity_levels <- Soil_water_holding_capacity_tmp$levels
Soil_water_holding_capacity <- cptable (~Soil_water_holding_capacity|Soil_type:Manure_application, values = Soil_water_holding_capacity_values,levels = Soil_water_holding_capacity_levels)

## Evapotranspiration_at_initial_stage ####

Evapotranspiration_at_initial_stage <- cptable (~Evapotranspiration_at_initial_stage, values = c(0.1, 0.2, 0.7),
                                                levels = c('High', 'Medium', 'Low'))

## Rainfall_amount_at_initial_stage ####

Rainfall_amount_at_initial_stage <- cptable (~Rainfall_amount_at_initial_stage, values = c(0.25, 0.6, 0.15),levels = c('< normal', 'Normal', '> normal'))

## Initial_soil_water_content ####

Initial_soil_water_content <- cptable (~Initial_soil_water_content, values = c(0.40, 0.30, 0.15, 0.15),levels = c('Very low', 'Low', 'Medium', 'High'))

## Available_soil_water_at_initial_stage ####

Available_soil_water_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2,3),
                                                                                  c(1,2,3),
                                                                                  c(0,2,3.5),
                                                                                  c(1,2,3,4),
                                                                                  c(0, 2, 2)),
                                                            parent_weights = c(4, 2.5, 2, 1, 3),
                                                            b = 1.5,
                                                            child_prior = c(0.1, 0.6, 0.3),
                                                            child_states = c('Drought risk', 'Normal', 'Waterlogging risk'),
                                                            parent_states = list(c('Low', 'Medium', 'High'),
                                                                                 c('High', 'Medium', 'Low'),
                                                                                 c('< normal', 'Normal', '> normal'), 
                                                                                 c('Very low', 'Low', 'Medium', 'High'),
                                                                                 c('Too little', 'Desired', 'Too much')))
Available_soil_water_at_initial_stage_values <- Available_soil_water_at_initial_stage_tmp$values
Available_soil_water_at_initial_stage_levels <- Available_soil_water_at_initial_stage_tmp$levels
Available_soil_water_at_initial_stage <- cptable (~Available_soil_water_at_initial_stage|
                                                    Soil_water_holding_capacity:
                                                    Evapotranspiration_at_initial_stage:
                                                    Rainfall_amount_at_initial_stage:
                                                    Initial_soil_water_content:
                                                    Amount_of_flood_reaching_the_plot_at_initial_stage, values = Available_soil_water_at_initial_stage_values,levels = Available_soil_water_at_initial_stage_levels)


#-----------------------------------------------------------------------------------#
# Available soil nutrients at initial stage, Pest and desease impact at initial stage, Weeds impacts
#-----------------------------------------------------------------------------------#

## Relative_wealth_status ####

Relative_wealth_status <- cptable(~Relative_wealth_status, values = c(0.6, 0.3, 0.1), levels = c('Poor', 'Middle class', 'Rich'))

## Mutual_aids ####

Mutual_aids <- cptable(~Mutual_aids, values = c(0.3, 0.7), levels = c('False', 'True'))

## Access_to_inputs_at_initial_stage ####

Access_to_inputs_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2,3), 
                                                                              c(0,2)),
                                                        parent_weights = c(1.5, 1),
                                                        b = 1.5,
                                                        child_prior = c(0.4,0.6),
                                                        child_states = c('False', 'True'),
                                                        parent_states = list(c('Poor', 'Middle class', 'Rich'),
                                                                             c('False', 'True')))
Access_to_inputs_at_initial_stage_values <- Access_to_inputs_at_initial_stage_tmp$values
Access_to_inputs_at_initial_stage_levels <- Access_to_inputs_at_initial_stage_tmp$levels
Access_to_inputs_at_initial_stage <- cptable (~Access_to_inputs_at_initial_stage|Relative_wealth_status:Mutual_aids, values = Access_to_inputs_at_initial_stage_values,levels = Access_to_inputs_at_initial_stage_levels)

## Available_paid_labor_at_initial_stage #####
Available_paid_labor_at_initial_stage <- cptable(~Available_paid_labor_at_initial_stage, values = c(0.4, 0.6), levels = c('Unavailable', 'Available'))
## Available_Labor_force_at_initial_stage #####

Available_Labor_force_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(2, 1.5, 0), 
                                                                                   c(0,1),
                                                                                   c(1, 2)),
                                                             parent_weights = c(3, 1, 3),
                                                             b = 1.5,
                                                             child_prior = c(0.2,0.8),
                                                             child_states = c('Unsufficient', 'Sufficient'),
                                                             parent_states = list(c('Poor', 'Middle class', 'Rich'),
                                                                                  c('False', 'True'),
                                                                                  c('Unavailable', 'Available')))
# Available_Labor_force_at_initial_stage_values <- Available_Labor_force_at_initial_stage_tmp$values
Available_Labor_force_at_initial_stage_values <- c(0.35, 0.65, 
                                                   0.654986522911051, 0.345013477088949, 
                                                   0.986483256178967, 0.0135167438210335, 
                                                   
                                                   0.3, 0.7,                                                   
                                                   0.5, 0.5,
                                                   0.7, 0.3,
                                                   
                                                   0.3, 0.7,                                                   
                                                   0.4, 0.6,
                                                   0.4, 0.6,
                                                   
                                                   0.00288192548646564, 0.997118074513534, 
                                                   0.1, 0.9,
                                                   0.15, 0.85)
Available_Labor_force_at_initial_stage_levels <- Available_Labor_force_at_initial_stage_tmp$levels
Available_Labor_force_at_initial_stage <- cptable (~Available_Labor_force_at_initial_stage|Relative_wealth_status:Mutual_aids:Available_paid_labor_at_initial_stage, values = Available_Labor_force_at_initial_stage_values,levels = Available_Labor_force_at_initial_stage_levels)

## skills_of_the_farmer #####

skills_of_the_farmer <- cptable(~skills_of_the_farmer, values = c(0.3, 0.7), levels = c('Defavorable', 'Favorable'))

## Chemical_application_at_initial_stage ####
Chemical_application_at_initial_stage <- cptable (~Chemical_application_at_initial_stage|Access_to_inputs_at_initial_stage, 
                                                  values = c(0.8, 0.2,
                                                             0.4, 0.6),
                                                  levels = c('Unufficient', 'Sufficient'))

### Application_of_traditional_crop_protection_Methods_at_initial_stage #####

Application_of_traditional_crop_protection_Methods_at_initial_stage <- cptable(~Application_of_traditional_crop_protection_Methods_at_initial_stage, values = c(0.1, 0.9), levels = c('Uncommon', 'Common'))

## Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage ####

Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                                                                   c(1,2),
                                                                                                                   c(1,2),
                                                                                                                   c(1,2)),
                                                                                             parent_weights = c(1.5,2,1,3),
                                                                                             b = 1.5,
                                                                                             child_prior = c(0.3, 0.4, 0.3),
                                                                                             child_states = c('Poor', 'Good', 'Excellent'),
                                                                                             parent_states = list(c('Unsufficient', 'Sufficient'),
                                                                                                                  c('Uncommon', 'Common'),
                                                                                                                  c('Unsufficient', 'Sufficient'),
                                                                                                                  c('Defavorable', 'Favorable')))


Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage_values <- Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage_tmp$values
Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage_levels <- Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage_tmp$levels
Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage <- cptable (~Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage|
                                                                                     Chemical_application_at_initial_stage:
                                                                                     Application_of_traditional_crop_protection_Methods_at_initial_stage:
                                                                                     Available_Labor_force_at_initial_stage:
                                                                                     skills_of_the_farmer, values = Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage_values,levels = Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage_levels)

## Effectiveness_of_Weeding_at_initial_stage #####

Effectiveness_of_Weeding_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2), c(1,2)),
                                                                parent_weights = c(1, 1.5),
                                                                b = 1.75,
                                                                child_prior = c(0.3, 0.4, 0.3),
                                                                child_states = c('Poor', 'Good', 'Excellent'),
                                                                parent_states = list(c('Unsufficient', 'Sufficient'),
                                                                                     c('Defavorable', 'Favorable')))



Effectiveness_of_Weeding_at_initial_stage_values <- Effectiveness_of_Weeding_at_initial_stage_tmp$values
Effectiveness_of_Weeding_at_initial_stage_levels <- Effectiveness_of_Weeding_at_initial_stage_tmp$levels
Effectiveness_of_Weeding_at_initial_stage <- cptable (~Effectiveness_of_Weeding_at_initial_stage|Available_Labor_force_at_initial_stage:skills_of_the_farmer, values = Effectiveness_of_Weeding_at_initial_stage_values,levels = Effectiveness_of_Weeding_at_initial_stage_levels)


## Fertilizers_application_at_initial_stage #####

Fertilizers_application_at_initial_stage <- cptable (~Fertilizers_application_at_initial_stage|Access_to_inputs_at_initial_stage, 
                                                     values = c(0.9, 0.1,
                                                                0.3, 0.7),
                                                     levels = c('False', 'True'))

## Rich_sediments_addition_from_flood_at_initial_stage ####

Rich_sediments_addition_from_flood_at_initial_stage <- cptable(~Rich_sediments_addition_from_flood_at_initial_stage, values = c(0.4, 0.6), levels = c('False', 'True'))

## Available_soil_nutrients_at_initial_stage ####

Available_soil_nutrients_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2), 
                                                                                      c(1,2), 
                                                                                      c(1,2)),
                                                                parent_weights = c(2,3,1),
                                                                b = 1.5,
                                                                child_prior = c(0.25, 0.5, 0.25),
                                                                child_states = c('Deficient','Satisfactory', 'Plenty'),
                                                                parent_states = list(c('False', 'True'),
                                                                                     c('False', 'True'),
                                                                                     c('False', 'True')))



Available_soil_nutrients_at_initial_stage_values <- Available_soil_nutrients_at_initial_stage_tmp$values
Available_soil_nutrients_at_initial_stage_levels <- Available_soil_nutrients_at_initial_stage_tmp$levels
Available_soil_nutrients_at_initial_stage <- cptable (~Available_soil_nutrients_at_initial_stage|
                                                        Manure_application:
                                                        Fertilizers_application_at_initial_stage:
                                                        Rich_sediments_addition_from_flood_at_initial_stage, values = Available_soil_nutrients_at_initial_stage_values,levels = Available_soil_nutrients_at_initial_stage_levels)

### Pest_and_desease_impact_at_initial_stage ####

# Pest_and_desease_impact_at_initial_stage <- cptable (~Pest_and_desease_impact_at_initial_stage|Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage, 
#                                                      values = c(0.6, 0.35, 0.05,
#                                                                 0.05, 0.35, 0.6),
#                                                      levels = c('Severe', 'Significant', 'Minor'))
# 
Pest_and_desease_impact_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(4, 3, 2, 1.75)),
                                                               parent_weights = c(2, 1),
                                                               b = 2,
                                                               child_prior = c(0.3, 0.4, 0.3),
                                                               child_states = c('Severe', 'Significant', 'Minor'),
                                                               parent_states = list(c('Poor', 'Good', "Excellent"),
                                                                                    c('Sorghum', 'Maize','Teff', 'Rice')
                                                               ))



Pest_and_desease_impact_at_initial_stage_values <- Pest_and_desease_impact_at_initial_stage_tmp$values
Pest_and_desease_impact_at_initial_stage_levels <- Pest_and_desease_impact_at_initial_stage_tmp$levels
Pest_and_desease_impact_at_initial_stage <- cptable (~Pest_and_desease_impact_at_initial_stage|
                                                       Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage:
                                                       Crop_type,
                                                     values = Pest_and_desease_impact_at_initial_stage_values,levels = Pest_and_desease_impact_at_initial_stage_levels)


## Weeds_impact_at_initial_stage ####

# Weeds_impact_at_initial_stage <- cptable (~Weeds_impact_at_initial_stage|Effectiveness_of_Weeding_at_initial_stage, 
#                                           values = c(0.6, 0.35, 0.05,
#                                                      0.05, 0.35, 0.6),
#                                           levels = c('Significant', 'Moderate', 'Negligible'))

Weeds_impact_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), rev(c(4,3,2,1))),
                                                    parent_weights = c(2, 1.5),
                                                    b = 3,
                                                    child_prior = c(0.3, 0.4, 0.3),
                                                    child_states = c('Significant', 'Moderate', 'Negligible'),
                                                    parent_states = list(c('Poor', 'Good', 'Excellent'),
                                                                         c('Sorghum', 'Maize','Teff', 'Rice')
                                                    ))



Weeds_impact_at_initial_stage_values <- Weeds_impact_at_initial_stage_tmp$values
Weeds_impact_at_initial_stage_levels <- Weeds_impact_at_initial_stage_tmp$levels
Weeds_impact_at_initial_stage <- cptable (~Weeds_impact_at_initial_stage|
                                            Effectiveness_of_Weeding_at_initial_stage:
                                            Crop_type,
                                          values = Weeds_impact_at_initial_stage_values,levels = Weeds_impact_at_initial_stage_levels)


#---------------------------------------------------------------------------------------------#
# Local constraints, crop grown, Agticultural management
#-----------------------------------------------------------------------------------#

## Agricultural_management_efficiency at initial stage ####

Agricultural_management_efficiency_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3), c(1,2,3), c(1,2,3)),
                                                                          parent_weights = c(1.5, 1.75, 2),
                                                                          # parent_weights = c(2, 1, 1),
                                                                          # parent_weights = c(2, 1, 0.5),
                                                                          # parent_weights = c(0.5, 2, 0.25),
                                                                          b = 2,
                                                                          child_prior = c(0.3, 0.4, 0.3),
                                                                          child_states = c('Low','Medium', 'High'),
                                                                          parent_states = list(c("Inadequate", "Adequate"),
                                                                                               c('Severe', 'Significant', 'Minor'),
                                                                                               c('Significant', 'Moderate', 'Negligible')
                                                                          ))



Agricultural_management_efficiency_at_initial_stage_values <- Agricultural_management_efficiency_at_initial_stage_tmp$values
Agricultural_management_efficiency_at_initial_stage_levels <- Agricultural_management_efficiency_at_initial_stage_tmp$levels
Agricultural_management_efficiency_at_initial_stage <- cptable (~Agricultural_management_efficiency_at_initial_stage|
                                                                  nutrient_supply_adequacy_at_initial_stage:
                                                                  Pest_and_desease_impact_at_initial_stage:
                                                                  Weeds_impact_at_initial_stage,
                                                                values = Agricultural_management_efficiency_at_initial_stage_values,levels = Agricultural_management_efficiency_at_initial_stage_levels)

## Crop_type ####

Crop_type <- cptable(~Crop_type, values = c(0.3, 0.4, 0.1, 0.2), levels = c('Sorghum', 'Maize','Teff', 'Rice'))

## Intercropping ####

Intercropping <- cptable(~Intercropping, values = c(0.3, 0.1, 0.6), levels = c('Dense', 'Sparse', 'None'))

## Crop_variety #####

Crop_variety <- cptable(~Crop_variety, values = c(0.6, 0.4), levels = c('Local', 'Improved'))

## Rotation ####

Previous_crop <- cptable(~Previous_crop, values = c(0.3,0.6, 0.1), levels = c('Same', 'Different', 'Fallow'))

## Planting date ####

Planting_date <- cptable(~Planting_date, values = c(0.3, 0.7), levels = c('Late', 'Early'))


### Effectiveness_of_cropping_options ####

Effectiveness_of_cropping_options_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(1,2,3), c(1,2), c(1,2), c(1, 2, 3, 4)),
                                                        parent_weights = c(2, 1, 4, 3, 0.5),
                                                        b = 1.5,
                                                        child_prior = c(0.5, 0.5),
                                                        child_states = c("Ineffective", "Effective"),
                                                        parent_states = list(c('Same', 'Different', 'Fallow'),
                                                                             c('Dense', 'Sparse', 'None'),
                                                                             c('Local', 'Improved'),
                                                                             c('Late', 'Early'),
                                                                             c('Sorghum', 'Maize','Teff', 'Rice')))



Effectiveness_of_cropping_options_values <- Effectiveness_of_cropping_options_tmp$values
Effectiveness_of_cropping_options_levels <- Effectiveness_of_cropping_options_tmp$levels
Effectiveness_of_cropping_options <- cptable (~Effectiveness_of_cropping_options|
                                                Previous_crop:
                                                Intercropping:
                                                Crop_variety:
                                                Planting_date:
                                                Crop_type,
                                              values = Effectiveness_of_cropping_options_values,levels = Effectiveness_of_cropping_options_levels)


### nutrient_supply_adequacy_at_initial_stage ####

nutrient_supply_adequacy_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(4,3,2,1)),
                                                        parent_weights = c(2,1),
                                                        b = 1.5,
                                                        child_prior = c(0.5, 0.5),
                                                        child_states = c("Inadequate", "Adequate"),
                                                        parent_states = list(c('Deficient','Satisfactory', 'Plenty'),
                                                                             c('Sorghum', 'Maize','Teff', 'Rice')))

nutrient_supply_adequacy_at_initial_stage_values <- nutrient_supply_adequacy_at_initial_stage_tmp$values
nutrient_supply_adequacy_at_initial_stage_levels <- nutrient_supply_adequacy_at_initial_stage_tmp$levels
nutrient_supply_adequacy_at_initial_stage <- cptable (~nutrient_supply_adequacy_at_initial_stage|
                                                        Available_soil_nutrients_at_initial_stage:
                                                        Crop_type,
                                                      values = nutrient_supply_adequacy_at_initial_stage_values,levels = nutrient_supply_adequacy_at_initial_stage_levels)


### Water_supply_adequacy_at_initial_stage ####


Water_supply_adequacy_at_initial_stage_values <- c(0.5, 0.5, 
                                                   0.3, 0.7,
                                                   0.7, 0.3, # In most cases, crop would not accommodate water logging at late stage
                                                   
                                                   0.8, 0.2, 
                                                   0.3, 0.7,
                                                   0.6, 0.4, # In most cases, crop would not accommodate water logging at late stage
                                                   
                                                   0.6, 0.4, 
                                                   0.7, 0.3,
                                                   0.3, 0.7, # In most cases, crop would not accommodate water logging at late stage
                                                   
                                                   0.9, 0.1, 
                                                   0.5, 0.5,
                                                   0.1, 0.9)

Water_supply_adequacy_at_initial_stage_levels <- c("Inadequate", "Adequate")
Water_supply_adequacy_at_initial_stage <- cptable (~Water_supply_adequacy_at_initial_stage|
                                                     Available_soil_water_at_initial_stage:
                                                     Crop_type,
                                                   values = Water_supply_adequacy_at_initial_stage_values,levels = Water_supply_adequacy_at_initial_stage_levels)

## Local_constraints at initial stage ####

Local_constraints_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3), c(1,2), c(1,2,3)),
                                                         parent_weights = c(2, 4, 1),
                                                         b = 1.5,
                                                         child_prior = c(0.1, 0.7, 0.2),
                                                         child_states = c('High','Medium', 'Low'),
                                                         parent_states = list(c("Ineffective", "Effective"),
                                                                              c("Inadequate", "Adequate"),
                                                                              c("Low", "Medium", "High")))



Local_constraints_at_initial_stage_values <- Local_constraints_at_initial_stage_tmp$values
Local_constraints_at_initial_stage_levels <- Local_constraints_at_initial_stage_tmp$levels
Local_constraints_at_initial_stage <- cptable (~Local_constraints_at_initial_stage|
                                                 Effectiveness_of_cropping_options:
                                                 Water_supply_adequacy_at_initial_stage:
                                                 Agricultural_management_efficiency_at_initial_stage, 
                                               values = Local_constraints_at_initial_stage_values,levels = Local_constraints_at_initial_stage_levels)
## DEVELOPMENT STAGE ####
#------------------------------------------------------------------------------------#
# amount of water reaching the plot #
#------------------------------------------------------------------------------------#

# ## Type of water diversion ####
# Type_of_water_diversion <- cptable(~Type_of_water_diversion, values = c(0.3, 0.4, 0.3),
#                                    levels = c('Modern', 'Hybrid', 'Traditional'))


## Sediment_load_at_development_stage ####
Sediment_load_at_development_stage <- cptable (~Sediment_load_at_development_stage|Type_of_water_diversion, 
                                               values =   c(0.1, 0.4, 0.5,
                                                            0.4, 0.5, 0.1,
                                                            0.799, 0.2, 0.01),
                                               # c(0.1, 0.3, 0.6,
                                               #   0.1, 0.6, 0.3,
                                               #   0.7, 0.2, 0.1),
                                               
                                               levels = c("High", 'Medium', 'Low'))


## Rain_event_occurence_at_development_stage ####
Rain_event_occurence_at_development_stage <- cptable (~Rain_event_occurence_at_development_stage, values = c(0.65, 0.35),levels = c('Off-site', 'On-site'))

# ## Slope ####
# Slope <- cptable (~Slope, values = c(0.1, 0.2, 0.7),levels = c('Steep', 'Moderate', 'Gentil'))

# ## Socio-institutional arrangements ####
# Social_arrangements <- cptable (~Social_arrangements, values = c(0.3, 0.7),levels = c('Inadequate', 'Adequate'))

## Main_Canals_Maitnenance_at_development_stage ####
Main_Canals_Maitnenance_at_initial_stage_tmp <- make_gRain_CPT(parent_effects = list(c(4,3,2),
                                                                                     c(0,1)),
                                                               parent_weights = c(1,2),
                                                               b = 1.5,
                                                               child_prior = c(0.5,0.5),
                                                               child_states = c('Poor', 'Good'),
                                                               parent_states = list(c('Modern', 'Hybrid', 'Traditional'),
                                                                                    c('Inadequate', 'Adequate')))

# Main_Canals_Maitnenance_at_initial_stage_values <- Main_Canals_Maitnenance_at_initial_stage_tmp$values
Main_Canals_Maitnenance_at_initial_stage_values <- c(0.919293820933165, 0.0807061790668348,
                                                     0.835051546391752, 0.164948453608247, 
                                                     0.692307692307692, 0.307692307692308, 
                                                     0.0807061790668348, 0.919293820933165, 
                                                     0.164948453608247, 0.835051546391752, 
                                                     0.307692307692308, 0.692307692307692)
Main_Canals_Maitnenance_at_initial_stage_levels <- Main_Canals_Maitnenance_at_initial_stage_tmp$levels
Main_Canals_Maitnenance_at_initial_stage <- cptable (~Main_Canals_Maitnenance_at_initial_stage|Type_of_water_diversion:Social_arrangements, values = Main_Canals_Maitnenance_at_initial_stage_values,levels = Main_Canals_Maitnenance_at_initial_stage_levels)

Main_Canals_Maitnenance_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3,2),
                                                                                         c(0,1)),
                                                                   parent_weights = c(1,2),
                                                                   b = 1.5,
                                                                   # child_prior = c(0.5,0.5),
                                                                   child_prior = c(0.6,0.4),
                                                                   child_states = c('Poor', 'Good'),
                                                                   parent_states = list(c('Modern', 'Hybrid', 'Traditional'),
                                                                                        c('Inadequate', 'Adequate')))

# Main_Canals_Maitnenance_at_development_stage_values <- Main_Canals_Maitnenance_at_development_stage_tmp$values
Main_Canals_Maitnenance_at_development_stage_values <- c(0.919293820933165, 0.0807061790668348,
                                                         0.835051546391752, 0.164948453608247,
                                                         0.692307692307692, 0.307692307692308,
                                                         0.0807061790668348, 0.919293820933165,
                                                         0.164948453608247, 0.835051546391752,
                                                         0.307692307692308, 0.692307692307692)
Main_Canals_Maitnenance_at_development_stage_levels <- Main_Canals_Maitnenance_at_development_stage_tmp$levels
Main_Canals_Maitnenance_at_development_stage <- cptable (~Main_Canals_Maitnenance_at_development_stage|Type_of_water_diversion:Social_arrangements, values = Main_Canals_Maitnenance_at_development_stage_values,levels = Main_Canals_Maitnenance_at_development_stage_levels)


## Amount_of_shared_flood_at_development_stage ####
Amount_of_shared_flood_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2), 
                                                                                        c(1,2), 
                                                                                        c(1,2,4),
                                                                                        c(1,2,3.5)),
                                                                  parent_weights = c(1,2,3,2),
                                                                  b = 2.5,
                                                                  child_prior = c(0.3, 0.6, 0.1),
                                                                  child_states = c('< Enough', 'Enough', '> Enough'),
                                                                  parent_states = list(c('Off-site', 'On-site'),
                                                                                       c('Poor', 'Good'),
                                                                                       c('High', 'Medium',  'Low'), 
                                                                                       c('Steep','Moderate', 'Gentil')))

Amount_of_shared_flood_at_development_stage_values <- Amount_of_shared_flood_at_development_stage_tmp$values
Amount_of_shared_flood_at_development_stage_levels <- Amount_of_shared_flood_at_development_stage_tmp$levels
Amount_of_shared_flood_at_development_stage <- cptable (~Amount_of_shared_flood_at_development_stage|
                                                          Rain_event_occurence_at_development_stage:
                                                          Main_Canals_Maitnenance_at_development_stage:
                                                          Sediment_load_at_development_stage:
                                                          Slope, values = Amount_of_shared_flood_at_development_stage_values,levels = Amount_of_shared_flood_at_development_stage_levels)

# ## Location_of_the_plot ####
# Location_of_the_plot <- cptable (~Location_of_the_plot, values = c(0.6, 0.3, 0.1),levels = c('Lowlands', 'Midlands', 'Highlands'))
## Upstream_abstraction_at_development_stage ####

Upstream_abstraction_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                                      c(0,1,2)),
                                                                parent_weights = c(1,2),
                                                                # b = 3,
                                                                b = 2.5,
                                                                child_prior = c(0.6,0.4),
                                                                child_states = c('Unfair', 'Fair'),
                                                                parent_states = list(c('Inadequate', 'Adequate'),
                                                                                     c('Lowlands', 'Midlands', 'Highlands')))



# Upstream_abstraction_at_development_stage_values <- Upstream_abstraction_at_development_stage_tmp$values
Upstream_abstraction_at_development_stage_values <- c(0.999086340794883, 0.000913659205116492,
                                                      0.242857142857143, 0.757142857142857,
                                                      0.931034482758621, 0.0689655172413793, 
                                                      0.142857142857143, 0.857142857142857,
                                                      0.142857142857143, 0.857142857142857, 
                                                      0.00205338809034908, 0.997946611909651)
Upstream_abstraction_at_development_stage_levels <- Upstream_abstraction_at_development_stage_tmp$levels
Upstream_abstraction_at_development_stage <- cptable (~Upstream_abstraction_at_development_stage|Social_arrangements:Location_of_the_plot, values = Upstream_abstraction_at_development_stage_values,levels = Upstream_abstraction_at_development_stage_levels)

## Field_canal_maintenance_at_development_stage####
Field_canal_maintenance_at_development_stage<- cptable (~Field_canal_maintenance_at_development_stage, values = c(0.2, 0.8),levels = c('Poor', 'Good'))

## Presence_of_the_farmer_during_flood_event_at_development_stage ###
Presence_of_the_farmer_during_flood_event_at_development_stage <- cptable (~Presence_of_the_farmer_during_flood_event_at_development_stage, values = c(0.2, 0.8),levels = c('False', 'True'))

## Amount_of_flood_reaching_the_plot_at_development_stage ####
Amount_of_flood_reaching_the_plot_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2), 
                                                                                                   c(0,2), 
                                                                                                   c(1,2,3), 
                                                                                                   c(1,2)),
                                                                             parent_weights = c(1,1,3,3),
                                                                             b = 2,
                                                                             child_prior = c(0.2,0.7,0.1),
                                                                             child_states = c('Too little', 'Desired', 'Too much'),
                                                                             parent_states = list(c('Poor', 'Good'),
                                                                                                  c('False', 'True'),
                                                                                                  c('< Enough', 'Enough', '> Enough'),
                                                                                                  c('Unfair', 'Fair')))



Amount_of_flood_reaching_the_plot_at_development_stage_values <- Amount_of_flood_reaching_the_plot_at_development_stage_tmp$values
Amount_of_flood_reaching_the_plot_at_development_stage_levels <- Amount_of_flood_reaching_the_plot_at_development_stage_tmp$levels
Amount_of_flood_reaching_the_plot_at_development_stage <- cptable (~Amount_of_flood_reaching_the_plot_at_development_stage|Field_canal_maintenance_at_development_stage:Presence_of_the_farmer_during_flood_event_at_development_stage:Amount_of_shared_flood_at_development_stage:Upstream_abstraction_at_development_stage, values = Amount_of_flood_reaching_the_plot_at_development_stage_values,levels = Amount_of_flood_reaching_the_plot_at_development_stage_levels)


#------------------------------------------------------------------------------------#
# Available soil water #
#------------------------------------------------------------------------------------#

# ## Soil_type ####
# 
# Soil_type <- cptable (~Soil_type, values = c(0.05, 0.55, 0.4),levels = c('Sandy', 'Loamy', 'Clayey'))

# ## Manure_application ####
# 
# Manure_application <- cptable(~Manure_application, values = c(0.3, 0.7), levels = c('False', 'True'))

# ## Soil_water_holding_capacity ####
# 
# Soil_water_holding_capacity_tmp <- make_gRain_CPT(parent_effects = list(c(0, 2.5, 3),
#                                                                         c(0, 2)),
#                                                   parent_weights = c(2,1),
#                                                   b = 3,
#                                                   child_prior = c(0.2,0.5,0.3),
#                                                   child_states = c('Low', 'Medium', 'High'),
#                                                   parent_states = list(c('Sandy', 'Loamy', 'Clayey'),
#                                                                        c('False', 'True')))
# Soil_water_holding_capacity_values <- Soil_water_holding_capacity_tmp$values
# Soil_water_holding_capacity_levels <- Soil_water_holding_capacity_tmp$levels
# Soil_water_holding_capacity <- cptable (~Soil_water_holding_capacity|Soil_type:Manure_application, values = Soil_water_holding_capacity_values,levels = Soil_water_holding_capacity_levels)

## Evapotranspiration_at_development_stage ####

Evapotranspiration_at_development_stage <- cptable (~Evapotranspiration_at_development_stage, values = c(0.05, 0.9, 0.05), # c(0.1, 0.2, 0.7),
                                                    levels = c('High', 'Medium', 'Low'))

## Rainfall_amount_at_development_stage ####

Rainfall_amount_at_development_stage <- cptable (~Rainfall_amount_at_development_stage, values = c(0.25, 0.6, 0.15),levels = c('< normal', 'Normal', '> normal'))

# ## Initial_soil_water_content ####
# 
# Initial_soil_water_content <- cptable (~Initial_soil_water_content, values = c(0.40, 0.30, 0.15, 0.15),levels = c('Very low', 'Low', 'Medium', 'High'))

## Available_soil_water_at_development_stage ####

Available_soil_water_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2,3),
                                                                                      c(1,2,3),
                                                                                      c(0,2,3.5),
                                                                                      # c(1,2,3,4),
                                                                                      c(0, 2, 2),
                                                                                      c(1,2,3)),
                                                                # parent_weights = c(4, 2.5, 2, 1, 3),
                                                                parent_weights = c(4, 2, 2.5, 3, 1.5),
                                                                b = 1.5,
                                                                child_prior = c(0.1, 0.6, 0.3),
                                                                child_states = c('Drought risk', 'Normal', 'Waterlogging risk'),
                                                                parent_states = list(c('Low', 'Medium', 'High'),
                                                                                     c('High', 'Medium', 'Low'),
                                                                                     c('< normal', 'Normal', '> normal'), 
                                                                                     # c('Very low', 'Low', 'Medium', 'High'),
                                                                                     c('Too little', 'Desired', 'Too much'),
                                                                                     c('Too little', 'Normal', 'Too much')))
Available_soil_water_at_development_stage_values <- Available_soil_water_at_development_stage_tmp$values
Available_soil_water_at_development_stage_levels <- Available_soil_water_at_development_stage_tmp$levels
Available_soil_water_at_development_stage <- cptable (~Available_soil_water_at_development_stage|
                                                        Soil_water_holding_capacity:
                                                        Evapotranspiration_at_development_stage:
                                                        Rainfall_amount_at_development_stage:
                                                        # Initial_soil_water_content:
                                                        Amount_of_flood_reaching_the_plot_at_development_stage:
                                                        Available_soil_water_at_initial_stage, values = Available_soil_water_at_development_stage_values,levels = Available_soil_water_at_development_stage_levels)


#-----------------------------------------------------------------------------------#
# Available soil nutrients at initial stage, Pest and desease impact at initial stage, Weeds impacts
#-----------------------------------------------------------------------------------#
# 
# ## Relative_wealth_status ####
# 
# Relative_wealth_status <- cptable(~Relative_wealth_status, values = c(0.6, 0.3, 0.1), levels = c('Poor', 'Middle class', 'Rich'))

# ## Mutual_aids ####
# 
# Mutual_aids <- cptable(~Mutual_aids, values = c(0.3, 0.7), levels = c('False', 'True'))

## Access_to_inputs_at_development_stage ####

Access_to_inputs_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2,3), 
                                                                                  c(0,2)),
                                                            parent_weights = c(1.5, 1),
                                                            b = 1.5,
                                                            child_prior = c(0.4,0.6),
                                                            child_states = c('False', 'True'),
                                                            parent_states = list(c('Poor', 'Middle class', 'Rich'),
                                                                                 c('False', 'True')))
Access_to_inputs_at_development_stage_values <- Access_to_inputs_at_development_stage_tmp$values
Access_to_inputs_at_development_stage_levels <- Access_to_inputs_at_development_stage_tmp$levels
Access_to_inputs_at_development_stage <- cptable (~Access_to_inputs_at_development_stage|Relative_wealth_status:Mutual_aids, values = Access_to_inputs_at_development_stage_values,levels = Access_to_inputs_at_development_stage_levels)

## Available_paid_labor_at_development_stage #####
Available_paid_labor_at_development_stage <- cptable(~Available_paid_labor_at_development_stage, values = c(0.4, 0.6), levels = c('Unavailable', 'Available'))
## Available_Labor_force_at_development_stage #####

Available_Labor_force_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(2, 1.5, 0), 
                                                                                       c(0,1),
                                                                                       c(1, 2)),
                                                                 parent_weights = c(3, 1, 3),
                                                                 b = 1.5,
                                                                 child_prior = c(0.2,0.8),
                                                                 child_states = c('Unsufficient', 'Sufficient'),
                                                                 parent_states = list(c('Poor', 'Middle class', 'Rich'),
                                                                                      c('False', 'True'),
                                                                                      c('Unavailable', 'Available')))
# Available_Labor_force_at_development_stage_values <- Available_Labor_force_at_development_stage_tmp$values
Available_Labor_force_at_development_stage_values <- c(0.35, 0.65, 
                                                       0.654986522911051, 0.345013477088949, 
                                                       0.986483256178967, 0.0135167438210335, 
                                                       
                                                       0.3, 0.7,                                                   
                                                       0.5, 0.5,
                                                       0.7, 0.3,
                                                       
                                                       0.3, 0.7,                                                   
                                                       0.4, 0.6,
                                                       0.4, 0.6,
                                                       
                                                       0.00288192548646564, 0.997118074513534, 
                                                       0.1, 0.9,
                                                       0.15, 0.85)
Available_Labor_force_at_development_stage_levels <- Available_Labor_force_at_development_stage_tmp$levels
Available_Labor_force_at_development_stage <- cptable (~Available_Labor_force_at_development_stage|Relative_wealth_status:Mutual_aids:Available_paid_labor_at_development_stage, values = Available_Labor_force_at_development_stage_values,levels = Available_Labor_force_at_development_stage_levels)

# ## skills_of_the_farmer #####
# 
# skills_of_the_farmer <- cptable(~skills_of_the_farmer, values = c(0.3, 0.7), levels = c('Defavorable', 'Favorable'))

## Chemical_application_at_development_stage ####
Chemical_application_at_development_stage <- cptable (~Chemical_application_at_development_stage|Access_to_inputs_at_development_stage, 
                                                      values = c(0.8, 0.2,
                                                                 0.4, 0.6),
                                                      levels = c('Unufficient', 'Sufficient'))

### Application_of_traditional_crop_protection_Methods_at_development_stage #####

Application_of_traditional_crop_protection_Methods_at_development_stage <- cptable(~Application_of_traditional_crop_protection_Methods_at_development_stage, values = c(0.1, 0.9), levels = c('Uncommon', 'Common'))

## Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage ####

Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                                                                       c(1,2),
                                                                                                                       c(1,2),
                                                                                                                       c(1,2)),
                                                                                                 parent_weights = c(1.5,2,1,3),
                                                                                                 b = 1.5,
                                                                                                 child_prior = c(0.3,0.4,0.3),
                                                                                                 child_states = c('Poor', 'Good', 'Excellent'),
                                                                                                 parent_states = list(c('Unsufficient', 'Sufficient'),
                                                                                                                      c('Uncommon', 'Common'),
                                                                                                                      c('Unsufficient', 'Sufficient'),
                                                                                                                      c('Defavorable', 'Favorable')))


Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage_values <- Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage_tmp$values
Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage_levels <- Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage_tmp$levels
Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage <- cptable (~Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage|
                                                                                         Chemical_application_at_development_stage:
                                                                                         Application_of_traditional_crop_protection_Methods_at_development_stage:
                                                                                         Available_Labor_force_at_development_stage:
                                                                                         skills_of_the_farmer, values = Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage_values,levels = Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage_levels)

## Effectiveness_of_Weeding_at_development_stage #####

Effectiveness_of_Weeding_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                                          c(1,2)),
                                                                    parent_weights = c(1, 1.5),
                                                                    b = 1.75,
                                                                    child_prior = c(0.3,0.4,0.3),
                                                                    child_states = c('Poor', 'Good', 'Excellent'),
                                                                    parent_states = list(c('Unsufficient', 'Sufficient'),
                                                                                         c('Defavorable', 'Favorable')))



Effectiveness_of_Weeding_at_development_stage_values <- Effectiveness_of_Weeding_at_development_stage_tmp$values
Effectiveness_of_Weeding_at_development_stage_levels <- Effectiveness_of_Weeding_at_development_stage_tmp$levels
Effectiveness_of_Weeding_at_development_stage <- cptable (~Effectiveness_of_Weeding_at_development_stage|Available_Labor_force_at_development_stage:skills_of_the_farmer, values = Effectiveness_of_Weeding_at_development_stage_values,levels = Effectiveness_of_Weeding_at_development_stage_levels)


## Fertilizers_application_at_development_stage #####

Fertilizers_application_at_development_stage <- cptable (~Fertilizers_application_at_development_stage|Access_to_inputs_at_development_stage, 
                                                         values = c(0.9, 0.1,
                                                                    0.3, 0.7),
                                                         levels = c('False', 'True'))

## Rich_sediments_addition_from_flood_at_development_stage ####

Rich_sediments_addition_from_flood_at_development_stage <- cptable(~Rich_sediments_addition_from_flood_at_development_stage, values = c(0.4, 0.6), levels = c('False', 'True'))

## Available_soil_nutrients_at_development_stage ####

Available_soil_nutrients_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(
  # c(1,2), 
  c(1,2), 
  c(1,2),
  c(1,2,3)),
  # parent_weights = c(2,3,1),
  parent_weights = c(2,3,1),
  
  b = 1.5,
  child_prior = c(0.25, 0.5, 0.25),
  child_states = c('Deficient','Satisfactory', 'Plenty'),
  parent_states = list(
    # c('False', 'True'),
    c('False', 'True'),
    c('False', 'True'),
    c('Deficient','Satisfactory', 'Plenty')))



Available_soil_nutrients_at_development_stage_values <- Available_soil_nutrients_at_development_stage_tmp$values
Available_soil_nutrients_at_development_stage_levels <- Available_soil_nutrients_at_development_stage_tmp$levels
Available_soil_nutrients_at_development_stage <- cptable (~Available_soil_nutrients_at_development_stage|
                                                            #Manure_application:
                                                            Fertilizers_application_at_development_stage:
                                                            Rich_sediments_addition_from_flood_at_development_stage:
                                                            Available_soil_nutrients_at_initial_stage, values = Available_soil_nutrients_at_development_stage_values,levels = Available_soil_nutrients_at_development_stage_levels)

### Pest_and_desease_impact_at_development_stage ####

# Pest_and_desease_impact_at_development_stage <- cptable (~Pest_and_desease_impact_at_development_stage|
#                                                    Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage,
#                                                     values = c(0.5, 0.3, 0.2,
#                                                                0.15, 0.25, 0.6),
#                                                     levels = c('Severe', 'Significant', 'Minor'))

Pest_and_desease_impact_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(1, 2, 3), c(4,3,2,1)),
                                                                   parent_weights = c(2, 3, 1),
                                                                   b = 1.75,
                                                                   child_prior = c(0.3, 0.4, 0.3),
                                                                   child_states = c('Severe', 'Significant', 'Minor'),
                                                                   parent_states = list(c('Poor', 'Good', 'Excellent'),
                                                                                        c('Severe', 'Significant', 'Minor'),
                                                                                        c('Sorghum', 'Maize','Teff', 'Rice')))
Pest_and_desease_impact_at_development_stage_values <- Pest_and_desease_impact_at_development_stage_tmp$values
Pest_and_desease_impact_at_development_stage_levels <- Pest_and_desease_impact_at_development_stage_tmp$levels
Pest_and_desease_impact_at_development_stage <- cptable (~Pest_and_desease_impact_at_development_stage|
                                                           Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage:
                                                           Pest_and_desease_impact_at_initial_stage:
                                                           Crop_type, 
                                                         values = Pest_and_desease_impact_at_development_stage_values,levels = Pest_and_desease_impact_at_development_stage_levels)

## Weeds_impact_at_development_stage ####

# Weeds_impact_at_development_stage <- cptable (~Weeds_impact_at_development_stage|Effectiveness_of_Weeding_at_development_stage, 
#                                          values = c(0.5, 0.3, 0.2,
#                                                     0.15, 0.25, 0.6),
#                                          levels = c('Significant', 'Moderate', 'Negligible'))

Weeds_impact_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(1, 2, 3), rev(c(4,3,2,1))),
                                                        parent_weights = c(2, 3, 1),
                                                        b = 2.5,
                                                        child_prior = c(0.3, 0.4, 0.3),
                                                        child_states = c('Significant', 'Moderate', 'Negligible'),
                                                        parent_states = list(c('Poor', 'Good', 'Excellent'),
                                                                             c('Significant', 'Moderate', 'Negligible'),
                                                                             c('Sorghum', 'Maize','Teff', 'Rice')))
Weeds_impact_at_development_stage_values <- Weeds_impact_at_development_stage_tmp$values
Weeds_impact_at_development_stage_levels <- Weeds_impact_at_development_stage_tmp$levels
Weeds_impact_at_development_stage <- cptable (~Weeds_impact_at_development_stage|
                                                Effectiveness_of_Weeding_at_development_stage:
                                                Weeds_impact_at_initial_stage:
                                                Crop_type, 
                                              values = Weeds_impact_at_development_stage_values,levels = Weeds_impact_at_development_stage_levels)

#---------------------------------------------------------------------------------------------#
# Local constraints, crop grown, Agticultural management
#-----------------------------------------------------------------------------------#

## Farming_inefficiency at developemnt stage ####

Agricultural_management_efficiency_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3), c(1,2,3), c(1,2,3)),
                                                                              # parent_weights = c(1.5, 1.75, 2, 1.5, 1),
                                                                              parent_weights = c(1.5, 1.5, 1.75),
                                                                              
                                                                              # parent_weights = c(2, 1, 0.5),
                                                                              # parent_weights = c(0.5, 2, 0.25),
                                                                              b = 2,
                                                                              child_prior = c(0.3, 0.4, 0.3),
                                                                              child_states = c('Low','Medium', 'High'),
                                                                              parent_states = list(c("Inadequate", "Adequate"),
                                                                                                   c('Severe', 'Significant', 'Minor'),
                                                                                                   c('Significant', 'Moderate', 'Negligible')
                                                                              ))



Agricultural_management_efficiency_at_development_stage_values <- Agricultural_management_efficiency_at_development_stage_tmp$values
Agricultural_management_efficiency_at_development_stage_levels <- Agricultural_management_efficiency_at_development_stage_tmp$levels
Agricultural_management_efficiency_at_development_stage <- cptable (~Agricultural_management_efficiency_at_development_stage|
                                                                      nutrient_supply_adequacy_at_development_stage:
                                                                      Pest_and_desease_impact_at_development_stage:
                                                                      Weeds_impact_at_development_stage,
                                                                    values = Agricultural_management_efficiency_at_development_stage_values,levels = Agricultural_management_efficiency_at_development_stage_levels)

### nutrient_supply_adequacy_at_development_stage ####

nutrient_supply_adequacy_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(4,3,2,1)),
                                                                parent_weights = c(2,1),
                                                                b = 3,
                                                                child_prior = c(0.5, 0.5),
                                                                child_states = c("Inadequate", "Adequate"),
                                                                parent_states = list(c('Deficient','Satisfactory', 'Plenty'),
                                                                                     c('Sorghum', 'Maize','Teff', 'Rice')))

nutrient_supply_adequacy_at_development_stage_values <- nutrient_supply_adequacy_at_development_stage_tmp$values
nutrient_supply_adequacy_at_development_stage_levels <- nutrient_supply_adequacy_at_development_stage_tmp$levels
nutrient_supply_adequacy_at_development_stage <- cptable (~nutrient_supply_adequacy_at_development_stage|
                                                        Available_soil_nutrients_at_development_stage:
                                                        Crop_type,
                                                      values = nutrient_supply_adequacy_at_development_stage_values,levels = nutrient_supply_adequacy_at_development_stage_levels)


### Water_supply_adequacy_at_development_stage ####


Water_supply_adequacy_at_development_stage_values <- c(0.5, 0.5, 
                                                       0.3, 0.7,
                                                       0.7, 0.3, # In most cases, crop would not accommodate water logging at late stage
                                                       
                                                       0.8, 0.2, 
                                                       0.3, 0.7,
                                                       0.6, 0.4, # In most cases, crop would not accommodate water logging at late stage
                                                       
                                                       0.6, 0.4, 
                                                       0.7, 0.3,
                                                       0.3, 0.7, # In most cases, crop would not accommodate water logging at late stage
                                                       
                                                       0.9, 0.1, 
                                                       0.5, 0.5,
                                                       0.1, 0.9)
Water_supply_adequacy_at_development_stage_levels <- c("Inadequate", "Adequate")
Water_supply_adequacy_at_development_stage <- cptable (~Water_supply_adequacy_at_development_stage|
                                                         Available_soil_water_at_development_stage:
                                                         Crop_type,
                                                       values = Water_supply_adequacy_at_development_stage_values,levels = Water_supply_adequacy_at_development_stage_levels)



## Local_constraints at initial stage ####

Local_constraints_at_development_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3), c(0.5, 3), c(1,2,3)),
                                                             # parent_weights = c(2, 4, 1),
                                                             # parent_weights = c(1.5, 4, 1.5),
                                                             parent_weights = c(2, 4, 1.5),
                                                             b = 1.5,
                                                             child_prior = c(0.1, 0.7, 0.2),
                                                             child_states = c('High','Medium', 'Low'),
                                                             parent_states = list(c("Ineffective", "Effective"),
                                                                                  c("Inadequate", "Adequate"),
                                                                                  c("Low", "Medium", "High")))



Local_constraints_at_development_stage_values <- Local_constraints_at_development_stage_tmp$values
Local_constraints_at_development_stage_levels <- Local_constraints_at_development_stage_tmp$levels
Local_constraints_at_development_stage <- cptable (~Local_constraints_at_development_stage|
                                                     Effectiveness_of_cropping_options:
                                                     Water_supply_adequacy_at_development_stage:
                                                     Agricultural_management_efficiency_at_development_stage, 
                                                   values = Local_constraints_at_development_stage_values,levels = Local_constraints_at_development_stage_levels)


## MID STAGE ####
#------------------------------------------------------------------------------------#
# amount of water reaching the plot #
#------------------------------------------------------------------------------------#

# ## Type of water diversion ####
# Type_of_water_diversion <- cptable(~Type_of_water_diversion, values = c(0.3, 0.4, 0.3),
#                                    levels = c('Modern', 'Hybrid', 'Traditional'))


## Sediment_load_at_mid_stage ####
Sediment_load_at_mid_stage <- cptable (~Sediment_load_at_mid_stage|Type_of_water_diversion, values =   c(0.3, 0.6, 0.1,
                                                                                                         0.5, 0.4, 0.1,
                                                                                                         0.85, 0.14, 0.01),
                                       levels = c("High", 'Medium', 'Low'))


## Rain_event_occurence_at_mid_stage ####
Rain_event_occurence_at_mid_stage <- cptable (~Rain_event_occurence_at_mid_stage, values = c(0.65, 0.35),levels = c('Off-site', 'On-site'))

# ## Slope ####
# Slope <- cptable (~Slope, values = c(0.1, 0.2, 0.7),levels = c('Steep', 'Moderate', 'Gentil'))

# ## Socio-institutional arrangements ####
# Social_arrangements <- cptable (~Social_arrangements, values = c(0.3, 0.7),levels = c('Inadequate', 'Adequate'))

## Main_Canals_Maitnenance_at_mid_stage ####
Main_Canals_Maitnenance_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3,2),
                                                                                 c(0,1)),
                                                           parent_weights = c(1,2),
                                                           b = 1.5,
                                                           # child_prior = c(0.5,0.5),
                                                           # child_prior = c(0.6,0.4),
                                                           child_prior = c(0.7,0.3),
                                                           child_states = c('Poor', 'Good'),
                                                           parent_states = list(c('Modern', 'Hybrid', 'Traditional'),
                                                                                c('Inadequate', 'Adequate')))

# Main_Canals_Maitnenance_at_mid_stage_values <- Main_Canals_Maitnenance_at_mid_stage_tmp$values
Main_Canals_Maitnenance_at_mid_stage_values <- c(0.919293820933165, 0.0807061790668348,
                                                 0.835051546391752, 0.164948453608247,
                                                 0.692307692307692, 0.307692307692308,
                                                 0.0807061790668348, 0.919293820933165,
                                                 0.164948453608247, 0.835051546391752,
                                                 0.307692307692308, 0.692307692307692)
Main_Canals_Maitnenance_at_mid_stage_levels <- Main_Canals_Maitnenance_at_mid_stage_tmp$levels
Main_Canals_Maitnenance_at_mid_stage <- cptable (~Main_Canals_Maitnenance_at_mid_stage|Type_of_water_diversion:Social_arrangements, values = Main_Canals_Maitnenance_at_mid_stage_values,levels = Main_Canals_Maitnenance_at_mid_stage_levels)


## Amount_of_shared_flood_at_mid_stage ####
Amount_of_shared_flood_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2), 
                                                                                c(1,2), 
                                                                                c(1,2,4),
                                                                                c(1,2,3.5)),
                                                          parent_weights = c(1,2,3,2),
                                                          b = 2.5,
                                                          child_prior = c(0.3, 0.6, 0.1),
                                                          child_states = c('< Enough', 'Enough', '> Enough'),
                                                          parent_states = list(c('Off-site', 'On-site'),
                                                                               c('Poor', 'Good'),
                                                                               c('High', 'Medium',  'Low'), 
                                                                               c('Steep','Moderate', 'Gentil')))

Amount_of_shared_flood_at_mid_stage_values <- Amount_of_shared_flood_at_mid_stage_tmp$values
Amount_of_shared_flood_at_mid_stage_levels <- Amount_of_shared_flood_at_mid_stage_tmp$levels
Amount_of_shared_flood_at_mid_stage <- cptable (~Amount_of_shared_flood_at_mid_stage|
                                                  Rain_event_occurence_at_mid_stage:
                                                  Main_Canals_Maitnenance_at_mid_stage:
                                                  Sediment_load_at_mid_stage:
                                                  Slope, values = Amount_of_shared_flood_at_mid_stage_values,levels = Amount_of_shared_flood_at_mid_stage_levels)

# ## Location_of_the_plot ####
# Location_of_the_plot <- cptable (~Location_of_the_plot, values = c(0.6, 0.3, 0.1),levels = c('Lowlands', 'Midlands', 'Highlands'))
## Upstream_abstraction_at_mid_stage ####
Upstream_abstraction_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                              c(0,1,2)),
                                                        parent_weights = c(1,2),
                                                        # b = 3,
                                                        # b = 2.5
                                                        b = 2,
                                                        child_prior = c(0.6,0.4),
                                                        child_states = c('Unfair', 'Fair'),
                                                        parent_states = list(c('Inadequate', 'Adequate'),
                                                                             c('Lowlands', 'Midlands', 'Highlands')))



# Upstream_abstraction_at_mid_stage_values <- Upstream_abstraction_at_mid_stage_tmp$values
Upstream_abstraction_at_mid_stage_values <- c(0.999086340794883, 0.000913659205116492,
                                              0.242857142857143, 0.757142857142857,
                                              0.931034482758621, 0.0689655172413793, 
                                              0.142857142857143, 0.857142857142857,
                                              0.142857142857143, 0.857142857142857, 
                                              0.00205338809034908, 0.997946611909651)
Upstream_abstraction_at_mid_stage_levels <- Upstream_abstraction_at_mid_stage_tmp$levels
Upstream_abstraction_at_mid_stage <- cptable (~Upstream_abstraction_at_mid_stage|Social_arrangements:Location_of_the_plot, values = Upstream_abstraction_at_mid_stage_values,levels = Upstream_abstraction_at_mid_stage_levels)

## Field_canal_maintenance_at_mid_stage####
Field_canal_maintenance_at_mid_stage<- cptable (~Field_canal_maintenance_at_mid_stage, values = c(0.2, 0.8),levels = c('Poor', 'Good'))

## Presence_of_the_farmer_during_flood_event_at_mid_stage ###
Presence_of_the_farmer_during_flood_event_at_mid_stage <- cptable (~Presence_of_the_farmer_during_flood_event_at_mid_stage, values = c(0.2, 0.8),levels = c('False', 'True'))

## Amount_of_flood_reaching_the_plot_at_mid_stage ####
Amount_of_flood_reaching_the_plot_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2), 
                                                                                           c(0,2), 
                                                                                           c(1,2,3), 
                                                                                           c(1,2)),
                                                                     parent_weights = c(1,1,3,3),
                                                                     b = 2,
                                                                     child_prior = c(0.2,0.7,0.1),
                                                                     child_states = c('Too little', 'Desired', 'Too much'),
                                                                     parent_states = list(c('Poor', 'Good'),
                                                                                          c('False', 'True'),
                                                                                          c('< Enough', 'Enough', '> Enough'),
                                                                                          c('Unfair', 'Fair')))



Amount_of_flood_reaching_the_plot_at_mid_stage_values <- Amount_of_flood_reaching_the_plot_at_mid_stage_tmp$values
Amount_of_flood_reaching_the_plot_at_mid_stage_levels <- Amount_of_flood_reaching_the_plot_at_mid_stage_tmp$levels
Amount_of_flood_reaching_the_plot_at_mid_stage <- cptable (~Amount_of_flood_reaching_the_plot_at_mid_stage|Field_canal_maintenance_at_mid_stage:Presence_of_the_farmer_during_flood_event_at_mid_stage:Amount_of_shared_flood_at_mid_stage:Upstream_abstraction_at_mid_stage, values = Amount_of_flood_reaching_the_plot_at_mid_stage_values,levels = Amount_of_flood_reaching_the_plot_at_mid_stage_levels)


#------------------------------------------------------------------------------------#
# Available soil water #
#------------------------------------------------------------------------------------#

# ## Soil_type ####
# 
# Soil_type <- cptable (~Soil_type, values = c(0.05, 0.55, 0.4),levels = c('Sandy', 'Loamy', 'Clayey'))

# ## Manure_application ####
# 
# Manure_application <- cptable(~Manure_application, values = c(0.3, 0.7), levels = c('False', 'True'))

# ## Soil_water_holding_capacity ####
# 
# Soil_water_holding_capacity_tmp <- make_gRain_CPT(parent_effects = list(c(0, 2.5, 3),
#                                                                         c(0, 2)),
#                                                   parent_weights = c(2,1),
#                                                   b = 3,
#                                                   child_prior = c(0.2,0.5,0.3),
#                                                   child_states = c('Low', 'Medium', 'High'),
#                                                   parent_states = list(c('Sandy', 'Loamy', 'Clayey'),
#                                                                        c('False', 'True')))
# Soil_water_holding_capacity_values <- Soil_water_holding_capacity_tmp$values
# Soil_water_holding_capacity_levels <- Soil_water_holding_capacity_tmp$levels
# Soil_water_holding_capacity <- cptable (~Soil_water_holding_capacity|Soil_type:Manure_application, values = Soil_water_holding_capacity_values,levels = Soil_water_holding_capacity_levels)

## Evapotranspiration_at_mid_stage ####

Evapotranspiration_at_mid_stage <- cptable (~Evapotranspiration_at_mid_stage, values = c(0.9, 0.05, 0.05), # c(0.05, 0.9, 0.05), # c(0.1, 0.2, 0.7),
                                            levels = c('High', 'Medium', 'Low'))

## Rainfall_amount_at_mid_stage ####

Rainfall_amount_at_mid_stage <- cptable (~Rainfall_amount_at_mid_stage, values = c(0.25, 0.6, 0.15),levels = c('< normal', 'Normal', '> normal'))

# ## Initial_soil_water_content ####
# 
# Initial_soil_water_content <- cptable (~Initial_soil_water_content, values = c(0.40, 0.30, 0.15, 0.15),levels = c('Very low', 'Low', 'Medium', 'High'))

## Available_soil_water_at_mid_stage ####

Available_soil_water_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2,3),
                                                                              c(1,2,3),
                                                                              c(0,2,3.5),
                                                                              # c(1,2,3,4),
                                                                              c(0, 2, 2),
                                                                              c(1,2,3)),
                                                        # parent_weights = c(4, 2.5, 2, 1, 3),
                                                        parent_weights = c(4, 2, 2.5, 3, 1.5),
                                                        b = 1.5,
                                                        child_prior = c(0.1, 0.6, 0.3),
                                                        child_states = c('Drought risk', 'Normal', 'Waterlogging risk'),
                                                        parent_states = list(c('Low', 'Medium', 'High'),
                                                                             c('High', 'Medium', 'Low'),
                                                                             c('< normal', 'Normal', '> normal'), 
                                                                             # c('Very low', 'Low', 'Medium', 'High'),
                                                                             c('Too little', 'Desired', 'Too much'),
                                                                             c('Too little', 'Normal', 'Too much')))
Available_soil_water_at_mid_stage_values <- Available_soil_water_at_mid_stage_tmp$values
Available_soil_water_at_mid_stage_levels <- Available_soil_water_at_mid_stage_tmp$levels
Available_soil_water_at_mid_stage <- cptable (~Available_soil_water_at_mid_stage|
                                                Soil_water_holding_capacity:
                                                Evapotranspiration_at_mid_stage:
                                                Rainfall_amount_at_mid_stage:
                                                # Initial_soil_water_content:
                                                Amount_of_flood_reaching_the_plot_at_mid_stage:
                                                Available_soil_water_at_development_stage, values = Available_soil_water_at_mid_stage_values,levels = Available_soil_water_at_mid_stage_levels)


#-----------------------------------------------------------------------------------#
# Available soil nutrients at initial stage, Pest and desease impact at initial stage, Weeds impacts
#-----------------------------------------------------------------------------------#
# 
# ## Relative_wealth_status ####
# 
# Relative_wealth_status <- cptable(~Relative_wealth_status, values = c(0.6, 0.3, 0.1), levels = c('Poor', 'Middle class', 'Rich'))

# ## Mutual_aids ####
# 
# Mutual_aids <- cptable(~Mutual_aids, values = c(0.3, 0.7), levels = c('False', 'True'))

## Access_to_inputs_at_mid_stage ####

Access_to_inputs_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2,3), 
                                                                          c(0,2)),
                                                    parent_weights = c(1.5, 1),
                                                    b = 1.5,
                                                    child_prior = c(0.4,0.6),
                                                    child_states = c('False', 'True'),
                                                    parent_states = list(c('Poor', 'Middle class', 'Rich'),
                                                                         c('False', 'True')))
Access_to_inputs_at_mid_stage_values <- Access_to_inputs_at_mid_stage_tmp$values
Access_to_inputs_at_mid_stage_levels <- Access_to_inputs_at_mid_stage_tmp$levels
Access_to_inputs_at_mid_stage <- cptable (~Access_to_inputs_at_mid_stage|Relative_wealth_status:Mutual_aids, values = Access_to_inputs_at_mid_stage_values,levels = Access_to_inputs_at_mid_stage_levels)

## Available_paid_labor_at_mid_stage #####
Available_paid_labor_at_mid_stage <- cptable(~Available_paid_labor_at_mid_stage, values = c(0.4, 0.6), levels = c('Unavailable', 'Available'))
## Available_Labor_force_at_mid_stage #####

Available_Labor_force_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(2, 1.5, 0), 
                                                                               c(0,1),
                                                                               c(1, 2)),
                                                         parent_weights = c(3, 1, 3),
                                                         b = 1.5,
                                                         child_prior = c(0.2,0.8),
                                                         child_states = c('Unsufficient', 'Sufficient'),
                                                         parent_states = list(c('Poor', 'Middle class', 'Rich'),
                                                                              c('False', 'True'),
                                                                              c('Unavailable', 'Available')))
# Available_Labor_force_at_mid_stage_values <- Available_Labor_force_at_mid_stage_tmp$values
Available_Labor_force_at_mid_stage_values <- c(0.35, 0.65, 
                                               0.654986522911051, 0.345013477088949, 
                                               0.986483256178967, 0.0135167438210335, 
                                               
                                               0.3, 0.7,                                                   
                                               0.5, 0.5,
                                               0.7, 0.3,
                                               
                                               0.3, 0.7,                                                   
                                               0.4, 0.6,
                                               0.4, 0.6,
                                               
                                               0.00288192548646564, 0.997118074513534, 
                                               0.1, 0.9,
                                               0.15, 0.85)
Available_Labor_force_at_mid_stage_levels <- Available_Labor_force_at_mid_stage_tmp$levels
Available_Labor_force_at_mid_stage <- cptable (~Available_Labor_force_at_mid_stage|Relative_wealth_status:Mutual_aids:Available_paid_labor_at_mid_stage, values = Available_Labor_force_at_mid_stage_values,levels = Available_Labor_force_at_mid_stage_levels)

# ## skills_of_the_farmer #####
# 
# skills_of_the_farmer <- cptable(~skills_of_the_farmer, values = c(0.3, 0.7), levels = c('Defavorable', 'Favorable'))

## Chemical_application_at_mid_stage ####
Chemical_application_at_mid_stage <- cptable (~Chemical_application_at_mid_stage|Access_to_inputs_at_mid_stage, 
                                              values = c(0.8, 0.2,
                                                         0.4, 0.6),
                                              levels = c('Unufficient', 'Sufficient'))

### Application_of_traditional_crop_protection_Methods_at_mid_stage #####

Application_of_traditional_crop_protection_Methods_at_mid_stage <- cptable(~Application_of_traditional_crop_protection_Methods_at_mid_stage, values = c(0.1, 0.9), levels = c('Uncommon', 'Common'))

## Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage ####

Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                                                               c(1,2),
                                                                                                               c(1,2),
                                                                                                               c(1,2)),
                                                                                         parent_weights = c(1.5,2,1,3),
                                                                                         b = 1.5,
                                                                                         child_prior = c(0.3,0.4,0.3),
                                                                                         child_states = c('Poor', 'Good', 'Excellent'),
                                                                                         parent_states = list(c('Unsufficient', 'Sufficient'),
                                                                                                              c('Uncommon', 'Common'),
                                                                                                              c('Unsufficient', 'Sufficient'),
                                                                                                              c('Defavorable', 'Favorable')))


Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage_values <- Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage_tmp$values
Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage_levels <- Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage_tmp$levels
Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage <- cptable (~Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage|
                                                                                 Chemical_application_at_mid_stage:
                                                                                 Application_of_traditional_crop_protection_Methods_at_mid_stage:
                                                                                 Available_Labor_force_at_mid_stage:
                                                                                 skills_of_the_farmer, values = Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage_values,levels = Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage_levels)

## Effectiveness_of_Weeding_at_mid_stage #####

Effectiveness_of_Weeding_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                                  c(1,2)),
                                                            parent_weights = c(1, 1.5),
                                                            b = 1.75,
                                                            child_prior = c(0.3,0.4,0.3),
                                                            child_states = c('Poor', 'Good', 'Excellent'),
                                                            parent_states = list(c('Unsufficient', 'Sufficient'),
                                                                                 c('Defavorable', 'Favorable')))



Effectiveness_of_Weeding_at_mid_stage_values <- Effectiveness_of_Weeding_at_mid_stage_tmp$values
Effectiveness_of_Weeding_at_mid_stage_levels <- Effectiveness_of_Weeding_at_mid_stage_tmp$levels
Effectiveness_of_Weeding_at_mid_stage <- cptable (~Effectiveness_of_Weeding_at_mid_stage|Available_Labor_force_at_mid_stage:skills_of_the_farmer, values = Effectiveness_of_Weeding_at_mid_stage_values,levels = Effectiveness_of_Weeding_at_mid_stage_levels)


## Fertilizers_application_at_mid_stage #####

Fertilizers_application_at_mid_stage <- cptable (~Fertilizers_application_at_mid_stage|Access_to_inputs_at_mid_stage, 
                                                 values = c(0.9, 0.1,
                                                            0.3, 0.7),
                                                 levels = c('False', 'True'))

## Rich_sediments_addition_from_flood_at_mid_stage ####

Rich_sediments_addition_from_flood_at_mid_stage <- cptable(~Rich_sediments_addition_from_flood_at_mid_stage, values = c(0.4, 0.6), levels = c('False', 'True'))

## Available_soil_nutrients_at_mid_stage ####

Available_soil_nutrients_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(
  # c(1,2), 
  c(1,2), 
  c(1,2),
  c(1,2,3)),
  # parent_weights = c(2,3,1),
  parent_weights = c(2,3,1),
  
  b = 1.5,
  child_prior = c(0.25, 0.5, 0.25),
  child_states = c('Deficient','Satisfactory', 'Plenty'),
  parent_states = list(
    # c('False', 'True'),
    c('False', 'True'),
    c('False', 'True'),
    c('Deficient','Satisfactory', 'Plenty')))



Available_soil_nutrients_at_mid_stage_values <- Available_soil_nutrients_at_mid_stage_tmp$values
Available_soil_nutrients_at_mid_stage_levels <- Available_soil_nutrients_at_mid_stage_tmp$levels
Available_soil_nutrients_at_mid_stage <- cptable (~Available_soil_nutrients_at_mid_stage|
                                                    #Manure_application:
                                                    Fertilizers_application_at_mid_stage:
                                                    Rich_sediments_addition_from_flood_at_mid_stage:
                                                    Available_soil_nutrients_at_development_stage, values = Available_soil_nutrients_at_mid_stage_values,levels = Available_soil_nutrients_at_mid_stage_levels)

### Pest_and_desease_impact_at_mid_stage ####

# Pest_and_desease_impact_at_mid_stage <- cptable (~Pest_and_desease_impact_at_mid_stage|
#                                                    Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage,
#                                                     values = c(0.5, 0.3, 0.2,
#                                                                0.15, 0.25, 0.6),
#                                                     levels = c('Severe', 'Significant', 'Minor'))

Pest_and_desease_impact_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(1, 2, 3), c(4,3,2,1)),
                                                           parent_weights = c(2, 3, 1),
                                                           # b = 1.75,
                                                           b = 1.5,
                                                           child_prior = c(0.3, 0.4, 0.3),
                                                           child_states = c('Severe', 'Significant', 'Minor'),
                                                           parent_states = list(c('Poor', 'Good', 'Excellent'),
                                                                                c('Severe', 'Significant', 'Minor'),
                                                                                c('Sorghum', 'Maize','Teff', 'Rice')))
Pest_and_desease_impact_at_mid_stage_values <- Pest_and_desease_impact_at_mid_stage_tmp$values
Pest_and_desease_impact_at_mid_stage_levels <- Pest_and_desease_impact_at_mid_stage_tmp$levels
Pest_and_desease_impact_at_mid_stage <- cptable (~Pest_and_desease_impact_at_mid_stage|
                                                   Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage:
                                                   Pest_and_desease_impact_at_development_stage:
                                                   Crop_type, 
                                                 values = Pest_and_desease_impact_at_mid_stage_values,levels = Pest_and_desease_impact_at_mid_stage_levels)

## Weeds_impact_at_mid_stage ####

# Weeds_impact_at_mid_stage <- cptable (~Weeds_impact_at_mid_stage|Effectiveness_of_Weeding_at_mid_stage, 
#                                          values = c(0.5, 0.3, 0.2,
#                                                     0.15, 0.25, 0.6),
#                                          levels = c('Significant', 'Moderate', 'Negligible'))
Weeds_impact_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(1, 2, 3), rev(c(4,3,2,1))),
                                                parent_weights = c(2, 3, 1),
                                                # b = 2.5,
                                                b = 2,
                                                child_prior = c(0.2, 0.4, 0.2),
                                                child_states = c('Significant', 'Moderate', 'Negligible'),
                                                parent_states = list(c('Poor', 'Good', 'Excellent'),
                                                                     c('Significant', 'Moderate', 'Negligible'),
                                                                     c('Sorghum', 'Maize','Teff', 'Rice')))
Weeds_impact_at_mid_stage_values <- Weeds_impact_at_mid_stage_tmp$values
Weeds_impact_at_mid_stage_levels <- Weeds_impact_at_mid_stage_tmp$levels
Weeds_impact_at_mid_stage <- cptable (~Weeds_impact_at_mid_stage|
                                        Effectiveness_of_Weeding_at_mid_stage:
                                        Weeds_impact_at_development_stage:
                                        Crop_type, 
                                      values = Weeds_impact_at_mid_stage_values,levels = Weeds_impact_at_mid_stage_levels)

#---------------------------------------------------------------------------------------------#
# Mid stage: Local constraints, crop grown, Agticultural management
#-----------------------------------------------------------------------------------#

## Farming_inefficiency at mid stage ####

Agricultural_management_efficiency_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3), c(1,2,4), c(1,2,4)),
                                                                      # parent_weights = c(1.5, 1.75, 2, 1.5, 1),
                                                                      # parent_weights = c(1.5, 1.5, 1.75, 1.5, 1),
                                                                      parent_weights = c(1.5, 1.5, 1.25),
                                                                      b = 2,
                                                                      child_prior = c(0.3, 0.4, 0.3),
                                                                      child_states = c('Low','Medium', 'High'),
                                                                      parent_states = list(c("Inadequate", "Adequate"),
                                                                                           c('Severe', 'Significant', 'Minor'),
                                                                                           c('Significant', 'Moderate', 'Negligible')
                                                                      ))



Agricultural_management_efficiency_at_mid_stage_values <- Agricultural_management_efficiency_at_mid_stage_tmp$values
Agricultural_management_efficiency_at_mid_stage_levels <- Agricultural_management_efficiency_at_mid_stage_tmp$levels
Agricultural_management_efficiency_at_mid_stage <- cptable (~Agricultural_management_efficiency_at_mid_stage|
                                                              nutrient_supply_adequacy_at_mid_stage:
                                                              Pest_and_desease_impact_at_mid_stage:
                                                              Weeds_impact_at_mid_stage,
                                                            values = Agricultural_management_efficiency_at_mid_stage_values,levels = Agricultural_management_efficiency_at_mid_stage_levels)

### nutrient_supply_adequacy_at_development_stage ####

nutrient_supply_adequacy_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(4,3,2,1)),
                                                                    parent_weights = c(2,1),
                                                                    b = 2,
                                                                    child_prior = c(0.5, 0.5),
                                                                    child_states = c("Inadequate", "Adequate"),
                                                                    parent_states = list(c('Deficient','Satisfactory', 'Plenty'),
                                                                                         c('Sorghum', 'Maize','Teff', 'Rice')))

nutrient_supply_adequacy_at_mid_stage_values <- nutrient_supply_adequacy_at_mid_stage_tmp$values
nutrient_supply_adequacy_at_mid_stage_levels <- nutrient_supply_adequacy_at_mid_stage_tmp$levels
nutrient_supply_adequacy_at_mid_stage <- cptable (~nutrient_supply_adequacy_at_mid_stage|
                                                            Available_soil_nutrients_at_mid_stage:
                                                            Crop_type,
                                                          values = nutrient_supply_adequacy_at_mid_stage_values,levels = nutrient_supply_adequacy_at_mid_stage_levels)


### Water_supply_adequacy_at_mid_stage ####

Water_supply_adequacy_at_mid_stage_values <- c(0.5, 0.5, 
                                               0.3, 0.7,
                                               0.7, 0.3, # In most cases, crop would not accommodate water logging at late stage
                                               
                                               0.8, 0.2, 
                                               0.3, 0.7,
                                               0.6, 0.4, # In most cases, crop would not accommodate water logging at late stage
                                               
                                               0.6, 0.4, 
                                               0.7, 0.3,
                                               0.3, 0.7, # In most cases, crop would not accommodate water logging at late stage
                                               
                                               0.9, 0.1, 
                                               0.5, 0.5,
                                               0.1, 0.9)
Water_supply_adequacy_at_mid_stage_levels <- c("Inadequate", "Adequate")
Water_supply_adequacy_at_mid_stage <- cptable (~Water_supply_adequacy_at_mid_stage|
                                                 Available_soil_water_at_mid_stage:
                                                 Crop_type,
                                               values = Water_supply_adequacy_at_mid_stage_values,levels = Water_supply_adequacy_at_mid_stage_levels)

## Local_constraints at initial stage ####

Local_constraints_at_mid_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2), c(1, 2), c(1,2,3)),
                                                     # parent_weights = c(2, 4, 1),
                                                     # parent_weights = c(1.5, 4, 1.5),
                                                     parent_weights = c(1, 4, 2),
                                                     b = 1.5,
                                                     child_prior = c(0.1, 0.7, 0.2),
                                                     child_states = c('High','Medium', 'Low'),
                                                     parent_states = list(c("Ineffective", "Effective"),
                                                                          c("Inadequate", "Adequate"),
                                                                          c("Low", "Medium", "High")))



Local_constraints_at_mid_stage_values <- Local_constraints_at_mid_stage_tmp$values
Local_constraints_at_mid_stage_levels <- Local_constraints_at_mid_stage_tmp$levels
Local_constraints_at_mid_stage <- cptable (~Local_constraints_at_mid_stage|
                                             Effectiveness_of_cropping_options:
                                             Water_supply_adequacy_at_mid_stage:
                                             Agricultural_management_efficiency_at_mid_stage, 
                                           values = Local_constraints_at_mid_stage_values,levels = Local_constraints_at_mid_stage_levels)


## LATE STAGE ####
#------------------------------------------------------------------------------------#
# amount of water reaching the plot #
#------------------------------------------------------------------------------------#

# ## Type of water diversion ####
# Type_of_water_diversion <- cptable(~Type_of_water_diversion, values = c(0.3, 0.4, 0.3),
#                                    levels = c('Modern', 'Hybrid', 'Traditional'))


## Sediment_load_at_late_stage ####
Sediment_load_at_late_stage <- cptable (~Sediment_load_at_late_stage|Type_of_water_diversion, values =   c(0.45, 0.45, 0.01,
                                                                                                           0.6, 0.3, 0.1,
                                                                                                           0.95, 0.09, 0.01),
                                        
                                        # c(0.3, 0.6, 0.1,
                                        #   0.5, 0.4, 0.1,
                                        #   0.85, 0.14, 0.01)
                                        
                                        # c(0.05, 0.05, 0.9,
                                        #   0.05, 0.9, 0.05,
                                        #   0.95, 0.025, 0.025),
                                        
                                        # c(0.1, 0.2, 0.7,
                                        #   0.1, 0.7, 0.2,
                                        #  0.8, 0.1, 0.1),
                                        
                                        # c(0.1, 0.3, 0.6,
                                        #   0.1, 0.6, 0.3,
                                        #   0.7, 0.2, 0.1),
                                        
                                        levels = c("High", 'Medium', 'Low'))


## Rain_event_occurence_at_late_stage ####
Rain_event_occurence_at_late_stage <- cptable (~Rain_event_occurence_at_late_stage, values = c(0.65, 0.35),levels = c('Off-site', 'On-site'))

# ## Slope ####
# Slope <- cptable (~Slope, values = c(0.1, 0.2, 0.7),levels = c('Steep', 'Moderate', 'Gentil'))

# ## Socio-institutional arrangements ####
# Social_arrangements <- cptable (~Social_arrangements, values = c(0.3, 0.7),levels = c('Inadequate', 'Adequate'))

## Main_Canals_Maitnenance_at_late_stage ####
Main_Canals_Maitnenance_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3,2),
                                                                                  c(0,1)),
                                                            parent_weights = c(1,2),
                                                            b = 1.5,
                                                            # child_prior = c(0.5,0.5),
                                                            # child_prior = c(0.6,0.4),
                                                            # child_prior = c(0.7,0.3),
                                                            child_prior = c(0.8,0.2),
                                                            child_states = c('Poor', 'Good'),
                                                            parent_states = list(c('Modern', 'Hybrid', 'Traditional'),
                                                                                 c('Inadequate', 'Adequate')))

# Main_Canals_Maitnenance_at_late_stage_values <- Main_Canals_Maitnenance_at_late_stage_tmp$values
Main_Canals_Maitnenance_at_late_stage_values <- c(0.919293820933165, 0.0807061790668348,
                                                  0.835051546391752, 0.164948453608247,
                                                  0.692307692307692, 0.307692307692308,
                                                  0.0807061790668348, 0.919293820933165,
                                                  0.164948453608247, 0.835051546391752,
                                                  0.307692307692308, 0.692307692307692)
Main_Canals_Maitnenance_at_late_stage_levels <- Main_Canals_Maitnenance_at_late_stage_tmp$levels
Main_Canals_Maitnenance_at_late_stage <- cptable (~Main_Canals_Maitnenance_at_late_stage|Type_of_water_diversion:Social_arrangements, values = Main_Canals_Maitnenance_at_late_stage_values,levels = Main_Canals_Maitnenance_at_late_stage_levels)


## Amount_of_shared_flood_at_late_stage ####
Amount_of_shared_flood_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2), 
                                                                                 c(1,2), 
                                                                                 c(1,2,4),
                                                                                 c(1,2,3.5)),
                                                           parent_weights = c(1,2,3,2),
                                                           b = 2.5,
                                                           child_prior = c(0.3, 0.6, 0.1),
                                                           child_states = c('< Enough', 'Enough', '> Enough'),
                                                           parent_states = list(c('Off-site', 'On-site'),
                                                                                c('Poor', 'Good'),
                                                                                c('High', 'Medium',  'Low'), 
                                                                                c('Steep','Moderate', 'Gentil')))

Amount_of_shared_flood_at_late_stage_values <- Amount_of_shared_flood_at_late_stage_tmp$values
Amount_of_shared_flood_at_late_stage_levels <- Amount_of_shared_flood_at_late_stage_tmp$levels
Amount_of_shared_flood_at_late_stage <- cptable (~Amount_of_shared_flood_at_late_stage|
                                                   Rain_event_occurence_at_late_stage:
                                                   Main_Canals_Maitnenance_at_late_stage:
                                                   Sediment_load_at_late_stage:
                                                   Slope, values = Amount_of_shared_flood_at_late_stage_values,levels = Amount_of_shared_flood_at_late_stage_levels)

# ## Location_of_the_plot ####
# Location_of_the_plot <- cptable (~Location_of_the_plot, values = c(0.6, 0.3, 0.1),levels = c('Lowlands', 'Midlands', 'Highlands'))
## Upstream_abstraction_at_late_stage ####
Upstream_abstraction_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                               c(0,1,2)),
                                                         parent_weights = c(1,2),
                                                         # b = 3,
                                                         # b = 2.5
                                                         # b = 2,
                                                         b = 1.5,
                                                         child_prior = c(0.6,0.4),
                                                         child_states = c('Unfair', 'Fair'),
                                                         parent_states = list(c('Inadequate', 'Adequate'),
                                                                              c('Lowlands', 'Midlands', 'Highlands')))



# Upstream_abstraction_at_late_stage_values <- Upstream_abstraction_at_late_stage_tmp$values
Upstream_abstraction_at_late_stage_values <- c(0.999086340794883, 0.000913659205116492,
                                               0.242857142857143, 0.757142857142857,
                                               0.931034482758621, 0.0689655172413793, 
                                               0.142857142857143, 0.857142857142857,
                                               0.142857142857143, 0.857142857142857, 
                                               0.00205338809034908, 0.997946611909651)
Upstream_abstraction_at_late_stage_levels <- Upstream_abstraction_at_late_stage_tmp$levels
Upstream_abstraction_at_late_stage <- cptable (~Upstream_abstraction_at_late_stage|Social_arrangements:Location_of_the_plot, values = Upstream_abstraction_at_late_stage_values,levels = Upstream_abstraction_at_late_stage_levels)

## Field_canal_maintenance_at_late_stage####
Field_canal_maintenance_at_late_stage<- cptable (~Field_canal_maintenance_at_late_stage, values = c(0.2, 0.8),levels = c('Poor', 'Good'))

## Presence_of_the_farmer_during_flood_event_at_late_stage ###
Presence_of_the_farmer_during_flood_event_at_late_stage <- cptable (~Presence_of_the_farmer_during_flood_event_at_late_stage, values = c(0.2, 0.8),levels = c('False', 'True'))

## Amount_of_flood_reaching_the_plot_at_late_stage ####
Amount_of_flood_reaching_the_plot_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2), 
                                                                                            c(0,2), 
                                                                                            c(1,2,3), 
                                                                                            c(1,2)),
                                                                      parent_weights = c(1,1,3,3),
                                                                      b = 2,
                                                                      child_prior = c(0.2,0.7,0.1),
                                                                      child_states = c('Too little', 'Desired', 'Too much'),
                                                                      parent_states = list(c('Poor', 'Good'),
                                                                                           c('False', 'True'),
                                                                                           c('< Enough', 'Enough', '> Enough'),
                                                                                           c('Unfair', 'Fair')))



Amount_of_flood_reaching_the_plot_at_late_stage_values <- Amount_of_flood_reaching_the_plot_at_late_stage_tmp$values
Amount_of_flood_reaching_the_plot_at_late_stage_levels <- Amount_of_flood_reaching_the_plot_at_late_stage_tmp$levels
Amount_of_flood_reaching_the_plot_at_late_stage <- cptable (~Amount_of_flood_reaching_the_plot_at_late_stage|Field_canal_maintenance_at_late_stage:Presence_of_the_farmer_during_flood_event_at_late_stage:Amount_of_shared_flood_at_late_stage:Upstream_abstraction_at_late_stage, values = Amount_of_flood_reaching_the_plot_at_late_stage_values,levels = Amount_of_flood_reaching_the_plot_at_late_stage_levels)


#------------------------------------------------------------------------------------#
# Available soil water #
#------------------------------------------------------------------------------------#

# ## Soil_type ####
# 
# Soil_type <- cptable (~Soil_type, values = c(0.05, 0.55, 0.4),levels = c('Sandy', 'Loamy', 'Clayey'))

# ## Manure_application ####
# 
# Manure_application <- cptable(~Manure_application, values = c(0.3, 0.7), levels = c('False', 'True'))

# ## Soil_water_holding_capacity ####
# 
# Soil_water_holding_capacity_tmp <- make_gRain_CPT(parent_effects = list(c(0, 2.5, 3),
#                                                                         c(0, 2)),
#                                                   parent_weights = c(2,1),
#                                                   b = 3,
#                                                   child_prior = c(0.2,0.5,0.3),
#                                                   child_states = c('Low', 'Medium', 'High'),
#                                                   parent_states = list(c('Sandy', 'Loamy', 'Clayey'),
#                                                                        c('False', 'True')))
# Soil_water_holding_capacity_values <- Soil_water_holding_capacity_tmp$values
# Soil_water_holding_capacity_levels <- Soil_water_holding_capacity_tmp$levels
# Soil_water_holding_capacity <- cptable (~Soil_water_holding_capacity|Soil_type:Manure_application, values = Soil_water_holding_capacity_values,levels = Soil_water_holding_capacity_levels)

## Evapotranspiration_at_late_stage ####

Evapotranspiration_at_late_stage <- cptable (~Evapotranspiration_at_late_stage, values = c(0.1, 0.8, 0.1), # c(0.9, 0.05, 0.05), # c(0.05, 0.9, 0.05), # c(0.1, 0.2, 0.7),
                                             levels = c('High', 'Medium', 'Low'))

## Rainfall_amount_at_late_stage ####

Rainfall_amount_at_late_stage <- cptable (~Rainfall_amount_at_late_stage, values = c(0.25, 0.6, 0.15),levels = c('< normal', 'Normal', '> normal'))

# ## Initial_soil_water_content ####
# 
# Initial_soil_water_content <- cptable (~Initial_soil_water_content, values = c(0.40, 0.30, 0.15, 0.15),levels = c('Very low', 'Low', 'Medium', 'High'))

## Available_soil_water_at_late_stage ####

Available_soil_water_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2,3),
                                                                               c(1,2,3),
                                                                               c(0,2,3.5),
                                                                               # c(1,2,3,4),
                                                                               c(0, 2, 2),
                                                                               c(1,2,3)),
                                                         # parent_weights = c(4, 2.5, 2, 1, 3),
                                                         parent_weights = c(4, 2, 2.5, 3, 1.5),
                                                         b = 1.5,
                                                         child_prior = c(0.1, 0.6, 0.3),
                                                         child_states = c('Drought risk', 'Normal', 'Waterlogging risk'),
                                                         parent_states = list(c('Low', 'Medium', 'High'),
                                                                              c('High', 'Medium', 'Low'),
                                                                              c('< normal', 'Normal', '> normal'), 
                                                                              # c('Very low', 'Low', 'Medium', 'High'),
                                                                              c('Too little', 'Desired', 'Too much'),
                                                                              c('Too little', 'Normal', 'Too much')))
Available_soil_water_at_late_stage_values <- Available_soil_water_at_late_stage_tmp$values
Available_soil_water_at_late_stage_levels <- Available_soil_water_at_late_stage_tmp$levels
Available_soil_water_at_late_stage <- cptable (~Available_soil_water_at_late_stage|
                                                 Soil_water_holding_capacity:
                                                 Evapotranspiration_at_late_stage:
                                                 Rainfall_amount_at_late_stage:
                                                 # Initial_soil_water_content:
                                                 Amount_of_flood_reaching_the_plot_at_late_stage:
                                                 Available_soil_water_at_mid_stage, values = Available_soil_water_at_late_stage_values,levels = Available_soil_water_at_late_stage_levels)


#-----------------------------------------------------------------------------------#
# Available soil nutrients at initial stage, Pest and desease impact at initial stage, Weeds impacts
#-----------------------------------------------------------------------------------#
# 
# ## Relative_wealth_status ####
# 
# Relative_wealth_status <- cptable(~Relative_wealth_status, values = c(0.6, 0.3, 0.1), levels = c('Poor', 'Middle class', 'Rich'))

# ## Mutual_aids ####
# 
# Mutual_aids <- cptable(~Mutual_aids, values = c(0.3, 0.7), levels = c('False', 'True'))

## Access_to_inputs_at_late_stage ####

Access_to_inputs_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(0,2,3), 
                                                                           c(0,2)),
                                                     parent_weights = c(1.5, 1),
                                                     b = 1.5,
                                                     child_prior = c(0.4,0.6),
                                                     child_states = c('False', 'True'),
                                                     parent_states = list(c('Poor', 'Middle class', 'Rich'),
                                                                          c('False', 'True')))
Access_to_inputs_at_late_stage_values <- Access_to_inputs_at_late_stage_tmp$values
Access_to_inputs_at_late_stage_levels <- Access_to_inputs_at_late_stage_tmp$levels
Access_to_inputs_at_late_stage <- cptable (~Access_to_inputs_at_late_stage|Relative_wealth_status:Mutual_aids, values = Access_to_inputs_at_late_stage_values,levels = Access_to_inputs_at_late_stage_levels)

## Available_paid_labor_at_late_stage #####
Available_paid_labor_at_late_stage <- cptable(~Available_paid_labor_at_late_stage, values = c(0.4, 0.6), levels = c('Unavailable', 'Available'))
## Available_Labor_force_at_late_stage #####

Available_Labor_force_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(2, 1.5, 0), 
                                                                                c(0,1),
                                                                                c(1, 2)),
                                                          parent_weights = c(3, 1, 3),
                                                          b = 1.5,
                                                          child_prior = c(0.2,0.8),
                                                          child_states = c('Unsufficient', 'Sufficient'),
                                                          parent_states = list(c('Poor', 'Middle class', 'Rich'),
                                                                               c('False', 'True'),
                                                                               c('Unavailable', 'Available')))
# Available_Labor_force_at_late_stage_values <- Available_Labor_force_at_late_stage_tmp$values
Available_Labor_force_at_late_stage_values <- c(0.35, 0.65, 
                                                0.654986522911051, 0.345013477088949, 
                                                0.986483256178967, 0.0135167438210335, 
                                                
                                                0.3, 0.7,                                                   
                                                0.5, 0.5,
                                                0.7, 0.3,
                                                
                                                0.3, 0.7,                                                   
                                                0.4, 0.6,
                                                0.4, 0.6,
                                                
                                                0.00288192548646564, 0.997118074513534, 
                                                0.1, 0.9,
                                                0.15, 0.85)
Available_Labor_force_at_late_stage_levels <- Available_Labor_force_at_late_stage_tmp$levels
Available_Labor_force_at_late_stage <- cptable (~Available_Labor_force_at_late_stage|Relative_wealth_status:Mutual_aids:Available_paid_labor_at_late_stage, values = Available_Labor_force_at_late_stage_values,levels = Available_Labor_force_at_late_stage_levels)

# ## skills_of_the_farmer #####
# 
# skills_of_the_farmer <- cptable(~skills_of_the_farmer, values = c(0.3, 0.7), levels = c('Defavorable', 'Favorable'))

## Chemical_application_at_late_stage ####
Chemical_application_at_late_stage <- cptable (~Chemical_application_at_late_stage|Access_to_inputs_at_late_stage, 
                                               values = c(0.8, 0.2,
                                                          0.4, 0.6),
                                               levels = c('Unufficient', 'Sufficient'))

### Application_of_traditional_crop_protection_Methods_at_late_stage #####

Application_of_traditional_crop_protection_Methods_at_late_stage <- cptable(~Application_of_traditional_crop_protection_Methods_at_late_stage, values = c(0.1, 0.9), levels = c('Uncommon', 'Common'))

## Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage ####

Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                                                                c(1,2),
                                                                                                                c(1,2),
                                                                                                                c(1,2)),
                                                                                          parent_weights = c(1.5,2,1,3),
                                                                                          b = 1.5,
                                                                                          child_prior = c(0.3,0.4, 0.3),
                                                                                          child_states = c('Poor', 'Good', 'Excellent'),
                                                                                          parent_states = list(c('Unsufficient', 'Sufficient'),
                                                                                                               c('Uncommon', 'Common'),
                                                                                                               c('Unsufficient', 'Sufficient'),
                                                                                                               c('Defavorable', 'Favorable')))


Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage_values <- Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage_tmp$values
Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage_levels <- Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage_tmp$levels
Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage <- cptable (~Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage|
                                                                                  Chemical_application_at_late_stage:
                                                                                  Application_of_traditional_crop_protection_Methods_at_late_stage:
                                                                                  Available_Labor_force_at_late_stage:
                                                                                  skills_of_the_farmer, values = Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage_values,levels = Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage_levels)

## Effectiveness_of_Weeding_at_late_stage #####

Effectiveness_of_Weeding_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2),
                                                                                   c(1,2)),
                                                             parent_weights = c(1, 1.5),
                                                             b = 1.75,
                                                             child_prior = c(0.3,0.4,0.3),
                                                             child_states = c('Poor', 'Good', 'Excellent'),
                                                             parent_states = list(c('Unsufficient', 'Sufficient'),
                                                                                  c('Defavorable', 'Favorable')))



Effectiveness_of_Weeding_at_late_stage_values <- Effectiveness_of_Weeding_at_late_stage_tmp$values
Effectiveness_of_Weeding_at_late_stage_levels <- Effectiveness_of_Weeding_at_late_stage_tmp$levels
Effectiveness_of_Weeding_at_late_stage <- cptable (~Effectiveness_of_Weeding_at_late_stage|Available_Labor_force_at_late_stage:skills_of_the_farmer, values = Effectiveness_of_Weeding_at_late_stage_values,levels = Effectiveness_of_Weeding_at_late_stage_levels)


## Fertilizers_application_at_late_stage #####

Fertilizers_application_at_late_stage <- cptable (~Fertilizers_application_at_late_stage|Access_to_inputs_at_late_stage, 
                                                  values = c(0.9, 0.1,
                                                             0.3, 0.7),
                                                  levels = c('False', 'True'))

## Rich_sediments_addition_from_flood_at_late_stage ####

Rich_sediments_addition_from_flood_at_late_stage <- cptable(~Rich_sediments_addition_from_flood_at_late_stage, values = c(0.4, 0.6), levels = c('False', 'True'))

## Available_soil_nutrients_at_late_stage ####

Available_soil_nutrients_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(
  # c(1,2), 
  c(1,2), 
  c(1,2),
  c(1,2,3)),
  # parent_weights = c(2,3,1),
  parent_weights = c(2,3,1),
  
  b = 1.5,
  child_prior = c(0.25, 0.5, 0.25),
  child_states = c('Deficient','Satisfactory', 'Plenty'),
  parent_states = list(
    # c('False', 'True'),
    c('False', 'True'),
    c('False', 'True'),
    c('Deficient','Satisfactory', 'Plenty')))



Available_soil_nutrients_at_late_stage_values <- Available_soil_nutrients_at_late_stage_tmp$values
Available_soil_nutrients_at_late_stage_levels <- Available_soil_nutrients_at_late_stage_tmp$levels
Available_soil_nutrients_at_late_stage <- cptable (~Available_soil_nutrients_at_late_stage|
                                                     #Manure_application:
                                                     Fertilizers_application_at_late_stage:
                                                     Rich_sediments_addition_from_flood_at_late_stage:
                                                     Available_soil_nutrients_at_mid_stage, values = Available_soil_nutrients_at_late_stage_values,levels = Available_soil_nutrients_at_late_stage_levels)

### Pest_and_desease_impact_at_late_stage ####

# Pest_and_desease_impact_at_late_stage <- cptable (~Pest_and_desease_impact_at_late_stage|
#                                                    Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage,
#                                                     values = c(0.5, 0.3, 0.2,
#                                                                0.15, 0.25, 0.6),
#                                                     levels = c('Severe', 'Significant', 'Minor'))

Pest_and_desease_impact_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,5), c(1, 2, 3), rev(c(4,3,2,1.5))),
                                                            parent_weights = c(2, 3, 1),
                                                            # b=2,
                                                            # b = 1.75,
                                                            # b = 1.5,
                                                            b = 2.5,
                                                            child_prior = c(0.3, 0.4, 0.3),
                                                            child_states = c('Severe', 'Significant', 'Minor'),
                                                            parent_states = list(c('Poor', 'Good','Excellent'),
                                                                                 c('Severe', 'Significant', 'Minor'),
                                                                                 c('Sorghum', 'Maize','Teff', 'Rice')))
Pest_and_desease_impact_at_late_stage_values <- Pest_and_desease_impact_at_late_stage_tmp$values
Pest_and_desease_impact_at_late_stage_levels <- Pest_and_desease_impact_at_late_stage_tmp$levels
Pest_and_desease_impact_at_late_stage <- cptable (~Pest_and_desease_impact_at_late_stage|
                                                    Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage:
                                                    Pest_and_desease_impact_at_mid_stage:
                                                    Crop_type, 
                                                  values = Pest_and_desease_impact_at_late_stage_values,levels = Pest_and_desease_impact_at_late_stage_levels)

## Weeds_impact_at_late_stage ####

# Weeds_impact_at_late_stage <- cptable (~Weeds_impact_at_late_stage|Effectiveness_of_Weeding_at_late_stage, 
#                                          values = c(0.5, 0.3, 0.2,
#                                                     0.15, 0.25, 0.6),
#                                          levels = c('Significant', 'Moderate', 'Negligible'))
Weeds_impact_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,2), c(1, 2, 3), c(4,3,2,1.5)),
                                                 parent_weights = c(1, 2, 1),
                                                 # b = 2.5,
                                                 # b = 2,
                                                 b = 1.75,
                                                 child_prior = c(0.2, 0.6, 0.2),
                                                 child_states = c('Significant', 'Moderate', 'Negligible'),
                                                 parent_states = list(c('Poor', 'Good','Excellent'),
                                                                      c('Significant', 'Moderate', 'Negligible'),
                                                                      c('Sorghum', 'Maize','Teff', 'Rice')))
Weeds_impact_at_late_stage_values <- Weeds_impact_at_late_stage_tmp$values
Weeds_impact_at_late_stage_levels <- Weeds_impact_at_late_stage_tmp$levels
Weeds_impact_at_late_stage <- cptable (~Weeds_impact_at_late_stage|
                                         Effectiveness_of_Weeding_at_late_stage:
                                         Weeds_impact_at_mid_stage:
                                         Crop_type, 
                                       values = Weeds_impact_at_late_stage_values,levels = Weeds_impact_at_late_stage_levels)

#---------------------------------------------------------------------------------------------#
# Late stage: Local constraints, crop grown, Agticultural management
#-----------------------------------------------------------------------------------#

## Farming_inefficiency at late stage ####

Agricultural_management_efficiency_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3), c(1,2,3), c(1,2,3)),
                                                                       
                                                                       # parent_weights = c(1.5, 1.75, 2, 1.5, 1),
                                                                       # parent_weights = c(1.5, 1.5, 1.75, 1.5, 1),
                                                                       # parent_weights = c(1.5, 1.5, 1.25, 1.5, 1),
                                                                       parent_weights = c(0.5, 2, 0.5),
                                                                       
                                                                       b = 2,
                                                                       child_prior = c(0.3 , 0.4, 0.3),
                                                                       child_states = c('Low','Medium', 'High'),
                                                                       parent_states = list(c("Inadequate", "Adequate"),
                                                                                            c('Severe', 'Significant', 'Minor'),
                                                                                            c('Significant', 'Moderate', 'Negligible')
                                                                       ))



Agricultural_management_efficiency_at_late_stage_values <- Agricultural_management_efficiency_at_late_stage_tmp$values
Agricultural_management_efficiency_at_late_stage_levels <- Agricultural_management_efficiency_at_late_stage_tmp$levels
Agricultural_management_efficiency_at_late_stage <- cptable (~Agricultural_management_efficiency_at_late_stage|
                                                               nutrient_supply_adequacy_at_late_stage:
                                                               Pest_and_desease_impact_at_late_stage:
                                                               Weeds_impact_at_late_stage,
                                                             values = Agricultural_management_efficiency_at_late_stage_values,levels = Agricultural_management_efficiency_at_late_stage_levels)

### Cropping_systems_efficiency_at_late_stage ####

### nutrient_supply_adequacy_at_development_stage ####

nutrient_supply_adequacy_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,2,3), c(4,3,2,1)),
                                                            parent_weights = c(2,1),
                                                            b = 1.25,
                                                            child_prior = c(0.5, 0.5),
                                                            child_states = c("Inadequate", "Adequate"),
                                                            parent_states = list(c('Deficient','Satisfactory', 'Plenty'),
                                                                                 c('Sorghum', 'Maize','Teff', 'Rice')))

nutrient_supply_adequacy_at_late_stage_values <- nutrient_supply_adequacy_at_late_stage_tmp$values
nutrient_supply_adequacy_at_late_stage_levels <- nutrient_supply_adequacy_at_late_stage_tmp$levels
nutrient_supply_adequacy_at_late_stage <- cptable (~nutrient_supply_adequacy_at_late_stage|
                                                    Available_soil_nutrients_at_late_stage:
                                                    Crop_type,
                                                  values = nutrient_supply_adequacy_at_late_stage_values,levels = nutrient_supply_adequacy_at_late_stage_levels)


### Water_supply_adequacy_at_late_stage ####

Water_supply_adequacy_at_late_stage_values <- c(0.5, 0.5, 
                                                0.3, 0.7,
                                                0.7, 0.3, # In most cases, crop would not accommodate water logging at late stage
                                                
                                                0.8, 0.2, 
                                                0.3, 0.7,
                                                0.6, 0.4, # In most cases, crop would not accommodate water logging at late stage
                                                
                                                0.6, 0.4, 
                                                0.7, 0.3,
                                                0.7, 0.3, # In most cases, crop would not accommodate water logging at late stage
                                                
                                                0.9, 0.1, 
                                                0.5, 0.5,
                                                0.9, 0.1)


Water_supply_adequacy_at_late_stage_levels <- c("Inadequate", "Adequate")
Water_supply_adequacy_at_late_stage <- cptable (~Water_supply_adequacy_at_late_stage|
                                                  Available_soil_water_at_late_stage:
                                                  Crop_type,
                                                values = Water_supply_adequacy_at_late_stage_values,levels = Water_supply_adequacy_at_late_stage_levels)

## Local_constraints at initial stage ####

Local_constraints_at_late_stage_tmp <- make_gRain_CPT(parent_effects = list(c(1,3), c(0.5, 3), c(1,2,3)),
                                                      # parent_weights = c(2, 4, 1),
                                                      # parent_weights = c(1.5, 4, 1.5),
                                                      # parent_weights = c(1, 4, 2),
                                                      # parent_weights = c(4, 1, 2),
                                                      parent_weights = c(4, 2, 1),
                                                      b = 1.5,
                                                      child_prior = c(0.1, 0.7, 0.2),
                                                      child_states = c('High','Medium', 'Low'),
                                                      parent_states = list(c("Ineffective", "Effective"),
                                                                           c("Inadequate", "Adequate"),
                                                                           c("Low", "Medium", "High")))



Local_constraints_at_late_stage_values <- Local_constraints_at_late_stage_tmp$values
Local_constraints_at_late_stage_levels <- Local_constraints_at_late_stage_tmp$levels
Local_constraints_at_late_stage <- cptable (~Local_constraints_at_late_stage|
                                              Effectiveness_of_cropping_options:
                                              Water_supply_adequacy_at_late_stage:
                                              Agricultural_management_efficiency_at_late_stage, 
                                            values = Local_constraints_at_late_stage_values,levels = Local_constraints_at_late_stage_levels)



## 1) Compile conditional probability tables / cliques potentials ####

net <- compileCPT(list(
  
  # At the initial stage
  
  Type_of_water_diversion,
  Sediment_load_at_initial_stage,
  Rain_event_occurence_at_initial_stage,
  Slope,
  Social_arrangements,
  Main_Canals_Maitnenance_at_initial_stage,
  Amount_of_shared_flood_at_initial_stage,
  Location_of_the_plot,
  Upstream_abstraction_at_initial_stage,
  Field_canal_maintenance_at_initial_stage,
  Presence_of_the_farmer_during_flood_event_at_initial_stage,
  Amount_of_flood_reaching_the_plot_at_initial_stage,
  Soil_type,
  Manure_application,
  Soil_water_holding_capacity,
  Evapotranspiration_at_initial_stage,
  Rainfall_amount_at_initial_stage,
  Initial_soil_water_content,
  Available_soil_water_at_initial_stage,
  Relative_wealth_status,
  Mutual_aids,
  Access_to_inputs_at_initial_stage,
  Available_paid_labor_at_initial_stage,
  Available_Labor_force_at_initial_stage,
  skills_of_the_farmer,
  Chemical_application_at_initial_stage,
  Application_of_traditional_crop_protection_Methods_at_initial_stage,
  Effectiveness_of_pest_and_disease_reduction_practices_at_initial_stage,
  Effectiveness_of_Weeding_at_initial_stage,
  Fertilizers_application_at_initial_stage,
  Rich_sediments_addition_from_flood_at_initial_stage,
  Available_soil_nutrients_at_initial_stage,
  Pest_and_desease_impact_at_initial_stage,
  Weeds_impact_at_initial_stage,
  Agricultural_management_efficiency_at_initial_stage,
  Local_constraints_at_initial_stage,
  Crop_type,
  Intercropping,
  Crop_variety,
  Planting_date,
  Effectiveness_of_cropping_options,
  Previous_crop,
  # Effectiveness_of_agricultural_management_at_initial_stage,
  Water_supply_adequacy_at_initial_stage, 
  # Cropping_systems_efficiency_at_initial_stage,
  
  # At the development stage: note the nodes that have been commented.
  # These are commun nodes for each stage and are omited in this stage
  # because they are already captured at initial stage
  
  # Type_of_water_diversion,
  Sediment_load_at_development_stage,
  Rain_event_occurence_at_development_stage,
  # Slope,
  # Social_arrangements,
  Main_Canals_Maitnenance_at_development_stage,
  Amount_of_shared_flood_at_development_stage,
  # Location_of_the_plot,
  Upstream_abstraction_at_development_stage,
  Field_canal_maintenance_at_development_stage,
  Presence_of_the_farmer_during_flood_event_at_development_stage,
  Amount_of_flood_reaching_the_plot_at_development_stage,
  # Soil_type,
  # Manure_application,
  # Soil_water_holding_capacity,
  Evapotranspiration_at_development_stage,
  Rainfall_amount_at_development_stage,
  # Initial_soil_water_content,
  Available_soil_water_at_development_stage,
  # Relative_wealth_status,
  # Mutual_aids,
  Access_to_inputs_at_development_stage,
  Available_paid_labor_at_development_stage,
  Available_Labor_force_at_development_stage,
  # skills_of_the_farmer,
  Chemical_application_at_development_stage,
  Application_of_traditional_crop_protection_Methods_at_development_stage,
  Effectiveness_of_pest_and_disease_reduction_practices_at_development_stage,
  Effectiveness_of_Weeding_at_development_stage,
  Fertilizers_application_at_development_stage,
  Rich_sediments_addition_from_flood_at_development_stage,
  Available_soil_nutrients_at_development_stage,
  Pest_and_desease_impact_at_development_stage,
  Weeds_impact_at_development_stage,
  Agricultural_management_efficiency_at_development_stage,
  # Effectiveness_of_agricultural_management_at_development_stage, 
  Water_supply_adequacy_at_development_stage, 
  # Cropping_systems_efficiency_at_development_stage,
  Local_constraints_at_development_stage,
  #Crop_type,
  #Intercropping,
  #Crop_variety,
  #Effectiveness_of_cropping_options,
  #Available_soil_water_at_initial_stage,
  #Available_soil_nutrients_at_initial_stage,
  #Pest_and_desease_impact_at_initial_stage,
  #Weeds_impact_at_initial_stage
  
  # At the mid stage: note the nodes that have been commented.
  # These are commun nodes for each stage and are omited in this stage
  # because they are already captured at initial stage
  
  # Type_of_water_diversion,
  Sediment_load_at_mid_stage,
  Rain_event_occurence_at_mid_stage,
  # Slope,
  # Social_arrangements,
  Main_Canals_Maitnenance_at_mid_stage,
  Amount_of_shared_flood_at_mid_stage,
  # Location_of_the_plot,
  Upstream_abstraction_at_mid_stage,
  Field_canal_maintenance_at_mid_stage,
  Presence_of_the_farmer_during_flood_event_at_mid_stage,
  Amount_of_flood_reaching_the_plot_at_mid_stage,
  # Soil_type,
  # Manure_application,
  # Soil_water_holding_capacity,
  Evapotranspiration_at_mid_stage,
  Rainfall_amount_at_mid_stage,
  # Initial_soil_water_content,
  Available_soil_water_at_mid_stage,
  # Relative_wealth_status,
  # Mutual_aids,
  Access_to_inputs_at_mid_stage,
  Available_paid_labor_at_mid_stage,
  Available_Labor_force_at_mid_stage,
  # skills_of_the_farmer,
  Chemical_application_at_mid_stage,
  Application_of_traditional_crop_protection_Methods_at_mid_stage,
  Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage,
  Effectiveness_of_Weeding_at_mid_stage,
  Fertilizers_application_at_mid_stage,
  Rich_sediments_addition_from_flood_at_mid_stage,
  Available_soil_nutrients_at_mid_stage,
  Pest_and_desease_impact_at_mid_stage,
  Weeds_impact_at_mid_stage,
  Agricultural_management_efficiency_at_mid_stage,
  Local_constraints_at_mid_stage,
  # Effectiveness_of_agricultural_management_at_mid_stage, 
  Water_supply_adequacy_at_mid_stage, 
  # Cropping_systems_efficiency_at_mid_stage,
  #Crop_type,
  #Intercropping,
  #Crop_variety,
  #Effectiveness_of_cropping_options,
  #Available_soil_water_at_initial_stage,
  #Available_soil_nutrients_at_initial_stage,
  #Pest_and_desease_impact_at_initial_stage,
  #Weeds_impact_at_initial_stage,
  
  # At the late stage: note the nodes that have been commented.
  # These are commun nodes for each stage and are omited in this stage
  # because they are already captured at initial stage
  
  # Type_of_water_diversion,
  Sediment_load_at_late_stage,
  Rain_event_occurence_at_late_stage,
  # Slope,
  # Social_arrangements,
  Main_Canals_Maitnenance_at_late_stage,
  Amount_of_shared_flood_at_late_stage,
  # Location_of_the_plot,
  Upstream_abstraction_at_late_stage,
  Field_canal_maintenance_at_late_stage,
  Presence_of_the_farmer_during_flood_event_at_late_stage,
  Amount_of_flood_reaching_the_plot_at_late_stage,
  # Soil_type,
  # Manure_application,
  # Soil_water_holding_capacity,
  Evapotranspiration_at_late_stage,
  Rainfall_amount_at_late_stage,
  # Initial_soil_water_content,
  Available_soil_water_at_late_stage,
  # Relative_wealth_status,
  # Mutual_aids,
  Access_to_inputs_at_late_stage,
  Available_paid_labor_at_late_stage,
  Available_Labor_force_at_late_stage,
  # skills_of_the_farmer,
  Chemical_application_at_late_stage,
  Application_of_traditional_crop_protection_Methods_at_late_stage,
  Effectiveness_of_pest_and_disease_reduction_practices_at_late_stage,
  Effectiveness_of_Weeding_at_late_stage,
  Fertilizers_application_at_late_stage,
  Rich_sediments_addition_from_flood_at_late_stage,
  Available_soil_nutrients_at_late_stage,
  Pest_and_desease_impact_at_late_stage,
  Weeds_impact_at_late_stage,
  Agricultural_management_efficiency_at_late_stage,
  Local_constraints_at_late_stage,
  # Effectiveness_of_agricultural_management_at_late_stage, 
  Water_supply_adequacy_at_late_stage, 
  # Cropping_systems_efficiency_at_late_stage 
  #Crop_type,
  #Intercropping,
  #Crop_variety,
  #Effectiveness_of_cropping_options,
  #Available_soil_water_at_initial_stage,
  #Available_soil_nutrients_at_initial_stage,
  #Pest_and_desease_impact_at_initial_stage,
  #Weeds_impact_at_initial_stage
  nutrient_supply_adequacy_at_initial_stage,
  nutrient_supply_adequacy_at_development_stage,
  nutrient_supply_adequacy_at_mid_stage,
  nutrient_supply_adequacy_at_late_stage
))

## 2) Graphical Independence Network ####
network <- grain(net)

## 3) Coarse it to bnlearn object ####
network_bn_fit <- as.bn.fit(network)