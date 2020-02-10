## Available soil water BNs  ####

net <- bnlearn::model2network("[SoilType][ManureApp][SoilHoldWater|SoilType:ManureApp][EvapoTranspirat][InitWaterCont][RainAmount][FloodReachsPlot][AvailSoilWater|SoilHoldWater:EvapoTranspirat:RainAmount:InitWaterCont:FloodReachsPlot]")

abbr_node_names <- bnlearn::nodes(net)

net <- bnlearn::model2network("[Soil_type][Manure_application][Soil_water_holding_capacity|Soil_type:Manure_application][Evapotranspiration_at_initial_stage][Initial_soil_water_content][Rainfall_amount_at_initial_stage][Amount_of_flood_reaching_the_plot_at_initial_stage][Available_soil_water_at_initial_stage|Soil_water_holding_capacity:Evapotranspiration_at_initial_stage:Rainfall_amount_at_initial_stage:Initial_soil_water_content:Amount_of_flood_reaching_the_plot_at_initial_stage]")

legende <- bnlearn::nodes(net)

## Extracting Available soil water bn ####
ficher <- "output_files/Modelling_FBFS_BNs_Case_study_1.rds"
if(!file.exists(ficher)){
  net <- decisionSupportExtra::extract_bn(bn = network_bn_fit, string_model = net)
  if (!dir.exists("output_files")) {
    dir.create("output_files")
  }
  saveRDS(net, ficher)
} else {
  net <- readRDS(ficher)
}

