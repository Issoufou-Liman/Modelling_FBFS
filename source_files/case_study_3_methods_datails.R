legende = c(AgricManagEff = "Agricultural_management_efficiency_at_mid_stage", 
            AvailSoilNut = "Available_soil_nutrients_at_mid_stage", 
            AvailSoilWater = "Available_soil_water_at_mid_stage",
            CropType = "Crop_type",
            EffCropOptions = "Effectiveness_of_cropping_options", 
            EffPestDiseaseReduc = "Effectiveness_of_pest_and_disease_reduction_practices_at_mid_stage", 
            EffWeeding = "Effectiveness_of_weeding_at_mid_stage", 
            FarmConstraintsLate = "Local_constraints_at_late_stage",
            FarmConstraints = "Local_constraints_at_mid_stage", 
            AdeqNutSupl = "nutrient_supply_adequacy_at_mid_stage",
            PestdiseaseImpactDev = "Pest_and_disease_impacts_at_development_stage",
            PestdiseaseImpact = "Pest_and_disease_impacts_at_mid_stage", 
            AdeqWatSupl = "Water_supply_adequacy_at_mid_stage", 
            WeedsImpactDev = "Weed_impacts_at_development_stage", 
            WeedsImpact = "Weed_impacts_at_mid_stage"
)

legende <- mapply(paste, names(legende), sprintf('\u2192'), legende)
legende <- gsub("_", " ",  legende)
legende <- gsub(" at mid stage", "",  legende)

bnlearn::nodes(net) <- names(legende)

png(export_fun(export = "Modelling_FBFS_Farming_Constraints"),
    res = min_plots_res,
    units = 'in',
    width = max_plots_width_in, 
    height = max_plots_height_in/2,
    pointsize = 10.5)
par(font=plots_font, family = plots_font_family, lwd=plots_lwd)

graphviz_chart_bn (x = net, type = "barprob", layout = "dot", draw.levels = TRUE,abbreviate=FALSE,
                   grid = TRUE, scale = c(max_plots_height_in/2, max_plots_width_in), col = "black", bg = "transparent",
                   text.col = "black", bar.col = "green", strip.bg = "lightyellow")
legend('bottomleft', legend = legende, text.width = 0.59*strwidth(legende[which.max(nchar(legende))]), 
       cex=0.59, ncol = 1,
       bty="o", box.lwd=1, box.col='lightyellow', xjust=1, yjust=1, bg=legend_bg,
       title = 'Legend', title.col = 'blue', inset=c(0.04, 0.08))
box(col = 'lightgray')
dev.off()