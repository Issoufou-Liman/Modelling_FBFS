## constructing the evidence list ####
ficher <- "output_files/Modelling_FBFS_ssp_Case_study_1.rds"
if(!file.exists(ficher)){
  evidence <- c('Soil_type', 'Manure_application', 'Amount_of_flood_reaching_the_plot_at_initial_stage')
  
  bloks = 1:10
  ssp <- sapply(bloks, function (nothing){
    out <- sample_cpdist(bn = network_bn_fit, node = "Available_soil_water_at_initial_stage",
                         op = "proba", evidence = evidence, include_relatives = FALSE,n_run = 1)
    cbind(out$prior, out$posterior)
    
  }, simplify = F, USE.NAMES = TRUE)
  
  ssp  <- reshape2::melt(ssp)
  ssp$Soil_type <- factor(ssp$Soil_type, levels = unique(ssp$Soil_type))
  ssp$Manure_application <- factor(ssp$Manure_application, levels = unique(ssp$Manure_application))
  ssp$Amount_of_flood_reaching_the_plot_at_initial_stage <- factor(ssp$Amount_of_flood_reaching_the_plot_at_initial_stage, levels = unique(ssp$Amount_of_flood_reaching_the_plot_at_initial_stage))
  ssp$variable <- factor(ssp$variable, levels = unique(ssp$variable))
  saveRDS(ssp, ficher)
} else {
  ssp <- readRDS(ficher)
}

titres <- structure(list(Soil_type = structure(1L, .Label = "Soil type", class = "factor"), 
                         Manure_application = structure(1L, .Label = "Manure", class = "factor"), 
                         Amount_of_flood_reaching_the_plot_at_initial_stage = structure(1L, .Label = "Floodwater", class = "factor"), 
                         variable = structure(1L, .Label = "Probability of\n available soil water", class = "factor"), 
                         value = structure(1L, .Label = "value", class = "factor"), 
                         L1 = structure(1L, .Label = "L1", class = "factor")), class = "data.frame", row.names = c(NA, 
                                                                                                                   -1L))
p1 <- ggplot(data = ssp, aes(x=L1, y = value, fill = variable))+
  geom_bar(stat="identity", position="stack")+
  ggh4x::facet_nested(.~Soil_type+Amount_of_flood_reaching_the_plot_at_initial_stage+Manure_application, 
                         space = 'free_x', scales = 'free_x')+
  scale_fill_discrete(labels = function(x) gsub("[.]", " ", x))+
  scale_x_discrete(breaks=1:10, expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0), breaks = seq(.25, 1, by=.25),
                     labels = function(x) sprintf("%.2f", x))+
  guides(color=FALSE)+
  my_theme +
  # switch the facet strip label to outside 
  # remove background color
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    legend.text = element_text(size = plot_font_size+2),
    legend.position = 'top',
    legend.spacing.x = unit(1.0, 'cm'),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(0,0,-5, 0),
    legend.key.size = unit(1,"line"),
    strip.text = element_text(size = plot_font_size),
    strip.text.x = element_text(margin = margin(t=3, b = 3)),
    axis.text = element_text(size = plot_font_size+2)
  )

p2 = ggplot(data = titres, aes(x=1, y = 1, label = variable, fill=variable))+ 
  geom_text(angle = -90, size=plot_font_size-6, family=plots_font_family,lineheight = 0.9)+
  ggh4x::facet_nested(.~Soil_type+Amount_of_flood_reaching_the_plot_at_initial_stage+Manure_application,
                         space = 'free_x', scales = 'free_x')+
  theme_void(base_family = plots_font_family)+
  theme(
    plot.margin =margin(19.5,0,0, 0),
    strip.text = element_text(size = plot_font_size),
    strip.text.x = element_text(margin = margin(t=3, b = 3)))

p1 <- list(p1, p2)
p1$ncol=2
p1$widths=c(12.8,1.2)

grid.newpage()
ga = do.call("grid.arrange", p1)
gb = grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
               gp=gpar(lwd=1, fill=NA, col="lightgray"))
g <- gTree(children = gList(ga, gb))

ggsave(plot=g, device = 'png', filename = "figures/Modelling_FBFS_factor_of_soil_water.png", width = max_plots_width_in, 
       height =max_plots_height_in/3, units = "in", dpi = min_plots_res
)
ggsave(plot=g, device = 'pdf', filename = "figures/Modelling_FBFS_factor_of_soil_water.pdf", width = max_plots_width_in, 
       height =max_plots_height_in/3, units = "in", dpi = min_plots_res
)