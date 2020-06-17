source('source_files/north_arrow_and_scalebar.R')

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

## exporting the plot to file.####
export_fun <- function(export, output_dir = "figures"){
  paste0(output_dir, '/', export, ".png")
}

my_theme0 <- theme_minimal()+
  theme(
    plot.background = element_rect(fill = 'white', colour = 'lightgrey'),
    plot.margin = margin(t=1, r=1,b=-1, l=-1),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
my_theme1 <- theme(
  axis.title = element_blank(),
  plot.margin = margin(t=1, r=1,b=-1, l=-1)
)

fmt_dcimals <- function(decimals=0){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}

## Tigray #####

afr <- readOGR("data_files/shapefiles/Africa.shp")


Ethiopia_L1 <- getData("GADM", country="ETH", level=1, path = 'data_files/shapefiles')
Ethiopia_L0 <- aggregate(Ethiopia_L1)
Tigray <- Ethiopia_L1 [Ethiopia_L1$NAME_1 == "Tigray", ]
writeOGR(Tigray, 'data_files/shapefiles', "Tigray", driver="ESRI Shapefile", overwrite_layer=TRUE)

Tigray_names <- list(Tigray=Tigray, Ethiopia=Ethiopia_L0)
Tigray_names <- sapply(Tigray_names, function (i) {
  out=as.data.frame(coordinates(rgeos::gCentroid(i)))
  names(out)<- c('Long', 'Lat')
  out
}, simplify = FALSE
)
Tigray_names <- do.call(rbind, Tigray_names)
Tigray_names$Place <- rownames(Tigray_names)

inst00 <- ggplot()+
  geom_path(data=fortify(afr), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  
  geom_polygon(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), fill='red', size = 0.05, colour='red')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  my_theme0

inst0 <- ggplot()+
  geom_path(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.05, fill='red', colour='red')+
  
  geom_text(data = Tigray_names[2, ], aes(x = Long, y = Lat, label = Place),
            position = position_nudge(y = -0.3),
            hjust=0.5, vjust=1, size=2)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  my_theme0

inst_add <- ggplot()+
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.25, fill='white', colour='black')+
  scalebar(data=fortify(Tigray), dist = 50, dist_unit = "km",
           transform = TRUE, model = "WGS84", anchor=c(x=38.5, y=12.25+0.2), 
           height=0.03, st.dist=0.025, st.size=2, border.size =0.25)+
  north(data=fortify(Tigray), scale = 0.2, anchor=c(x=37.25, y=14.85))+
  coord_equal()+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  my_theme1

source('source_files/schemes.R')

inst_add <- inst_add+
  geom_point(data = data.frame(Tigray_localities), aes(x = coords.x1, y = coords.x2), color = 'red', size = 1, alpha = 0.5)+
  geom_text(data = data.frame(Tigray_localities), aes(x = coords.x1, y = coords.x2, label = SchemeName_short),
            position = position_nudge(y = 0.025),
            hjust=0.5, vjust=0, size=2)

inst_add <- inst_add +
  annotation_custom(
    grob = ggplotGrob(inst0+
                        theme(panel.grid = element_blank(),
                              plot.background = element_rect(fill = 'white'),
                              panel.border = element_blank()
                        )),
    xmin = 37.5,
    xmax = 38.5,
    ymin = 12.25+0.2,
    ymax = 13.25+0.2
  )+
  annotation_custom(
    grob = ggplotGrob(inst00+
                        theme(panel.grid = element_blank(),
                              plot.background = element_rect(fill = 'white'),
                              panel.border = element_blank()
                        )),
    xmin = 36.4,
    xmax = 37.5,
    ymin = 12.3,
    ymax = 13.25
  )

  
inst <- readRDS('data_files/ggmaps/Tigray_ggmap.rds')


inst <- inst +
  coord_equal()+
  scalebar(x.min = min(inst$data$lon), x.max = max(inst$data$lon),
           y.min = min(inst$data$lat), y.max = max(inst$data$lat),
           dist = 20, dist_unit = "km",
           transform = TRUE, model = "WGS84", anchor=c(x=39.8, y=12.155), 
           height=0.02, st.dist=0.015, st.size=3, border.size =0.25)+
  north(x.min = min(inst$data$lon), x.max = max(inst$data$lon),
        y.min = min(inst$data$lat), y.max = max(inst$data$lat),
        scale = 0.2, anchor=c(x=40.075, y=12.34))+
  
  geom_point(data = data.frame(Tigray_localities), aes(x = coords.x1, y = coords.x2), color = 'red', size = 1, alpha = 0.5)+
  geom_text(data = data.frame(Tigray_localities), aes(x = coords.x1, y = coords.x2, label = SchemeName),
            alpha = 0.8,
            position = position_nudge(y = -0.01),
            hjust=0, vjust=1, size=2)+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  my_theme1+
  theme(plot.margin = margin(t=-0.25, b=-0.25))

  # ggmap::inset(ggplotGrob(inst0), xmin = 39.19-0.01, xmax = 39.59-0.01, ymin = 12.55+0.025, ymax = 13.05+0.025)

img_list <- list.files('data_files/images/', full.names = T, pattern = 'Tigray')
top <- c('Cropping systems', 'Water diversion', 'Tillage')

f <- function(img_list){
  lapply(img_list, function(i){
    out <- image_ggplot(image_read(i))+
      theme(plot.margin = margin(l=1, r=1))
  })
} 

img_list <- f(img_list)
img_list$nrow = 1
img_list <- do.call(grid.arrange, img_list)
top0 <-   grobTree(rectGrob(gp=gpar(fill="NA", col="lightgray")), 
                   grid::textGrob('Common agricultural practices in Tigray (F)',
                         x=0.04, y=0.7, hjust=0, vjust = 1,
                         gp = gpar(fontfamily='serif',fontsize=10,fontface="bold.italic", col="blue")))
top <- lapply(top, function(i) grobTree(rectGrob(gp=gpar(fill="bisque4", col="lightgray")), 
                grid::textGrob(i, hjust=0.5, vjust = 0.5,
                               gp = gpar(fontfamily='serif',fontsize=10,fontface="bold", col="black"))))

top$ncol=3
top <- do.call(grid.arrange, top)
top <- grid.arrange(top0, top,ncol=1)
img_list <- grid.arrange(top, img_list, heights=unit(c(1,2.5), 'null'))

img_list <- gTree(children = gList(img_list, grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                                       gp=gpar(lwd=2, fill="NA", col="white"))))

inst <- grid.arrange(
  grid::textGrob("Sampled areas in Tigray (D)",
                 x=0.04, y=0.7, hjust=0, vjust = 1,
                 gp = gpar(fontfamily='serif',fontsize=10,fontface="bold.italic", col="blue")),
  inst,
  # inst+theme(plot.background = element_rect(fill='gray')),
  heights=unit(c(1,10), 'null')
)

inst_add <- grid.arrange(
  grid::textGrob("Location of Tigray, Ethiopia (B)",
                 x=0.04, y=0.7, hjust=0, vjust = 1,
                 gp = gpar(fontfamily='serif',fontsize=10,fontface="bold.italic", col="blue")),
  inst_add,
  heights=unit(c(1,10), 'null')
)
inst_Tigray <- grid.arrange(inst, img_list, heights=unit(c(7/1.95, 3.76/2), 'in'))

# inst_Tigray <- gTree(children = gList(grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
#                                                 gp=gpar(lwd=1, fill="lightgray", col="white")), inst_Tigray))

ggsave(plot = inst_Tigray, filename = 'figures/test_grid.png', device = 'png', dpi = 300, height = (((7/2)+(3.76/2))), width = ((6.58/2)*2.1)/2)

inst_Tigray <- grid.arrange(inst_add, inst_Tigray, heights=unit(c(7/2.5, (7/1.95+(3.76/2)) ), 'in'))

inst_Tigray <- gTree(children = gList(grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"),
                                                gp=gpar(lwd=1, fill="lightgray", col="white")), inst_Tigray))

# ggsave(plot = inst_Tigray, filename = 'figures/test_grid.png', device = 'png', dpi = 300, height = (((7/2.5)+(7/2)+(3.76/2))), width = ((6.58/2)*2.1)/2)

## Kisumu######

source('source_files/north_arrow_and_scalebar.R')

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

## exporting the plot to file.####
export_fun <- function(export, output_dir = "figures"){
  paste0(output_dir, '/', export, ".png")
}

my_theme0 <- theme_minimal()+
  theme(
    plot.background = element_rect(fill = 'white', colour = 'lightgrey'),
    plot.margin = margin(t=1, r=1,b=-1, l=-1),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
my_theme1 <- theme(
  axis.title = element_blank()
)

fmt_dcimals <- function(decimals=0){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}
afr <- readOGR("data_files/shapefiles/Africa.shp")


Kenya_L1 <- getData("GADM", country="KEN", level=1, path = 'data_files/shapefiles')
Kenya_L0 <- aggregate(Kenya_L1)
Kisumu <- Kenya_L1 [Kenya_L1$NAME_1 == "Kisumu", ]
writeOGR(Kisumu, 'data_files/shapefiles', "Kisumu", driver="ESRI Shapefile", overwrite_layer=TRUE)

Kisumu_names <- list(Kisumu=Kisumu, Kenya=Kenya_L0)
Kisumu_names <- sapply(Kisumu_names, function (i) {
  out=as.data.frame(coordinates(rgeos::gCentroid(i)))
  names(out)<- c('Long', 'Lat')
  out
}, simplify = FALSE
)
Kisumu_names <- do.call(rbind, Kisumu_names)
Kisumu_names$Place <- rownames(Kisumu_names)

inst00 <- ggplot()+
  geom_path(data=fortify(afr), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  
  geom_polygon(data=fortify(Kenya_L0), aes(x=long, y= lat, group=group), fill='red', size = 0.05, colour='red')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  my_theme0

inst0 <- ggplot()+
  geom_path(data=fortify(Kenya_L0), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  
  geom_polygon(data=fortify(Kisumu), aes(x=long, y= lat, group=group), size = 0.05, fill='red', colour='red')+
  
  geom_text(data = Kisumu_names[2, ], aes(x = Long, y = Lat, label = Place),
            position = position_nudge(y = -0.3),
            hjust=0.5, vjust=1, size=2)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  my_theme0

inst_add <- ggplot()+
  geom_polygon(data=fortify(Kisumu), aes(x=long, y= lat, group=group), size = 0.25, fill='white', colour='black')+
  scalebar(data=fortify(Kisumu), dist = 15, dist_unit = "km",
           transform = TRUE, model = "WGS84", anchor=c(x=35.06, y=-0.55), 
           height=0.03, st.dist=0.05, st.size=2, border.size =0.25)+
  north(data=fortify(Kisumu), scale = 0.32, anchor=c(x=35.44, y=0.03))+
  coord_equal()+
  scale_x_continuous(expand = c(0, 0))+
  # scale_y_continuous(expand = c(0, 0))+
  scale_y_continuous()+
  my_theme1

source('source_files/schemes.R')

inst_add <- inst_add+
  geom_point(data = data.frame(Kisumu_localities), aes(x = coords.x1, y = coords.x2), color = 'red', size = 1, alpha = 0.5)+
  geom_text(data = data.frame(Kisumu_localities), aes(x = coords.x1, y = coords.x2, label = SchemeName),
            alpha = 0.8,
            position = position_nudge(y = -0.005),
            hjust=0, vjust=1, size=2)

inst_add <- inst_add +
  annotation_custom(
    grob = ggplotGrob(inst0+
                        theme(panel.grid = element_blank(),
                              plot.background = element_rect(fill = 'white'),
                              panel.border = element_blank()
                        )),
    xmin = 35.09,
    xmax = 35.35-0.01,
    ymin = -0.74,
    ymax = -0.15
  )+
  annotation_custom(
    grob = ggplotGrob(inst00+
                        theme(panel.grid = element_blank(),
                              plot.background = element_rect(fill = 'white'),
                              panel.border = element_blank()
                        )),
    xmin = 34.42,
    xmax = 34.7,
    ymin = -0.75,
    ymax = -0.15
  )+
  theme(plot.margin = margin(b=-1, t=1))


inst <- readRDS('data_files/ggmaps/Kisumu_ggmap.rds')

inst <- inst +
  coord_equal()+
  scalebar(x.min = min(inst$data$lon), x.max = max(inst$data$lon),
           y.min = min(inst$data$lat), y.max = max(inst$data$lat),
           dist = 10, dist_unit = "km",
           transform = TRUE, model = "WGS84", anchor=c(x=35, y=-0.416), 
           height=0.02, st.dist=0.015, st.size=3, border.size =0.25)+
  north(x.min = min(inst$data$lon), x.max = max(inst$data$lon),
        y.min = min(inst$data$lat), y.max = max(inst$data$lat),
        scale = 0.2, anchor=c(x=35.125, y=-0.325))+
  
  geom_point(data = data.frame(Kisumu_localities), aes(x = coords.x1, y = coords.x2), color = 'red', size = 1, alpha = 0.5)+
  geom_text(data = data.frame(Kisumu_localities), aes(x = coords.x1, y = coords.x2, label = SchemeName),
            alpha = 0.8,
            position = position_nudge(y = -0.0025),
            hjust=0, vjust=1, size=2)+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  my_theme1+
  theme(plot.margin = margin(b=-0.5))
# ggmap::inset(ggplotGrob(inst0), xmin = 34.675-0.038, xmax = 34.9-0.038, ymin = -0.2+0.025, ymax = 0+0.0055)

img_list <- list.files('data_files/images/', full.names = T, pattern = 'Kisumu')
top <- c('Cropping systems', 'Water diversion', 'Tillage')

f <- function(img_list){
  lapply(img_list, function(i){
    out <- image_ggplot(image_read(i))+
      theme(plot.margin = margin(l=1, r=1))
  })
} 

img_list <- f(img_list)
img_list$nrow = 1
img_list <- do.call(grid.arrange, img_list)
top0 <-   grobTree(rectGrob(gp=gpar(fill="NA", col="lightgray")), 
                   grid::textGrob('Common agricultural practices in Kisumu County (E)',
                                  x=0.04, y=0.7, hjust=0, vjust = 1,
                                  gp = gpar(fontfamily='serif',fontsize=10,fontface="bold.italic", col="blue")))
top <- lapply(top, function(i) grobTree(rectGrob(gp=gpar(fill="bisque4", col="lightgray")), 
                                        grid::textGrob(i, hjust=0.5, vjust = 0.5,
                                                       gp = gpar(fontfamily='serif',fontsize=10,fontface="bold", col="black"))))

top$ncol=3
top <- do.call(grid.arrange, top)
top <- grid.arrange(top0, top,ncol=1)
img_list <- grid.arrange(top, img_list, heights=unit(c(1,2.5), 'null'))

img_list <- gTree(children = gList(img_list, grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                                       gp=gpar(lwd=2, fill="NA", col="white"))))

inst <- grid.arrange(
  grid::textGrob("Sampled areas in Kisumu County (C)",
                 x=0.04, y=0.7, hjust=0, vjust = 1,
                 gp = gpar(fontfamily='serif',fontsize=10,fontface="bold.italic", col="blue")),
  inst,
  # inst+theme(plot.background = element_rect(fill='gray')),
  heights=unit(c(1,10), 'null')
)

inst_add <- grid.arrange(
  grid::textGrob("Location of Kisumu County, Kenya (A)",
                 x=0.04, y=0.7, hjust=0, vjust = 1,
                 gp = gpar(fontfamily='serif',fontsize=10,fontface="bold.italic", col="blue")),
  inst_add,
  heights=unit(c(1,10), 'null')
)
inst_Kisumu <- grid.arrange(inst, img_list, heights=unit(c(7/1.95, 3.76/2), 'in'))

# inst_Kisumu <- gTree(children = gList(grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
#                                                 gp=gpar(lwd=1, fill="lightgray", col="white")), inst_Kisumu))

ggsave(plot = inst_Kisumu, filename = 'figures/test_grid.png', device = 'png', dpi = 300, height = (((7/2)+(3.76/2))), width = ((6.58/2)*2.1)/2.1)

inst_Kisumu <- grid.arrange(inst_add, inst_Kisumu, heights=unit(c(7/2.5, (7/1.95+(3.76/2)) ), 'in'))

inst_Kisumu <- gTree(children = gList(grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"),
                                                gp=gpar(lwd=1, fill="lightgray", col="white")), inst_Kisumu))

ggsave(plot = inst_Kisumu, filename = 'figures/test_grid.png', device = 'png', dpi = 300, height = (((7/2.5)+(7/2)+(3.76/2))), width = ((6.58/2)*2.1)/2.1)

#############################################
inst <- grid.arrange(inst_Kisumu, inst_Tigray, ncol = 2)

inst <- gTree(children = gList(inst, grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                               gp=gpar(lwd=2, fill="NA", col="darkgray"))))

ggsave(plot = inst, filename = 'figures/Modelling_FBFS_study_area_revision.png', device = 'png', dpi = 300, height = (((7/2.5)+(7/2)+(3.76/2))), width = ((6.58/2)*2.15))
ggsave(plot = inst, filename = 'figures/Modelling_FBFS_study_area_revision.pdf', device = 'pdf', dpi = 300, height = (((7/2.5)+(7/2)+(3.76/2))), width = ((6.58/2)*2.15))

# ggsave(plot = inst, filename = 'figures/Modelling_FBFS_study_area.png', device = 'png', dpi = 300, height = (((7/2)+(3.76/2))), width = (6.58/2)*2.1)
# ggsave(plot = inst, filename = 'figures/Modelling_FBFS_study_area.pdf', device = 'pdf', dpi = 300, height = (((7/2)+(3.76/2))), width = (6.58/2)*2.1)
# 
