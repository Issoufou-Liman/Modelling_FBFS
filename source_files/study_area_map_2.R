library(rgdal)
library(raster)
# library(ggsn)
# library(magick)
library(gridExtra)

# source('source_files/north_arrow_and_scalebar.R')
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

my_theme <- theme_bw(base_size = plot_font_size, 
                     base_family = plots_font_family) +
  theme(
    # panel.grid.minor = element_blank(),
    # strip.background = element_blank(),
    legend.title = element_blank(),
    # remove facet spacing on x-direction
    panel.spacing = unit(0,"line"),
    
    panel.border = element_rect(fill=NA, size = 0.05),
    panel.grid.major = element_line(size = 0.05),
    panel.grid.minor = element_line(size = 0.02),
    axis.text.y = element_text(hjust = 0),
    axis.ticks = element_line(colour = 'black', size = 0.05),
    legend.position="top",
    legend.justification = 'right',
    strip.background = element_rect(fill='lightgoldenrodyellow', color = "gray", size = 0.075),
    strip.text = element_text(colour = 'black', face = 'bold'),
    strip.text.x = element_text(margin = margin(t=10, b = 10)),
    plot.title=element_text(size=10, face="bold", color="black", 
                            margin=unit(c(0, 0, 0, 0), "pt")),
    plot.margin=unit(c(0, 0, 0, 0), "pt"),
    plot.subtitle=element_text(size=10, face="italic", color="black", 
                               margin=unit(c(0, 0, 0, 0), "pt")))

## Tigray #####

Ethiopia_L1 <- getData("GADM", country="ETH", level=1, path = 'data_files/shapefiles')
Ethiopia_L0 <- aggregate(Ethiopia_L1)
Tigray <- Ethiopia_L1 [Ethiopia_L1$NAME_1 == "Tigray", ]
writeOGR(Tigray, 'data_files/shapefiles', "Tigray", driver="ESRI Shapefile", overwrite_layer=TRUE)

Tigray_localities <- list(Tigray=Tigray, Ethiopia=Ethiopia_L0)
Tigray_localities <- sapply(Tigray_localities, function (i) {
  out=as.data.frame(coordinates(rgeos::gCentroid(i)))
  names(out)<- c('Long', 'Lat')
  out
}, simplify = FALSE
)
Tigray_localities <- do.call(rbind, Tigray_localities)
Tigray_localities$Place <- rownames(Tigray_localities)

inst0 <- ggplot()+
  geom_path(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.05, fill='red', colour='red')+
  
  geom_text(data = Tigray_localities[2, ], aes(x = Long, y = Lat, label = Place),
            position = position_nudge(y = -0.7),
            hjust=0.5, vjust=1, size=3.5)+
  theme_minimal()+
  my_theme +
  theme(
    axis.text = element_blank(),
    # axis.text = element_text(size = 8),
    axis.ticks = element_blank(),
    
    # axis.title = element_text(size = 12, face = 'bold'),
    axis.title = element_blank(),
    strip.text = element_text(size = 8, face = "bold"),
    legend.text = element_text(family = 'serif', face = 'bold', size = 8),
    plot.title = element_text(size = 14, face = 'bold', color = 'black'),
    plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=1, r=1,b=-1, l=-1))


source('source_files/schemes.R')
inst <- readRDS('data_files/ggmaps/Tigray_ggmap.rds')

fmt_dcimals <- function(decimals=0){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}
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
  
  geom_point(data = data.frame(Tigray_localities), aes(x = coords.x1, y = coords.x2), color = 'red', size = 4, alpha = 0.5)+
  geom_text(data = data.frame(Tigray_localities), aes(x = coords.x1, y = coords.x2, label = SchemeName),
            alpha = 0.8,
            position = position_nudge(y = -0.02),
            hjust=0, vjust=1, size=3.5)+
  theme_minimal()+
  my_theme +
  theme(
    axis.text = element_text(size = 12),
    # axis.title = element_text(size = 12, face = 'bold'),
    # strip.text = element_text(size = 8, face = "bold"),
    legend.text = element_text(family = 'serif', face = 'bold', size = 8.5,
                               margin=margin(t=0, b=0)),
    plot.background = element_rect(fill = 'white', colour = 'lightgrey'),
    # plot.title = element_text(size = 14, face = 'bold', color = 'black'),
    # plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue')
  )+
  scale_x_continuous(expand = c(0.005, 0.005))+
  scale_y_continuous(expand = c(0.005, 0.005))+
  # labs(subtitle='Tigray, region, Ethiopia')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=0, b=0))+
  ggmap::inset(ggplotGrob(inst0), xmin = 39.19-0.01, xmax = 39.59-0.01, ymin = 12.55+0.025, ymax = 13.05+0.025)

img_list <- list.files('data_files/images/', full.names = T, pattern = 'Tigray')
top <- c('Cropping systems', 'Water diversion', 'Tillage')

f <- function(img_list){
  lapply(img_list, function(i){
    out <- image_ggplot(image_read(i))
  })
} 

img_list <- f(img_list)
img_list <- lapply(1:length(img_list), function(i){
  out <- ggplotGrob(img_list[[i]] + theme(plot.margin = margin(r=1, l=1)))
  top <- grobTree(rectGrob(gp=gpar(fill="bisque4", col="lightgray"), height=1.3), 
                  grid::textGrob(top[i], hjust=0.5, vjust = 0.5,
                                 gp = gpar(fontfamily='serif',fontsize=13,fontface="bold", col="black")))
  arrangeGrob(top,out, 
              layout_matrix = rbind(
                rep(1,10),
                rep(1,10),
                rep(1,10),
                rep(1,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10)
                
              ))
})

# img_list$layout_matrix = matrix(c(NA, 1, 2, 3), ncol = 2)
img_list$ncol = 3
img_list <- do.call(grid.arrange, img_list)
img_list <- list(img_list)
img_list$top <- grid::textGrob('Common agricultural practices in Tigray (d)\n',
               x=0.04, y=0.6, hjust=0, vjust = 1,
               gp = gpar(fontfamily='serif',fontsize=15.5,fontface="bold.italic", col="blue"))
img_list <- do.call(grid.arrange, img_list)
img_list <- gTree(children = gList(img_list, grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                                       gp=gpar(lwd=2, fill="NA", col="white"))))

img_list <- list(ggplotGrob(inst), img_list)
img_list$layout_matrix = rbind(
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2)
)
img_list$top <- grid::textGrob("Sampled areas Tigray region, Ethiopia (b)\n",
                               x=0.04, y=0.6, hjust=0, vjust = 1,
                               gp = gpar(fontfamily='serif',fontsize=15.5,fontface="bold.italic", col="blue"))
inst_Tigray <- do.call(grid.arrange, img_list)

inst_Tigray <- gTree(children = gList(grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                                gp=gpar(lwd=1, fill="lightgray", col="white")), inst_Tigray))

## Kisumu #####

Ethiopia_L1 <- getData("GADM", country="KEN", level=1, path = 'data_files/shapefiles')
Ethiopia_L0 <- aggregate(Ethiopia_L1)
Tigray <- Ethiopia_L1 [Ethiopia_L1$NAME_1 == "Kisumu", ]
writeOGR(Tigray, 'data_files/shapefiles', "Kisumu", driver="ESRI Shapefile", overwrite_layer=TRUE)

Tigray_localities <- list(Kisumu = Tigray, Kenya=Ethiopia_L0)
Tigray_localities <- sapply(Tigray_localities, function (i) {
  out=as.data.frame(coordinates(rgeos::gCentroid(i)))
  names(out)<- c('Long', 'Lat')
  out
}, simplify = FALSE
)
Tigray_localities <- do.call(rbind, Tigray_localities)
Tigray_localities$Place <- rownames(Tigray_localities)


inst0 <- ggplot()+
  geom_path(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.05, fill='red', colour='red')+
  geom_text(data = Tigray_localities[2, ], aes(x = Long, y = Lat, label = Place),
            position = position_nudge(y = -0.7),
            hjust=0.5, vjust=1, size=3.5)+
  theme_minimal()+
  #my_line_theme+
  my_theme +
  theme(
    axis.text = element_blank(),
    # axis.text = element_text(size = 8),
    axis.ticks = element_blank(),
    
    # axis.title = element_text(size = 12, face = 'bold'),
    axis.title = element_blank(),
    strip.text = element_text(size = 8, face = "bold"),
    legend.text = element_text(family = 'serif', face = 'bold', size = 8),
    plot.title = element_text(size = 14, face = 'bold', color = 'black'),
    plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=1, r=1,b=-1, l=-1))
###########################

source('source_files/schemes.R')
inst <- readRDS('data_files/ggmaps/Kisumu_ggmap.rds')

fmt_dcimals <- function(decimals=0){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}


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

  scale_x_continuous(labels = fmt_dcimals(1), expand = c(0.005, 0.005))+
  scale_y_continuous(labels = fmt_dcimals(1), expand = c(0.005, 0.005))

inst <-  inst +
  geom_point(data = data.frame(Kisumu_localities), aes(x = coords.x1, y = coords.x2), color = 'red', size = 4, alpha = 0.5)+
  geom_text(data = data.frame(Kisumu_localities), aes(x = coords.x1, y = coords.x2, label = SchemeName),
            alpha = 0.8,
            position = position_nudge(y = -0.01),
            hjust=0, vjust=1, size=3.5)+
  theme_minimal()+
  my_theme +
  theme(
    axis.text = element_text(size = 12),
    # axis.title = element_text(size = 12, face = 'bold'),
    # strip.text = element_text(size = 8, face = "bold"),
    legend.text = element_text(family = 'serif', face = 'bold', size = 8.5,
                               margin=margin(t=0, b=0)),
    plot.background = element_rect(fill = 'white', colour = 'lightgrey'),
    # plot.title = element_text(size = 14, face = 'bold', color = 'black'),
    # plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue')
  )+
  scale_x_continuous(expand = c(0.005, 0.005))+
  scale_y_continuous(expand = c(0.005, 0.005))+
  # labs(subtitle='Tigray, region, Ethiopia')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=0, b=0))+
  ggmap::inset(ggplotGrob(inst0), xmin = 34.675-0.038, xmax = 34.9-0.038, ymin = -0.2+0.025, ymax = 0+0.0055)


img_list <- list.files('data_files/images', full.names = T, pattern = 'Kisumu')
top <- c('Cropping systems', 'Water diversion', 'Tillage')

f <- function(img_list){
  lapply(img_list, function(i){
    out <- image_ggplot(image_read(i))
  })
} 

img_list <- f(img_list)
img_list <- lapply(1:length(img_list), function(i){
  out <- ggplotGrob(img_list[[i]] + theme(plot.margin = margin(r=1, l=1)))
  top <- grobTree(rectGrob(gp=gpar(fill="bisque4", col="lightgray"), height=1.3), 
                  grid::textGrob(top[i], hjust=0.5, vjust = 0.5,
                                 gp = gpar(fontfamily='serif',fontsize=13,fontface="bold", col="black")))
  arrangeGrob(top,out, 
              layout_matrix = rbind(
                rep(1,10),
                rep(1,10),
                rep(1,10),
                rep(1,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10),
                rep(2,10)
                
              ))
})

# img_list$layout_matrix = matrix(c(NA, 1, 2, 3), ncol = 2)
img_list$ncol = 3
img_list <- do.call(grid.arrange, img_list)
img_list <- list(img_list)
img_list$top <- grid::textGrob('Common agricultural practices in Kisumu (c)\n',
                               x=0.04, y=0.6, hjust=0, vjust = 1,
                               gp = gpar(fontfamily='serif',fontsize=15.5,fontface="bold.italic", col="blue"))
img_list <- do.call(grid.arrange, img_list)
img_list <- gTree(children = gList(img_list, grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                               gp=gpar(lwd=2, fill="NA", col="white"))))

img_list <- list(ggplotGrob(inst), img_list)
img_list$layout_matrix = rbind(
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2)
)
img_list$top <- grid::textGrob("Sampled areas in Kisumu County, Kenya (a)\n",
                               x=0.04, y=0.6, hjust=0, vjust = 1,
                               gp = gpar(fontfamily='serif',fontsize=15.5,fontface="bold.italic", col="blue"))
inst_Kisumu <- do.call(grid.arrange, img_list)

inst_Kisumu <- gTree(children = gList(grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                                gp=gpar(lwd=1, fill="lightgray", col="white")), inst_Kisumu))




inst <- grid.arrange(inst_Kisumu, inst_Tigray, ncol=2)

inst <- gTree(children = gList(inst, grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                               gp=gpar(lwd=2, fill="NA", col="darkgray"))))
# inst <- grid.arrange(inst)
# 
# inst <- grid.arrange(inst)

schemes <- rbind(
  c('Sampled areas', "Original design idea", "Diversion type", "Water Source", 'Water Source hydrology', "Water acquisition method"),
  c('East Kano', 'Engineers', "Modern", "Nyando River", 'Permanent', 'Pump'),
  c('West Kano', 'Engineers', "Modern", "Lake Victoria", "Permanent", "Pump"),
  c("Ahero out-growers", "Farmers", 'Traditional', "Nyando River, East Kano", 'Permanent', 'Gravity'),
  c("Awach out-growers", "Farmers", "Traditional", "Awach river", 'Permanent', 'Gravity'),
  c("East Nyankach", 'Farmers', 'Modern and Traditional', 'Runoff harvesting', 'Ephemeral', 'Household pond/roof'),
  c("Tsige'a (Guguf)", "Farmers and engineers", "Improved", "Dry Wadis", 'Ephemeral','Gravity'),
  c("Dayu (Gerjele)", "Engineers", "Modern", "Dry Wadis", 'Ephemeral', "Gravity"),
  c("Harosha (Tumuga)", "Farmers", "Traditional", "Dry Wadis", 'Ephemeral', "Graviy")
)
schemes <- tableGrob(schemes,
                     theme=ttheme_minimal(base_size =8.7,
                                          core=list(bg_params = list(fill = 'white', col=c('lightgrey')),
                                                    fg_params=list(fontface=3, fontsize=7.7, rot=0)),
                                          colhead=list(bg_params = list(fill = 'white', col=c('lightgrey'))),
                                          rowhead=list(bg_params = list(fill = 'white', col=c('lightgrey'))),
                                          padding = unit(c(1, 1), "mm")),
                     rows = NULL)
# grid.draw(schemes)
areas <- c("Study region", "Kisumu", "Tigray")
areas <- tableGrob(areas,
                   theme=ttheme_minimal(base_size =8.7,
                                        core=list(bg_params = list(fill = 'white', col=c('lightgrey')),
                                                  fg_params=list(fontface=3, fontsize=7.7, rot=0)),
                                        colhead=list(bg_params = list(fill = 'white', col=c('lightgrey'))),
                                        rowhead=list(bg_params = list(fill = 'white', col=c('lightgrey'))),
                                        padding = unit(c(1, 1), "mm")),
                   rows = NULL)

jn <- gtable_combine(areas, schemes)
jn$widths <- rep(max(jn$widths), length(jn$widths)) # make column widths equal

jn$layout[1:6 , c("t", "b")] <- list(c(1, 2, 7), c(1, 6, 9))
# jn$layout[2:9 , c("t", "b")] <- list(c(2, 5), c(4, 9))
# jn$layout[2:9 , c("t", "b")] <- list(c(2, 5), c(4, 9))
# grid.newpage()
# grid.draw(jn)
jn <- list(jn)
jn$top <- grid::textGrob("Characteristics of the sampled schemes (e)\n",
                               x=0.5, y=0.7, hjust=0.5, vjust = 1,
                               gp = gpar(fontfamily='serif',fontsize=15.5,fontface="bold.italic", col="blue"))

jn$bottom <- grid::textGrob("
Note: Agricultural practices are mainly homogeneous within study regions.The term 'out-growers' refers to farmers outside the scope of the Kenyan national irrigation
board. Traditional flood water diversions are physical infrastructure, such as deflecting spurs or soil bunds that are constructed by farmers across flood channels
using locally available materials. Modern diversion structures, such as diversion weirs, are usually designed by engineers and made of concrete. The improved diversion
type constitutes an integration of farmer and engineer knowledge.",
                         x = 0, y=0.6, hjust=0, vjust = 0.5,
                         gp = gpar(fontfamily='serif',fontsize=9,fontface="italic", col="black"))

jn <- do.call(grid.arrange, jn)


instt <- arrangeGrob(inst, jn, layout_matrix = rbind(
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2)
  
))
# ggsave(inst , filename = 'figures/Modelling_FBFS_study_area_2.png', width = 8.76, height = 5.84)
insttt <- gTree(children = gList(instt, grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                               gp=gpar(lwd=2, fill="NA", col="darkgray"))))

ggsave(insttt , filename = 'figures/Modelling_FBFS_study_area_3.png', width = 8.76, height = 8.75)

