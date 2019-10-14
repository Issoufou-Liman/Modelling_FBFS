library(ggsn)
library(magick)
library(rgdal)
library(raster)
library(gridExtra)
afr <- readOGR("data_files/shapefiles/Africa.shp")

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

inst00 <- ggplot()+
  geom_path(data=fortify(afr), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  
  geom_polygon(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), fill='red', size = 0.05, colour='red')+
  theme_minimal()+
  my_theme +
  theme(
    # axis.text = element_text(size = 8),
    axis.text = element_blank(),
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
  # labs(x='Long', y='Lat')+
  # labs(subtitle='')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=1, r=1, b=-1, l=-1))
###########################

inst0 <- ggplot()+
  geom_path(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.05, fill='red', colour='red')+

  geom_point(data = Tigray_localities[2, ], aes(x = Long, y = Lat))+
  geom_text(data = Tigray_localities[2, ], aes(x = Long, y = Lat, label = Place),
            position = position_nudge(y = -0.7),
            hjust=0.5, vjust=1, size=3)+
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
###########################

inst <- ggplot()+
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.25, fill='white', colour='black')+
  # geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.25, fill='white', colour='black')+
  # scalebar(location="bottomright", y.min=-0.2, y.max=0, 
  #                 x.min=34, x.max=35, dist=.1, dd2km = TRUE, model='WGS84',
  #                 st.dist=.04, transform=TRUE)+
  scalebar(data=fortify(Tigray), dist = 50, dist_unit = "km",
           transform = TRUE, model = "WGS84", anchor=c(x=38.5, y=12.25+0.2), 
           height=0.03, st.dist=0.05, st.size=2, border.size =0.25)+
  north(data=fortify(Tigray), scale = 0.2, anchor=c(x=37.25, y=14.85))+
  
  # geom_point(data = Tigray_localities[1, ], aes(x = Long, y = Lat))+
  # geom_text(data = Tigray_localities[1, ], aes(x = Long, y = Lat, label = ""),
  #           position = position_nudge(y = -0.425),
  #           hjust=0.5, vjust=1, size=10, alpha=1)+ # used only to make rooms for inset maps
  # geom_text(data = Tigray_localities[1, ], aes(x = Long, y = Lat, label = "Tigray region, Ethiopia"),
  #           position = position_nudge(y = 0.05),
  #           hjust=0.5, vjust=0, size=3)+
  theme_minimal()+
  my_theme +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12, face = 'bold'),
        strip.text = element_text(size = 8, face = "bold"),
        legend.text = element_text(family = 'serif', face = 'bold', size = 8),
        plot.background = element_rect(fill = 'lightgrey', colour = 'lightgrey'),
        plot.title = element_text(size = 14, face = 'bold', color = 'black'),
        plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  # labs(subtitle='Tigray, region, Ethiopia')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=0, b=0))

inst <- inst +
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

source('source_files/schemes.R')

inst <- inst+
  geom_point(data = data.frame(Tigray_localities), aes(x = coords.x1, y = coords.x2, colour = SchemeName))+
  geom_text(data = data.frame(Tigray_localities), aes(x = coords.x1, y = coords.x2, label = SchemeName_short),
            position = position_nudge(y = 0.05),
            hjust=0.5, vjust=0, size=3)
  
  

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
  top <- grobTree(rectGrob(gp=gpar(fill="bisque4", col="white"), height=1.25), 
                  grid::textGrob(top[i], hjust=0.5, vjust = 0.5,
                                 gp = gpar(fontfamily='serif',fontsize=13,fontface="bold", col="black")))
  arrangeGrob(top,out, 
              layout_matrix = rbind(
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
                rep(2,10)
                
              ))
})

# img_list$layout_matrix = matrix(c(NA, 1, 2, 3), ncol = 2)
img_list$ncol = 3
img_list <- do.call(grid.arrange, img_list)
img_list <- list(ggplotGrob(inst), img_list)
img_list$layout_matrix = rbind(
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2)
)
img_list$top <- grid::textGrob("Tigray region, Ethiopia",
                               x=0.04, hjust=0, vjust = 0.50,
                               gp = gpar(fontfamily='serif',fontsize=12,fontface="italic", col="blue"))
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

inst00 <- ggplot()+
  geom_path(data=fortify(afr), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  
  geom_polygon(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), fill='red', size = 0.05, colour='red')+
  
  # geom_point(data = Tigray_localities[2, ], aes(x = Long, y = Lat), alpha=0.5)+
  # geom_text(data = Tigray_localities[2, ], aes(x = Long, y = Lat, label = Place),
  #           position = position_nudge(y = -0.5),
  #           hjust=0.5, vjust=1, size=4, alpha=0.8)+
  theme_minimal()+
  #my_line_theme+
  my_theme +
  theme(
    # axis.text = element_text(size = 8),
    axis.text = element_blank(),
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
  # labs(x='Long', y='Lat')+
  # labs(subtitle='')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=1, r=1, b=-1, l=-1))
###########################

inst0 <- ggplot()+
  geom_path(data=fortify(Ethiopia_L0), aes(x=long, y= lat, group=group), size = 0.1, colour='black')+
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.05, fill='red', colour='red')+
  geom_point(data = Tigray_localities[2, ], aes(x = Long, y = Lat))+
  geom_text(data = Tigray_localities[2, ], aes(x = Long, y = Lat, label = Place),
            position = position_nudge(y = -0.7),
            hjust=0.5, vjust=1, size=3)+
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

inst <- ggplot()+
  geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.25, fill='white', colour='black')+
  # geom_polygon(data=fortify(Tigray), aes(x=long, y= lat, group=group), size = 0.25, fill='white', colour='black')+
  # scalebar(location="bottomright", y.min=-0.2, y.max=0, 
  #                 x.min=34, x.max=35, dist=.1, dd2km = TRUE, model='WGS84',
  #                 st.dist=.04, transform=TRUE)+
  scalebar(data=fortify(Tigray), dist = 20, dist_unit = "km",
           transform = TRUE, model = "WGS84", anchor=c(x=35.075, y=-0.55), 
           height=0.03, st.dist=0.05, st.size=2.5, border.size =0.25)+
  north(data=fortify(Tigray), scale = 0.32, anchor=c(x=35.44, y=0.03))+
  
  # geom_point(data = Tigray_localities[1, ], aes(x = Long, y = Lat))+
  geom_text(data = Tigray_localities[1, ], aes(x = Long, y = Lat, label = ""),
            position = position_nudge(y = -0.425),
            hjust=0.5, vjust=1, size=10, alpha=1)+ # used only to make rooms for inset maps
  # geom_text(data = Tigray_localities[1, ], aes(x = Long, y = Lat, label = "Tigray region, Ethiopia"),
  #           position = position_nudge(y = 0.05),
  #           hjust=0.5, vjust=0, size=3)+
  theme_minimal()+
  #my_line_theme+
  my_theme +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12, face = 'bold'),
        strip.text = element_text(size = 8, face = "bold"),
        legend.text = element_text(family = 'serif', face = 'bold', size = 8),
        plot.background = element_rect(fill = 'lightgrey', colour = 'lightgrey'),
        plot.title = element_text(size = 14, face = 'bold', color = 'black'),
        plot.subtitle = element_text(size = 12, face = 'italic', color = 'blue'))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  coord_equal()+
  # labs(x='Long', y='Lat')+
  # labs(subtitle='Kisumu, County, Kenya')+
  theme(axis.title = element_blank())+
  theme(plot.margin = margin(t=0, b=0))

inst <- inst +
  annotation_custom(
    grob = ggplotGrob(inst0+
                        theme(panel.grid = element_blank(),
                              plot.background = element_rect(fill = 'white'),
                              panel.border = element_blank()
                        )),
    xmin = 35.09,
    xmax = 35.35-0.01,
    ymin = -0.72,
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
    ymin = -0.73,
    ymax = -0.15
  )

source('source_files/schemes.R')

inst <- inst+
  geom_point(data = data.frame(Kisumu_localities), aes(x = coords.x1, y = coords.x2, colour = SchemeName))+
  geom_text(data = data.frame(Kisumu_localities), aes(x = coords.x1, y = coords.x2, label = SchemeName_short),
            position = position_nudge(y = -0.03),
            hjust=0.5, vjust=0, size=3)+
  theme(legend.spacing.x = unit(0, 'mm'))



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
  top <- grobTree(rectGrob(gp=gpar(fill="bisque4", col="white"), height=1.25), 
                  grid::textGrob(top[i], hjust=0.5, vjust = 0.5,
                                 gp = gpar(fontfamily='serif',fontsize=13,fontface="bold", col="black")))
  arrangeGrob(top,out, 
              layout_matrix = rbind(
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
                rep(2,10)
                
              ))
})

# img_list$layout_matrix = matrix(c(NA, 1, 2, 3), ncol = 2)
img_list$ncol = 3
img_list <- do.call(grid.arrange, img_list)
img_list <- list(ggplotGrob(inst), img_list)
img_list$layout_matrix = rbind(
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(1,1,1),
  c(2,2,2),
  c(2,2,2),
  c(2,2,2)
)
img_list$top <- grid::textGrob("Kisumu County, Kenya",
                               x=0.04, hjust=0, vjust = 0.50,
                               gp = gpar(fontfamily='serif',fontsize=12,fontface="italic", col="blue"))
inst_Kisumu <- do.call(grid.arrange, img_list)

inst_Kisumu <- gTree(children = gList(grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                                gp=gpar(lwd=1, fill="lightgray", col="white")), inst_Kisumu))




inst <- grid.arrange(inst_Kisumu, inst_Tigray, ncol=2)

inst <- gTree(children = gList(inst, grid.rect(.5,.5,width=unit(1,"npc"), height=unit(1,"npc"), 
                                               gp=gpar(lwd=2, fill="NA", col="darkgray"))))
# inst <- grid.arrange(inst)
# 
# inst <- grid.arrange(inst)
ggsave(inst , filename = 'figures/Modelling_FBFS_study_area_2.png', width = 8.76, height = 5.84)
