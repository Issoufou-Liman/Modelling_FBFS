source('source_files/schemes.R')

center <- as(extent(Kisumu_localities), 'SpatialPolygons')
center <- geosphere::centroid(center)

inst <- ggmap(
  get_googlemap(
    center=as.numeric(center), 
    zoom=11, 
    maptype='terrain', 
    scale = 2,
    style = 'feature:all|element:labels|visibility:off'),
  extent='normal', 
  darken = 0)

saveRDS(inst, 'data_files/ggmaps/Kisumu_ggmap.rds')

## Tigray ####

center <- as(extent(Tigray_localities), 'SpatialPolygons')
center <- geosphere::centroid(center)

inst <- ggmap(
  get_googlemap(
    center=as.numeric(center), 
    zoom=10, 
    maptype='terrain', 
    scale = 2,
    style = 'feature:all|element:labels|visibility:off'),
  extent='normal', 
  darken = 0)

saveRDS(inst, 'data_files/ggmaps/Tigray_ggmap.rds')
