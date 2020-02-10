bnlearn::nodes(net) <- abbr_node_names[c(3, 1, 2, 4, 5, 6, 8, 7)]
abbr_node_names <- abbr_node_names[c(3, 1, 2, 4, 5, 6, 8, 7)]

legende <- mapply(paste, abbr_node_names, sprintf('\u2192'), legende)
legende <- gsub("_", " ",  legende)
legende <- gsub(" at initial stage", "",  legende)

leg_breaks <- list(1:3, 4:5, 6:length(legende))
legende <- lapply(leg_breaks, function(i) legende[i])
legend_pos <- c('topleft', "bottomleft", 'bottomright')
legend_title <- c("Legend (1 of 3)", "Legend (2 of 3)", "Legend (3 of 3)")

inset <- list(c(0.04, 0.08), c(0.04, 0.08), c(0.04, 0.08))
text_width_ext <- c(0.85, 0.85, 0.85)

png(export_fun(export = "Modelling_FBFS_Avail_soil_water"),
    res = min_plots_res,
    units = 'in',
    # compression = plots_compression,
    width = max_plots_width_in, 
    height = max_plots_height_in/2.75,
    pointsize = 10.5)

par(font = plots_font, family = plots_font_family, lwd=plots_lwd)

graphviz_chart_bn (x = net, type = "barprob", layout = "dot", draw.levels = TRUE,abbreviate=FALSE,
                   grid = TRUE, scale = c(max_plots_height_in/2, max_plots_width_in), col = "black", bg = "transparent",
                   text.col = "black", bar.col = "green", strip.bg = "lightyellow")

lapply(1:length(legende), function(i) {
  legend(legend_pos[i], legend = legende[[i]], text.width = text_width_ext[i]*strwidth(legende[[i]][which.max(nchar(legende[[i]]))]), 
         cex=text_width_ext[i], ncol = 1,
         bty="o", box.lwd=1, box.col='lightyellow', xjust=1, yjust=1, bg=legend_bg,
         title = legend_title[i], title.col = 'blue', inset=inset[[i]])
})

box(col = 'lightgray')
dev.off()