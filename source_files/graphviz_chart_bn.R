#' @example see below the function
#' @details
#' @param target_node_plot_spar smoothing degree (if smooth = TRUE)
#' @param target_node_plot_smooth Logical should the curves of the target node states be smoothed?
#' @param target_node_plot_bg target node background when plotting
#' @param leg_title.col legend title color
#' @param leg_box.col legend box color
#' @param leg_bg Legend background
#' @param leg_cex, expansion factor for legend text
#' @param leg_ncol nomber of column when plotting the legend
#' @param leg_pos legend position (if abbrebiate = TRUE).
#' @param leg_w legend width (if abbrebiate = TRUE).
#' @param abbreviate Logical should nodes' names be abbreviated. could be useful for long node names when plotting
#' @param op option for bnlearn::cpdist (), must be "proba".
#' @param target_states_cols Colors to use for differenciating the states of the node of interst during plotting.
#' @param target_node the node of interest for which to sample and plot porterior probabilities of all possible queries
#' @param plt see par (): A vector of the form c(x1, x2, y1, y2) giving the coordinates of the plot region as fractions of the current figure region.
#' @param sub see bnlearn::graphviz.chart: a character string, a subtitle which is plotted at the bottom of the graph.
#' @param main see bnlearn::graphviz.chart: a character string, the main title of the graph. It's plotted at the top of the graph.
#' @param col @param bg, @param text.col, @param bar.col, @param strip.bg see bnlearn::graphviz.chart:
#' @param scale see bnlearn::graphviz.chart: a vector of two positive numbers, used by Rgraphviz to determine the size and the aspect ratio of the nodes
#' @param grid see bnlearn::graphviz.chart: a boolean value, whether to draw to a reference grid for the probability distributions. If grid is TRUE, a vertical grid is drawn at probabilities c(0, 0.25, 0.50, 0.75). If grid is a numeric vector, a verical grid is drawn at the specified probabilities.
#' @param draw.levels see bnlearn::graphviz.chart: a boolean value, whether to print the labels of the levels of each variable.
#' @param layout see bnlearn::graphviz.chart: a character string, the layout argument that will be passed to Rgraphviz. Possible values are dots, neato, twopi, circo and fdp. See Rgraphviz documentation for details.
#' @param type see bnlearn::graphviz.chart: a character string, the type of graph used to plot the probability distributions in the nodes. Possible values are barchart, dotplot and barprob (a brachart with probabilities printed over the bars).
#' @param x an object of class bn.fit.
graphviz_chart_bn <- function(x, type = "barchart", layout = "dot", draw.levels = TRUE,
                              grid = FALSE, scale = c(0.75, 1.1), col = "black", bg = "transparent",
                              text.col = "black", bar.col = "black", strip.bg = bg, main = NULL,
                              sub = NULL, plt= c(0.7, 0.95, 0, 0.95-0.7), # target_node, 
                              target_states_cols, 
                              op = "proba", abbreviate=FALSE, leg_w=strwidth("1,000,000"),
                              leg_pos="bottomleft", leg_ncol=2, leg_cex=0.75, leg_bg="lightgrey", 
                              leg_box.col="white", leg_title.col = "white", target_node_plot_bg = "lightgray", 
                              target_node_plot_smooth = TRUE, target_node_plot_spar=0.2, 
                              target_node_title=NULL, target_node_leg_pos="topright", target_node_leg_title=NULL,
                              target_node_title_cex=0.25, target_node_leg_cex=0.25,
                              target_node_plot_mgp=c(3,1,0)){
  # yy <- eval(as.expression(substitute(sample_cpdist(x, target_node, op))))[[2]]
  nam0 <- bnlearn::nodes(x)
  nam1 <- gsub("_"," ", nam0)
  if (abbreviate){
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    from_first_lo <- function(x) {
      substring(x, 2) <- tolower(substring(x, 2))
      x
    }
    # target_node <- firstup(from_first_lo(target_node))
    nam1 <- firstup(from_first_lo(nam1))
    leg <- nam1
    nam1 <- abbreviate(nam1)
    leg <- mapply(paste, nam1, leg, MoreArgs=list(sep = " = "))
    bnlearn::nodes(x) <- nam1
  }
  # if (split_labels_accross_lines){
  #   split_labels_accross_lines <- function(x) sub(" ","\n",x,fixed=TRUE)
  #   nam1 <- split_labels_accross_lines(nam1)
  # }
  bnlearn::graphviz.chart(x, type = type, layout = layout, draw.levels = draw.levels,
                 grid = grid, scale = scale, col = col, bg = bg,
                 text.col = text.col, bar.col = bar.col, strip.bg = strip.bg, main = main,
                 sub = sub)
  bnlearn::graphviz.chart(x, type = type, layout = layout, draw.levels = draw.levels,
                 grid = grid, scale = scale, col = col, bg = bg,
                 text.col = text.col, bar.col = bar.col, strip.bg = strip.bg, main = main,
                 sub = sub)
  if(abbreviate) {
    legend(leg_pos, legend = leg, text.width = leg_w, cex=leg_cex, ncol = leg_ncol,
           bty="o", box.lwd=1, box.col=leg_box.col, xjust=1, yjust=1, bg=leg_bg,
           title = "Legend", title.col = leg_title.col)
  }
  
  # if(is.null(target_node_leg_title)){
  #   target_node_leg_title <- gsub(target_node,pattern = "_", replacement = " ")
  # }
  # par("plt" = plt, bg = "lightgray")
  # par(new = TRUE)
  # plot_target_states (yy, cols = target_states_cols, bg = target_node_plot_bg, 
  #                     smooth = target_node_plot_smooth, spar=target_node_plot_spar,
  #                     main=target_node_title, leg_title = target_node_leg_title, 
  #                     cex.main = target_node_title_cex, leg_pos = target_node_leg_pos,
  #                     leg_cex=target_node_leg_cex, mgp = target_node_plot_mgp)
}
