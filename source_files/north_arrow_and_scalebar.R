# blindly copied from ggsn and sf packages, all credits to these packages.

NA_crs_ = structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs")
#' @name st_bbox
#' @param x object of class \code{bbox}
#' @export
is.na.bbox = function(x) identical(x, NA_bbox_)

bb_wrap = function(bb) {
  stopifnot(is.numeric(bb) && length(bb) == 4)
  structure(as.double(bb), names = c("xmin", "ymin", "xmax", "ymax"), class = "bbox")
}

bbox.Set = function(obj, ...) {
  sel = vapply(obj, function(x) { length(x) && !all(is.na(x)) }, TRUE)
  if (! any(sel))
    NA_bbox_
  else
    bb_wrap(CPL_get_bbox(unclass(obj)[sel], 0))
}
bbox.Mtrx = function(obj, ...) {
  if (length(obj) == 0)
    NA_bbox_
  else
    bb_wrap(CPL_get_bbox(list(obj), 1)) # note the list()
}
bbox.MtrxSet = function(obj, ...) {
  if (length(obj) == 0)
    NA_bbox_
  else
    bb_wrap(CPL_get_bbox(obj, 1))
}
bbox.MtrxSetSet = function(obj, ...) {
  if (length(obj) == 0)
    NA_bbox_
  else
    bb_wrap(CPL_get_bbox(obj, 2))
}
bbox.MtrxSetSetSet = function(obj, ...) {
  if (length(obj) == 0)
    NA_bbox_
  else
    bb_wrap(CPL_get_bbox(obj, 3))
}

#' Return bounding of a simple feature or simple feature set
#'
#' Return bounding of a simple feature or simple feature set
#' @param obj object to compute the bounding box from
#' @param ... ignored
#' @export
#' @return a numeric vector of length four, with \code{xmin}, \code{ymin}, \code{xmax}
#' and \code{ymax} values; if \code{obj} is of class \code{sf}, \code{sfc}, \code{Spatial} or \code{Raster}, the object
#' returned has a class \code{bbox}, an attribute \code{crs} and a method to print the
#' bbox and an \code{st_crs} method to retrieve the coordinate reference system
#' corresponding to \code{obj} (and hence the bounding box). \link{st_as_sfc} has a
#' methods for \code{bbox} objects to generate a polygon around the four bounding box points.
#' @name st_bbox
#' @examples
#' a = st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_point(1:2)), crs = 4326)
#' st_bbox(a)
#' st_as_sfc(st_bbox(a))
st_bbox = function(obj, ...) UseMethod("st_bbox")

#' @export
#' @name st_bbox
st_bbox.POINT = function(obj, ...) bb_wrap(c(obj[1L], obj[2L], obj[1L], obj[2L]))
#' @export
#' @name st_bbox
st_bbox.MULTIPOINT = bbox.Mtrx
#' @export
#' @name st_bbox
st_bbox.LINESTRING = bbox.Mtrx
#' @export
#' @name st_bbox
st_bbox.POLYGON = bbox.MtrxSet
#' @export
#' @name st_bbox
st_bbox.MULTILINESTRING = bbox.MtrxSet
#' @export
#' @name st_bbox
st_bbox.MULTIPOLYGON = bbox.MtrxSetSet

bbox_list = function(obj, ...) {
  s = vapply(obj, st_bbox, c(0.,0.,0.,0.)) # dispatch on class
  if (length(s) == 0 || all(is.na(s[1L,])))
    NA_bbox_
  else
    bb_wrap(c(min(s[1L,], na.rm = TRUE), min(s[2L,], na.rm = TRUE),
              max(s[3L,], na.rm = TRUE), max(s[4L,], na.rm = TRUE)))
}

#' @name st_bbox
#' @export
st_bbox.GEOMETRYCOLLECTION = bbox_list

#' @name st_bbox
#' @export
st_bbox.MULTISURFACE = bbox_list

#' @name st_bbox
#' @export
st_bbox.MULTICURVE = bbox_list

#' @name st_bbox
#' @export
st_bbox.CURVEPOLYGON = bbox_list

#' @name st_bbox
#' @export
st_bbox.COMPOUNDCURVE = bbox_list

#' @name st_bbox
#' @export
st_bbox.POLYHEDRALSURFACE = bbox.MtrxSetSet

#' @name st_bbox
#' @export
st_bbox.TIN = bbox.MtrxSetSet

#' @name st_bbox
#' @export
st_bbox.TRIANGLE = bbox.MtrxSet

#' @name st_bbox
#' @export
st_bbox.CIRCULARSTRING = function(obj, ...) {
  # this is of course wrong:
  st_bbox(st_cast(obj, "LINESTRING"))
}

#' @export
print.bbox = function(x, ...) {
  x = structure(x, crs = NULL, class = NULL)
  print(set_units(x, attr(x, "units"), mode = "standard"))
}

compute_bbox = function(obj) {
  switch(class(obj)[1],
         sfc_POINT = bb_wrap(bbox.Set(obj)),
         sfc_MULTIPOINT = bb_wrap(bbox.MtrxSet(obj)),
         sfc_LINESTRING = bb_wrap(bbox.MtrxSet(obj)),
         sfc_POLYGON = bb_wrap(bbox.MtrxSetSet(obj)),
         sfc_MULTILINESTRING = bb_wrap(bbox.MtrxSetSet(obj)),
         sfc_MULTIPOLYGON = bb_wrap(bbox.MtrxSetSetSet(obj)),
         bbox_list(obj)
  )
}

#' @name st_bbox
#' @export
st_bbox.sfc = function(obj, ...) structure(attr(obj, "bbox"), crs = st_crs(obj))

#' @name st_bbox
#' @export
st_bbox.sf = function(obj, ...) st_bbox(st_geometry(obj))

#' @name st_bbox
#' @export
st_bbox.Spatial = function(obj, ...) {
  if (!requireNamespace("sp", quietly = TRUE))
    stop("package sp required, please install it first")
  bb = sp::bbox(obj)
  structure(bb_wrap(c(bb[1,1],bb[2,1],bb[1,2],bb[2,2])),
            crs = st_crs(sp::proj4string(obj)))
}

#' @name st_bbox
#' @export
st_bbox.Raster = function(obj, ...) {
  if (!requireNamespace("sp", quietly = TRUE))
    stop("package sp required, please install it first")
  if (!requireNamespace("raster", quietly = TRUE))
    stop("package raster required, please install it first")
  bb = sp::bbox(obj)
  structure(bb_wrap(c(bb[1,1],bb[2,1],bb[1,2],bb[2,2])),
            crs = st_crs(sp::proj4string(obj)))
}

#' @name st_bbox
#' @export
st_bbox.Extent = function(obj, ..., crs = NA_crs_) {
  if (!requireNamespace("raster", quietly = TRUE))
    stop("package raster required, please install it first")
  structure(bb_wrap(c(obj@xmin, obj@ymin, obj@xmax, obj@ymax)), crs = st_crs(crs))
}

#' @name st_bbox
#' @param crs object of class \code{crs}, or argument to \link{st_crs}, specifying the CRS of this bounding box.
#' @examples
#' st_bbox(c(xmin = 16.1, xmax = 16.6, ymax = 48.6, ymin = 47.9), crs = st_crs(4326))
#' @export
st_bbox.numeric = function(obj, ..., crs = NA_crs_) {
  structure(bb_wrap(obj[c("xmin", "ymin", "xmax", "ymax")]), crs = st_crs(crs))
}

#' @export
st_bbox.bbox = function(obj, ...) obj


#' @export
"$.bbox" = function(x, name) {
  switch(name,
         xrange =,
         xlim = x[c("xmin", "xmax")],
         yrange =,
         ylim = x[c("ymin", "ymax")],
         xmin = x["xmin"],
         ymin = x["ymin"],
         xmax = x["xmax"],
         ymax = x["ymax"],
         stop("unsupported name")
  )
}

#' @name st_bbox
#' @details \code{NA_bbox_} represents the missing value for a \code{bbox} object
#' @export
NA_bbox_ = structure(rep(NA_real_, 4),
                     names = c("xmin", "ymin", "xmax", "ymax"),
                     crs = NA_crs_,
                     class = "bbox")

##############################################

#' North symbol
#' @description Adds a north symbol to maps created with ggplot or ggmap.
#' @param data the same \code{\link{data.frame}} passed to \code{\link{ggplot}} to plot the map.
#' @param location string indicating the symbol's location in the plot. Possible options: "topright" (default), "bottomright", "bottomleft" and "topleft".
#' @param scale number between 0 and 1 to indicate the symbol size as a proportion of the map size (bounding box).
#' @param symbol number between 1 and 18 to choose a symbol (see \code{\link{northSymbols}}).
#' @param anchor named \code{\link{vector}} with coordinates to control the symbol position. For \code{location = "topright"}, \code{anchor} defines the coordinates of the symbol's topright corner and so forth. The x coordinate must be named as x and the y coordinate as y.
#' @param x.min if \code{data} is not defined, number with the minimum x coordinate. Useful for ggmap.
#' @param x.max if \code{data} is not defined, number with the maximum x coordinate. Useful for ggmap.
#' @param y.min if \code{data} is not defined, number with the minimum y coordinate. Useful for ggmap.
#' @param y.max if \code{data} is not defined, number with the maximum y coordinate. Useful for ggmap.
#' @details
#' North symbols are included in the plot with the \code{\link{annotation_custom}} function, which do not works when used together with an empty call to ggplot (see last example). When it is convenient to use an empty call to ggplot, use \code{\link{north2}} instead.
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' data(domestic_violence)
#' ggplot(domestic_violence, aes(fill = Scaled)) +
#'     geom_sf() +
#'     north(domestic_violence, location = "bottomright", symbol = 15) +
#'     scale_fill_continuous(low = "#fff7ec", high = "#7F0000")
#' }
north <- function(data = NULL, location = 'topright', scale = 0.1, symbol = 1, x.min, x.max, y.min, y.max, anchor = NULL) {
  if (is.null(data)) {
    if (is.null(x.min) | is.null(x.max) |
        is.null(y.min) | is.null(y.max) ) {
      stop('If data is not defined, x.min, x.max, y.min and y.max must be.')
    }
    data <- data.frame(long = c(x.min, x.max), lat = c(y.min, y.max))
  }
  if (any(class(data) %in% "sf")) {
    xmin <- sf::st_bbox(data)["xmin"]
    xmax <- sf::st_bbox(data)["xmax"]
    ymin <- sf::st_bbox(data)["ymin"]
    ymax <- sf::st_bbox(data)["ymax"]
    scale.x <- (xmax - xmin) * scale
    scale.y <- (ymax - ymin) * scale
  } else {
    xmin <- min(data$long)
    xmax <- max(data$long)
    ymin <- min(data$lat)
    ymax <- max(data$lat)
    scale.x <- (xmax - xmin) * scale
    scale.y <- (ymax - ymin) * scale
  }
  if (location == 'bottomleft') {
    if (is.null(anchor)) {
      x.min <- xmin
      y.min <- ymin
    } else {
      x.min <- anchor['x']
      y.min <- anchor['y']
    }
    x.max <- x.min + scale.x
    y.max <- y.min + scale.y
  }
  if (location == 'bottomright') {
    if (is.null(anchor)) {
      x.max <- xmax
      y.min <- ymin
    } else {
      x.max <- anchor['x']
      y.min <- anchor['y']
    }
    x.min <- x.max - scale.x
    y.max <- y.min + scale.y
  }
  if (location == 'topleft') {
    if (is.null(anchor)) {
      x.min <- xmin
      y.max <- ymax
    } else {
      x.min <- anchor['x']
      y.max <- anchor['y']
    }
    x.max <- x.min + scale.x
    y.min <- y.max - scale.y
  }
  if (location == 'topright') {
    if (is.null(anchor)) {
      x.max <- xmax
      y.max <- ymax
    } else {
      x.max <- anchor['x']
      y.max <- anchor['y']
    }
    x.min <- x.max - scale.x
    y.min <- y.max - scale.y
  }
  symbol <- sprintf("%02.f", symbol)
  # symbol <- png::readPNG(paste0(system.file('symbols', package = 'ggsn'),
  #                               '/', symbol, '.png'))
  symbol <- png::readPNG('data_files/ggmaps/symbols/01.png')
  symbol <- grid::rasterGrob(symbol, interpolate = TRUE)
  return(annotation_custom(symbol, xmin = x.min, xmax = x.max,
                           ymin = y.min, 
                           ymax = y.max))
}

################

#' scalebar
#' @description Adds a scale bar to maps created with ggplot or ggmap.
#' @param data the same \code{\link{data.frame}} passed to \code{\link{ggplot}} to plot the map. If the \code{class} of \code{data} is not \code{sf}, it must contain columns whose names begin with \code{long} and \code{lat}.
#' @param location string indicating the scale bar's location in the plot. Possible options: "topright" (default), "bottomright", "bottomleft" and "topleft".
#' @param dist distance to represent with each segment of the scale bar.
#' @param dist_unit unit of measurement for \code{dist}. Possbile values: "km" (kilometers) and "m" (meters), "nm" (nautical miles) and "mi" (statue miles).
#' @param transform If TRUE, it is assumed that coordinates are in decimal degrees. If FALSE, it assumed that they are in meters.
#' @param dd2km deprecated. Use transform instead.
#' @param model choice of ellipsoid model ("WGS84", "GRS80", "Airy", "International", "Clarke", or "GRS67") Used when transform is TRUE.
#' @param height number between 0 and 1 to indicate the scale bar's height, as a proportion of the y axis.
#' @param st.dist number between 0 and 1 to indicate the distance between the scale bar and the scale bar's text, as a proportion of the y axis.
#' @param st.bottom logical. If TRUE (default) the scale bar's text is displayed at the bottom of the scale bar, if FALSE, it is displayed at the top.
#' @param st.size number to indicate the scale bar's size. It is passed to \code{size} in \code{\link{annotate}} function.
#' @param st.color color of the scale bar's text. Default is black.
#' @param box.fill fill color of the box. If vector of two colors, the two boxes are filled with a different color. Defaults to black and white.
#' @param box.color color of the box's border. If vector of two colors, the borders of the two boxes are colored differently. Defaults to black.
#' @param border.size number to define the border size.
#' @param anchor named \code{\link{vector}} with coordinates to control the symbol's position. For \code{location = "topright"}, \code{anchor} defines the coordinates of the symbol's topright corner and so forth. The x coordinate must be named as x and the y coordinate as y.
#' @param x.min if \code{data} is not defined, number with the minimum x coordinate. Useful for ggmap.
#' @param x.max if \code{data} is not defined, number with the maximum x coordinate. Useful for ggmap.
#' @param y.min if \code{data} is not defined, number with the minimum y coordinate. Useful for ggmap.
#' @param y.max if \code{data} is not defined, number with the maximum y coordinate. Useful for ggmap.
#' @param facet.var if faceting, character vector of variable names used for faceting. This is useful for placing the scalebar in only one facet and must be used together with \code{facet.lev}.
#' @param facet.lev character vector with the name of one level for each variable in \code{facet.var}. The scale bar will be drawn only in the \code{facet.lev} facet.
#' @param st.inherit logical. Set as FALSE if scalebar has unexpected behavior in animations.
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' data(domestic_violence)
#'
#' # Map in geographic coordinates
#' ggplot(domestic_violence, aes(fill = Scaled)) +
#'     geom_sf() +
#'     scalebar(domestic_violence, dist = 4, dist_unit = "km",
#'              transform = TRUE, model = "WGS84") +
#'     blank() +
#'     scale_fill_continuous(low = "#fff7ec", high = "#7F0000")
#' 
#' # Map in projected coordinates
#' domestic_violence2 <- st_transform(domestic_violence, 31983)
#' ggplot(domestic_violence2, aes(fill = Scaled)) +
#'     geom_sf() +
#'     scalebar(domestic_violence2, dist = 4, dist_unit = "km",
#'              transform = FALSE, model = "WGS84") +
#'     blank() +
#'     scale_fill_continuous(low = "#fff7ec", high = "#7F0000")
#' }
scalebar <- function(data = NULL, location = "bottomright", dist = NULL, dist_unit = NULL, transform = NULL, dd2km = NULL, model = NULL, height = 0.02, st.dist = 0.02, st.bottom = TRUE, st.size = 5, st.color = "black", box.fill = c("black", "white"), box.color = "black", border.size = 1, x.min = NULL, x.max = NULL, y.min = NULL, y.max = NULL, anchor = NULL, facet.var = NULL, facet.lev = NULL, st.inherit = TRUE){
  if (is.null(data)) {
    if (is.null(x.min) | is.null(x.max) |
        is.null(y.min) | is.null(y.max) ) {
      stop('If data is not defined, x.min, x.max, y.min and y.max must be.')
    }
    data <- data.frame(long = c(x.min, x.max), lat = c(y.min, y.max))
  }
  if (is.null(transform)) {
    stop("transform should be logical.")
  }
  if (any(class(data) %in% "sf")) {
    xmin <- sf::st_bbox(data)["xmin"]
    xmax <- sf::st_bbox(data)["xmax"]
    ymin <- sf::st_bbox(data)["ymin"]
    ymax <- sf::st_bbox(data)["ymax"]
  } else {
    if (any(startsWith(colnames(data), "lat")) & any(startsWith(colnames(data), "long"))) {
      xmin <- min(data$long)
      xmax <- max(data$long)
      ymin <- min(data$lat)
      ymax <- max(data$lat)
    } else {
      stop("'", substitute(data), "' must have columns with names that start with 'lat' and 'long'")
    }
  }
  if (location == 'bottomleft') {
    if (is.null(anchor)) {
      x <- xmin
      y <- ymin
    } else {
      x <- as.numeric(anchor['x'])
      y <- as.numeric(anchor['y'])
    }
    direction <- 1
    
  }
  if (location == 'bottomright') {
    if (is.null(anchor)) {
      x <- xmax
      y <- ymin
    } else {
      x <- as.numeric(anchor['x'])
      y <- as.numeric(anchor['y'])
    }
    direction <- -1
    
  }
  if (location == 'topleft') {
    if (is.null(anchor)) {
      x <- xmin
      y <- ymax
    } else {
      x <- as.numeric(anchor['x'])
      y <- as.numeric(anchor['y'])
    }
    direction <- 1
    
  }
  if (location == 'topright') {
    if (is.null(anchor)) {
      x <- xmax
      y <- ymax
    } else {
      x <- as.numeric(anchor['x'])
      y <- as.numeric(anchor['y'])
    }
    direction <- -1
    
  }
  if (!st.bottom) {
    st.dist <-
      y + (ymax - ymin) * (height + st.dist)
  } else {
    st.dist <- y - (ymax - ymin) * st.dist
  }
  height <- y + (ymax - ymin) * height
  
  if (dist_unit == "m") {
    dist <- dist / 1e3
    dist_unit0 <- "m"
    dist_unit <- "km"
  }
  if (!is.null(dd2km)) {
    if (dd2km) {
      transform <- TRUE
    }
    cat("dd2km is deprecated. Use ggsn::transform instead.")
  }
  if (transform) {
    break1 <- maptools::gcDestination(lon = x, lat = y,
                                      bearing = 90 * direction,
                                      dist = dist, dist.units = dist_unit,
                                      model = model)[1, 1]
    break2 <- maptools::gcDestination(lon = x, lat = y,
                                      bearing = 90 * direction,
                                      dist = dist*2, dist.units = dist_unit,
                                      model = model)[1, 1]
  } else {
    if (location == 'bottomleft' | location == 'topleft') {
      if (exists("dist_unit0") | (!exists("dist_unit0") & dist_unit == "km")) {
        break1 <- x + dist * 1e3
        break2 <- x + dist * 2e3
      } else if (dist_unit == "nm") {
        break1 <- x + dist * 1852
        break2 <- x + dist * 1852 * 2
      } else if (dist_unit == "mi") {
        break1 <- x + dist * 1609.34
        break2 <- x + dist * 1609.34 * 2
      } else {
        break1 <- x + dist
        break2 <- x + dist
      }
    } else {
      if (exists("dist_unit0") | (!exists("dist_unit0") & dist_unit == "km")) {
        break1 <- x - dist * 1e3
        break2 <- x - dist * 2e3
      } else if (dist_unit == "nm") {
        break1 <- x - dist * 1852
        break2 <- x - dist * 1852 * 2
      } else if (dist_unit == "mi") {
        break1 <- x - dist * 1609.34
        break2 <- x - dist * 1609.34 * 2
      } else {
        break1 <- x - dist
        break2 <- x - dist
      }
    }
    
  }
  
  out_of_range <- function(low, n, high) {
    n < low | n > high 
  }
  
  if (out_of_range(xmin, break1, xmax) | out_of_range(xmin, break2, xmax)) {
    stop("The requested scalebar distance (", 
         substitute(dist), " ", substitute(dist_unit), 
         ") is too large to fit on the map.\n  Try reducing it.")
  }
  
  
  box1 <- data.frame(x = c(x, x, rep(break1, 2), x),
                     y = c(y, height, height, y, y), group = 1)
  box2 <- data.frame(x = c(rep(break1, 2), rep(break2, 2), break1),
                     y=c(y, rep(height, 2), y, y), group = 1)
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    for (i in 1:length(facet.var)){
      if (any(class(data) == "sf")) {
        if (!is.factor(data[ , facet.var[i]][[1]])) {
          data[ , facet.var[i]] <- factor(data[ , facet.var[i]][[1]])
        }
        box1[ , facet.var[i]] <- factor(facet.lev[i],
                                        levels(data[ , facet.var[i]][[1]]))
        box2[ , facet.var[i]] <- factor(facet.lev[i],
                                        levels(data[ , facet.var[i]][[1]]))
      } else {
        if (!is.factor(data[ , facet.var[i]])) {
          data[ , facet.var[i]] <- factor(data[ , facet.var[i]])
        }
        box1[ , facet.var[i]] <- factor(facet.lev[i],
                                        levels(data[ , facet.var[i]]))
        box2[ , facet.var[i]] <- factor(facet.lev[i],
                                        levels(data[ , facet.var[i]]))
      }
      
    }
  }
  if (exists("dist_unit0")) {
    legend <- cbind(text = c(0, dist * 1e3, dist * 2e3), row.names = NULL)
  } else {
    legend <- cbind(text = c(0, dist, dist * 2), row.names = NULL)
  }
  gg.box1 <- geom_polygon(data = box1, aes(x, y),
                          fill = utils::tail(box.fill, 1),
                          color = utils::tail(box.color, 1),
                          size = border.size)
  gg.box2 <- geom_polygon(data = box2, aes(x, y), fill = box.fill[1],
                          color = box.color[1],
                          size = border.size)
  x.st.pos <- c(box1[c(1, 3), 1], box2[3, 1])
  if (location == 'bottomright' | location == 'topright') {
    x.st.pos <- rev(x.st.pos)
  }
  label <- NULL
  if (exists("dist_unit0")) {
    legend2 <- cbind(data[1:3, ], x = unname(x.st.pos), y = unname(st.dist),
                     label = paste0(legend[, "text"], c("", "", "m")))
  } else {
    legend2 <- cbind(data[1:3, ], x = unname(x.st.pos), y = unname(st.dist),
                     label = paste0(legend[, "text"], c("", "", dist_unit)))
  }
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    for (i in 1:length(facet.var)){
      if (any(class(data) == "sf")) {
        legend2[ , facet.var[i]] <- factor(facet.lev[i],
                                           levels(data[ , facet.var[i]][[1]]))
      } else {
        legend2[ , facet.var[i]] <- factor(facet.lev[i],
                                           levels(data[ , facet.var[i]]))
      }
    }
  } else if (!is.null(facet.var) & is.null(facet.lev)) {
    facet.levels0 <- unique(as.data.frame(data)[, facet.var])
    facet.levels <- unlist(unique(as.data.frame(data)[, facet.var]))
    legend2 <- do.call("rbind", replicate(length(facet.levels),
                                          legend2, simplify = FALSE))
    if (length(facet.var) > 1) {
      facet.levels0 <- expand.grid(facet.levels0)
      legend2[, facet.var] <-
        facet.levels0[rep(row.names(facet.levels0), each = 3), ]
    } else {
      legend2[, facet.var] <- rep(facet.levels0, each = 3)
    }
  }
  if (!st.inherit) {
    legend2 <- legend2[, c("x", "y", "label")]
  }
  gg.legend <- geom_text(data = legend2, aes(x, y, label = label),
                         size = st.size, color = st.color,
                         inherit.aes = st.inherit)
  return(list(gg.box1, gg.box2, gg.legend))
}