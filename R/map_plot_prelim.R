map_plot <- function(object,
                     indicator = "all",
                     MSE = FALSE,
                     CV = FALSE,
                     map_obj = NULL,
                     map_dom_id = NULL,
                     map_tab = NULL,
                     color = c("white", "red4"),
                     scale_points = NULL,
                     guide = "colourbar",
                     return_data = FALSE
){
  if (is.null(map_obj)) {
    message("No Map Object has been provided. An artificial polygone is used for
            visualization")
    map_pseudo(object = object , indicator = indicator, panelplot = FALSE, MSE = MSE,
               CV = CV)
  } else if (class(map_obj) != "SpatialPolygonsDataFrame" ||
             attr(class(map_obj), "package") != "sp") {
    stop("map_obj is not of class SpatialPolygonsDataFrame from the sp package")
  } else {
    if (length(color) != 2 || !is.vector(color)) {
      stop("col needs to be a vector of length 2
           defining the starting, mid and upper color of the map-plot")
    }

    plot_real(object,
              indicator = indicator,
              MSE = MSE,
              CV = CV,
              map_obj = map_obj,
              map_dom_id = map_dom_id,
              map_tab = map_tab,
              col = color,
              scale_points = scale_points,
              return_data = return_data,
              guide = guide
    )
  }
}

map_pseudo <- function(object, indicator, panelplot, MSE, CV)
{
  x <- y <- id <- value <- NULL #avoid note due to usage in ggplot
  values <-  summarize_indicators(object = object, indicator = indicator,
                        MSE = MSE, CV = CV)$ind
  indicator <- colnames(values)[-1]

  tplot <- get_polygone(values = values)

  if (panelplot) {
    ggplot(tplot, aes(x = x, y = y)) + geom_polygon(aes(group=id, fill = value))
    + facet_wrap( ~ variable, ncol = ceiling(sqrt(length(unique(tplot$variable)))))
  } else {
    for (ind in indicator) {
      print(ggplot(tplot[tplot$variable == ind,], aes(x = x, y = y)) +
              ggtitle(paste0(ind)) + geom_polygon(aes(group = id, fill = value)) )
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
}

plot_real <- function(object,
                      indicator = "all",
                      MSE = FALSE,
                      CV = FALSE,
                      map_obj = NULL,
                      map_dom_id = NULL,
                      map_tab = NULL,
                      col = col,
                      scale_points = NULL,
                      return_data = FALSE,
                      guide = NULL
) {
  if (!is.null(map_obj) && is.null(map_dom_id)) {
    stop("No district ID for the map object is given")
  }
  long <- lat <- group <- NULL



  map_data <- summarize_indicators(object = object, indicator = indicator,
                         MSE = MSE, CV = CV)$ind

  if (!is.null(map_tab)) {
    map_data <- merge(x = map_data, y = map_tab,
                      by.x = "district", by.y = names(map_tab)[1])
    matcher <- match(map_obj@data[map_dom_id][,1], map_data[,names(map_tab)[2]])

    if (any(is.na(matcher))) {
      if (all(is.na(matcher))) {
        stop("districts of map_tab and Map object do not match. Check map_tab")
      } else {
        warnings("Not all districts of map_tab and Map object could be matched.
                 Check map_tab")
      }
    }
    map_data <- map_data[matcher,]
    map_data <- map_data[,!colnames(map_data) %in% c("district",
                                                     map_dom_id,
                                                     names(map_tab)), drop = F]
  } else {
    matcher <- match(map_obj@data[map_dom_id][,1], map_data[,"district"])

    if (any(is.na(matcher))) {
      if (all(is.na(matcher))) {
        stop("district of EMDI and Map object do not match. Try using map_tab")
      } else {
        warnings("Not all districts of EMDI and Map object could be matched.
                 Try using map_tab")
      }
    }
    map_data <- map_data[matcher, ]
  }

  map_obj@data[colnames(map_data)] <- map_data


  map_obj.fort <- fortify(map_obj, region = map_dom_id)
  map_obj.fort <- merge(map_obj.fort, map_obj@data,
                        by.x = "id", by.y = map_dom_id)

  indicator <- colnames(map_data)
  indicator <- indicator[!(indicator %in% "district")]
  for (ind in indicator) {
    map_obj.fort[ind][,1][!is.finite(map_obj.fort[ind][,1])] <- NA
    scale_point <- get_scale_points(map_obj.fort[ind][,1], ind, scale_points)
    print(ggplot(map_obj.fort, aes(long, lat, group = group,
                                   fill = map_obj.fort[ind][,1])) +
            geom_polygon(color = "azure3") + coord_equal() +
            labs(x = "", y = "", fill = ind) +
            ggtitle(gsub(pattern = "_",replacement = " ",x = ind)) +
            scale_fill_gradient(low = col[1], high = col[2],limits = scale_point,
                                guide = guide) +
            theme(axis.ticks = element_blank(), axis.text = element_blank(),
                  legend.title = element_blank())

    )
    if (!ind == tail(indicator,1)) {
      cat("Press [enter] to continue")
      line <- readline()
    }
  }
  if (return_data) {
    return(map_obj.fort)
  }
}

get_polygone <- function(values) {
  if (is.null(dim(values))) {
    values = as.data.frame(values)
  }
  n <- nrow(values)
  cols <- ceiling(sqrt(n))
  n <- cols^2

  values["id"] <- seq_len(nrow(values))

  poly <- data.frame(id = rep(seq_len(n), each = 4),
                     ordering = seq_len((n*4)),
                     x = c(0,1,1,0) + rep(0:(cols - 1), each = (cols * 4)),
                     y = rep(c(0,0,1,1) + rep(0:(cols - 1), each = 4), cols)
  )

  combo <- merge(poly, values, by = "id", all = TRUE, sort = FALSE)
  melt(combo[order(combo$ordering),], id.vars = c("id","x","y","ordering"))
}

get_scale_points <- function(y, ind, scale_points){
  result <- NULL
  if (!is.null(scale_points)) {
    if (class(scale_points) == "numeric" && length(scale_points) == 2) {
      result <- scale_points
    } else {
      splt <- strsplit(ind, "_\\s*(?=[^_]+$)", perl = TRUE)[[1]]
      indicator_name <- splt[1]
      if (length(splt) == 2) {
        measure <- splt[2]
      } else {
        measure <- "ind"
      }
      if (indicator_name %in% names(scale_points)) {
        pointset <- scale_points[[indicator_name]]
        try(result <- pointset[[measure]])
      }
      if (is.null(result) || length(result) != 2)
      {
        warning("scale_points is of no apropriate form, default values will
                 be used. See the descriptions and examples for details")
        result <- NULL
      }
    }
  }
  if (is.null(result)) {
    rg <- range(y, na.rm = TRUE)
    result <- rg
  }
  return(result)
}
