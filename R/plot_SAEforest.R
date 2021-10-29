#' Plot function for SAEforest Object
#'
#' Write some scientific information here
#'
#' @param obj An object of type "SAEforest" representing point and MSE estimates.
#' @param num_features Number of features you want to see in plots
#' @param col Color of plots
#' @param fill Fill color of plots
#' @param alpha Density of plots
#' @param include_type Include the importance type in the vip plot. Default is TRUE
#' @param horizontal Plot the vip horizontally. Default is TRUE
#' @param gg_specs Additional \pkg{ggplot2}-options that one can add by + such as the theme
#' @param lsize The line size for the pdp-plots
#' @param lty The line type for the pdp plots
#' @param grid_row How many rows the pdp plots should display. Default is set to 2
#' @param out_list Whether the user wants the plots as additioanl list object to perform further
#' modifications via \pkg{ggplot2} modifications. Default is FALSE
#' @param pdp_plot Whether the user wants to see a pdp-plot for selected features.
#'
#' @return Plots of objects
#' @export
#'
#' @details Some scientific or function specific details. What are pdp and vip plots?
#' How can one modify them best with ggplot?
#' @seealso \code{\link{SAEforest}}

plot.SAEforest <- function(obj, num_features =2, col ="darkgreen", fill = "darkgreen", alpha=0.55,
                           include_type =TRUE, horizontal = TRUE, gg_specs = theme_minimal(),
                           lsize=1.5, lty= "solid", grid_row=2, out_list = FALSE, pdp_plot =TRUE){

  class_error(obj)

  input_checks_plot(num_features = num_features, alpha = alpha, include_type = include_type, horizontal =horizontal,
                                lsize = lsize, grid_row = grid_row, out_list = out_list, pdp_plot=pdp_plot)

  # VIP PLOT
  vip_plot <- vip::vip(obj$MERFmodel$Forest, aes = list(col =col, fill = fill, alpha=alpha),
                  include_type =include_type, horizontal = horizontal, num_features=num_features)+ ggtitle("Variable Importance")+ gg_specs

  print(vip_plot)

  # PdP PLOT
  # Check if variables are factors or characters
  pdp_curves <- NULL

  if(pdp_plot == TRUE){
  set_fact <- names(obj$MERFmodel$data)[sapply(obj$MERFmodel$data,is.factor)]
  set_char <- names(obj$MERFmodel$data)[sapply(obj$MERFmodel$data,is.character)]

  set_rm <- levels(factor(c(set_fact, set_char)))

  if(length(set_rm) !=0){
    print(paste0("The data contained ", length(set_rm) ," character or factor variables unsuitable for pdp plots(",paste(set_rm, collapse=", ") ,")."))
  }

  forest_imp <- as.data.frame(vip::vi(mod_alt$MERFmodel$Forest))
  forest_imp <- forest_imp[order(forest_imp$Importance, decreasing = TRUE),]
  forest_imp <- forest_imp[!forest_imp[,"Variable"] %in% set_rm,]
  forest_imp <- na.omit(forest_imp[1:num_features,])

  pdp_curves <- lapply(forest_imp[,"Variable"], FUN = function(feature) {
    pd <- pdp::partial(obj$MERFmodel$Forest, pred.var = feature, train = obj$MERFmodel$data, plot=FALSE)
    colnames(pd)[2] <- "y"
    ggplot(data=pd, aes_string(y = "y", x = feature)) + geom_line(linetype = lty, color=col, size=lsize)+
      ggtitle(paste("Partial Dependence of",feature)) + gg_specs
  })

  vip::grid.arrange(grobs = pdp_curves, nrow = grid_row)
}

  # Output for furhter adaptions
  if(out_list == TRUE){
    outlist <- list(vip = vip_plot, pdp = pdp_curves)
    return(outlist)
  }
}




