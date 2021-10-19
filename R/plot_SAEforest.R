
#' @export

plot.SAEforest <- function(obj, num_features =2, col ="darkgreen", fill = "darkgreen", alpha=0.55,
                           include_type =TRUE, horizontal = TRUE, gg_specs = theme_minimal(),
                           lsize=1.5, lty= "solid", grid_row=2, out_list = FALSE, pdp_plot =TRUE){
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
    print(paste("The data contained",length(set_rm) ,"character or factor variables from which no pdp plots can be made."))
  }

  forest_imp <- as.data.frame(vi(mod_alt$MERFmodel$Forest))
  forest_imp <- forest_imp[order(forest_imp$Importance, decreasing = TRUE),]
  forest_imp <- forest_imp[!forest_imp[,"Variable"] %in% set_rm,]
  forest_imp <- na.omit(forest_imp[1:num_features,])

  pdp_curves <- lapply(forest_imp[,"Variable"], FUN = function(feature) {
    pd <- pdp::partial(obj$MERFmodel$Forest, pred.var = feature, train = obj$MERFmodel$data, plot=FALSE)
    colnames(pd)[2] <- "y"
    ggplot(data=pd, aes_string(y = "y", x = feature)) + geom_line(linetype = lty, color=col, size=lsize)+
      ggtitle(paste("Partial Dependence of",feature)) + gg_specs
  })

  grid.arrange(grobs = pdp_curves, nrow = grid_row)
}

  # Output for furhter adaptions
  if(out_list == TRUE){
    outlist <- list(vip = vip_plot, pdp = pdp_curves)
    return(outlist)
  }
}




