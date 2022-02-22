#' @export
EH_RemoveOutliersBasedOnQuartile <- function(dframe, col)
{
  Q <- quantile(col, probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(col)
  NewDataFrame<- subset(dframe, col> (Q[1] - 1.5*iqr) & col < (Q[2]+1.5*iqr))
  print("Outliers Removed.")
  return(NewDataFrame)
}
