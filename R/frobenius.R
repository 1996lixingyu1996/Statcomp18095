#' @title frobenius norm
#' @description This package is used for calculate frobenius norm
#' @param x the matrix
#' @return  the norm of the matrix x
#' @examples
#' \dontrun{
#' robenius_norm(x)
#' }
#' @export
robenius_norm<-function(x)
{
  trace1=sum(diag(x%*%t(x)));
  return(sqrt(trace1));
}
