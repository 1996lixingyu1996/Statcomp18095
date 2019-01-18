#' @title chi-square distribution QQplot
#' @description This package is used for plot chis-square qqplot
#' @importFrom graphics abline plot
#' @importFrom stats qchisq
#' @param x the data
#' @param df the freedom of chi-square distribution
#' @return NULL
#' @examples
#' \dontrun{
#' a<-rchisq(100,3)
#' cs_qqplot1(a,3)
#' }
#' @export
cs_qqplot1<-function(x,df)#df:degrees of freedom
{
  a<-sort(x)
  n=length(x)
  b<-1:n
  b<-(b-0.5)/n
  theoretic_quan<-qchisq(b,df)
  plot(a,theoretic_quan)
  abline(a=0,b=1)
}
