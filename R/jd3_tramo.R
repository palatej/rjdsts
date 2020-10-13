#' @include jd3_ts.R jd3_rslts.R
#' @checkmate
NULL

#' Title
#'
#' @param y 
#' @param order 
#' @param seasonal 
#' @param mean 
#' @param X 
#' @param X.td 
#' @param ao 
#' @param ls 
#' @param so 
#' @param tc 
#' @param cv 
#' @param ml 
#'
#' @return
#' @export
#'
#' @examples
tramooutliers<-function(y, order=c(0L,1L,1L), seasonal=c(0L,1L,1L), mean=F,
                      X=NULL, X.td=NULL, ao=T, ls=T, tc=F, so=F, cv=0, ml=F){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  if (! is.null(X.td)){
    sy<-start(y)
    td<-tradingdays(X.td, frequency(y), sy[1], sy[2], length(y))
    X<-cbind(X, td)
  }
  
  
  jtramo<-.jcall("demetra/tramoseats/r/TramoOutliersDetection", "Ldemetra/tramoseats/r/TramoOutliersDetection$Results;", "process", ts_r2jd(y), 
               as.integer(order), as.integer(seasonal), mean, matrix_r2jd(X),
               ao, ls, tc, so, cv, ml)
  model<-list(
    y=as.numeric(y),
    variables=proc_vector(jtramo, "variables"),
    X=proc_matrix(jtramo, "regressors"),
    b=proc_vector(jtramo, "b"),
    bcov=proc_matrix(jtramo, "bvar"),
    linearized=proc_vector(jtramo, "linearized")
  )
  
  ll0<-proc_likelihood(jtramo, "initiallikelihood.")
  ll1<-proc_likelihood(jtramo, "finallikelihood.")
  
  return(structure(list(
    model=model,
    likelihood=list(initial=ll0, final=ll1)),
    class="JDSTS"))
}
