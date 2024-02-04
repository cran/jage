#' A function for estimating age from 8 teeth via multivariate cumulative probit and a user-determined prior.
#' Call directly iff you do NOT want to correct for fuzzy posteriors. Call find_fuzzies.f otherwise.
#' @param dat as input data.table of rows of dental development scores, may have only 1 row
#' @param prior as string argument for desired prior
#' @return returns postm as a data.table of posterior probability distributions
#' @export

mvcp_est.f=function(prior,dat){
  drop=c()
  postm=apply(dat,1,function(x){
    mv.probit(x,prior=prior,drop=drop)})
  postm=t(postm)
  age=seq(2,23,.01)
  postm.save=postm
  return(postm)
}
