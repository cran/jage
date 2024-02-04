#' Age estimation from a single case, called by mvcp_est.f
#' @param case as input vector from object dat
#' @param prior as string argument for desired prior
#' @param drop as column to be dropped based on outcome of find_fuzzies.f
#' @return returns probs as a vector of posterior probabilities
#' @import mvtnorm stats
#' @export

mv.probit=function(case,prior,drop){
  numvars=length(sto)/2
  slopes=sto[(1+numvars):length(sto)]
  numvars1=length(sto1)/2
  slopes1=sto1[(1+numvars1):length(sto1)]
  S=sqrt(slopes1)
  S1=outer(S,S) * R1
  lo=c(thresh[1,case[[3]]],thresh[2,case[[4]]],thresh[3,case[[5]]],thresh[4,case[[6]]],thresh[5,case[[7]]],thresh[6,case[[8]]],thresh[7,case[[9]]],thresh[8,case[[10]]])
  hi=c(thresh[1,case[[3]]+1],thresh[2,case[[4]]+1],thresh[3,case[[5]]+1],thresh[4,case[[6]]+1],thresh[5,case[[7]]+1],thresh[6,case[[8]]+1],thresh[7,case[[9]]+1],thresh[8,case[[10]]+1])
  if(length(drop)>0){
    lo=lo[-drop]
    hi=hi[-drop]
    S1=S1[-drop,-drop]
    R1=R1[-drop,-drop]
    slopes=slopes[-drop]
  }
  if(prior=="unit"){
    get.denom=function(age,denom=1) {
      pmvnorm(log(age)*slopes,lower=lo,upper=hi,sigma=S1/(n.subj+1))[1]*pmvnorm(log(age)*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
    denom=integrate(Vectorize(get.denom),lower=2,upper=23,denom=1)$val
    age=seq(2,23,.01)
    N=length(age)
    probs=vector(length=length(age))
    for(i in 1:N){
      probs[i]=pmvnorm(log(age[i])*slopes,lower=lo,upper=hi,sigma=S1/(n.subj+1))[1]*pmvnorm(log(age[i])*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
  }
  if(prior=="cdc"){
    get.denom=function(age,denom=1) {
      kde_cdc(age)*pmvnorm(log(age)*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
    denom=integrate(Vectorize(get.denom),lower=2,upper=23,denom=1)$val
    age=seq(2,23,.01)
    N=length(age)
    probs=vector()
    for(i in 1:N){
      probs[i]=kde_cdc(age[i])*pmvnorm(log(age[i])*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
  }
  if(prior=="hom"){
    get.denom=function(age,denom=1) {
      kde_hom(age)*pmvnorm(log(age)*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
    denom=integrate(Vectorize(get.denom),lower=2,upper=23,denom=1)$val
    age=seq(2,23,.01)
    N=length(age)
    probs=vector()
    for(i in 1:N){
      probs[i]=kde_hom(age[i])*pmvnorm(log(age[i])*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
  }
  if(prior=="mp"){
    get.denom=function(age,denom=1) {
      kde_mp(age)*pmvnorm(log(age)*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
    denom=integrate(Vectorize(get.denom),lower=2,upper=23,denom=1)$val
    age=seq(2,23,.01)
    N=length(age)
    probs=vector()
    for(i in 1:N){
      probs[i]=kde_mp(age[i])*pmvnorm(log(age[i])*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
  }
  if(prior=="jeff"){
    get.denom=function(age,denom=1) {
      pmvnorm(log(age)*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
    denom=integrate(Vectorize(get.denom),lower=2,upper=23,denom=1)$val
    age=seq(2,23,.01)
    N=length(age)
    probs=vector()
    for(i in 1:N){
      probs[i]=pmvnorm(log(age[i])*slopes,lower=lo,upper=hi,corr=R1)[1]/denom
    }
  }
  #probs=as.data.table(t(probs))
  return(probs)
}
