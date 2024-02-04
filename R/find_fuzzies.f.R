#' A function for finding and correcting fuzzy posteriors produced by mvcp_est.f
#' @param cases as input data.table of rows of collapsed dental development scores, or vector of scores from single individual
#' @param prior as string argument for desired prior
#' @return for one case, list object with interpretation and data.table of true age, lower and upper bounds of HDR, mode, and posterior. If multiple cases are entered, only the data.table is returned.
#' @import pracma stats data.table
#' @export
#' @examples
#' \donttest{find_fuzzies.f(c(NA,NA,9,10,11,14,15,10,15,11),prior="jeff")}

find_fuzzies.f=function(cases,prior){
  if(class(cases)[1]=="numeric"){
    dat=data.table(t(cases))
  } else if(class(cases)[1]=="data.table"){dat=cases
  } else {stop("cases is not a numeric vector or data.table")
  }
  colnames(dat)=c("drn","age","t31","t32","t33","t34","t35","t36","t37","t38")
  postm=mvcp_est.f(prior,dat)
  find.rough=function (dt,thold) {
    lag.one = function (x) cor(x[-length(x)],x[-1])

    n=NROW(dt)
    is.rough=vector()
    for(i in 1:n) is.rough[i]=lag.one(dt[i,])
    #plot(1:n,is.rough,ylab='Lag one autocorrelation',xlab='Case',type='l')
    bad=which(is.rough<thold)
    bad=data.frame(bad,is.rough[bad])
    colnames(bad)=c('case','ac lag one')
    return(bad)
  }
  bad=find.rough(postm,0.99)
  alsobad=cbind(which(complete.cases(postm)==FALSE),rep(NA,length(which(complete.cases(postm)==FALSE))))
  colnames(alsobad)<-colnames(bad)
  bad=rbind(bad,alsobad)
  drop.choose=function(case){
    postcheck=rbindlist(list(lapply(as.list(1:8),mv.probit,case=case,prior=prior)))
    postcheck=t(postcheck)
    mybad=find.rough(as.matrix(postcheck),0.9999999)
    idx_ok=apply(postcheck,1,max)>0
    postout=vector()
    postout=postcheck[mybad[which.max(mybad$`ac lag one`),1],]
    #print(list(mybad[which.max(mybad[,2]),1],mybad,idx_ok))
    return(postout)
  }
  if(nrow(bad)>0){
    warning(c(nrow(bad)," case(s) required posterior corrections"))
    postm.adj=t(apply(dat[bad$case,],1,drop.choose))
    #postm.adj=t(rbindlist(list(postm.adj)))
    for(i in 1:nrow(bad)){
      postm[bad$case[i],]<-postm.adj[i,]
    }
  }
  age=seq(2,23,0.01)
  maxpage3=rep(0,nrow(postm))
  for(i in 1:nrow(postm)){
    maxpage3[i]=age[which.max(postm[i,])]
  }
  #plot(nmdid.val[1:20,age],maxpage3)

  # get prediciton intervals
  edges=function(target,alpha=0.05,inc=0.001){
    areas=matrix(rep(0,3/inc),nrow=1/inc,ncol=3)
    peak=which.max(target)
    upslope=target[c(1:peak)]
    downslope=target[c(peak:length(target))]
    for(i in seq(1,1/inc,1)){
      areas[i,2]=which.min(abs(upslope-(inc*i)))
      areas[i,3]=which.min(abs(downslope-(inc*i)))+length(upslope)-1
      lower=trapz(age[c(1:areas[i,2])],target[c(1:areas[i,2])])
      upper=trapz(age[c(areas[i,3]:length(age))],target[c(areas[i,3]:length(age))])
      areas[i,1]=lower+upper
    }
    #plot(seq(1,1/inc,1),areas[,1],type='l',ylim=c(0,0.2))
    index=which(areas[,1]<0.05)
    return(areas[index[length(index)],])
  }
  hdrs2=matrix(rep(0,2*nrow(postm)),nrow=nrow(postm),ncol=2)
  for(i in 1:nrow(postm)){
    hdr2=edges(postm[i,])
    hdrs2[i,]=c(age[hdr2[2]],age[hdr2[3]])
  }
  out.est=as.data.table(cbind(hdrs2,maxpage3,postm))

  if(nrow(out.est)==1){
    lw=as.numeric(hdrs2[1])
    up=as.numeric(hdrs2[2])
    pe=as.numeric(maxpage3[1])
    interpretation=as.character(c("There is a 95% probability that the individual's age is between ", lw, " and ", up, "years. The point estimate of age is ", pe, "years."))
    post=out.est
    out.est=list(interpretation,post)
    }
  return(out.est)
}
