#' A function for collapsing Moorrees et al. (1963) dental development stages for use in find_fuzzies.f and mvcp_est.f
#'
#' WARNING: this function is based on scoring with an additional crypt stage. Pre-collapse staging: 0-no crypt, 1-crypt, 2-Ci, 3-Cco, 4-Coc, 5-Cr1/2, 6-Cr3/4, 7-Crc, 8-Ri, 9-Cli, 10-R1/4, 11-R1/2, 12-R3/4, 13-Rc, 14-A1/2, 15-Ac
#' @param cases as input data.table of Moorrees et al. dental development scores, or vector of scores from single individual
#' @return returns data.table of collapsed scores
#' @import data.table
#' @export
#' @examples
#' mfh_collapse(c(NA,NA,15,15,14,12,11,15,11,8))

mfh_collapse=function(cases){
  old <- new <- t31 <- t32 <- t33 <- t34 <- t35 <- t36 <- t37 <- t38 <- NULL
  if(class(cases)[1]=="numeric"){
    dat=data.table(t(cases))
  } else if(class(cases)[1]=="data.table"){dat=cases
  } else {stop("cases is not a numeric vector or data.table")
  }
  colnames(dat)=c("drn","age","t31","t32","t33","t34","t35","t36","t37","t38")

  replacements31=data.table(old=c(6,7,8,10,11,12,13,14,15),new=c(1,2,3,4,5,6,7,8,9))
  replacements32=data.table(old=c(5,6,7,8,10,11,12,13,14,15),new=c(1,2,3,4,5,6,7,8,9,10))
  replacements33=data.table(old=c(4,5,6,7,8,10,11,12,13,14,15),new=c(1,2,3,4,5,6,7,8,9,10,11))
  replacements34=data.table(old=c(1,2,3,4,5,6,7,8,10,11,12,13,14,15),new=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
  replacements35=data.table(old=c(0,1,2,3,4,5,6,7,8,10,11,12,13,14,15),new=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
  replacements36=data.table(old=c(5,6,7,8,9,10,11,12,13,14,15),new=c(1,2,3,4,4,5,6,7,8,9,10))
  replacements37=data.table(old=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),new=c(1,2,3,4,5,6,7,8,9,9,10,11,12,13,14,15))
  replacements38=data.table(old=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),new=c(1,2,3,4,5,6,7,8,9,9,9,10,11,11,12,13))

  dat[replacements31, t31 := new, on=list(t31 = old)]
  dat[replacements32, t32 := new, on=list(t32 = old)]
  dat[replacements33, t33 := new, on=list(t33 = old)]
  dat[replacements34, t34 := new, on=list(t34 = old)]
  dat[replacements35, t35 := new, on=list(t35 = old)]
  dat[replacements36, t36 := new, on=list(t36 = old)]
  dat[replacements37, t37 := new, on=list(t37 = old)]
  dat[replacements38, t38 := new, on=list(t38 = old)]

  return(dat)
}
