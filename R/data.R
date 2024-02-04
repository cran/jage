#' test data from NMDID
#'
#' Collapsed London Atlas scores of dental development from NMDID images by Stull.
#' Do not apply mfh_collapse before using. Stages are already collapsed!
#'
#' @format ## `nmdid.test`
#' A data.table with 188 rows and 10 columns:
#' \describe{
#'   \item{drn}{Decedent record number from NMDID}
#'   \item{age}{age in decimal years}
#'   \item{t31, t32, t33, t34, t35, t36, t37, t38}{Collapsed London atlas score of left permanent mandibular teeth I1-M3}
#' }
"nmdid.test"
