#' Request current and historical USDA-AMS MPR data
#'
#' This is the primary function in the \code{usdaams} package to request market information from the United States
#' Department of Agriculture Market News \url{https://mymarketnews.ams.usda.gov/}.
#' This function is built on the MyMarketNews API provided by USDA-AMS.
#'
#' The \code{mpr_request} function  provides flexible ways to request data. Specifically, users can download data from a single report or
#' multiple reports. Users can also specify slug IDs or the legacy slug IDs to request data.
#'
#'
#'
#'
#' @param slugIDs Valid slug IDs. Should be a 4-digit number, either a numerical value or a character.
#'                Users can provide can either one slug ID or multiple slug IDs. See details.
#' @param slugIDs_legacy Valid legacy slug IDs. Examples: LM_XB401, LM_XB403.
#'
#' @return
#' The function returns a list with the requested data.'
#'
#' @export


ams_request <- function(slugIDs = NULL, slugIDs_legacy = NULL){
  if(is.null(slugIDs) & is.null(slugIDs_legacy))
    stop('slugIDs or slugIDs_legacy must be provided.')

  if(!is.null(slugIDs) & !is.null(slugIDs_legacy))
    stop("Please provide slugIDs or slugIDs_legacy, not both.")

  if(!is.null(slugIDs)){
    if(length(slugIDs) == 1){
      out <- ams_request_single(slugID = slugIDs)
    }else{
      out <- lapply(slugIDs, function(i) ams_request_single(slugID = i))
      names(out) <- slugIDs
    }
  }else{
    if(length(slugIDs_legacy) == 1){
      out <- ams_request_single(slugID_legacy = slugIDs_legacy)
    }else{
      out <- lapply(slugIDs_legacy, function(i) ams_request_single(slugID_legacy = i))
      names(out) <- slugID_legacy
    }
  }
  return(out)
}
