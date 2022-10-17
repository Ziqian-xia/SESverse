########### Qiu, 2017 (CFPS & PSID)############
# SES = household income
#' Qiu: Computation method of SES proposed by Qiu, 2017 (CFPS & PSID)
#'
#' @param faminc household income
#' @param data A character indicate 'CFPS'  or 'PSID' dataset
#'
#' @return A single column of SES
#' @export
#'
qiu<-function(faminc,data='CFPS'){
  if(data=='CFPS'){
    tibble(faminc)%>%
      dplyr::mutate(SES_qiu_cfps = faminc) -> df
    return(df)
  }
  if(data=='PSID'){
    tibble(faminc)%>%
      dplyr::mutate(SES_qiu_psid = fincome) -> df
    return(df)
  }}
