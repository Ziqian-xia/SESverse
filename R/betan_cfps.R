
# Reproduce SES indexes
####### Betancourt, L, 2016 (CFPS & PSID)###########

#' Betan: Computation method of SES proposed by Betancourt, L, 2016
#'
#' @param faminc Family income
#' @param familysize An integer indicating the number of people in the household
#' @param itn (Optional) income-to-need ratio, only supplied when ITN is pre-calculated
#' @param edu_m Mother's education
#' @param method \code{'raw'}(default) indicate calculate SES by family income,
#' education and family size. \code{'itn'} indicate calculate SES by pre-calculated ITN
#' @param data A character indicate ‘CFPS' dataset or 'PSID'
#' @param povertyline Only used in 'PSID' dataset, the value, by default is 12060
#' @param increase Only used in 'PSID' dataset, the value, by default is 4180
#'
#' @import dplyr
#'
#' @return A single column of SES
#' @export
#'


betan<-function(faminc,familysize,itn,edu_m,method='raw',data='CFPS',
                povertyline=12060, increase=4180){
  if(data=='CFPS'){

  if(method == 'raw'){
  tibble(faminc,familysize,edu_m) %>%
    dplyr::mutate(itn = base::cut(faminc/familysize,
                                  breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf),
                                  labels = c("1", "2", "3", "4", "5"))) %>%
    # set 7 levels for mother's education
    dplyr::mutate(edu_m = base::cut(edu_m,
                                           breaks = c(-0.01, 9.5, 12.5, 13.5, 14.5, 16.5, 22.5),
                                           labels = c("1", "2", "3", "4", "5", "6")))%>%
      dplyr::mutate(itn = as.numeric(as.character(itn)),   # convert factors into numeric variable
                    edu_m = as.numeric(as.character(edu_m)))%>%
      dplyr::mutate(SES_betan_cfps = (itn + edu_m)/2)%>%
      dplyr:: select(SES_betan_cfps) -> df
    return(df)
  }
  if(method == 'itn'){
  tibble::tibble(itn=itn,edu_m)%>%
  dplyr::mutate(itn = as.numeric(as.character(itn)),   # convert factors into numeric variable
                edu_m = as.numeric(as.character(edu_m)))%>%
  dplyr::mutate(SES_betan_cfps = (itn + edu_m)/2)%>%
  dplyr:: select(SES_betan_cfps) -> df2
    return(df2)}}

  if(data=='PSID'){
    tibble::tibble(faminc,familysize,edu_m) %>%
    dplyr::mutate(edu_m = cut(edu_m,
                                     breaks = c(-0.01, 9.5, 12.5, 13.5, 14.5, 16.5, 17.5, 100),
                                     labels = c("1", "2", "3", "4", "5", "6", NA)),  # set 7 levels for mother's education
                  edu_m = as.numeric(as.character(edu_m))) %>%
      # set the poverty line for every family: poverty line 12060 for one people and increase 4180 for an extra person
      dplyr::mutate(itn1 = povertyline +  (familysize-1)*increase) %>%
      # get the interval of ITN
      dplyr::mutate(itn = ifelse(faminc < itn1, 1,
                                 ifelse(itn1 <= faminc & faminc < itn1*2, 2,
                                        ifelse(itn1*2 <= faminc & faminc < itn1*3, 3,
                                               ifelse(itn1*3 <= faminc & faminc < itn1*4, 4,
                                                      ifelse(itn1*4 <= faminc, 5, NA)))))) %>%  # No NA
      dplyr::mutate(SES_betan_psid = (itn + edu_m)/2)%>%select(SES_betan_psid)->df4
      return(df4)}
}
