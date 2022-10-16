########## Jednoróg,  2012 (CFPS only) ##########
# Subject: child
# SES = mother's education * 4 + mother's occupation *7
# mother's education: see 'Education & Occupation recode.xlsx'
# mother's occupation: see 'Education & Occupation recode.xlsx'
# Note: we reversed the score from the original so that the correlation between it and other SES scores are positive
# Note: only reproduced using CFPS data because occupation data is not available from PSID
## CFPS ##

#' Jednoróg: Computation method of SES proposed by Jednoróg,  2012 (CFPS only)
#'
#' @param occup_m mother's occupation
#' @param edu_m mother's education
#' @param data A character indicate 'CFPS' dataset or 'PSID'.And in this case, only CFPS is applicable,because occupation data is not available from PSID
#'
#' @return A single column of SES
#' @export
#'
jedn<-function(occup_m,edu_m,data='CFPS'){
  if(data=='CFPS'){
    tibble(occup_m,edu_m)%>%
      dplyr::mutate(occup_cat = recode(occup_m, "1"= 7, "2"=6, "3"=5,  "7"= 5, "4"=4, "5"= 4,
                                       "6"= 4, "8"= 3, "9"=2, "10"=1, "11"=1,
                                       "-8"=-8,"80000"= 8, .default = -8))%>% # recode education
      dplyr::mutate(edu_cat = cut(edu_m,
                                  breaks = c(-0.01,3.5,6.5,9.5,12.5,14.5,16.5, 22.5),
                                  labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
      dplyr::mutate(occup_cat = as.numeric(as.character(occup_cat)),# convert income and education into numeric variables
                    edu_cat = as.numeric(as.character(edu_cat)))%>%
      dplyr::mutate(SES_jedn_cfps = (occup_cat*7+edu_cat*4)) %>%
      dplyr:: select(SES_jedn_cfps) -> df
    return(df)
  }
}
