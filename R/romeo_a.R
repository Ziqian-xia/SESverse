############# Romeo, 2018a (CFPS)##################
# Subject: adolescent
# SES = mother's education and occupation
# education and occupation: see 'Education & Occupation recode.xlsx'
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: only reproduced using CFPS data because of the occupation data
#' Computation method of SES proposed by Romeo, 2018a (CFPS only)
#'
#' @param occup_m mother's occupation
#' @param edu_m mother's education
#' @param data A character indicate 'CFPS' dataset or 'PSID'.And in this case, only CFPS is applicable,because occupation data is not available from PSID
#'
#' @return A single column of SES
#' @export
#'
romeo_a<-function(occup_m,edu_m,data='CFPS'){
  if(data=='CFPS'){
    tibble(occup_m,edu_m)%>%
      dplyr::mutate(occup_cat = recode(occup_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5,
                                       "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1,  .default = -8))%>% # recode occupation
      dplyr::mutate(edu_cat = cut(edu_m,
                                  breaks = c(-0.5, 6.5, 9.5,11.5,12.5,14.5,16.5,22.5),
                                  labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
      dplyr::mutate(occup_cat = as.numeric(as.character(occup_cat)),# convert occupation and education into numeric variables
                    edu_cat = as.numeric(as.character(edu_cat)))%>%
      dplyr::mutate(SES_romeo_a_cfps = (occup_cat*5+edu_cat*3)) %>%
      dplyr:: select(SES_romeo_a_cfps) -> df
    return(df)
  }
}



