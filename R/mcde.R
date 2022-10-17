############ McDermott, 2019 (CFPS only)##########
# Subject: child/young adult
# SES: (SES_father + SES_mother)/2 (replace each other if one of them is NA)
# SES_father = father's occupation*5 + father's education*3;
# SES_mother = mother's occupation*5 + mother's education*3
# education and occupation: see 'Education & Occupation recode.xlsx'
# Note: “higher SES” refers to a lower Hollingshead score.
# Note: can only be reproduced using CFPS data because of the occupation

## CFPS ##
#' Title
#'
#' @param occup_m mother's occupation
#' @param edu_m mother's education
#' @param occup_f father's occupation
#' @param edu_f father's education
#' @param data A character indicate 'CFPS' dataset or 'PSID'.And in this case, only CFPS is applicable
#'
#' @return A single column of SES
#' @export
#'

mcde<-function(occup_m,edu_m,occup_f,edu_f,data='CFPS'){
  if(data=='CFPS'){
    tibble(occup_m,edu_m)%>%
      dplyr::mutate(occup_cat_m = recode(occup_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                       "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8))%>% # recode education
      dplyr::mutate(edu_cat_m = cut(edu_m,
                                  breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5),
                                  labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
      dplyr::mutate(occup_cat_m = as.numeric(as.character(occup_cat_m)),# convert things into numeric variables
                    edu_cat_m = as.numeric(as.character(edu_cat_m)))%>%
      dplyr::mutate(SES_mcde_cfps_m = (occup_cat_m*5+edu_cat_m*3)) %>%

      dplyr::mutate(occup_cat_f = recode(occup_f, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                         "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8))%>% # recode education
      dplyr::mutate(edu_cat_f = cut(edu_f,
                                    breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5),
                                    labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
      dplyr::mutate(occup_cat_f = as.numeric(as.character(occup_cat_f)),# convert things into numeric variables
                    edu_cat_f = as.numeric(as.character(edu_cat_f)))%>%
      dplyr::mutate(SES_mcde_cfps_f = (occup_cat_f*5+edu_cat_f*3)) %>%
      dplyr::mutate(SES_mcde_cfps = (SES_mcde_cfps_m+SES_mcde_cfps_f)/2) %>%

      dplyr:: select(SES_mcde_cfps) -> df
    return(df)
  }
}

