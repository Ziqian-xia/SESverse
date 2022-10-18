############### Romeo, 2018b (CFPS & PSID)############
# Subject: adolescent
# SES = (Z_{parents' education} + Z_{family income})/2
# education: recode into 5 levels (0-4) and then calculate z-score of the parents' education
#           (replace parents' education with each other if one of them is NA)

#' Romeo_b: Computation method of SES proposed by Romeo, 2018b (CFPS & PSID)
#'
#' @param faminc Family income
#' @param edu_m Mother's education (If only one side of the parent's data is available, it is possible to fill in the list with all NA)
#' @param edu_f Father's education (If only one side of the parent's data is available, it is possible to fill in the list with all NA)
#' @param data A character indicate 'CFPS'  or 'PSID' dataset
#'
#' @return A single column of SES
#' @export
#'
romeo_b<-function(faminc,edu_m,edu_f,data='CFPS'){
  if(data=='CFPS'){
    tibble(faminc,edu_m,edu_f)%>%
      dplyr::mutate(income_zscore = (faminc - mean(faminc, na.rm = TRUE))/sd(faminc, na.rm = TRUE)) %>%
      dplyr::mutate(edu_cat_m = cut(edu_m,
                                    breaks = c(-0.5,3.5,4.5,5.5,6.5,8.5),
                                    labels = c("1", "2", "3", "4","5")))%>% # recode education
      dplyr::mutate(edu_cat_m = as.numeric(as.character(edu_cat_m))-1) %>% # recode education 0-4
      dplyr::mutate(edu_cat_f = cut(edu_f,
                                    breaks = c(-0.5,3.5,4.5,5.5,6.5,8.5),
                                    labels = c("1", "2", "3", "4","5")))%>% # recode education
      dplyr::mutate(edu_cat_f = as.numeric(as.character(edu_cat_f))-1) %>% # recode education 0-4
      dplyr::mutate(edu_cat_f = ifelse(is.na(edu_cat_f), edu_cat_m, edu_cat_f),  # calculate composite parents' education score
                    edu_cat_m = ifelse(is.na(edu_cat_m), edu_cat_f, edu_cat_m))%>%
      dplyr::mutate(edu_parents = (edu_cat_f + edu_cat_m)/2)%>%  # calculate composite parents' education score
      dplyr::mutate(edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE)) %>%  # calculate mean of edu and income (ses)
      dplyr::mutate(SES_romeo_b_cfps = (edu_zscore+income_zscore)/2) %>%# calculate ses
      dplyr::select(SES_romeo_b_cfps) -> df
    return(df)
  }
  if(data=='PSID'){
    tibble(faminc,edu_m,edu_f)%>%
      dplyr::mutate(income_zscore = (faminc - mean(faminc, na.rm = TRUE))/sd(faminc, na.rm = TRUE)) %>%
      dplyr::mutate(edu_cat_m = cut(edu_m,
                                    breaks = c(-0.00001, 8.5, 12.5,  14.5, 16.5, 17.5, 100),
                                    labels = c("1", "2", "3", "4", "5",  NA)))%>% # recode education
      dplyr::mutate(edu_cat_m = as.numeric(as.character(edu_cat_m))-1) %>% # recode education 0-4
      dplyr::mutate(edu_cat_f = cut(edu_f,
                                    breaks = c(-0.00001, 8.5, 12.5,  14.5, 16.5, 17.5, 100),
                                    labels = c("1", "2", "3", "4", "5",  NA)))%>% # recode education
      dplyr::mutate(edu_cat_f = as.numeric(as.character(edu_cat_f))-1) %>% # recode education 0-4
      dplyr::mutate(edu_cat_f = ifelse(is.na(edu_cat_f), edu_cat_m, edu_cat_f),  # calculate composite parents' education score
                      edu_cat_m = ifelse(is.na(edu_cat_m), edu_cat_f, edu_cat_m))%>%
      dplyr::mutate(edu_parents = (edu_cat_f + edu_cat_m)/2)%>%  # calculate composite parents' education score
      dplyr::mutate(edu_zscore = (edu_parents - mean(edu_parents, na.rm = TRUE))/sd(edu_parents, na.rm = TRUE)) %>%  # calculate mean of edu and income (ses)
      dplyr::mutate(SES_romeo_b_psid = (edu_zscore+income_zscore)/2) %>%
      dplyr::select(SES_romeo_b_psid) -> df
    return(df)
  }}
