####### Moog, et al., 2008 (CFPS & PSID) ###########
# Subject：neonates
# SES = (mother's highest education + income)/2
# mother's highest edu: 5 levels, see 'Education & Occupation recode.xlsx'
# income: recode income into 5 levels
#        original (year 2018) categories: <= $15,000, >=100,000 (no specific bins, set as 15,000, 45,000, 75,000, 100,000)
#        CFPS: convert to equivalence in US 2010 using CPI and then convert to equivalence in China 2010 using PPP
#             CPI US 2010/2008 = 218.056/215.303 = 1.013; PPP China CNY/US dollar (2010) = 3.329
#        PSID: convert to equivalence in US 2010


#' Moog: Computation method of SES proposed by Moog, 2008
#'
#' @param faminc Family income
#' @param edu_m Mother's education
#' @param data A character indicate 'CFPS' dataset or 'PSID'
#'
#' @return A single column of SES
#' @export
#'

moog<-function(faminc,edu_m,data='CFPS'){
  if(data=='CFPS'){
    tibble(faminc,edu_m)%>%
      dplyr::mutate(edu_cat = dplyr::recode_factor(edu_m, "1" = 1,"2" = 1,"3" = 1,
                                                   "4" = 2,"5" = 3,"6" = 4,"7" = 5,"8" = 5))%>% # recode education
      dplyr::mutate(income_cat = base::cut(faminc, #recode income,
                                           breaks= c(-0.01, 15000*1.013*3.329, 45000*1.013*3.329,
                                                     75000*1.013*3.329, 100000*1.013*3.329, 300000),
                                           labels = c("1", "2", "3", "4", "5"))) %>%
      dplyr::mutate(income_cat = as.numeric(as.character(income_cat)),# convert income and education into numeric variables
                    edu_cat = as.numeric(as.character(edu_cat)))%>%
      dplyr::mutate(SES_moog_cfps = (edu_cat+income_cat)/2) %>%
        dplyr:: select(SES_moog_cfps) -> df
      return(df)
  }
  if(data=='PSID'){
    tibble(faminc,edu_m)%>%
      dplyr::mutate(income_cat=cut(faminc,
                                   breaks= c(-0.01, 15000*1.139, 45000*1.139, 75000*1.139, 100000*1.139, 300000),
                                   labels = c("1", "2","3", "4","5"))) %>% #recode income:CPI US 2017/2008 = 245.120/215.303 = 1.139;
      dplyr::mutate(edu_m_recode = cut(edu_m,
                                       breaks = c(-0.00001, 8.5, 12.5, 14.5, 16.5, 17.5, 100),
                                       labels = c("1", "2", "3", "4",  "5", NA))) %>% #recode education: cut into 5 groups
      dplyr::mutate(income_cat = as.numeric(as.character(income_cat)),  # convert variables into numeric ones
                    edu_m_recode = as.numeric(as.character(edu_m_recode)))%>%
      dplyr::mutate(SES_moog_psid = (income_cat + edu_m_recode)/2)%>%select(SES_moog_psid)->df  # calculate composite SES
    return(df)
  }}
