recode_and_cut <- function(data, var, recode_map, breaks, labels) {
  data %>%
    dplyr::mutate(var_recode = recode(var, recode_map)) %>%
    dplyr::mutate(var_cut = cut(var, breaks = breaks, labels = labels))
}
######example still needed for reduce errors######
library(haven)


recode_and_cut(CFPS_adult, var, recode_map = list("1"= 9, "2"=8, "3"=7, "4"=6, "5"= 5, "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8),
               breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5),
               labels = c("1", "2", "3", "4", "5", "6", "7"))
