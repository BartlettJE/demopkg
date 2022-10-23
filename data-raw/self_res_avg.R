## code to prepare `self_res_avg` dataset goes here

library(readr)
library(glue)

ct <- cols(
  id = col_character(),
  sex = col_factor(levels = c("male", "female")),
  group = col_factor(),
  m_avg = col_double(),
  f_avg = col_double(),
  m_self = col_double(),
  f_self = col_double(),
  m_non = col_double(),
  f_non = col_double()
)

self_res_avg <- read_csv("data-raw/DeBruine_2004_PRSLB_avg.csv",
                         col_types = ct)

usethis::use_data(self_res_avg, overwrite = TRUE)

coldesc <- rep("", ncol(self_res_avg))
names(coldesc) <- names(self_res_avg)
dput(coldesc)

vars <- c(id = "participant unique ID",
          sex = "sex of the participant (male/female)",
          group = "unique group ID",
          m_avg = "number of times out of a possible 6 chose a male composite face as more attractive",
          f_avg = "number of times out of a possible 6 chose a female composite face as more attractive",
          m_self = "umber of times out of a possible 6 chose their male self-res face as more attractive",
          f_self = "number of times out of a possible 6 chose their female self-res face as more attractive",
          m_non = "mean number of times the other group members chose that male face as more attractive",
          f_non = "mean number of times the other group members chose that female face as more attractive")

glue("#'   \\item{[colname]}{[coldesc]}",
  colname = names(vars),
  coldesc = vars,
  .open = "[",
  .close = "]")

