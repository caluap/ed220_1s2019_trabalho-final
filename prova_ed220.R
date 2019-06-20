library(readr)
Prova19_268 <- read_delim("Prova19_268.csv", ";", escape_double = FALSE, trim_ws = TRUE)

View(Prova19_268)


library(lavaan)

ed220Model <- '
  # measurement model
  DESEJO_ENVOLV =~ DE_1 + DE_2 + DE_3 + DE_4 + DE_5 + DE_6 + DE_7 + DE_8
  FLEXI_COGNITIVA =~ FC_1 + FC_2 + FC_3 + FC_4 + FC_5 + FC_6 + FC_7 + FC_8 + FC_9 + FC_10 + FC_11
  AUTO_CONTROL =~ AC_1 + AC_2 + AC_3 + AC_4 + AC_5 + AC_6 + AC_7 + AC_8
  CONTR_EMOC =~ Cem_1 + Cem_2 + Cem_3 + Cem_4
  TOL_INCERT =~ TI_1 + TI_2 + TI_3 + TI_4 + TI_5 + TI_6 + TI_7
  # regressions
  TOL_INCERT ~ CONTR_EMOC + DESEJO_ENVOLV + FLEXI_COGNITIVA + AUTO_CONTROL
'