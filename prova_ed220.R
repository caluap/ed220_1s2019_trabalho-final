library(readr)
Prova19_268 <- read_delim("Prova19_268.csv", ";", escape_double = FALSE, trim_ws = TRUE)

View(Prova19_268)


library(lavaan)
library(semTools)

# model as presented
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

fit <- sem(ed220Model, data=Prova19_268)
reliability(fit)


# attempt to maximize AVE:
best_ave_de <- 'DESEJO_ENVOLV =~ DE_1 + DE_2 + DE_6'
fit <- sem(best_ave_de, data=Prova19_268)
reliability(fit)

best_ave_fg <- '
  #FLEXI_COGNITIVA =~ FC_1 + FC_10 + FC_11  # this is ave > .5, but gives out a warning about negative variance
  FLEXI_COGNITIVA =~ FC_2 + FC_6 + FC_7 + FC_8
'
fit <- sem(best_ave_fg, data=Prova19_268)
reliability(fit)

best_ave_ac <- 'AUTO_CONTROL =~ AC_6 + AC_7 + AC_8'
fit <- sem(best_ave_ac, data=Prova19_268)
reliability(fit)

best_ave_ce <- 'CONTR_EMOC =~ Cem_1 + Cem_3 + Cem_4'
fit <- sem(best_ave_ce, data=Prova19_268)
reliability(fit)

best_ave_ti <- 'TOL_INCERT =~ TI_1 + TI_4 + TI_5'
fit <- sem(best_ave_ti, data=Prova19_268)
reliability(fit)

# not great, not terrible

rec <- '  TOL_INCERT ~ CONTR_EMOC + DESEJO_ENVOLV + FLEXI_COGNITIVA + AUTO_CONTROL'
best_ave_model <- paste(best_ave_de, best_ave_fg, best_ave_ac, best_ave_ce, best_ave_ti, rec, sep='\n')
fit <- sem(best_ave_model, data=Prova19_268)
reliability(fit)

#summary(fit, standardized=TRUE)
#semPaths(fit, what="paths", whatLabels="stand", rotation=2)


