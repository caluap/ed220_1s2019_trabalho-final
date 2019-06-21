library(readr)
Prova19_268 <- read_delim("Prova19_268.csv", ";", escape_double = FALSE, trim_ws = TRUE)

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

iterateAVEs <- function(latVar, measuredVars) {
  biggestAVE <- 0
  for (i in 2:length(measuredVars)){
    AVE <- maxAVE(latVar, measuredVars, i)
    if (AVE > biggestAVE) {
      biggestAVE <- AVE
    }
  }
  print(biggestAVE)
}

maxAVE <- function(latVar, measuredVars, n) {
  library(combinat)
  
  combinations <- combn(measuredVars, n, simplify=TRUE)
  i <- 1
  s <- ''
  biggestRel <- 0
  biggestRelS <- ''
  
  for (el in combinations) {
    if (i == 1) {
      s <- paste(s, el, sep='')    
    } else {
      s <- paste(s, el, sep=' + ')    
    }
    
    i <- i + 1
    
    if (i == n+1) {
      s <- paste(latVar,' =~ ', s, sep='')
      fit <- sem(s, data=Prova19_268)
      rel <- reliability(fit)
      if (rel[5] > biggestRel && rel[5] < 1) {
        biggestRel <- rel[5]
        biggestRelS <- s
        print(s)
        print(biggestRel)
      }
      s <- ''
      i <- 1
    }
  }  
  return(c(biggestRel, biggestRelS))
}


#####
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



# ----BRUTE FORCE---------

de_elements <- c('DE_1','DE_2','DE_3','DE_4','DE_5','DE_6','DE_7','DE_8')
iterateAVEs('DESEJO_ENVOLV', de_elements)
# [1] "0.506420934232238"            "DESEJO_ENVOLV =~ DE_5 + DE_8"

fc_elements <- c('FC_1','FC_2','FC_3','FC_4','FC_5','FC_6','FC_7','FC_8','FC_9','FC_10','FC_11')
iterateAVEs('FLEXI_COGNITIVA', fc_elements)
# [1] "0.828123912360467"                     "FLEXI_COGNITIVA =~ FC_2 + FC_5 + FC_9"

ac_elements <- c('AC_1','AC_2','AC_3','AC_4','AC_5','AC_6','AC_7','AC_8')
iterateAVEs('AUTO_CONTROL', ac_elements)
# [1] "0.835481983117922"                  "AUTO_CONTROL =~ AC_4 + AC_7 + AC_8"

ce_elements <- c('Cem_1','Cem_2','Cem_3','Cem_4')
iterateAVEs('CONTR_EMOC', ce_elements)
# [1] "0.533768166778273"           "CONTR_EMOC =~ Cem_3 + Cem_4"

ti_elements <- c('TI_1','TI_2','TI_3','TI_4','TI_5','TI_6','TI_7')
iterateAVEs('TOL_INCERT', ti_elements)
# [1] "0.456619832412627"                "TOL_INCERT =~ TI_2 + TI_3 + TI_5"

# ------------------------

# not great, not terrible

notGreatNotTerribleModel <- '
  DESEJO_ENVOLV =~ DE_5 + DE_8
  FLEXI_COGNITIVA =~ FC_2 + FC_5 + FC_9
  AUTO_CONTROL =~ AC_4 + AC_7 + AC_8
  CONTR_EMOC =~ Cem_3 + Cem_4
  TOL_INCERT =~ TI_2 + TI_3 + TI_5
  TOL_INCERT ~ CONTR_EMOC + DESEJO_ENVOLV + FLEXI_COGNITIVA + AUTO_CONTROL
'
fit <- sem(notGreatNotTerribleModel, data=Prova19_268)
reliability(fit)

yetAgain <- '
  DESEJO_ENVOLV =~ DE_3 + DE_8
  # FLEXI_COGNITIVA =~ FC_6 + FC_7
  AUTO_CONTROL =~ AC_3 + AC_6 + AC_7
  CONTR_EMOC =~ Cem_3 + Cem_4
  TOL_INCERT =~ TI_3 + TI_5

  TOL_INCERT ~ CONTR_EMOC + DESEJO_ENVOLV + AUTO_CONTROL # + FLEXI_COGNITIVA 
'
fit <- sem(yetAgain, data=Prova19_268)
reliability(fit)


# -- step 1: composite reliability --
# following recommendations from https://stats.stackexchange.com/questions/97013/calculating-average-variance-extracted-ave-in-r-for-checking-discriminant-vali
# and https://groups.google.com/forum/#!topic/lavaan/TpWINJo_CRI, it seems that the reliability function's omega3 is a 
# conservative and therefore appropriate measure of composite reliability. omega 1 and 2 have
# some simplifying assumptions which i don't understand, so i'll leave them out for now.
reliability(fit)

#           DE           AC         CE         TI        total
#omega      0.6650048    0.7432014  0.7009346  0.5721891 0.7320654
#omega2     0.6650048    0.7432014  0.7009346  0.5721891 0.7320654
#omega3     0.6650047    0.7465899  0.7009345  0.5721892 0.7266407 <<<


# -- step 2: ave --
#           DE           AC         CE         TI        total
#avevar     0.5339257    0.5108456  0.5413090  0.4812402 0.5127562 <<<

# -- step 3: discrimant validity --

# -- step 4: pearson's r-squared --

# -- step 5: predictive validity --

# -- step 6: effect size --
# https://stats.stackexchange.com/questions/223729/estimating-effect-size-for-mean-difference-cohens-d-in-a-structural-equation

# -- step 7: path coefficients --

# -- step 8: unobserved heterogeneity --



#summary(fit, standardized=TRUE)
#semPaths(fit, what="paths", whatLabels="stand", rotation=2)


