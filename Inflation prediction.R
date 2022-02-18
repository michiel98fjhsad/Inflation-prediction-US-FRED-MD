# R script goes here
head(fred_final$CPIAUCSL)

f <- ICfactors(fred_final[,-106]) # selects 17 factors
names(f)
prcomp(na.omit(fred_final[,-106], rank. = 20 )) # CPIAUCSL is 106th column

  