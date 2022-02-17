# R script goes here
head(fred_final$CPIAUCSL)

ICfactors(fred_final)
prcomp(fred_final)
summary(fred_final$CPIAUCSL)
