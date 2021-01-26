library(psych) ## I explore the "bfi" dataset from "psych" package
View(bfi) 
str(bfi)
nrow(bfi) ## This dataset includes 2800 observations.
ncol(bfi) ## This dataset includes 28 variables.
View(bfi.keys) ## The variables are BFI items, gender, education, and age.
describe(bfi) ## The mean age of participants is 28.78
summary(bfi)