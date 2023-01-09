library(nicheRealisation)

wd_variables <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables'
wd_varialves2 <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5'
wd_standard_var <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/Standardised_variables'

#download worldclim variables

getWorldClim(res = 2.5, path = wd_variables)

#standardise variables

variableStandardisation(original_path = wd_varialves2,
                        new_path = wd_standard_var, original_format = '.bil$')
