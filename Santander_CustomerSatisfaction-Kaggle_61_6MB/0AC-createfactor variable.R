
# @@@@@@@@@  creating factor varaibles
  
mutate(SeriousDlqin2yrs = factor(SeriousDlqin2yrs,                           # factor variable for classification
                                   labels = c("Failure", "Success")))


