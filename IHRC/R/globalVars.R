# This is just to stop RMD check from throwing Notes about these
# These are column names used with the dplyr package
# The alternative would be to use package rlang and .data$x in dplyr.
utils::globalVariables(c("SCORE", "ID", "Group", "Age", "HR", "CI_NEG", "CI_POS", "Endpoint", "CCI", "predict", "GROUP", "ENDPOINT", "EXP_LEN", "EXP_END", "EXP_END_PERIOD", "EXP_AGE", "DATE_OF_BIRTH", "EXP_END_DATE", "EXP_START_DATE", "EXP_START", "VAR", "SEX", "ANCESTRY", "END_OF_FOLLOWUP", "BATCH", "SMOKING", "BMI", "EDU_cont", "EDU_prob", "ZIP_prob", "ZIP", "FIN_AGE_MODE", "EDU"))