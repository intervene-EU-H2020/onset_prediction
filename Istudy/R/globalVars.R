# This is just to stop RMD check from throwing Notes about these
# These are column names used with the dplyr package
# The alternative would be to use package rlang and .data$x in dplyr.
utils::globalVariables(c("DATE_OF_BIRTH", "DATE_EXP", "AGE_AT_DIAG", "ID", "FOLLOWUP", "SEX", "START_OF_FOLLOWUP", "END_OF_FOLLOWUP", "ANCESTRY", "ENDPT_FREE", "STUDY_TIME", "AGE_AT_ONSET", "YEAR_OF_BIRTH", "EXP_LEN"))