# This is just to stop RMD check from throwing Notes about these
# These are column names used with the dplyr package
# The alternative would be to use package rlang and .data$x in dplyr.
utils::globalVariables(c("DATE_OF_BIRTH", "DATE_EXP", "AGE_AT_DIAG", "ID", "FOLLOWUP", "SEX", "START_OF_FOLLOWUP", "END_OF_FOLLOWUP", "ANCESTRY", "ENDPT_FREE_PERIOD", "STUDY_TIME", "AGE_AT_ONSET", "YEAR_OF_BIRTH", "EXP_END", "EXP_START_DATE", "OBS_END_DATE", "WASH_END_DATE", "AGE_AT_BASE"))