# This is just to stop RMD check from throwing Notes about these
# These are column names used with the dplyr package
# The alternative would be to use package rlang and .data$x in dplyr.
utils::globalVariables(c("SCORE", "SCORE_GROUP", "ID", "Group", "Age", "HR", "CI_NEG", "CI_POS", "Endpoint", "CCI_score", "predict", "GROUP", "ENDPOINT", "EXP_AGE", ""))