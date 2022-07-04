# This is just to stop RMD check from throwing Notes about these
# These are column names used with the dplyr package
# The alternative would be to use package rlang and .data$x in dplyr.
utils::globalVariables(c("SCORE", "SCORE_GROUP", "ID", "Group", "Age", "HR", "CI_neg", "CI_pos", "Endpoint"))