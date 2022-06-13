# This is just to stop RMD check from throwing Notes about these
# These are column names used with the dplyr package
# The alternative would be to use package rlang and .data$x in dplyr.
utils::globalVariables(c("ID", "ID_num", "score"))