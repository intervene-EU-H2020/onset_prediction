# This is just to stop RMD check from throwing Notes about these
# These are column names used with the dplyr package
# The alternative would be to use package rlang and .data$x in dplyr.
utils::globalVariables(c("FINNGENID", "chip", "cohort", "IID", "ID", "SCORE1_AVG", "SOURCE", "EVENT_AGE", "CODE1", "#ID", "SOURCE", "pred_class1_prob"))