# test_that("test_endpt_is_single_string works", {
  
#   test_data <- create_test_df(2)
#   test_data <- add_study_interval_cols(test_data)

#   # No error
#   expect_error(adj_case_cntrl_status(test_data, "J10_ASTHMA"), regexp=NA)

#   # Errors
#   expect_error(adj_case_cntrl_status(test_data, 2), 
#                regexp="The variable endpt needs to be a character string. Instead got: numeric",
#                fixed=TRUE)
#   expect_error(adj_case_cntrl_status(test_data, 
#                                      c("J10_ASTHMA", "C3_BREAST")), 
#                "The variable endpt needs to be a character string and not a vector of characters.",
#                fixed=TRUE)
#   expect_error(adj_case_cntrl_status(test_data, "ASTHMA"),      regexp="The chosen endpoint ASTHMA is not part of the data.
# The data.frame pheno_data has columns:
# ID, SEX, DATE_OF_BIRTH, START_OF_FOLLOWUP, END_OF_FOLLOWUP, ANCESTRY, C3_BREAST, C3_BREAST_DATE, T2D, T2D_DATE, C3_PROSTATE, C3_PROSTATE_DATE, COVHOSP, COVHOSP_DATE, I9_VTE, I9_VTE_DATE, J10_ASTHMA, J10_ASTHMA_DATE, FOLLOWUP, ENDPT_FREE, STUDY_TIME",
#   fixed=TRUE)

# })

# test_that("test_date_var_correct works", {
#   bds = c(as.Date("2020/01/01", "1977/01/01"))
#   bds_wrong = c(1,2,3)
#   # No error
#   expect_error(calc_endpt_free_time(bds), regexp=NA)

#   # Errors
#   bds_wrong = c(1,2,3)
#   expect_error(calc_endpt_free_time(bds_wrong), regexp="The variable bds should be of type Date.", fixed=TRUE)
# })

# test_that("test_length_vars_are_integers works", {
#   set.seed(9123)
#   test_data = create_test_df(10)

#   # No error
#   # expect_error(suppressMessages(get_study_elig_indv(test_data,
#                                    endpt="J10_ASTHMA", 
#                                    exp_age=30,
#                                    exp_len=10,
#                                    wash_len=2,
#                                    obs_len=8)), 
#                 regexp=NA)

#   # Errors
#   expect_error(get_study_elig_indv(test_data,
#                                    endpt="J10_ASTHMA",
#                                    exp_age=30.3,
#                                    exp_len=10,
#                                    wash_len=2,
#                                    obs_len=8), 
#                 regexp="The variable exp_age needs to be an integer.",
#                 fixed=TRUE)

#   expect_error(get_study_elig_indv(test_data,
#                                    endpt="J10_ASTHMA",
#                                    exp_age="bla",
#                                    exp_len=10,
#                                    wash_len=2,
#                                    obs_len=8), 
#                 regexp="The variable exp_age needs to be an integer.",
#                 fixed=TRUE)
# })

