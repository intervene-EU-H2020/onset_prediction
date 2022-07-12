#   set.seed(21923)
#   test_data <- create_test_df(10)
#   study <- get_backward_study(test_data)

# run_back_study <- function(pheno_data, 
#                            score_data,
#                            score_type,
#                            endpts=c("J10_ASTHMA"),
#                            wash_len=2,
#                            obs_len=8,
#                            downsample_fctr=NULL,
#                            ancs=NA_character,
#                            covs=c("SEX", "YEAR_OF_BIRTH"),
#                            bin_cut=1,
#                            min_indvs=5,
#                            write_res=FALSE,
#                            res_dir=NULL)