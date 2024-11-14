fin_data <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg/2019-01-01_o8_w2_e10_32_70_cidx.tsv")
uk_data <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/ukb/2019-01-01_o8_w2_e10_32_70_cidx.tsv")
estbb_data <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-14/bbs/estb/2019-01-01_o8_w2_e6_32_70_cidx.tsv")
cdix_data <- rbind(fin_data, uk_data, estbb_data)
cdix_data
readr::write_delim(cdix_data, "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/meta/2019-01-01_o8_w2_10_32_70_cidx.tsv", delim = "\t")
cdix_data %>% filter(Biobank == "EstB")
