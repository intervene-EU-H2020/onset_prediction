library(metafor)
library(readr)
library(tidyr)
library(lubridate)
library(dplyr)
library(stringr)
library(plotly) # plotly::ggplotly for interactive plots
library(cowplot) # For combining plots
library(ggplot2)
library(ggforce) #  ggforce::geom_sina for violin scatter plots
library(matrixStats) # For rowMedians, vars etc
#' Calculate 95% Confidence Interval
#'
#' This function calculates the 95% confidence interval for a set of 
#' estimates using the standard error of the estimates.
#'
#' @param ML A numeric vector of estimates.
#' @param SE A numeric vector of standard errors.
#' @return A list with two elements, `neg` (negative limits of the confidence interval) and 
#' `pos` (positive limits of the confidence interval).
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_CI <- function(ML, SE) {
    CIneg <- ML-1.96*SE
    CIpos <- ML+1.96*SE
    return(list(neg=CIneg, pos=CIpos))
}

#' Helps with pretty plot axis labels
so_formatter <- function(num,
                         n_digits=1) {
  dplyr::case_when(
    num < 1e3 ~ as.character(round(num, n_digits)),
    num < 1e6 ~ paste0(as.character(round(num/1e3, n_digits)), "K"),
    num < 1e9 ~ paste0(as.character(round(num/1e6, n_digits)), "M"),
    TRUE ~ "To be implemented..."
  )  
}

#' Reads in data with biobank information and some filtering
get_data <- function(path, biobank="FinnGen") {
    endpt_names <- readr::read_delim("/home/detrokir/Documents/DSGE/GitHub/onset_prediction/analysis/results/tables/endpt_name_maps/endpoint_name_map.csv")
    data <- readr::read_delim(path)
    data <- dplyr::filter(data, !(ENDPOINT %in% c("G6_SLEEPAPNO", "K11_IBD_STRICT", "I9_HEARTFAIL_NS", "N14_CHRONKIDNEYDIS", "G6_AD_WIDE", "C3_CANCER")))
    endpt_names <- dplyr::select(endpt_names, ENDPOINT, Endpoint)
    data <- dplyr::left_join(data, endpt_names, by = c("ENDPOINT"))
    #data <- dplyr::filter(data, !is.na(SURV_MODEL))
    data <- dplyr::distinct(data)
    data$Biobank <- biobank
    return(data)
}

#' Custom color selection 
#' 
#' Based on https://colorbrewer2.org/#type=diverging&scheme=RdBu&n=10
#' 
#' @param N_colors The number of colors
#' 
#' @author Kira E. Detrois
#' 
#' @export 
custom_colors_brewer <- function(N_colors) {
    if(N_colors == 1)
      c("#000000")
    else if(N_colors == 2) {
      c("#D5694F", "#29557B")
    } else if(N_colors == 3)  {
      c("#D5694F", "#29557B", "#EAB034")
    } else if(N_colors == 4) {
      c("#D5694F", "#29557B", "#EAB034", "#748AAA")
    } else if(N_colors == 5) {
      c("#D5694F", "#29557B", "#EAB034", "#748AAA", "#CCB6AF")
    } else if(N_colors == 10) {
      c("#D5694F", "#29557B", "#EAB034", "#748AAA", "#CCB6AF", "#841C26", "#7D7C7F", "#FBCF9D",  "#7BA05B", "#588986")
    } else if(N_colors == 17) {
      c("#841C26", "#B53389", "#C6878F", "#A81C07", "#D5694F", "#FBCF9D", "#59260B", "#CCB6AF", "#7D7C7F", "#91A3B0",
         "#3C4E2D", "#7BA05B", "#9BB59B", "#588986","#29557B","#748AAA", "#ADD8E6")
    } else if(N_colors == 18) {
      c("#841C26", "#B53389", "#C6878F", "#A81C07", "#D5694F", "#FBCF9D", "#FBCF9D", "#CCB6AF", "#7D7C7F", "#91A3B0",
         "#3C4E2D", "#7BA05B", "#9BB59B", "#588986","#29557B","#748AAA", "#ADD8E6", "#D6ECFF")
    }
}

#' set bb colors
get_bb_colors <- function() {
bb_colors <- c(FinnGen="#EFB78C", UKB="#B7988F", EstB="#688D68", GS="#5E7382", `Meta-analysis`=color_pal_paper(N=1, highlight_position=1))
return(bb_colors)

}

#' Selection before 
get_crnt_endpt_data <- function(data) {
    data <- dplyr::filter(data, !(Endpoint %in% c("Interstitial Lung Disease", "Appendicitis",   "Lung Cancer", "Epilepsy", "Rheumatoid Arthritis", "Melanoma", "AllCancers")))
    return(data)
}

get_new_crnt_endpt_data <- function(data) {
    data <- dplyr::filter(data, !(Endpoint %in% c("Interstitial Lung Disease", "Appendicitis",  "Rheumatoid Arthritis", "Melanoma", "AllCancers")))
    return(data)
}

#' Original color list
get_color_list <- function() {
    endpt_names <- readr::read_delim("/home/detrokir/Documents/DSGE/GitHub/onset_prediction/analysis/results/tables/endpt_name_maps/endpoint_name_map.csv")

    color_list <- endpt_names$NEW_COLOR
    names(color_list) <- endpt_names$Endpoint
    return(color_list)
}

#' Colorlist for subset of diseases
get_new_color_list <- function() {
  new_color_list <- c(J10_ASTHMA="#A81C07", C3_BREAST="#D5694F", T2D="#FBCF9D", KNEE_ARTHROSIS="#59260B", F5_DEPRESSIO="#CCB6AF", COX_ARTHROSIS="#7D7C7F",  C3_COLORECTAL="#7BA05B", I9_CHD="#9BB59B", C3_PROSTATE="#3C4E2D", GOUT="#748AAA", I9_AF="#29557B")
  names(new_color_list) <- c("Asthma", "Breast Cancer", "Type 2 Diabetes", "Knee Osteoarthritis", "Major Depression", "Hip Osteoarthritis", "Colorectal Cancer", "Coronary Heart Disease", "Prostate Cancer", "Gout", "Atrial Fibrillation")
  return(new_color_list)
}

#' Final color list for manuscript
get_final_color_list <- function() {

    endpt_names <- readr::read_delim("/home/detrokir/Documents/DSGE/GitHub/onset_prediction/analysis/results/tables/endpt_name_maps/endpoint_name_map.csv")

    color_list <- endpt_names$NEW_COLOR
    names(color_list) <- endpt_names$Endpoint
    color_list$`Knee Osteoarthritis` <- "#59260B"
    color_list$`Hip Osteoarthritis` <- "#CCB6AF"
    color_list$`Prostate Cancer` <- "#C6878F"
    color_list$`Colorectal Cancer` <- "#748AAA"

  return(color_list)
}

#' Two-sided p-value calculation using z-scores for HRs
calc_hr_pval <- function(elem1, elem2, alpha=0.05, one_sided=FALSE) {
  #est 1 - est 2 / sqrt(se1^2 + se2^2)
  zs <- (((elem1 %>% select(matches("BETA") & !matches("SE"))) - (elem2 %>% select(matches("BETA") & !matches("SE")))) /sqrt((elem1%>% select(matches("SE")))**2+(elem2 %>% select(matches("SE")))**2)) %>% as.vector() %>% unlist()
  if(one_sided) {
    pvals <- pnorm(zs)
  } else {
    pvals <- 2*pnorm(-abs(zs))
  }
  sign <- (pvals < alpha)
  multi_sign <- (pvals < alpha/length(pvals))
  return(tibble(pvals=pvals, sign=sign, multi_sign=multi_sign))
}

#' Two-sided p-value calculation using z-scores for c-index results
calc_cidx_pval <- function(elem1, elem2, alpha=0.05, one_side=TRUE) {
  top_z <- ((elem1 %>% select(matches("C_IDX") & !matches("^SE") & !matches("_CI_"))) - (elem2 %>% select(matches("C_IDX") & !matches("^SE") & !matches("_CI_")))) %>% as.vector() %>% unlist()
  bottom_z <- sqrt((elem1%>% select(matches("^SE")))**2+(elem2 %>% select(matches("^SE")))**2) %>% as.vector() %>% unlist()
  zs <- (top_z/bottom_z) %>% as.vector() %>% unlist()

  if(one_side) {
    pvals <- pnorm(zs)
  } else  {
    pvals <- 2*pnorm(-abs(zs))
  }
  sign <- (pvals < alpha)
  multi_sign <- (pvals < alpha/length(pvals))
  return(tibble(pvals=pvals, sign=sign, multi_sign=multi_sign))
}

#' Calculate increase in c-index and p-values
calc_increase <- function(full, base) {
    incr <- dplyr::select(full, ENDPOINT, Endpoint, C_IDX, Biobank) %>% dplyr::distinct()
    full <- dplyr::select(full, ENDPOINT, Endpoint, C_IDX, SE, Biobank) %>% dplyr::distinct()
    base <- dplyr::select(base, ENDPOINT, Endpoint, C_IDX, SE, Biobank) %>% dplyr::distinct()

    incr$C_IDX_INCR <- full$C_IDX - base$C_IDX
    incr$C_IDX_INCR_SE <- sqrt(full$SE**2 + base$SE**2)
    C_IDX_INCR_CI <- get_CI(incr$C_IDX_INCR, incr$C_IDX_INCR_SE)
    incr$C_IDX_INCR_CIneg <- C_IDX_INCR_CI$neg
    incr$C_IDX_INCR_CIpos <- C_IDX_INCR_CI$pos
    incr$Biobank <- full$Biobank
    incr$C_IDX_INCR_pval <- pnorm((incr$C_IDX_INCR)/incr$C_IDX_INCR_SE, lower.tail=FALSE)
    incr$C_IDX_INCR_sign <- (incr$C_IDX_INCR_pval < 0.05)
    incr$C_IDX_INCR_multi_sign <- (incr$C_IDX_INCR_pval < 0.05**(length(incr$C_IDX_INCR_pval)))
    
    return(incr)
}

#' mutate c-index data with two models to wider format with p-value for difference
cidx_mdl_comp <- function(crnt_cidx,
                     lower_mdl,
                     upper_mdl) {
  crnt_cidx <- crnt_cidx %>% 
                get_new_crnt_endpt_data()   %>%
                mutate(SURV_MODEL=case_when(SURV_MODEL == lower_mdl ~ "mdl1", 
                                            SURV_MODEL == upper_mdl ~ "mdl2")) %>%
                pivot_wider(values_from=c(C_IDX, C_IDX_CI_NEG, C_IDX_CI_POS, SE), names_from=SURV_MODEL, id_cols=c(Endpoint, Biobank))
  crnt_cidx$P_VAL_DIFFERENCE <- as.numeric(calc_cidx_pval(crnt_cidx %>% select(C_IDX_mdl1, SE_mdl1), crnt_cidx %>% select(C_IDX_mdl2, SE_mdl2))$pvals)
  crnt_cidx$sign <- as.numeric(calc_cidx_pval(crnt_cidx %>% select(C_IDX_mdl1, SE_mdl1), crnt_cidx %>% select(C_IDX_mdl2, SE_mdl2))$sign)
  return(crnt_cidx)
}

#'  meta analysis of HRs
meta_hrs <- function(data, var, surv_model, bbs=c("FinnGen", "UKB", "EstB"), group=-1, crnt_method="REML"){
    data <- dplyr::filter(data, SURV_MODEL == surv_model, Biobank %in% bbs, VAR == var, GROUP==group)
    # For order - deprecated
    endpt_names <- readr::read_delim("/home/detrokir/Documents/DSGE/GitHub/onset_prediction/analysis/results/tables/endpt_name_maps/endpoint_name_map.csv")
    # Results tibble
    meta <- tibble(Endpoint=character(), SURV_MODEL=character(), VAR=character(), GROUP=numeric(), N_CONTROLS=numeric(), N_CASES=numeric(), 
                   BETA=numeric(), BETA_SE=numeric(), P_VAL=numeric(), I2=numeric(), I2_CIneg=numeric(), I2_CIpos=numeric(), QHet=numeric(), QHet_CIneg=numeric(), 
                   QHet_CIpos=numeric(), Q_pval=numeric(), N_BBs=numeric(), HR=numeric(), CI_NEG=numeric(), CI_POS=numeric(), ENDPT_ORDER=numeric())    
    # Data with more than one biobank
    data_select <- dplyr::filter(data, VAR == var, Biobank %in% bbs, GROUP == group) %>% dplyr::group_by(Endpoint) %>% reframe(N_bbs=n())

    for(endpt in unique(data$Endpoint)) {
      # Crnt endpt data
      crnt_data <- data %>% filter(VAR == var & Endpoint == endpt & Biobank %in% bbs & GROUP==group & !is.na(BETA))
      if(nrow(crnt_data) == 0) next # Breast or prostate cancer with sex
      # Checking has more than one biobank
      if(data_select %>% filter(Endpoint == endpt) %>% pull(N_bbs) > 1) {
          endpt_BETA <- c()
          for(bb in bbs) {
              endpt_BETA <- c(endpt_BETA, crnt_data %>% filter(Biobank == bb) %>% pull(BETA))
          }
          endpt_SE <- c()
          for(bb in bbs) {
              endpt_SE <- c(endpt_SE, crnt_data %>% filter(Biobank == bb) %>% pull(BETA_SE))
          }

          endpt_meta <- rma(yi=endpt_BETA, sei=endpt_SE, method=crnt_method)
          endpt_hr <- exp(endpt_meta$b[,1])
          endpt_ci <- get_CI(endpt_hr, endpt_meta$se)

          # For REML adding I2 and QHet results
          if(crnt_method == "REML") {
              meta <- tibble::add_row(meta, 
                                  Endpoint=endpt,
                                  SURV_MODEL=surv_model,
                                  VAR=var,
                                  GROUP=group,
                                  N_CONTROLS=sum(crnt_data %>% pull(N_CONTROLS)),
                                  N_CASES=sum(crnt_data %>% pull(N_CASES)),
                                  BETA=endpt_meta$b[,1],
                                  BETA_SE=endpt_meta$se,
                                  P_VAL=endpt_meta$pval,
                                  I2=confint(endpt_meta)$random[3,1],
                                  I2_CIneg=confint(endpt_meta)$random[3,2],
                                  I2_CIpos=confint(endpt_meta)$random[3,3],
                                  QHet=confint(endpt_meta)$random[4,1],
                                  QHet_CIneg=confint(endpt_meta)$random[4,2],
                                  QHet_CIpos=confint(endpt_meta)$random[4,3],
                                  Q_pval=endpt_meta$QEp,
                                  N_BBs=crnt_data  %>% pull(Biobank) %>% unique() %>% length(),
                                  HR=endpt_hr,
                                  CI_NEG=endpt_ci$neg,
                                  CI_POS=endpt_ci$pos,
                                  ENDPT_ORDER=endpt_names %>% filter(Endpoint == endpt) %>% pull(ENDPT_ORDER))        
          } else {
              meta <- tibble::add_row(meta, 
                                  Endpoint=endpt,
                                  SURV_MODEL=surv_model,
                                  VAR=var,
                                  GROUP=group,
                                  N_CONTROLS=sum(crnt_data %>% pull(N_CONTROLS)),
                                  N_CASES=sum(crnt_data %>% pull(N_CASES)),
                                  BETA=endpt_meta$b[,1],
                                  BETA_SE=endpt_meta$se,
                                  P_VAL=endpt_meta$pval,
                                  HR=endpt_hr,
                                  N_BBs=crnt_data %>% pull(Biobank) %>% unique() %>% length(),
                                  CI_NEG=endpt_ci$neg,
                                  CI_POS=endpt_ci$pos,
                                  ENDPT_ORDER=endpt_names %>% filter(Endpoint == endpt) %>% pull(ENDPT_ORDER)) 
          }        
      # If only one bb adding data to results - bug or feature? - anyways should be filtering for duplicates in the end
      } else {
              meta <- tibble::add_row(meta, 
                                      Endpoint=endpt,
                                      SURV_MODEL=surv_model,
                                      VAR=var,
                                     GROUP=group,
                                     N_CONTROLS=sum(crnt_data%>% pull(N_CONTROLS)),
                                     N_CASES=sum(crnt_data %>% pull(N_CASES)),
                                     BETA=crnt_data$BETA,
                                     BETA_SE=crnt_data$BETA_SE,
                                     P_VAL=crnt_data$P_VAL,
                                     HR=crnt_data$HR,
                                     N_BBs=1,
                                     CI_NEG=crnt_data$CI_NEG,
                                     CI_POS=crnt_data$CI_POS,
                                     ENDPT_ORDER=endpt_names %>% filter(Endpoint == endpt) %>% pull(ENDPT_ORDER))
      }
      meta$Biobank <- "Meta-analysis"
    }

    return(meta)
}

#'  meta analysis of c-indices same as for HRs
meta_cidx <- function(data, surv_model, bbs=c("FinnGen", "UKB", "EstB"), crnt_method="REML"){
    data <- dplyr::filter(data, SURV_MODEL == surv_model, Biobank %in% bbs)
    endpt_names <- readr::read_delim("/home/detrokir/Documents/DSGE/GitHub/onset_prediction/analysis/results/tables/endpt_name_maps/endpoint_name_map.csv")
    meta <- tibble(Endpoint=character(), SURV_MODEL=character(), N_CONTROLS=numeric(), N_CASES=numeric(), 
                   P_VAL=numeric(), I2=numeric(), I2_CIneg=numeric(), I2_CIpos=numeric(), QHet=numeric(), QHet_CIneg=numeric(), 
                   QHet_CIpos=numeric(), Q_pval=numeric(), N_BBs=numeric(), C_IDX=numeric(), SE=numeric(), C_IDX_CI_NEG=numeric(), C_IDX_CI_POS=numeric(), ENDPT_ORDER=numeric())    

    data_select <- dplyr::filter(data,Biobank %in% bbs) %>% dplyr::group_by(Endpoint) %>% reframe(N_bbs=n())

    for(endpt in unique(data$Endpoint)) {
      crnt_data <- data %>% filter(Endpoint == endpt & Biobank %in% bbs & !is.na(C_IDX))
      print(crnt_data)
      if(nrow(crnt_data) == 0 | all(crnt_data$SE == 0)) next
      if(data_select %>% filter(Endpoint == endpt) %>% pull(N_bbs) == 1) {
              meta <- tibble::add_row(meta, 
                                  Endpoint=endpt,
                                  SURV_MODEL=surv_model,
                                  N_CONTROLS=sum(crnt_data%>% pull(N_CONTROLS)),
                                  N_CASES=sum(crnt_data %>% pull(N_CASES)),
                                  C_IDX=crnt_data$C_IDX,
                                  SE=crnt_data$SE,
                                  P_VAL=crnt_data$P_VAL,
                                  HR=crnt_data$HR,
                                  N_BBs=1,
                                  C_IDX_CI_NEG=crnt_data$C_IDX_CI_NEG,
                                  C_IDX_CI_POS=crnt_data$C_IDX_CI_POS,
                                  ENDPT_ORDER=endpt_names %>% filter(Endpoint == endpt) %>% pull(ENDPT_ORDER))
      } else {
        endpt_BETA <- c()
        for(bb in bbs) {
            endpt_BETA <- c(endpt_BETA, crnt_data %>% filter(Biobank == bb) %>% pull(C_IDX))
        }
        endpt_SE <- c()
        for(bb in bbs) {
            endpt_SE <- c(endpt_SE, crnt_data %>% filter(Biobank == bb) %>% pull(SE))
        }

        endpt_meta <- rma(yi=endpt_BETA, sei=endpt_SE, method=crnt_method)
        endpt_ci <- get_CI(endpt_meta$b[,1], endpt_meta$se)

        if(crnt_method == "REML") {
            meta <- tibble::add_row(meta, 
                                Endpoint=endpt,
                                SURV_MODEL=surv_model,
                                N_CONTROLS=sum(data %>% filter(Endpoint == endpt) %>% pull(N_CONTROLS)),
                                N_CASES=sum(data %>% filter(Endpoint == endpt) %>% pull(N_CASES)),
                                P_VAL=endpt_meta$pval,
                                I2=confint(endpt_meta)$random[3,1],
                                I2_CIneg=confint(endpt_meta)$random[3,2],
                                I2_CIpos=confint(endpt_meta)$random[3,3],
                                QHet=confint(endpt_meta)$random[4,1],
                                QHet_CIneg=confint(endpt_meta)$random[4,2],
                                QHet_CIpos=confint(endpt_meta)$random[4,3],
                                Q_pval=endpt_meta$QEp,
                                N_BBs=data %>% filter(Endpoint == endpt) %>% pull(Biobank) %>% unique() %>% length(),
                                C_IDX=endpt_meta$b[,1],
                                SE=endpt_meta$se,
                                C_IDX_CI_NEG=endpt_ci$neg,
                                C_IDX_CI_POS=endpt_ci$pos,
                                ENDPT_ORDER=endpt_names %>% filter(Endpoint == endpt) %>% pull(ENDPT_ORDER))        
        } else {
            meta <- tibble::add_row(meta, 
                                Endpoint=endpt,
                                SURV_MODEL=surv_model,
                                N_CONTROLS=sum(data %>% filter(Endpoint == endpt) %>% pull(N_CONTROLS)),
                                N_CASES=sum(data %>% filter(Endpoint == endpt) %>% pull(N_CASES)),
                                P_VAL=endpt_meta$pval,
                                N_BBs=data %>% filter(Endpoint == endpt) %>% pull(Biobank) %>% unique() %>% length(),
                                C_IDX=endpt_meta$b[,1],
                                SE=endpt_meta$se,
                                C_IDX_CI_NEG=endpt_ci$neg,
                                C_IDX_CI_POS=endpt_ci$pos,
                                ENDPT_ORDER=endpt_names %>% filter(Endpoint == endpt) %>% pull(ENDPT_ORDER)) 
        }
      }
      
    
        meta$Biobank <- "Meta-analysis"
    }
          print(meta)

    return(meta)
}

#' Helper for plotting ads alterante strips to the data
strip_column <- function(data, 
                         includes_prs=FALSE, 
                         hrs=FALSE) {
  if("C_IDX" %in% colnames(data)) {
    data <- dplyr::filter(data, !is.na(C_IDX))
  } else {
    data <- dplyr::filter(data, !is.na(HR))
  }

  if("CRNT_ENDPT_ORDER" %in% colnames(data)) {
    data <- dplyr::select(data, -CRNT_ENDPT_ORDER, -stripe)
  }
  if(!hrs) {
    endpt_order <- dplyr::arrange(data, ENDPT_ORDER_BASE) %>% 
                    dplyr::select(Endpoint) %>%
                    distinct() %>% 
                    dplyr::mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
                    dplyr::mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))
  } else {
    endpt_order <- dplyr::filter(data, Biobank == "Meta-analysis") %>%
                    dplyr::arrange(HR) %>% 
                    dplyr::select(Endpoint) %>%
                    distinct() %>% 
                    dplyr::mutate(CRNT_ENDPT_ORDER=1:nrow(.)) %>%
                    dplyr::mutate(stripe = ifelse(CRNT_ENDPT_ORDER %% 2 == 0, "even", "odd"))
  }
  data <- dplyr::left_join(data, endpt_order, by="Endpoint") %>% dplyr::arrange(CRNT_ENDPT_ORDER, Biobank)
  if(("UKB" %in% data$Biobank) & includes_prs) {
    UKB_endpts <- dplyr::filter(data, Biobank == "UKB" & stringr::str_detect(SURV_MODEL, "PCs")) %>% dplyr::pull(Endpoint) %>% unique()
    print(UKB_endpts)

    if("C_IDX" %in% colnames(data)) {
        print(data)
        data <-  tibble::add_row(data, data %>% filter(Biobank == "FinnGen" & !(Endpoint %in% UKB_endpts)) %>% 
                                       dplyr::mutate(Biobank="UKB", C_IDX=NA, C_IDX_CI_POS=NA, C_IDX_CI_NEG=NA, SE=NA, N_CASES=NA, N_CONTROLS=NA) %>% 
                                       dplyr::distinct()) 
    }
    else
      data <-  tibble::add_row(data, data %>% filter(Biobank == "FinnGen" & !(Endpoint %in% UKB_endpts)) %>% 
                                       dplyr::mutate(Biobank="UKB", HR=NA, CI_NEG=NA, CI_POS=NA, BETA_SE=NA, N_CASES=NA, N_CONTROLS=NA) %>% 
                                       dplyr::distinct())
  }   
  return(data)
}

##################### THEMES ####################
theme_hrs <- function(base_size = 18,
                      legend_pos = "bottom",
                      plot_top_margin = -30,
                      axis_x_bottom_margin=0,
                      axis_y_left_margin=0,
                      legend_box_spacing=1,
                      line_size=1.5) {
    ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      text=ggplot2::element_text(colour="black"),
      # Titles
      plot.title=ggplot2::element_text(hjust=0, margin=margin(t=plot_top_margin, b=5), size=base_size),
      plot.subtitle=ggplot2::element_text(hjust=0, size=base_size*0.9,  margin=margin(t=plot_top_margin, b=5), face="bold"),
      plot.caption=ggplot2::element_text(size=base_size*0.6, hjust=0, margin=margin(t=10)),
      # Facet grid / wrap titles
      strip.text = ggplot2::element_text(hjust=0, face="bold", size=base_size*0.8, margin=margin(b=5)),
      # Legend
      legend.title=ggplot2::element_blank(),
      legend.position = legend_pos,
      legend.text=ggplot2::element_text(size=base_size*0.8),
      legend.key.spacing.y=grid::unit(0.5, "lines"),
      legend.margin = ggplot2::margin(legend_box_spacing, legend_box_spacing, legend_box_spacing, legend_box_spacing),
      # Axes
      axis.title=ggplot2::element_text(size=base_size*0.8),
      axis.text = ggplot2::element_text(size=base_size*0.75),
      axis.title.x = ggplot2::element_text(margin=margin(t=5, b=axis_x_bottom_margin)),
      axis.title.y = ggplot2::element_text(margin=margin(r=5), l=axis_y_left_margin, angle=90),
      # Other settings
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(colour=NA, fill=NA),
      # Grid settings
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour="black", linewidth=line_size-0.5*line_size, linetype=2),
      panel.grid.minor.x = ggplot2::element_line(colour = "black", linewidth=line_size-0.75*line_size, linetype=2),
    )
}

theme_custom_facet <- function(base_size = 18,
                      legend_pos = "bottom",
                      plot_top_margin = -30,
                      axis_x_bottom_margin=0,
                      axis_y_left_margin=0,
                      axis_y_right_margin=5,
                      legend_box_spacing=1,
                      line_size=1.5,
                      panel_bg="grey90") {
    ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      text=ggplot2::element_text(colour="black"),
      panel.background = element_rect(fill =panel_bg, color=panel_bg),
      # Titles
      plot.title=ggplot2::element_text(hjust=0, margin=margin(t=plot_top_margin, b=5), size=base_size),
      plot.subtitle=ggplot2::element_text(hjust=0, size=base_size*0.9,  margin=margin(t=plot_top_margin, b=5), face="bold"),
      plot.caption=ggplot2::element_text(size=base_size*0.6, hjust=0, margin=margin(t=10)),
      # Facet grid / wrap titles
      strip.text = ggplot2::element_text(face="bold", size=base_size*0.8, margin=margin(b=5)),
      # Legend
      legend.title=ggplot2::element_blank(),
      legend.position = legend_pos,
      legend.text=ggplot2::element_text(size=base_size*0.75),
      legend.key.spacing.y=grid::unit(0.5, "lines"),
      legend.margin = ggplot2::margin(legend_box_spacing, legend_box_spacing, legend_box_spacing, legend_box_spacing),
      # Axes
      axis.title=ggplot2::element_text(size=base_size*0.8),
      axis.text = ggplot2::element_text(size=base_size*0.75),
      axis.title.x = ggplot2::element_text(margin=margin(t=5, b=axis_x_bottom_margin)),
      axis.title.y = ggplot2::element_text(margin=margin(r=axis_y_right_margin, l=axis_y_left_margin), angle=90),
      # Other settings
      # Grid settings
      panel.grid.major.y = ggplot2::element_line(colour = "black", size = 0.5,linetype = 2),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),

      #axis.text.x=element_blank()
    )
}

theme_comp <- function(base_size = 18,
                       legend_pos = "bottom",
                       plot_top_margin = -30,
                       axis_x_bottom_margin=0,
                       axis_y_left_margin=0,
                       legend_box_spacing=1) {
    ggplot2::theme_minimal(base_size = base_size) %+replace%
    ggplot2::theme(
      text=ggplot2::element_text(colour="black"),
      # Titles
      plot.title=ggplot2::element_text(hjust=0, margin=margin(t=plot_top_margin, b=5), size=base_size),
      plot.subtitle=ggplot2::element_text(hjust=0, size=base_size*0.9,  margin=margin(t=plot_top_margin, b=5), face="bold"),
      plot.caption=ggplot2::element_text(size=base_size*0.5, hjust=0),
      # Facet grid / wrap titles
      strip.text = ggplot2::element_text(hjust=0, face="bold", size=base_size*0.8, margin=margin(b=5)),
      # Legend
      legend.title=ggplot2::element_blank(),
      legend.position = legend_pos,
      legend.text=ggplot2::element_text(size=base_size*0.75),
      legend.key.spacing.y=grid::unit(0.5, "lines"),
      legend.margin = ggplot2::margin(legend_box_spacing, legend_box_spacing, legend_box_spacing, legend_box_spacing),
      # Axes
      axis.title=ggplot2::element_text(size=base_size*0.8),
      axis.text = ggplot2::element_text(size=base_size*0.75),
      axis.title.x = ggplot2::element_text(margin=margin(t=5, b=axis_x_bottom_margin)),
      axis.title.y = ggplot2::element_text(margin=margin(r=5), l=axis_y_left_margin, angle=90),
      # Other settings
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(colour=NA, fill=NA),
      aspect.ratio=1
    )
}

plot_cidxs <- function(data,
                          bottom_model,
                          top_model,
                          shape_size=5,
                          line_size=1,
                          base_size=30,
                          color_list=get_color_list(),
                          max_increase_order=FALSE) {
  grey_width <- 0.5

  ### Making sure both models are present
  data <- data %>% dplyr::filter(SURV_MODEL %in% c(bottom_model, top_model)) 
  data <- data %>% group_by(Endpoint, Biobank, SURV_MODEL) %>% arrange(Endpoint, Biobank, SURV_MODEL, C_IDX) %>% slice(1L) %>% ungroup()
  if(max_increase_order) {
    endpt_order <- data %>% 
                          group_by(Endpoint, Biobank) %>% 
                          reframe(SUM_TOP_CIDX=sum(C_IDX_CI_NEG[SURV_MODEL == top_model]), 
                                  SUM_BOTTOM_CIDX=sum(C_IDX_CI_POS[SURV_MODEL == bottom_model]), 
                                  INCREASE_DIFFERENCE=SUM_TOP_CIDX-SUM_BOTTOM_CIDX) %>% 
                          ungroup() %>%
                          group_by(Endpoint) %>%
                          reframe(TOTAL_INCREASE_DIFFERENCE=sum(INCREASE_DIFFERENCE, na.rm=TRUE))  %>%
                          ungroup() %>%
                          arrange(desc(TOTAL_INCREASE_DIFFERENCE)) %>%
                          mutate(ENDPT_ORDER_BASE=1:n()) 
  } else {
    endpt_order <- all_cidx %>% 
                    filter(Biobank == "Meta-analysis", SURV_MODEL == lower_mdl) %>% 
                    select(Endpoint, C_IDX) %>% 
                    arrange(C_IDX) %>% 
                    distinct() %>% 
                    mutate(ENDPT_ORDER_BASE=1:nrow(.)) %>% 
                    select(Endpoint, ENDPT_ORDER_BASE)
  }
  data <- data %>% left_join(endpt_order, by="Endpoint") 
  data <- strip_column(data, includes_prs=ifelse(stringr::str_detect(bottom_model, "PRS") | stringr::str_detect(top_model, "PRS"), TRUE, FALSE), hrs=FALSE)
  data <- data %>% group_by(Endpoint, Biobank, SURV_MODEL) %>% arrange(Endpoint, Biobank, SURV_MODEL, C_IDX) %>% slice(1L) %>% ungroup()
  data$Biobank <- factor(data$Biobank, levels=c("FinnGen", "EstB",  "UKB","Meta-analysis"))
  data$SURV_MODEL <- factor(data$SURV_MODEL, levels=c(bottom_model, top_model))

  plt_b <- ggplot(data, aes(x=C_IDX, y=reorder(Endpoint, desc(CRNT_ENDPT_ORDER)), color=Endpoint)) +
          geom_point(aes(color=Endpoint), size=shape_size) +
          geom_linerange(aes(xmin=C_IDX_CI_NEG, xmax=C_IDX_CI_POS), linewidth=shape_size/4) +
          theme_hrs(base_size=base_size, line_size=line_size) +
          scale_color_manual(values=color_list) +
          scale_fill_manual(values = c("white", "grey50"), name="", guide="none") +
          labs(y="", x="C-index", fill="", caption=paste0(bottom_model, " -> ", top_model)) +
          guides(color="none", alpha="none") +
          scale_x_continuous(minor_breaks=c(0.6, 0.7, 0.8, 0.9, 1), breaks=c(0.5, 0.7, 0.9)) +
          geom_rect(aes(ymax = CRNT_ENDPT_ORDER-grey_width, ymin = CRNT_ENDPT_ORDER+grey_width, xmin = -Inf, xmax = Inf, fill=stripe), color=NA, alpha = 0.1) +
          geom_vline(xintercept=0.5, linetype=1, size=line_size-0.4*line_size) +
          facet_wrap(~Biobank, nrow=1) 
  plt_b
  # Calculate arrow endpoints
  scale_factor <- 0.01
  arrow_data <- data %>%
                  filter(SURV_MODEL %in% c(bottom_model, top_model)) %>%
                  group_by(Endpoint, CRNT_ENDPT_ORDER, Biobank) %>%
                  arrange(CRNT_ENDPT_ORDER, Biobank, desc(SURV_MODEL)) %>%
                  select(Endpoint, SURV_MODEL, C_IDX_CI_POS, C_IDX_CI_NEG) %>%
                  dplyr::mutate(C_IDX_CI_NEG=round(C_IDX_CI_NEG, 3), C_IDX_CI_POS=round(C_IDX_CI_POS, 3)) %>%
                  distinct() %>%
                  summarize(x_pos=C_IDX_CI_POS[2], next_x_neg=C_IDX_CI_NEG[1]) %>%
                  dplyr::filter((next_x_neg - x_pos > 0)) %>%
                  dplyr::mutate(x = x_pos + (scale_factor*(next_x_neg-x_pos)), next_x = next_x_neg - (scale_factor*(next_x_neg-x_pos))) %>%
                  dplyr::mutate(x=ifelse(next_x < x, x_pos, x), next_x=ifelse(x == x_pos, next_x_neg, next_x)) 
  # Add arrows
  plt_b_arrow <- plt_b + 
    geom_segment(data = arrow_data,
                aes(y = reorder(Endpoint, desc(CRNT_ENDPT_ORDER)), yend = reorder(Endpoint, desc(CRNT_ENDPT_ORDER)), x = x, xend = next_x),
                    arrow = arrow(type = "closed", length = unit(0.15, "inches")),
                    size = shape_size*0.1, color = "black")
  plt_b_arrow
  data <- data %>% dplyr::arrange(Endpoint, Biobank, SURV_MODEL) %>% dplyr::group_by(Endpoint, Biobank) %>% dplyr::filter(n() == 2) %>% ungroup()
  pvals <- calc_cidx_pval(data %>% filter(SURV_MODEL == bottom_model), 
                          data %>% filter(SURV_MODEL == top_model),
                          one_side = TRUE)
  # star_data <- data %>% select(Endpoint, Biobank) %>% distinct()
  # star_data$incr <- (pvals$pvals < (0.05 /(nrow(pvals)*3)))
  # star_data$pvals <- pvals$pvals
  # print(star_data)
  # star_data <- star_data %>%
  #                     dplyr::group_by(Endpoint) %>%
  #                     dplyr::reframe(sum_star=sum(incr, na.rm=TRUE))

  # if("FinnGen" %in% data$Biobank) {
  # star_data$Biobank <- "FinnGen"
  #   star_data$Biobank <- factor(star_data$Biobank, levels=c("FinnGen", "UKB", "EstB"))

  #   plt_star <- plt_b_arrow
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 1), aes(y=Endpoint, x=0.5), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 2), aes(y=Endpoint, x=0.495), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 2), aes(y=Endpoint, x=0.505), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 3), aes(y=Endpoint, x=0.49), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 3), aes(y=Endpoint, x=0.5), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 3), aes(y=Endpoint, x=0.51), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 4), aes(y=Endpoint, x=0.485), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 4), aes(y=Endpoint, x=0.495), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 4), aes(y=Endpoint, x=0.505), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   plt_star <- plt_star + geom_point(data=star_data %>% dplyr::filter(sum_star == 4), aes(y=Endpoint, x=0.515), shape = 8, size = shape_size*0.4, stroke=shape_size*0.35)
  #   return(plt_star)
  # } else {
  #   return(plt_b_arrow)
  # }
  return(plt_b_arrow)
}


########### PLOTTING FUNCTIONS ############
draw_var_meta_hrs <- function(data, 
                              var, 
                              model, 
                              limits = c(0.7, 2.5),
                              bbs=c("FinnGen", "UKB", "EstB"),
                              bb_colors=get_bb_colors(),
                              base_size=30) {
    hrs_data <- dplyr::filter(data, SURV_MODEL == model, VAR==var) %>% 
                    dplyr::mutate(highlight=ifelse(Biobank == "Meta-analysis", 1, 0.5)) 
    hrs_data <- strip_column(hrs_data, includes_prs=ifelse(var=="PRS", TRUE, FALSE), hrs=TRUE)
    hrs_data$Biobank <- factor(hrs_data$Biobank, levels=c(bbs, "Meta-analysis"))
    hrs_data <- hrs_data[order(hrs_data$Biobank),]

    if(limits[2] > 2) {
      if(limits[2] > 4) {
        x_list <- c(1, 2, 3, 4, 5, 6)
      } else {
        x_list <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
      }
    } else {
      x_list <- c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)
    }
    print(x_list)
    plt <- ggplot(hrs_data, 
                aes(x=HR, y=reorder(Endpoint, CRNT_ENDPT_ORDER), alpha=highlight, color=Biobank)) +
            geom_linerange(aes(xmin=CI_NEG, xmax=CI_POS), position=position_dodge(0.7), size=2, key_glyph=draw_key_rect) +
            # Scale manual
            scale_color_manual(values=c(bb_colors, color_pal_paper(N=1, highlight_position=1)), name="") +
            scale_fill_manual(values = c("white", "grey50"), name="", guide="none") +
            scale_alpha_continuous(range = c(0.7, 1.2), guide="none") +
            scale_x_continuous(breaks=x_list) +
            coord_cartesian(xlim=limits) + # Does not remove elements that are outside of limits
            # Stripes
            geom_rect(aes(ymax = CRNT_ENDPT_ORDER+0.5, ymin = CRNT_ENDPT_ORDER-0.5, xmin = -Inf, xmax = Inf, fill=stripe), color=NA, alpha = 0.1) +
            # Data points
            geom_point(aes(size=N_CASES), shape=18, position=position_dodge(0.7), key_glyph=draw_key_rect) +
            scale_size_continuous(range = c(4, 9), guide="none") +
            # Extra Lines
            geom_vline(xintercept=1, linetype=1, size=1) +
            # Theme
            labs(y="", x=paste0(var, " HRs (95% CI)"), title="", fill=NULL, caption=model) +
            theme_hrs(base_size=base_size) 

    return(plt)
}

draw_meta_cidx <- function(data, 
                              model, 
                              limits = c(0.5, 1),
                              bbs=c("FinnGen", "UKB", "EstB"),
                              bb_colors=get_bb_colors(),
                              base_size=26,
                              point_size=9,
                              line_size=1.9) {
print(data)
    cidx_data <- dplyr::filter(data, SURV_MODEL == model) %>% 
                    dplyr::mutate(highlight=ifelse(Biobank == "Meta-analysis", 1, 0.5)) 
    # Order by meta-analysis C-index
    endpt_order <- dplyr::filter(cidx_data, Biobank == "Meta-analysis") %>%
                    dplyr::arrange(C_IDX) %>% 
                    dplyr::select(Endpoint) %>%
                    dplyr::mutate(ENDPT_ORDER_BASE=1:nrow(.))
    cidx_data <- dplyr::left_join(cidx_data, endpt_order, by="Endpoint") 

    cidx_data <- strip_column(cidx_data)
    cidx_data$Biobank <- factor(cidx_data$Biobank, levels=c(bbs, "Meta-analysis"))
    cidx_data <- cidx_data[order(cidx_data$Biobank),]
    print(cidx_data)
    plt <- ggplot(cidx_data, 
                aes(x=C_IDX, y=reorder(Endpoint, CRNT_ENDPT_ORDER), alpha=highlight, color=Biobank)) +
            geom_linerange(aes(xmin=C_IDX_CI_NEG, xmax=C_IDX_CI_POS), position=position_dodge(0.7), size=line_size, key_glyph=draw_key_rect) +
            # Scale manual
            scale_color_manual(values=c(bb_colors, color_pal_paper(N=1, highlight_position=1)), name="") +
            scale_fill_manual(values = c("white", "grey50"), name="", guide="none") +
            scale_alpha_continuous(range = c(0.7, 1.2), guide="none") +
            scale_x_continuous(breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
            coord_cartesian(xlim=limits) + # Does not remove elements that are outside of limits
            # Stripes
            geom_rect(aes(ymax = CRNT_ENDPT_ORDER+0.5, ymin = CRNT_ENDPT_ORDER-0.5, xmin = -Inf, xmax = Inf, fill=stripe), color=NA, alpha = 0.1) +
            # Data points
            geom_point(aes(size=N_CASES), shape=18, position=position_dodge(0.7), key_glyph=draw_key_rect) +
            scale_size_continuous(range = c(point_size/2, point_size), guide="none") +
            # Extra Lines
            geom_vline(xintercept=0.5, linetype=1, size=line_size-0.3*line_size) +
            # Theme
            labs(y="", x=paste0("C-index (95% CI)"), title="", fill=NULL, caption=model) +
            theme_hrs(base_size=base_size) 

    return(plt)
}

color_pal_paper <- function(N=4, highlight_position=4) {
  highlight <- "#841C26"
  if(N == 5) {
    if(!is.na(highlight_position)) {
      rest <- c("#F5DBA3", "#CCB6AF", "#95B195", "#7D92A1")
      return(c(rest[1:highlight_position-1], highlight, rest[highlight_position:N]))
    } else {
      return(c("#EFB78C",  "#9088B5", "#A44A3F", "#297373","#68396A"))
    }
  } 
  if(N == 4) {
    if(is.na(highlight_position)) {
      return(c("#F5DBA3", "#CCB6AF", "#95B195", "#7D92A1"))
    } else {
      rest <- c("#CCB6AF", "#95B195", "#7D92A1")
      return(c(rest[1:highlight_position-1], highlight, rest[highlight_position:N]))
    }
  } 
  if(N == 3) {
    if(is.na(highlight_position)) {
      return(c("#CCB6AF", "#95B195", "#7D92A1"))
    } else {
      rest <- c("#CCB6AF", "#95B195")
      return(c(rest[1:highlight_position-1], highlight, rest[highlight_position:N]))
    }
  }
  if(N==1) {
    if(!is.na(highlight_position)) {
      return(highlight)
    } else {
      return("#CCB6AF")
    }
  }
if(N == 2) {
    if(is.na(highlight_position)) {
      rest <- c("#CCB6AF", "#95B195")
      return(rest)
    } 
  }
}

c("#F5DBA3", "#CCB6AF", "#95B195", "#7D92A1", "#841C26")

c("#CCB6AF", "#5E7382", "#841C26",  "#95B195" )