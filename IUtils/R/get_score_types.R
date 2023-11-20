#' Get list of score types for the analysis
#' 
#' Uses `IUtils::get_all_possible_score_type_combs`, if `create_score_combos`
#' is set to TRUE and otherwise creates a list of length 2 with the
#' `score_type` and an empty vector.
#' 
#' @param score_type A string (vector). The score types to be used for the analysis.
#' @param create_score_combos A boolean, whether or not to create all possible
#'                          score type combinations from the score_type vector.
#'                          see function [IUtils::get_all_possible_score_type_combs].
#' 
#' @return A list. The score type combinations to be run in the analyses.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_score_types <- function(score_type,
                            create_score_combos=FALSE,
                            bunch_phenos=FALSE,
                            no_covs=FALSE) {
    if(create_score_combos & !bunch_phenos) {
        score_types <- get_all_possible_score_type_combs(score_type, no_covs)
    } else{
        score_types <- list()
        score_types[[1]] <- score_type # Full model
        if(!no_covs)
            score_types[[2]] <- "" # Baseline model
    }
    if(bunch_phenos) {
        crnt_idx <- 3
        # Check if full pheno model not already in list
        full_pheno <- score_type[score_type != "PRS"]
        if(!all(full_pheno == score_types[[1]])) {
            score_types[[crnt_idx]] <- full_pheno
            crnt_idx <- crnt_idx + 1
        } 
        score_types[[crnt_idx]] <- score_type[!(score_type %in% c("PRS", "PheRS"))] # Full pheno model
        crnt_idx <- crnt_idx + 1
        if("PRS" %in% score_type) {
            if(!all("PRS" == score_types[[1]])) {
                score_types[[crnt_idx]] <- "PRS" # PRS model
                crnt_idx <- crnt_idx + 1
            }
        }
        if("PheRS" %in% score_type) {
            if(!all("PheRS" == score_types[[1]])) {
                score_types[[crnt_idx]] <- "PheRS" # PheRS model
                crnt_idx <- crnt_idx + 1
            }
        }
        if("PheRS_transfer" %in% score_type) {
            if(!all("PheRS_transfer" == score_types[[1]])) {
                score_types[[crnt_idx]] <- "PheRS_transfer" # PheRS_transfer model
                crnt_idx <- crnt_idx + 1
            }
            score_types[[crnt_idx]] <- c("PheRS", "PheRS_transfer") # Both PheRS models
        }
    }
    return(score_types)
}

#' Creates a list of all possible combinations of the selected scores
#' 
#' @param score_type A string (vector). The score types to be used for the analysis.
#' 
#' @return A list. The score type combinations to be run in the analyses.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
#' 
#' @examples score_types <- get_all_possible_score_type_combs(c("PRS", "CCI", "PheRS"))
get_all_possible_score_type_combs <- function(score_type,
                                              no_covs=FALSE) {
    score_type_combs <- list()    
    score_type_combs[[1]] <- score_type
    if(!no_covs)
        score_type_combs[[2]] <- ""
    n = 3

    while(n <= length(score_type)+2) {
        score_type_combs[n] <- score_type[n-2]
        n = n + 1
    }
    if(length(score_type) > 2) {
        m = 1
        while(m <= length(score_type)) {
            i = m + 1
            while(i <= length(score_type)) {
                score_type_combs[[n]] <- c(score_type[m], score_type[i])
                i = i + 1
                n = n + 1
            }
            m = m + 1
        }
    }

    if(length(score_type) > 3) {
        m = 1
        while(m <= length(score_type)) {
            i = m + 1
            while(i <= length(score_type)) {
                j = i + 1
                while(j <= length(score_type)) {
                    score_type_combs[[n]] <- c(score_type[m], score_type[i], score_type[j])
                    j = j + 1
                    n = n + 1
                }
                i = i + 1
            }
            m = m + 1
        }
    }
    if(length(score_type) > 4) {
        m = 1
        while(m <= length(score_type)) {
            i = m + 1
            while(i <= length(score_type)) {
                j = i + 1
                while(j <= length(score_type)) {
                    k = j + 1
                    while(k <= length(score_type)) {
                        score_type_combs[[n]] <- c(score_type[m], score_type[i], score_type[j], score_type[k])
                        k = k + 1
                        n = n + 1
                    }
                    j = j + 1
                }
                i = i + 1
            }
            m = m + 1
        } 
    }
    if(length(score_type) > 5) {
        m = 1
        while(m <= length(score_type)) {
            i = m + 1
            while(i <= length(score_type)) {
                j = i + 1
                while(j <= length(score_type)) {
                    k = j + 1
                    while(k <= length(score_type)) {
                        l = k + 1 
                        while(l <= length(score_type)) {
                            score_type_combs[[n]] <- c(score_type[m], score_type[i], score_type[j], score_type[k], score_type[l])
                            l = l +1
                            n = n + 1
                        }
                        k = k +1
                    }
                    j = j + 1
                }
                i = i + 1
            }
            m = m + 1
        } 
    }
    if(length(score_type) > 6) {
        writeLines("No implementation of number of score types >= 6 for score type combos.")
    }

    return(score_type_combs)
}