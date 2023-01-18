#' Creates a list of all possible combinations of the selected scores
#' 
#' @param score_type A character (vector). The score types to be used for the analysis.
#' 
#' @export 
#' 
#' @examples score_types <- get_all_possible_score_type_combs(c("PRS", "CCI", "PheRS"))
#'           print(score_types)
get_all_possible_score_type_combs <- function(score_type) {
    score_type_combs <- list()    
    score_type_combs[[1]] <- score_type
    score_type_combs[[2]] <- ""
    n = 3
    while(n <= length(score_type)) {
        score_type_combs[n] <- score_type[n]
        n = n + 1
    }

    if(length(score_type) >= 2) {
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

    if(length(score_type) >= 3) {
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

    return(score_type_combs)
}

#' Title
#' @export 
get_score_types <- function(score_type,
                            score_type_combos=FALSE) {
    if(score_type_combos) {
        score_types <- get_all_possible_score_type_combs(score_type)
    } else {
        score_types <- list()
        score_types[[1]] <- score_type
    }
    return(score_types)
}