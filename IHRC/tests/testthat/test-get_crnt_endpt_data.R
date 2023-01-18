test_that('get_crnt_endpt_data works', {
    endpts_indvs_mat <- data.frame(ID = c('a', 'b', 'c', 'd'),
                                  endpt1 = c(1, 0, 1, 0),
                                  endpt2 = c(0, 1, 0, 1))
    pheno_data <- data.frame(ID = c('a', 'b', 'c', 'd'),
                             age = c(20, 30, 40, 50),
                             sex = c('M', 'F', 'M', 'F'))

    endpt1 <- get_crnt_endpt_data(endpts_indvs_mat, 'endpt1', pheno_data)
    expect_endp1 <- data.frame(ID = c('a', 'c'),
                               age = c(20, 40),
                               sex = c('M', 'M'))
    #expect_equal(endpt1, expect_endp1)
    endpt2 <- get_crnt_endpt_data(endpts_indvs_mat, 'endpt2', pheno_data)
    expect_endp2 <- data.frame(ID = c('b', 'd'),
                               age = c(30, 50),
                               sex = c('F', 'F'))
    #expect_equal(endpt2, expect_endp2)
    expect_equal(get_crnt_endpt_data(NULL, 'endpt1', pheno_data),
                 pheno_data)
})
