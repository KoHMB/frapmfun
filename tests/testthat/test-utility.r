context("test")

library(tidyverse)
library(spict)
library(JABBA)
devtools::load_all()

test_that("test", {
    
    data_both <- get_bothres(res_spict, res_jabba)
    expect_equal(is.data.frame(data_both), TRUE)

    gg <- plot_both(res_spict, res_jabba)
    expect_equal(length(gg),2)
  
})
