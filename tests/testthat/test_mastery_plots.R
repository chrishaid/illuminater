require(dplyr)

data("ex_ill_obj")


p <- mastery_long_plot(ex_ill_obj$results,
                  ex_ill_obj$roster %>%
                    filter(school_initials == "BCS",
                           grade_level == 8),
                  school_id = 4,
                  school_name = "BCS",
                  assm_subj == "math",
                  assm_unit == "u1")

p_build <- ggplot_build(p)

test_that("mastery_long_plot generates a plot", {
  expect_is(p, c("gg", "ggplot"))
  expect_equal(sum(p_build$data[[1]]$ymax), 71.5)
})