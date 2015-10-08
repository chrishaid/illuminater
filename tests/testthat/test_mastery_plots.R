require(dplyr)

data("ex_ill_obj")


p_long <- mastery_long_plot(ex_ill_obj$results,
                  ex_ill_obj$roster %>%
                    filter(school_initials == "BCS",
                           grade_level == 8),
                  school_id = 4,
                  school_name = "BCS",
                  assm_subj == "math",
                  assm_unit == "u1")

p_long_build <- ggplot_build(p_long)


p_grid <- mastery_grid_plot(ex_ill_obj$results,
                            ex_ill_obj$roster %>%
                              filter(school_initials == "BCS",
                                     grade_level == 8,
                                     home_room == "University of Oregon"),
                            school_id = 4,
                            school_name = "BCS",
                            assm_subj == "math",
                            assm_unit == "u1")

p_grid_build <- ggplot_build(p_grid)




test_that("mastery_long_plot generates a plot", {
  expect_is(p_long, c("gg", "ggplot"))
  expect_equal(sum(p_long_build$data[[1]]$ymax), 71.5)
})


test_that("mastery_grid_plot generates a plot", {
  expect_is(p_grid, c("gg", "ggplot"))
  expect_equal(sum(p_grid_build$data[[1]]$ymax), 3877.5)
})
