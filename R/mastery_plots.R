#' Plot a student-level master grid for a unit's worth of tests
#'
#' @param results illuminate results from an illuminater object
#' @param roster a roster to filter by
#' @param school_id a school id
#' @param school the name of the school with the matching id
#' @param ... arguments passed to \code{\link[dplyr]{filter}}
#'
#' @return a \code{ggolot2 object}
#' @export
mastery_grid_plot <- function(results,
                              roster,
                              school_id,
                              school_name,
                              ...) {


  assm_results3 <- results %>%
    dplyr::mutate(local_student_id = as.integer(local_student_id)) %>%
    dplyr::inner_join(roster,
               by = c("local_student_id" = "student_number")
    ) %>%
    dplyr::filter_(sprintf("schoolid == %s", school_id),
                   "points_possible > 0") %>% #removes extra_credit
    dplyr::filter(...)

  assm_out <- assm_results3 %>%
    dplyr::mutate(Mastered = percent_correct >= 80,
           Standard_text = paste0(stringr::str_wrap(description.x,
                                                    width=15),
                                  "\n(",
                                  custom_code,
                                  ")"
                                  ),
           label = paste0(round(percent_correct,0),
                        "% (",
                        points,
                        "/",
                        points_possible,
                        ")"
           ),
           student_number = local_student_id
    )

  stopifnot(nrow(assm_out) > 0 )

  test_pct_correct_by_student<- assm_out %>%
    dplyr::group_by(student_number, lastfirst, local_assessment_id) %>%
    dplyr::summarize(points_possible = sum(points_possible),
                     points = sum(points),
                     percent_correct = points/points_possible * 100,
                     Standard_text = "Total Items Correct") %>%
    dplyr::mutate(Mastered = percent_correct >= 80,
                  label = paste0(round(percent_correct, 0),
                                 "% (",
                                 points,
                                 "/",
                                 points_possible,
                                 ")"
                  ),
                  schoolid = school_id
    )

  totals_by_student <- assm_out %>%
    dplyr::group_by(student_number, lastfirst) %>%
    dplyr::summarize(points_possible = n(),
              points = sum(as.integer(mastered)),
              percent_correct = points/points_possible * 100,
              assm_name = "Objectives\nMastered\nby Student",
              Standard_text = "Mastered") %>%
    dplyr::mutate(Mastered = percent_correct >= 80,
           label = paste0(round(percent_correct, 0),
                        "% (",
                        points,
                        "/",
                        points_possible,
                        ")"
           ),
           schoolid = school_id
    )

  assm_out_2<-dplyr::rbind_all(list(assm_out,
                                    totals_by_student))

  totals_by_standard <- assm_out_2 %>%
    dplyr::group_by(Standard_text, assm_name) %>%
    dplyr::summarize(points_possible=n(),
              points=sum(as.integer(mastered), na.rm=TRUE),
              percent_correct=points/points_possible*100
    ) %>%
    dplyr::mutate(lastfirst="Students Mastery \nof Objective",
           schoolid=0,
           Mastered=percent_correct>=80,
           label=paste0(round(percent_correct,0),
                        "% (",
                        points,
                        "/",
                        points_possible,
                        ")"
           )
    )


  test_pct_correct_by_student<- assm_out %>%
    dplyr::group_by(student_number, lastfirst, local_assessment_id, assm_name) %>%
    dplyr::summarize(points_possible = sum(points_possible),
                     points = sum(points),
                     percent_correct = points/points_possible * 100,
                     Standard_text = "Total Items Correct") %>%
    dplyr::mutate(Mastered = percent_correct >= 80,
                  label = paste0(round(percent_correct, 0),
                                 "% (",
                                 points,
                                 "/",
                                 points_possible,
                                 ")"
                  ),
                  schoolid = school_id
    )


  avg_test_scores <- assm_out_2 %>%
    dplyr::group_by(assm_name) %>%
    dplyr::summarize(points_possible = sum(points_possible, na.rm = TRUE),
                     points = sum(points, na.rm=TRUE),
                     percent_correct = points/points_possible*100
                     ) %>%
    dplyr::mutate(lastfirst = "Students Mastery \nof Objective",
                  schoolid = 0,
                  Mastered = percent_correct >= 80,
                  Standard_text = "Total Items Correct",
                  label = paste0(round(percent_correct,0),
                               " Avg")
                  )

  assm_out3 <- dplyr::rbind_list(assm_out_2,
                                 totals_by_standard,
                                 test_pct_correct_by_student,
                                 avg_test_scores) %>%
    dplyr::mutate_(School = sprintf("ifelse(schoolid == %s, '%s', 'Mastery')",
                           school_id,
                           school_name)
                   )

  assm_date <- assm_out3 %>%
    dplyr::mutate(administered_at=ifelse(assm_name=="Objectives\nMastered\nby Student",
                                  as.character(lubridate::today()),
                                  administered_at),
                  date=lubridate::ymd(administered_at),
                  week_number = gsub(".+Week (\\d+).+", "\\1", assm_name),
                  week_number = ifelse(grepl("Unit$", week_number),
                                       99,
                                       week_number),
                  week_number = ifelse(grepl("Objectives", week_number),
                                       999,
                                       week_number),
                  week_number = as.integer(week_number)
    )  %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::group_by(assm_name) %>%
    dplyr::summarize(week_number=min(week_number)) %>%
    dplyr::arrange(week_number)

  student_order <- test_pct_correct_by_student %>%
    dplyr::group_by(student_number) %>%
    dplyr::mutate(week_number = gsub(".+Week (\\d+).+", "\\1", assm_name),
                  week_number = ifelse(grepl("Unit$", week_number),
                                       99,
                                       week_number),
                  week_number = as.integer(week_number)
                  ) %>%
    dplyr::filter(week_number == max(week_number)) %>%
    ungroup() %>%
    dplyr::mutate(rank_by = ifelse(week_number == max(week_number),
                                   percent_correct,
                                   120), # <- missing last week scores are given
                                         # highest score then ranked at bottom
                  stu_rank = dplyr::row_number(-rank_by)) %>%
    dplyr::arrange(stu_rank)


    # get standaards in order
  objective_order <- assm_out %>%
    dplyr::select(Standard_text, custom_code) %>%
    unique() %>%
    dplyr::arrange(custom_code)


    assm_out4 <- assm_out3 %>%
    dplyr::mutate(lastfirst = factor(as.character(lastfirst),
                                     levels = c(as.character(
                                       student_order$lastfirst),
                                       "Students Mastery \nof Objective")
                                     ),
          Name = factor(as.character(assm_name),
                        levels = as.character(assm_date$assm_name)),
           pct_correct_cat = cut(percent_correct,right = FALSE,
                                 breaks = c(0,60,70,80,90,100),
                                 include.lowest = TRUE),
          Standard_text = factor(Standard_text,
                                 levels = c(objective_order$Standard_text,
                                            "Total Items Correct",
                                            "Mastered")
                                   ),
          week_number = gsub(".+Week (\\d+).+", "\\1", assm_name),
          week_number = ifelse(grepl("Unit$", assm_name),
                               "100",
                               week_number),
          week_number = ifelse(grepl("Objectives", assm_name),
                               "1000",
                               week_number),
          week_number = as.integer(week_number),
           most_recent = week_number == max(week_number,na.rm = TRUE))

    # create color scale functions
    mastery_cols <- c('#ff001a',
                  '#ffbf42',
                  '#fefe56',
                  '#91fd57',
                  '#00ca3f')

    mastery_cols_fn <- scales::col_factor(palette = mastery_cols,
                                          domain = unique(assm_out4$pct_correct_cat)
                                          )

    pct_correct_cols<-c("#f7f7f7","#cccccc","#969696","#636363","#252525")

    pct_correct_cols_fn <- scales::col_factor(palette = pct_correct_cols,
                                              domain = unique(assm_out4$pct_correct_cat)
                                              )


    assm_out5 <- assm_out4 %>%
      dplyr::mutate(mastery_color =
                      ifelse(Standard_text == "Total Items Correct",
                             "gray66",
                             mastery_cols_fn(pct_correct_cat)
                             ),
                    text_color =
                      ifelse(Standard_text == "Total Items Correct",
                             mastery_cols_fn(pct_correct_cat),
                             "gray33"),
                    label = stringr::str_replace(label, "\\s", "\n"))




  p2<-ggplot(assm_out5,
             aes(x=Standard_text,
                 y=lastfirst)) +
    geom_tile(aes(fill=mastery_color),
              color="white",
              alpha=.8) +
    geom_text(aes(label=label,
                  color=text_color),
              size=2.25
              ) +
    facet_grid(School~Name, scales = "free", space =  "free") +
#     scale_fill_manual(values = c('#ff001a',
#                                  '#ffbf42',
#                                  '#fefe56',
#                                  '#91fd57',
#                                  '#00ca3f')) +
    scale_fill_identity() +
    scale_color_identity() +
    theme_bw() +
    theme(axis.text.y = element_text(size=4.5),
          axis.text.x = element_text(size=4.5),
          strip.text.x = element_text(size=5),
          strip.text.y=element_text(size=5, angle=0)) +
    xlab("KIPP Chicago Objective") +
    ylab("")


  p2



}