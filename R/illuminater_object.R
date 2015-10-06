#' Title
#'
#' @param ill_con an
#' @param roster either a data frame with a student roster (incuding demographic)
#' inormation) or and `src_illuminater` object that defines a connection to
#' your data warehouse
#' @param roster_table_name if you are using a database connection then the name of
#' the table in the warehouse with student roster data.
#' @param ...
#'
#' @return an illuminater object containing a roster file and illuminate assesssment
#' results
#'
#' @export
illuminater <- function(ill_con,
                        roster,
                        roster_table_name = NULL, ...) UseMethod("illuminater")

#' @export
illuminater.default <- function(ill_con, roster, roster_table_name, ...) {

  prog <- dplyr::progress_estimated(n = 9, min_time = 2)

  assessments <- dplyr::tbl(ill, "assessments")
  prog$tick()$print()

  results <- dplyr::tbl(ill, "assessment_results_by_standard")
  prog$tick()$print()

  assm_results <- results %>%
    dplyr::inner_join(assessments, by="assessment_id")
  prog$tick()$print()


  # get data from local_assessment_id 7.SCI.U2.W1.KCCP
  assm_results_collected <- dplyr::collect(assm_results)
  prog$tick()$print()

  regex_fsa <- "^\\d\\.[a-z]+\\.u\\d\\.w\\d\\.[a-z]{3,4}$"
  regex_unit <- "^\\d\\.[a-z]+\\.u\\d\\.[a-z]{3,4}$"

  assm_results2 <- assm_results_collected %>%
    dplyr::mutate(local_assessment_id =
                    tolower(stringr::str_trim(local_assessment_id)),
                  local_id_conforms = (grepl(regex_fsa, local_assessment_id) |
                                         grepl(regex_unit, local_assessment_id)
                                               ),
                  assm_type = ifelse(grepl(regex_fsa,
                                           local_assessment_id
                                           ),
                                     "FSA",
                                     NA
                                     ),
                  assm_type = ifelse(grepl(regex_unit,
                                           local_assessment_id
                                           ),
                                     "Unit",
                                     assm_type
                                     ),
                  assm_grade = stringr::str_extract(local_assessment_id, "^\\d"),
                  assm_subj = stringr::str_extract(local_assessment_id, "[a-z]+"),
                  assm_unit = stringr::str_extract(local_assessment_id, "u\\d"),
                  assm_week = stringr::str_extract(local_assessment_id, "w\\d"),
                  assm_week = ifelse(is.na(assm_week) & assm_type == "Unit", 99, assm_week),
                  assm_school = stringr::str_extract(local_assessment_id, "[a-z]+$"),
                  assm_name = sprintf("%s %s Unit %s, Week %s\nType: %s",
                                      assm_grade,
                                      gsub("\\b([a-z])([a-z]+)", "\\U\\1\\E\\2",assm_subj, perl = TRUE),
                                      stringr::str_extract(assm_unit, "\\d"),
                                      stringr::str_extract(assm_week, "\\d"),
                                      assm_type
                                      ),
                  assm_name = ifelse(assm_type == "Unit",
                                     sprintf("%s %s Unit %s\nType: %s",
                                             assm_grade,
                                             gsub("\\b([a-z])([a-z]+)", "\\U\\1\\E\\2",assm_subj, perl = TRUE),
                                             stringr::str_extract(assm_unit, "\\d"),
                                             assm_type),
                                      assm_name
                                      )
                  )
  prog$tick()$print()


  if(inherits(roster, "src")) {
    students <- dplyr::collect(dplyr::tbl(roster, roster_table_name))
  } else {
    students <- roster
  }
  prog$tick()$print()

  names(students) <- tolower(names(students))
  prog$tick()$print()



  out <- list(roster = students,
              results = assm_results2)
  prog$tick()$print()

  class(out) <- "illuminater"
  prog$tick()$print()

  # return
  out
}


#' @export
is.illuminater <- function(x) inherits(x, "illuminater")