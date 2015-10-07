require(dplyr)
require(stringr)
require(rvest)
require(randomNames)
# Create Fake Data

# You'll want to create a an illuminater object as a base

# pull apart ill_obj ####
results <- ill_obj$results %>%
  filter(academic_year == 2016)


# reduce roster and schaffold new values ####
roster <- ill_obj$roster %>%
  filter(enroll_status == 0) %>% # enroll status
  mutate(new_lastfirst = randomNames(n=n(), gender = gender),
         new_first = str_extract(new_lastfirst, "\\w+$"),
         new_last = str_extract(new_lastfirst, "^\\w+"),
         new_student_number = 1000 + row_number())



schools <- roster %>%
  select(schoolid) %>%
  unique() %>%
  mutate(new_schoolid = row_number())

# let's get the presidents and randomly elect names for schools from them
wiki_pres <- "https://en.wikipedia.org/wiki/List_of_Presidents_of_the_United_States"

pres_html <- read_html(wiki_pres)

presidents <- pres_html %>%
  html_nodes("table.wikitable tr td b a") %>%
  html_text()
%>%
  data_frame(school_prefix = .)

school_suffixes <-  c("School", "Academy", "College Prep", "Institute")

school_names<-paste(sample(presidents,
                           size = nrow(schools)),
                    sample(school_suffixes,
                           size = nrow(schools),
                           replace = TRUE)
                    )

schools <- schools %>%
  mutate(new_school_name = school_names,
         new_school_initials = abbreviate(new_school_name,
                                     minlength = 3,
                                     strict = TRUE)
         )

# Same thing for homerooms
uni_wiki <- "https://en.wikipedia.org/wiki/List_of_research_universities_in_the_United_States"
uni_html <- read_html(uni_wiki)
unis <- uni_html %>%
  html_nodes("table.wikitable.sortable") %>%
  html_table %>%
  rbind_all

hrs <- roster %>%
  select(home_room) %>%
  unique()

hrs$new_home_room <- sample(unis$Institution, size=nrow(hrs), replace=FALSE)

# add new school name data and new homerooms to students and reduce to


roster_new <- roster %>%
  mutate(student_number = as.character(student_number)) %>%
  inner_join(hrs, by="home_room") %>%
  inner_join(schools, by="schoolid") %>%
  semi_join(results, by = c("student_number" = "local_student_id")) %>%
  select(lastfirst = new_lastfirst,
         first_name = new_first,
         last_name = new_last,
         student_number,
         enroll_status,
         grade_level,
         schoolid = new_schoolid,
         school_name = new_school_name,
         school_initials = new_school_initials,
         gender,
         ethnicity,
         home_room = new_home_room,
         new_student_number)

# Results ####
#need to change asm_schools then use that to regenerate local_assessment_ids
# need to also rewirte assessment name

results_new <- results %>%
  inner_join(roster_new %>%
               select(student_number,
                      new_student_number,
                      school_initials)
             , by=c("local_student_id" = "student_number")) %>%
  mutate(assm_school = ifelse(grepl("k\\w{2,3}", assm_school),
                              tolower(school_initials),
                              assm_school),
         local_assessment_id = ifelse(local_id_conforms & assm_type=="FSA",
                                      toupper(paste(assm_grade,
                                            assm_subj,
                                            assm_unit,
                                            assm_week,
                                            assm_school,
                                            sep = ".")),
                                      local_assessment_id
                                      ),
         local_assessment_id = ifelse(local_id_conforms & assm_type=="Unit",
                                      toupper(paste(assm_grade,
                                            assm_subj,
                                            assm_unit,
                                            assm_school,
                                            sep = ".")),
                                      local_assessment_id
                                      )
         ) %>%
  # now let's replace student ids and remove extraneous columns
  select(local_student_id = new_student_number,
         assessment_id.x:assm_name
         )

# now let's get roster_new looking lilke roster old by doing some replacement
# and dropping of the last unnecessary columns

roster_new2 <-roster_new %>%
  select(lastfirst:last_name,
         student_number = new_student_number,
         enroll_status:home_room)


#copy existing ill_obj to maintian classes, etc
ex_ill_obj <- ill_obj

# now write new results and roster into resp. slots

ex_ill_obj$roster <- roster_new2
ex_ill_obj$results <- results_new

#save to data
devtools::use_data(ex_ill_obj)



