library(glue)
library(gt)
library(writexl)
source("C:/Users/bfincham/Documents/project-root/R/Data_Cleaning.R")

setwd("C:/Users/bfincham/Documents/BoG Reports/Program Reviews/2025-2026")
output_dir <- "C:/Users/Documents/BoG Reports/Program Reviews/2025-2026" 

#########################################################
# Term depending data
ay_fall_term <- 202601

current_program_reviews <- c("ASSOCS", "AASMLTN", "AASHLIM", "CTTOPS") ## May also want to change code later that will updates
                                                                      ## program code of CTOPS to CTTOPS in all files.

fall_1 <- "202101"
spring_1 <- "202102"
summer_1 <- c("202103", "202104")
fall_2 <- "202201"
spring_2 <- "202202"
summer_2 <- c("202203", "202204")
fall_3 <- "202301"
spring_3 <- "202302"
summer_3 <- c("202303", "202304")
fall_4 <- "202401"
spring_4 <- "202402"
summer_4 <- c("202403", "202404")
fall_5 <- "202501"
spring_5 <- "202502"
summer_5 <- c("202503", "202504")
fall_6 <- "202601"

#########################################################
# Define census dates for each term to determine final enrollment
fall_1_wd_date <- as.Date("8/21/2020", format = "%m/%d/%Y")
fall_1_rs_wd_date <- as.Date("10/02/2020", format = "%m/%d/%Y")             
spring_1_wd_date <- as.Date("1/25/2021", format = "%m/%d/%Y")
spring_1_rs_wd_date <- as.Date("3/5/2021", format = "%m/%d/%Y")              
summer_10_1_wd_date <- as.Date("5/20/2021", format = "%m/%d/%Y")
summer_5_1_wd_date <- as.Date("6/24/2021", format = "%m/%d/%Y")


fall_2_wd_date <- as.Date("8/20/2021", format = "%m/%d/%Y")
fall_2_rs_wd_date <- as.Date("10/01/2021", format = "%m/%d/%Y")              
spring_2_wd_date <- as.Date("1/24/2022", format = "%m/%d/%Y")
spring_2_rs_wd_date <- as.Date("3/4/2022", format = "%m/%d/%Y")              
summer_10_2_wd_date <- as.Date("5/19/2022", format = "%m/%d/%Y")
summer_5_2_wd_date <- as.Date("6/24/2022", format = "%m/%d/%Y")

fall_3_wd_date <- as.Date("8/26/2022", format = "%m/%d/%Y")
fall_3_rs_wd_date <- as.Date("9/30/2022", format = "%m/%d/%Y")
spring_3_wd_date <- as.Date("1/23/2023", format = "%m/%d/%Y")
spring_3_rs_wd_date <- as.Date("3/3/2023", format = "%m/%d/%Y")
summer_10_3_wd_date <- as.Date("5/19/2023", format = "%m/%d/%Y")
summer_5_3_wd_date <- as.Date("6/23/2023", format = "%m/%d/%Y")

fall_4_wd_date <- as.Date("8/25/2023", format = "%m/%d/%Y")
fall_4_rs_wd_date <- as.Date("9/29/2023", format = "%m/%d/%Y")
spring_4_wd_date <- as.Date("1/22/2024", format = "%m/%d/%Y")
spring_4_rs_wd_date <- as.Date("3/1/2024", format = "%m/%d/%Y")
summer_10_4_wd_date <- as.Date("5/17/2024", format = "%m/%d/%Y")
summer_5_4_wd_date <- as.Date("6/21/2024", format = "%m/%d/%Y")

fall_5_wd_date <- as.Date("8/23/2024", format = "%m/%d/%Y")
fall_5_rs_wd_date <- as.Date("9/27/2024", format = "%m/%d/%Y")
spring_5_wd_date <- as.Date("1/27/2025", format = "%m/%d/%Y")
spring_5_rs_wd_date <- as.Date("3/7/2025", format = "%m/%d/%Y")
summer_10_5_wd_date <- as.Date("5/23/2025", format = "%m/%d/%Y")
summer_5_5_wd_date <- as.Date("6/27/2025", format = "%m/%d/%Y")

fall_6_wd_date <- as.Date("8/22/2025", format = "%m/%d/%Y")
fall_6_rs_wd_date <- as.Date("9/26/2025", format = "%m/%d/%Y")

####################################################
# Read in files
grads <- clean_csv(paste0(fall_1, "-", summer_5[2], "_GRADS.csv")) %>%
  mutate(
    ID = as.character(StudentID),
    Term = as.character(GraduationTerm),
    Program = case_when(
      Program == "CTOPS" ~ "CTTOPS",
      TRUE ~ toupper(Program)
    )
  ) %>%
  arrange(desc(Program)) %>%
  distinct(ID, Term, .keep_all = TRUE) %>%
  select(
    ID,
    Term,
    Program
  ) 

files <- list.files(
  path = "C:/Users/bfincham/Documents/BoG Reports/Program Reviews/2025-2026/SBT",
  pattern = "\\.csv$",
  full.names = TRUE
)

sbt_data_list <- lapply(files, function(x) {
  dat <- clean_csv(x)
  dat$source_file <- basename(x)
  dat
})

combined_sbt_data <- do.call("rbind", sbt_data_list) %>%
  mutate(
    Term = substr(source_file, start = 1, stop = 6)
  ) %>%
  distinct()

#########################################################
# mutate and select variables
sbt <- combined_sbt_data %>%
  mutate(
    ID = as.character(StudentID),
    StudentName = toupper(paste(LastName, FirstName, sep=", ")),
    TermAdmittedCode = term_to_code(TermAdmitted, ay_fall_term),
    PartsOfTerm = toupper(PartsOfTerm),
    TermGPA = as.numeric(TermGPA),
    FutureAwards = toupper(FutureAwards),
    Program = toupper(trimws(Program)),
    Program = ifelse(Program == "CTOPS", "CTTOPS", Program),
    Conc = toupper(Concentration),
    DW_Flag = toupper(TermInstStatus),
    WD_Date = as.Date(InstStatusDate, format = "%m/%d/%Y"),
    persistence_flag = if_else(
      TermGPA >= 2.0,
      TRUE,
      FALSE
    )
  ) %>%
  select(
    PartsOfTerm,
    ID,
    StudentName,
    StudentType,
    TermAdmittedCode,
    TermAdmitted,
    Program,
    Conc,
    TermGPA,
    DW_Flag,
    WD_Date,
    persistence_flag,
    FutureAwards,
    Term
  ) %>%
  mutate(
    TermYear = case_when(
      Term == fall_1 ~ "FALL 1",
      Term == spring_1 ~ "SPRING 1",
      Term %in% summer_1 ~ "SUMMER 1",
      Term == fall_2 ~ "FALL 2",
      Term == spring_2 ~ "SPRING 2",
      Term %in% summer_2 ~ "SUMMER 2",
      Term == fall_3 ~ "FALL 3",
      Term == spring_3 ~ "SPRING 3",
      Term %in% summer_3 ~ "SUMMER 3",
      Term == fall_4 ~ "FALL 4",
      Term == spring_4 ~ "SPRING 4",
      Term %in% summer_4 ~ "SUMMER 4",
      Term == fall_5 ~ "FALL 5",
      Term == spring_5 ~ "SPRING 5",
      Term %in% summer_5 ~ "SUMMER 5",
    )
  ) %>%
  filter(
    !is.na(Term)
  ) %>%
  distinct(ID, TermYear, .keep_all = TRUE)

#########################################################
#########################################################
# COHORT BASED DATA -- RETENTION ANALYSIS
#########################################################
# YEAR 1
#########################################################
fall_1_retention <- clean_csv(paste0(fall_1,"-extended.csv")) %>%
  rename(
         proceeding_spring = paste0("X",spring_1,"Reg"),
         proceeding_fall = paste0("X",fall_2,"Reg")
         ) %>%
  mutate(
    ID = as.character(StudentID),
    StudentName = toupper(paste(LastName, FirstName, sep=", ")),
    TermAdmittedCode = term_to_code(TermAdmitted, ay_fall_term),
    PartsOfTerm = toupper(PartsOfTerm),
    Program = case_when(
      Program == "CTOPS" ~ "CTTOPS",
      TRUE ~ toupper(Program)),
    Conc = toupper(Concentration),
    DW_Flag = toupper(TermInstStatus),
    WD_Date = as.Date(InstStatusDate, format = "%m/%d/%Y"),
    Term = fall_1
  ) %>%
  filter(
    Program %in% current_program_reviews,
    (PartsOfTerm != "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_1_wd_date)) |
      (PartsOfTerm == "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_1_rs_wd_date))
  ) %>%
  select(
    PartsOfTerm,
    ID,
    StudentName,
    StudentType,
    TermAdmittedCode,
    TermAdmitted,
    Program,
    Conc,
    FutureAwards,
    proceeding_spring,
    proceeding_fall,
    Term
  )

spring_1_sbt <- sbt %>%
  filter(
    Term == spring_1
  ) %>%
  select(
    ID,
    Program,
    Term
  )

fall_2_sbt <- sbt %>%
  filter(
    Term == fall_2
  ) %>%
  select(
    ID,
    Program,
    Term
  )

summer_1_grads <- grads %>%
  filter(
    Term %in% summer_1
  )


fall_1_extended <- fall_1_retention %>%
  left_join(spring_1_sbt, by = "ID", suffix = c("", ".sp")) %>%
  left_join(fall_2_sbt, by = "ID", suffix = c("",".f")) %>%
  left_join(grads, by = c("ID", "Term"), suffix = c("", ".f_grads")) %>%
  left_join(grads, by = c("ID", "Term.sp" = "Term"), suffix = c("", ".sp_grads")) %>%
  left_join(summer_1_grads, by = c("ID"), suffix = c("", ".su_grads"))

#########################################################
# YEAR 2
#########################################################
fall_2_retention <- clean_csv(paste0(fall_2,"-extended.csv")) %>%
  rename(
    proceeding_spring = paste0("X",spring_2,"Reg"),
    proceeding_fall = paste0("X",fall_3,"Reg")
  ) %>%
  mutate(
    ID = as.character(StudentID),
    StudentName = toupper(paste(LastName, FirstName, sep=", ")),
    TermAdmittedCode = term_to_code(TermAdmitted, ay_fall_term),
    PartsOfTerm = toupper(PartsOfTerm),
    Program = case_when(
      Program == "CTOPS" ~ "CTTOPS",
      TRUE ~ toupper(Program)),
    Conc = toupper(Concentration),
    DW_Flag = toupper(TermInstStatus),
    WD_Date = as.Date(InstStatusDate, format = "%m/%d/%Y"),
    Term = fall_2
  ) %>%
  filter(
    Program %in% current_program_reviews,
    (PartsOfTerm != "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_2_wd_date)) |
      (PartsOfTerm == "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_2_rs_wd_date))
  ) %>%
  select(
    PartsOfTerm,
    ID,
    StudentName,
    StudentType,
    TermAdmittedCode,
    TermAdmitted,
    Program,
    Conc,
    FutureAwards,
    proceeding_spring,
    proceeding_fall,
    Term
  )

spring_2_sbt <- sbt %>%
  filter(
    Term == spring_2
  ) %>%
  select(
    ID,
    Program,
    Term
  )

fall_3_sbt <- sbt %>%
  filter(
    Term == fall_3
  ) %>%
  select(
    ID,
    Program,
    Term
  )

summer_2_grads <- grads %>%
  filter(
    Term %in% summer_2
  )


fall_2_extended <- fall_2_retention %>%
  left_join(spring_2_sbt, by = "ID", suffix = c("", ".sp")) %>%
  left_join(fall_3_sbt, by = "ID", suffix = c("",".f")) %>%
  left_join(grads, by = c("ID", "Term"), suffix = c("", ".f_grads")) %>%
  left_join(grads, by = c("ID", "Term.sp" = "Term"), suffix = c("", ".sp_grads")) %>%
  left_join(summer_2_grads, by = c("ID"), suffix = c("", ".su_grads"))

#########################################################
# YEAR 3
#########################################################
fall_3_retention <- clean_csv(paste0(fall_3,"-extended.csv")) %>%
  rename(
    proceeding_spring = paste0("X",spring_3,"Reg"),
    proceeding_fall = paste0("X",fall_4,"Reg")
  ) %>%
  mutate(
    ID = as.character(StudentID),
    StudentName = toupper(paste(LastName, FirstName, sep=", ")),
    TermAdmittedCode = term_to_code(TermAdmitted, ay_fall_term),
    PartsOfTerm = toupper(PartsOfTerm),
    Program = case_when(
      Program == "CTOPS" ~ "CTTOPS",
      TRUE ~ toupper(Program)),
    Conc = toupper(Concentration),
    DW_Flag = toupper(TermInstStatus),
    WD_Date = as.Date(InstStatusDate, format = "%m/%d/%Y"),
    Term = fall_3
  ) %>%
  filter(
    Program %in% current_program_reviews,
    (PartsOfTerm != "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_3_wd_date)) |
      (PartsOfTerm == "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_3_rs_wd_date))
  ) %>%
  select(
    PartsOfTerm,
    ID,
    StudentName,
    StudentType,
    TermAdmittedCode,
    TermAdmitted,
    Program,
    Conc,
    FutureAwards,
    proceeding_spring,
    proceeding_fall,
    Term
  )

spring_3_sbt <- sbt %>%
  filter(
    Term == spring_3
  ) %>%
  select(
    ID,
    Program,
    Term
  )

fall_4_sbt <- sbt %>%
  filter(
    Term == fall_4
  ) %>%
  select(
    ID,
    Program,
    Term
  )

summer_3_grads <- grads %>%
  filter(
    Term %in% summer_3
  )


fall_3_extended <- fall_3_retention %>%
  left_join(spring_3_sbt, by = "ID", suffix = c("", ".sp")) %>%
  left_join(fall_4_sbt, by = "ID", suffix = c("",".f")) %>%
  left_join(grads, by = c("ID", "Term"), suffix = c("", ".f_grads")) %>%
  left_join(grads, by = c("ID", "Term.sp" = "Term"), suffix = c("", ".sp_grads")) %>%
  left_join(summer_3_grads, by = c("ID"), suffix = c("", ".su_grads"))

#########################################################
# YEAR 4
#########################################################
fall_4_retention <- clean_csv(paste0(fall_4,"-extended.csv")) %>%
  rename(
    proceeding_spring = paste0("X",spring_4,"Reg"),
    proceeding_fall = paste0("X",fall_5,"Reg")
  ) %>%
  mutate(
    ID = as.character(StudentID),
    StudentName = toupper(paste(LastName, FirstName, sep=", ")),
    TermAdmittedCode = term_to_code(TermAdmitted, ay_fall_term),
    PartsOfTerm = toupper(PartsOfTerm),
    Program = case_when(
      Program == "CTOPS" ~ "CTTOPS",
      TRUE ~ toupper(Program)),
    Conc = toupper(Concentration),
    DW_Flag = toupper(TermInstStatus),
    WD_Date = as.Date(InstStatusDate, format = "%m/%d/%Y"),
    Term = fall_4
  ) %>%
  filter(
    Program %in% current_program_reviews,
    (PartsOfTerm != "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_4_wd_date)) |
      (PartsOfTerm == "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_4_rs_wd_date))
  ) %>%
  select(
    PartsOfTerm,
    ID,
    StudentName,
    StudentType,
    TermAdmittedCode,
    TermAdmitted,
    Program,
    Conc,
    FutureAwards,
    proceeding_spring,
    proceeding_fall,
    Term
  )

spring_4_sbt <- sbt %>%
  filter(
    Term == spring_4
  ) %>%
  select(
    ID,
    Program,
    Term
  )

fall_5_sbt <- sbt %>%
  filter(
    Term == fall_5
  ) %>%
  select(
    ID,
    Program,
    Term
  )

summer_4_grads <- grads %>%
  filter(
    Term %in% summer_4
  )


fall_4_extended <- fall_4_retention %>%
  left_join(spring_4_sbt, by = "ID", suffix = c("", ".sp")) %>%
  left_join(fall_5_sbt, by = "ID", suffix = c("",".f")) %>%
  left_join(grads, by = c("ID", "Term"), suffix = c("", ".f_grads")) %>%
  left_join(grads, by = c("ID", "Term.sp" = "Term"), suffix = c("", ".sp_grads")) %>%
  left_join(summer_4_grads, by = c("ID"), suffix = c("", ".su_grads"))

#########################################################
# YEAR 5
#########################################################
fall_5_retention <- clean_csv(paste0(fall_5,"-extended.csv")) %>%
  rename(
    proceeding_spring = paste0("X",spring_5,"Reg"),
    proceeding_fall = paste0("X",fall_6,"Reg")
  ) %>%
  mutate(
    ID = as.character(StudentID),
    StudentName = toupper(paste(LastName, FirstName, sep=", ")),
    TermAdmittedCode = term_to_code(TermAdmitted, ay_fall_term),
    PartsOfTerm = toupper(PartsOfTerm),
    Program = case_when(
      Program == "CTOPS" ~ "CTTOPS",
      TRUE ~ toupper(Program)),
    Conc = toupper(Concentration),
    DW_Flag = toupper(TermInstStatus),
    WD_Date = as.Date(InstStatusDate, format = "%m/%d/%Y"),
    Term = fall_5
  ) %>%
  filter(
    Program %in% current_program_reviews,
    (PartsOfTerm != "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_5_wd_date)) |
      (PartsOfTerm == "ROLLING START" & (DW_Flag == "EL" | WD_Date > fall_5_rs_wd_date))
  ) %>%
  select(
    PartsOfTerm,
    ID,
    StudentName,
    StudentType,
    TermAdmittedCode,
    TermAdmitted,
    Program,
    Conc,
    FutureAwards,
    proceeding_spring,
    proceeding_fall,
    Term
  )

spring_5_sbt <- sbt %>%
  filter(
    Term == spring_5
  ) %>%
  select(
    ID,
    Program,
    Term
  )

fall_6_sbt <- sbt %>%
  filter(
    Term == fall_6
  ) %>%
  select(
    ID,
    Program,
    Term
  )

summer_5_grads <- grads %>%
  filter(
    Term %in% summer_5
  )


fall_5_extended <- fall_5_retention %>%
  left_join(spring_5_sbt, by = "ID", suffix = c("", ".sp")) %>%
  left_join(fall_6_sbt, by = "ID", suffix = c("",".f")) %>%
  left_join(grads, by = c("ID", "Term"), suffix = c("", ".f_grads")) %>%
  left_join(grads, by = c("ID", "Term.sp" = "Term"), suffix = c("", ".sp_grads")) %>%
  left_join(summer_5_grads, by = c("ID"), suffix = c("", ".su_grads"))


#########################################################
# COMBINE ALL YEARS INTO PRIMARY DATASET
#########################################################
cohort_based_data <- bind_rows(fall_1_extended, fall_2_extended, fall_3_extended, fall_4_extended, fall_5_extended) %>%
  distinct()

#########################################################
#########################################################
# TOTAL ENROLLMENT DATA -- INDIVIDUAL SEMESTER DATA
#########################################################
total_enrollment <- sbt %>%
  filter(
    Program %in% current_program_reviews,
    (Term == fall_1 & 
       (PartsOfTerm != "ROLLING START" &
       (DW_Flag == "EL" | WD_Date > fall_1_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > fall_1_rs_wd_date))) |
      (Term == spring_1 & 
         (PartsOfTerm != "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_1_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_1_rs_wd_date))) |
      (Term %in% summer_1 & 
         (Term == summer_1[1] &
            (DW_Flag == "EL" | WD_Date > summer_10_1_wd_date)) |
         (Term == summer_1[2] &
            (DW_Flag == "EL" | WD_Date > summer_5_1_wd_date))) |
      (Term == fall_2 & 
         (PartsOfTerm != "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > fall_2_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > fall_2_rs_wd_date))) |
      (Term == spring_2 & 
         (PartsOfTerm != "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_2_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_2_rs_wd_date))) |
      (Term %in% summer_2 & 
         (Term == summer_2[1] &
            (DW_Flag == "EL" | WD_Date > summer_10_2_wd_date)) |
         (Term == summer_2[2] &
            (DW_Flag == "EL" | WD_Date > summer_5_2_wd_date))) |
      (Term == fall_3 & 
         (PartsOfTerm != "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > fall_3_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > fall_3_rs_wd_date))) |
      (Term == spring_3 & 
         (PartsOfTerm != "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_3_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_3_rs_wd_date))) |
      (Term %in% summer_3 & 
         (Term == summer_3[1] &
            (DW_Flag == "EL" | WD_Date > summer_10_3_wd_date)) |
         (Term == summer_3[2] &
            (DW_Flag == "EL" | WD_Date > summer_5_3_wd_date))) |
      (Term == fall_4 & 
         (PartsOfTerm != "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > fall_4_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > fall_4_rs_wd_date))) |
      (Term == spring_4 & 
         (PartsOfTerm != "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_4_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_4_rs_wd_date))) |
      (Term %in% summer_4 & 
         (Term == summer_4[1] &
            (DW_Flag == "EL" | WD_Date > summer_10_4_wd_date)) |
         (Term == summer_4[2] &
            (DW_Flag == "EL" | WD_Date > summer_5_4_wd_date))) |
      (Term == fall_5 & 
         (PartsOfTerm != "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > fall_5_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > fall_5_rs_wd_date))) |
      (Term == spring_5 & 
         (PartsOfTerm != "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_5_wd_date)) |
         (PartsOfTerm == "ROLLING START" &
            (DW_Flag == "EL" | WD_Date > spring_5_rs_wd_date))) |
      (Term %in% summer_5 & 
         (Term == summer_5[1] &
            (DW_Flag == "EL" | WD_Date > summer_10_5_wd_date)) |
         (Term == summer_5[2] &
            (DW_Flag == "EL" | WD_Date > summer_5_5_wd_date)))
  ) %>%
  select(-c("DW_Flag", "WD_Date", "PartsOfTerm")) %>%
  distinct(ID, Term, .keep_all = TRUE)

total_enrollment_grads <- total_enrollment %>%
  left_join(grads, by = c("ID", "Term"), suffix = c("", ".grad"))

#########################################################
#########################################################
# CALCULATE METRICS
#########################################################
term_order <- c(
  "202101","202102","202103","202104",
  "202201","202202","202203","202204",
  "202301","202302","202303","202304",
  "202401","202402","202403","202404",
  "202501","202502","202503","202504"
)

annual_undup_enrollment <- total_enrollment %>%
  filter(Term != "202601") %>%
  mutate(
    review_year = case_when(
      Term == fall_1 | Term == spring_1 | Term %in% summer_1 ~ "Year 1",
      Term == fall_2 | Term == spring_2 | Term %in% summer_2 ~ "Year 2",
      Term == fall_3 | Term == spring_3 | Term %in% summer_3 ~ "Year 3",
      Term == fall_4 | Term == spring_4 | Term %in% summer_4 ~ "Year 4",
      Term == fall_5 | Term == spring_5 | Term %in% summer_5 ~ "Year 5"
    )
  ) %>%
  group_by(review_year) %>%
  distinct(ID, .keep_all = TRUE) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = review_year, values_from = n)

semester_enroll <- total_enrollment %>%
  filter(
    Term != "202601"
  ) %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

first_time <- total_enrollment %>%
  filter(
    Term != "202601"
  ) %>%
  filter(StudentType == "N") %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

persistence <- total_enrollment %>%
  filter(
    persistence_flag == TRUE,
    Term != "202601"
  ) %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

spring_retained_program <- cohort_based_data %>%
  filter(
    (!is.na(proceeding_spring) & Program.sp == Program) | 
      (Program.f_grads == Program)
  ) %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

fall_retained_program <- cohort_based_data %>%
  filter(
    (!is.na(proceeding_fall) & Program.f == Program) | 
      (Program.f_grads == Program | Program.sp_grads == Program | 
         Program.su_grads == Program)
  ) %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

spring_retained_college <- cohort_based_data %>%
  filter(
    !is.na(proceeding_spring) | !is.na(Program.f_grads)
  ) %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

fall_retained_college <- cohort_based_data %>%
  filter(
    !is.na(proceeding_fall) | !is.na(Program.f_grads) | !is.na(Program.sp_grads) | 
      !is.na(Program.su_grads)
  ) %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

graduated_wn_major <- total_enrollment_grads %>%
  filter(
    Program.grad == Program
  ) %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

graduated_wn_college <- total_enrollment_grads %>%
  filter(
    !is.na(Program.grad)
  ) %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

undpulicated_graduates <- total_enrollment_grads %>%
  filter(!is.na(Program.grad)) %>%
  distinct(ID, .keep_all = TRUE) %>%
  group_by(Program, Term) %>%
  count(Program) %>%
  ungroup() %>%
  pivot_wider(names_from = Term, values_from = n) %>%
  select(Program, any_of(term_order))

#########################################################
#########################################################
# WRITE DATA TO CSV OR XLSX
#########################################################
write.csv(cohort_based_data, "cohort_based_data.csv", row.names = FALSE)
write.csv(total_enrollment, "total_enrollment.csv", row.names = FALSE)