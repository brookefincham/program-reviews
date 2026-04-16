
# program-reviews
Code and process for regular BOG program reviews at the CTC level.
---
title: "Spring 2026 BoG Program Reviews"
author: "Brooke Fincham"
date: "`r Sys.Date()`"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

source("C:/Users/bfincham/Documents/BoG Reports/Program Reviews/2025-2026/Program Reviews v3.R")
```

## Argos Reports Needed:
* Student by Term (SBT) -- Reports for Fall, Spring, Summer 10 Week, and Summer 5 Week for Academic Years 2021 - 2025, and report for Fall 2025 were downloaded.
    + Fall SBT reports included columns to check enrollment in the proceeding Spring and Fall semesters.
* Awards by Term -- Report for Fall, Spring, Summer 10 Week, and Summer 5 Week for Academic Years 2021 - 2025 was downloaded.

## Main Process
### Primary Term Enrollment Data Frame
1. Set term-dependent information such as withdrawal dates.
    + For the purpose of this analysis, enrollment was considered as of the date following the add/drop period. This date was selected so that it would be consistent among all semesters and years. 
    + We considered the withdrawal dates for Full Term and Rolling Start for accuracy.
    
2. Read in all SBT files, cleaning the header row as needed.
    + Combine all SBT files into one primary data frame for easier manipulation.
    
3. Read in the Awards by Term report. 
    + We sort this file by descending Program then run `distinct(ID, Term, .keep_all = TRUE)`. This results in the highest degree awarded in one semester being kept. This step was included because many students receive a Certificate of Applied Science and an Associate of Applied Science in the same semester. In this case, the student's program is more than likely the A.A.S., which we will be joining on later Thus, we want to keep that record.
    
4. Because each record has the unique term code but the Program Review relies on the summer semesters being combined, we create a column to denote the Term (`FALL`, `SPRING` or `SUMMER`) and year (`1`, `2`, `3`, `4`, `5`). This combines both the 10-week and 5-week summer semesters.
    + We then use `distinct(ID, TermYear, .keep_all = TRUE)` so that only one summer record will be kept. This step is necessary because otherwise a student enrolled in the 10-week summer term and the 5-week summer term will be counted twice for the summer semester, which is not the purpose of the Program Review.
    + This primary data frame serves as the `total_enrollment' list.
    
5. We then filter this primary data frame to exclude students from each term that were not enrolled as of census.

6. We then join this on the graduates list by `ID` and `Term`. This results in a data frame that contains one row per student per semester with information such as their program of study that semester as well as the highest degree received that semester, if applicable.

### Primary Cohort Based Data Frame
Much of the Program Review analysis relies on tracking cohort persistence and retention. Thus, we create a data frame that would track Fall cohorts across the proceeding Spring and Fall semesters. We define persistence and retention as the following.
    + A student is defined as having **persisted** if they obtained  a `TermGPA` of at least 2.0 in the given semester.
    + A student is defined as having been **retained** if they enrolled in or graduated by the next semester in question.
    
1. One data frame was created per year, to track that Fall's cohort.

2. The appropriate Fall SBT report was read in, with columns indicating enrollment in the proceeding Spring and Fall semesters (this is an attribute that was selected in Argos during the report download).

3. We are interested not only if a student was retained within the college, but if they were also retained within the program of study.
    + To answer this, we look at the proceeding Spring and Fall SBT reports to obtain the program of study for those semesters.
    
4. Because retention also includes graduates, we need to include this in the cohort data frame as well.
    + Students that are retained in the proceeding Spring semester either enrolled in that Spring semester or graduated in the Fall semester. Thus we merge the graduate list on `ID` and `Term = fall_X` where `X` is the appropriate index of the term (`1`, `2`, `3`, `4`, or `5`). 
    + Students that are retained in the proceeding Fall semester either enrolled in that proceeding Fall semester or graduated in the current academic year (Fall, Spring, or Summer).
    + We merge the graduate list on `ID` and `Term = spring_X` or `Term = summer_10_X` or `Term = summer_5_X`.
  
5. These steps are repeated for the remaining years in the review period.

6. We append all five cohort data frames into one primary cohort data frame by calling
`cohort_based_data <- bind_rows(fall_1_extended, fall_2_extended, fall_3_extended, fall_4_extended, fall_5_extended) %>% distinct()`.

## Analyzing the Data
1. **Annual Unduplicated Enrollment**
    + Count the number of distinct students within each year of review.
      + We use the Term Codes to group the records into `Year 1`, `Year 2`, `Year 3`, `Year 4`, or `Year 5`. We then group by `Program` and `Year` to find the distinct count per year.
      + We filter out Term Code of the Fall semester in the current Academic Year because this was used only for validating Fall to Fall retention for the last year in the review period.
      
2. **Program Enrollment by Semester**
    + We follow a similar logic to counting the annual enrollment, except here we do not create flags for the Year and we group by `Program` and `Term` before aggregating.
    + We use the `pivot_wider()` function to switch the data from long to wide format, which matches the format in the report submitted to the BOG.
    + `term_order` is a list that contains all terms within the review period. Using `any_of(term_order)` ensures the terms are listed in the desired order and that the term only has to contain a non-empty value for at least Program to be included. For example, if only one program has a student enrolled in the Summer 5-week semester, this code will ensure that there is a column for that 5-week semester, in the order we desire.
 
3. **First-Time Students**
    + Here, we simply add a filter on `Student Type` to the code above. We maintain the grouping by `Program` and `Term`.
    
4. **Students Who Persisted**
    + In `total_enrollment`, we created a flag for persistence. If the record showed $\text{TermGPA} >= 2.0$, we set `persistence_flag = TRUE'. Otherwise, this flag was 'FALSE`.
    + Now, we add a filter on `persistence_flag`, then group by `Program` and `Term` and aggregate across these groups.
    
5.**Fall to Spring Retained Within Program**
    + For retention, we look at our cohort based data. Here, we are specifically interested in retention within the program. 
    + `!is.na(proceeding_spring)` checks for enrollment in the proceeding spring term. 
    + `Program.sp == Program` verifies that for those enrolled in the proceeding spring, they did not change their major. 
    + `Program.f_grads == Program` verifies that for those that graduated at the end of the fall term, they graduated within their major.
    + We keep the same groupings as before, aggregating across these groups, and switching to a wider data layout.
    
6.**Fall to Spring Retained Within College**
    + We use the same code as above, just removing the filter on Program.
    
7. **Fall to Fall Retained Within Program**
    + This process was very similar to the **Fall to Spring Retained Within Program**. The major difference is that we must check graduation across multiple semesters because a student could have graduated at any time within the the Academic Year to be considered retained.

8. **Fall to Fall Retained Within College**
    + Again, this is similar to the **Fall to Spring Retained Within College* just with additional filters to include all possible terms of graduation.
  
9. **Graduated Within Program**
    + This code is similar to the Retention Within Program code, except we do not check for enrollment in a proceeding term.
    
10. **Graduated from NRCTC**
    + Here, we adjust the filter to be `!is,na(Program.grad)`. This checks that the student graduated in general, and not that the degree received was in the specified program of study.
    
11. **Annual Graduates Within College**
    + This returns unduplicated counts from the previous counts of *Graduated from NRCTC*. We include this because students will often times receive a Certificate of Applied Science one semester as a stepping stone to their current Associate of Applied Science Degree. They will then receive the Associate of Applied Science in another semester. This portion removes that duplicate count.
