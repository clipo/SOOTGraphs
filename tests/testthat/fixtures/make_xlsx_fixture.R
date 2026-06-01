# Generates a small synthetic, PII-free SOOT XLSX fixture mirroring the real
# two-sheet structure (RawData + QuestionMapper). Run from this directory:
#   Rscript make_xlsx_fixture.R
library(writexl)

# Two instructor questions plus one non-instructor question that must be ignored,
# plus two context questions (interest before/after course).
mapper <- data.frame(
  Column = c("Question 1", "Question 2", "Question 3", "Question 4", "Question 5"),
  Question = c(
    "[InstructorName]-Instructor Effectiveness and Teaching Practices - The instructor is well prepared for class.",
    "[InstructorName]-Instructor Effectiveness and Teaching Practices - Overall, the instructor is an effective teacher.",
    "Year in School.",
    "Course Interest and Demands - My interest in subject before course",
    "Course Interest and Demands - My interest in subject after course"
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)

raw <- data.frame(
  CourseCode = rep("99999.202590", 4),
  CourseTitle = rep("Fall 2025 Test Course (TEST-101-01)", 4),
  InstructorName = rep("Test Instructor", 4),
  Enrollments = rep(20, 4),
  Respondents = rep(4, 4),
  ResponseRate = rep(20.0, 4),
  "Question 1" = c("5", "4", "4", ""),     # VeryHigh, High, High, no-response
  "Question 2" = c("5", "3", "0", "2"),    # VeryHigh, Average, NotApplicable, Low
  "Question 3" = c("2", "3", "4", "1"),    # Year in School - must be ignored
  "Question 4" = c("1", "1", "2", "2"),    # before: Low, Low, Medium, Medium
  "Question 5" = c("3", "3", "2", "3"),    # after:  High, High, Medium, High
  check.names = FALSE, stringsAsFactors = FALSE
)

write_xlsx(list(RawData = raw, QuestionMapper = mapper),
           path = "TEST101-01_FALL25.xlsx")
cat("wrote TEST101-01_FALL25.xlsx\n")
