#' Initialise Empty Roster
#'
#' Creates an empty roster for population into three columns: Name, Present, and Experience. 
#'
#' @returns An empty dataframe with columns Name (Character), Present (Logical - TRUE/FALSE), Experience (Factor with 3 levels: Beginner, Intermediate, Advanced)
#' @export
#'
#' @examples
#' Roster <- create_roster()
#' Roster
create_roster <- function() {
  data.frame(
    Name = character(),
    Present = logical(),
    Experience = factor(levels = c("Beginner", "Intermediate", "Advanced")),
    stringsAsFactors = FALSE
  )
}



#' Add Student To Roster
#' 
#' Adds a new student to the roster with their name and experience level. 
#' 
#' The student is initially marked as absent. The updated roster is returned and sorted
#' alphabetically by name. Duplicate names are not allowed. 
#'
#' @param Roster A data frame representing the current student roster. It should contain the columns: Name, Present, and Experience.
#' @param Name A character string indicating the students name. Must not already exist in the roster.
#' @param Experience A character string indicating the student's experience level. Must be one of the following: Beginner, Intermediate, or Advanced.
#'
#' @returns A data frame representing the updated roster (Name and Experience level)
#' @export
#'
#' @examples
#' Roster <- create_roster()
#' Roster <- add_student(Roster, "Nicola", "Beginner")
#' Roster <- add_student(Roster, "Keith", "Intermediate")
#' Roster <- add_student(Roster, "John", "Advanced")
add_student <- function(Roster, Name, Experience = NA) {
  # check for duplicate name
  if (Name %in% Roster$Name) {
    stop(paste("Student", Name, "already in roster."))
  }
  new_entry <- data.frame(
    Name = Name,
    Present = FALSE,
    Experience = factor(Experience, levels = c("Beginner", "Intermediate", "Advanced"))
  )
  # sort alphabetically
  dplyr::arrange(rbind(Roster, new_entry), Name)
}



#' Remove Student From Roster
#' 
#' Removes a student by name from the roster.
#'
#' @param Roster A data frame representing the current student roster. It should contain the column Name.
#' @param Name A character string specifying the name of the student to remove.
#'
#' @returns A data frame representing the updated roster, with the specified student removed.
#' @export
#'
#' @examples
#' Roster <- create_roster()
#' Roster <- add_student(Roster, "Nicola", "Beginner")
#' Roster <- add_student(Roster, "Keith", "Intermediate")
#' Roster <- add_student(Roster, "John", "Advanced")
#' Roster <- remove_student(Roster, "Nicola") # dropped out
remove_student <- function(Roster, Name) {
  Roster[Roster$Name != Name, ]
}



#' Mark Student As Present
#' 
#' Sets present status to TRUE for list of students by name. 
#'
#' @param Roster A data frame representing the current student roster. Must contain columns Name and Present.
#' @param Names A character vector of student names to be marked present.
#'
#' @returns A data frame representing the updated roster with students marked as present.
#' @export
#'
#' @examples
#' Roster <- create_roster()
#' Roster <- add_student(Roster, "Nicola", "Beginner")
#' Roster <- add_student(Roster, "Keith", "Intermediate")
#' Roster <- add_student(Roster, "John", "Advanced")
#' Roster <- mark_present(roster, c("Nicola", "Keith"))
mark_present <- function(Roster, Names) {
  Roster$Present[Roster$Name %in% Names] <- TRUE
  Roster
}



#' Mark All Students Absent
#' 
#' Sets present status to FALSE for all students in roster. 
#' 
#' Useful at start of class when needing to reset roster.
#'
#' @param Roster A data frame representing the current student roster. Must contain a Present column.
#'
#' @returns A data frame with all students marked as absent.
#' @export
#'
#' @examples
#' Roster <- create_roster()
#' Roster <- add_student(Roster, "Nicola", "Beginner")
#' Roster <- add_student(Roster, "Keith", "Intermediate")
#' Roster <- add_student(Roster, "John", "Advanced")
#' Roster <- mark_all_absent(Roster)
mark_all_absent <- function(Roster) {
  Roster$Present <- FALSE
  Roster
}



#' Filter Roster For Present Students
#' 
#' Returns a subset of the roster including only students currently marked as present.
#'
#' @param Roster A data frame representing the current present student roster.
#'
#' @returns A data frame containing only rows where Present is TRUE.
#' @export
#'
#' @examples
#' Roster <- create_roster()
#' Roster <- add_student(Roster, "Nicola", "Beginner")
#' Roster <- add_student(Roster, "Keith", "Intermediate")
#' Roster <- add_student(Roster, "John", "Advanced")
#' Roster <- mark_present(roster, c("Nicola", "Keith"))
#' Present_Roster <- present_roster(Roster)
present_roster <- function(Roster) {
  Roster[Roster$Present == TRUE, ]
}