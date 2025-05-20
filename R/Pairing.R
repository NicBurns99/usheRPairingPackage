#' Initialise Empty Pairing History 
#'
#' Initialises an empty data frame to store historical pairings of students over classes, semesters, and weeks.
#' 
#' Initialises an empty pairing history. Essential for the start.
#'
#' @returns An empty data frame with the columns: Class, Semester, Week, Student1, Student2.
#' @export
#'
#' @examples
#' history <- create_pair_history()
create_pair_history <- function() {
  data.frame(
    Class = character(),
    Semester = integer(),
    Week = integer(),
    Student1 = character(), 
    Student2 = character(), 
    stringsAsFactors = FALSE
  )
}



#' Generate All Unique Student Pairs 
#' 
#' Generates all possible unique pairs (combinations of two) from a list of student names.
#' 
#' This is a helper function. Core logic to create all possible student pairs, which is then used by generate_session_pairs()
#'
#' @param present_names A character vector of student names currently present in class.
#'
#' @returns A data frame with columns Student1 and Student2.
#'
#' @examples
#' generate_all_pairs(present_students)
generate_all_pairs <- function(present_names) {
  pairs <- combn(sort(present_names), 2)
  data.frame(Student1 = pairs[1,], 
             Student2 = pairs[2,], 
             stringsAsFactors = FALSE)
}



#' Generate Weekly Session Pairs 
#' 
#' Generates student pairs (groups of 2) for a class session, avoiding repeats from previous sessions. Handles odd numbers of students with optional strategies.
#' 
#' This contains the core logic for generating valid, non-repeating pairs, which is then used by interactive_pairing()
#'
#' @param present_roster A data frame of present students (from `present_roster()`).
#' @param pair_history A data frame of historical pairings (from `create_pair_history()` and `commit_pairs_to_history()`).
#' @param Class A character string identifying the class/course code.
#' @param Semester An integer representing the semester number.
#' @param Week An integer representing the week number.
#' @param odd_student_strategy Strategy for handling an odd student out. Options: "group_of_3", "manual".
#'
#' @returns A data frame with columns Class, Semester, Week, Student1, Student2, and optional Student3.
#'
#' @examples
#' pairs <- generate_session_pairs(presentStudents, pair_history, "DATASCIENCE101", 1, 1, "group_of_3")
generate_session_pairs <- function(present_roster, pair_history, Class, Semester, Week,
                                   odd_student_strategy = c("group_of_3", "manual")) {
  
  odd_student_strategy <- match.arg(odd_student_strategy)
  present_names <- present_roster$Name
  
  all_pairs <- generate_all_pairs(present_names)
  
  # Remove previously used pairs for that class only
  class_history <- pair_history %>% 
    filter(Class == !!Class)
  used_pairs <- class_history %>% 
    select(Student1, Student2)
  # ID new unused pairs
  new_pairs <- anti_join(all_pairs, used_pairs, 
                         by = c("Student1", "Student2"))
  
  # If all pairs used, reset history
  if (nrow(new_pairs) == 0) {
    pair_history <- pair_history %>% 
      filter(Class != !!Class)  # clear only this class history
    new_pairs <- all_pairs
    message(paste("All pair combinations used for", Class, ": Resetting pair history for this class."))
  }
  
  # Shuffle new pairs for randomness
  new_pairs <- new_pairs[sample(nrow(new_pairs)), ]
  
  # Select non-overlapping pairs for the session
  session_pairs <- list()
  used_students <- c()
  
  for (i in seq_len(nrow(new_pairs))) {
    s1 <- new_pairs$Student1[i]
    s2 <- new_pairs$Student2[i]
    
    if (!(s1 %in% used_students || s2 %in% used_students)) {
      session_pairs <- append(session_pairs, list(c(s1, s2)))
      used_students <- c(used_students, s1, s2)
    }
  }
  
  # Handle odd student 
  leftover <- setdiff(present_names, used_students)
  
  if (length(leftover) == 1) {
    lonely <- leftover[[1]]
    # GROUP OF 3
    if (odd_student_strategy == "group_of_3") {
      # Add to one existing pairs randomly
      idx <- sample(length(session_pairs), 1)
      session_pairs[[idx]] <- c(session_pairs[[idx]], lonely)
    # MANUAL ASSIGNMENT
    } else if (odd_student_strategy == "manual") {
      cat("Unpaired student:", lonely, "\n")
      for (j in seq_along(session_pairs)) {
        cat(j, ":", paste(session_pairs[[j]], collapse = ", "), "\n")
      }
      idx <- as.integer(readline("Enter the group number (index) to add this student to: "))
      if (!is.na(idx) && idx >= 1 && idx <= length(session_pairs)) {
        session_pairs[[idx]] <- c(session_pairs[[idx]], lonely)
      } 
    }
  }
  
  # Convert session pairs to data frame
  session_df <- do.call(rbind, lapply(session_pairs, function(p) {
    data.frame(
      Class = Class,
      Semester = Semester,
      Week = Week,
      Student1 = p[1], 
      Student2 = p[2], 
      Student3 = ifelse(length(p) >= 3, p[3], NA),
      stringsAsFactors = FALSE
    )
  }))
  
  return(session_df)
}





#' Commit Session Pairs To Pair History
#' 
#' Appends the generated session pairings to the cumulative pairing history.
#' 
#' Clean separation of pairs generation and history update (for when we don't like a pairing ect.)
#'
#' @param pair_history Existing history data frame.
#' @param session_df New session pairings to add.
#'
#' @returns An updated pairing history data frame.
#'
#' @examples
#' pair_history <- commit_pairs_to_history(pair_history, session_df)
commit_pairs_to_history <- function(pair_history, session_df) {
rbind(pair_history, session_df)
}



#' Interactive Session Pairing Tool
#' 
#' Generates session pairs, shows them to user, and asks for confirmation. Repeats until accepted, then commits to pairing history once accepted.
#'
#' @param present_roster Data frame of present students.
#' @param pair_history Pairing history data frame.
#' @param Class Class identifier.
#' @param Semester Semester number.
#' @param Week Week number.
#' @param odd_student_strategy Strategy to handle odd student out: group of 3 or manual.
#'
#' @returns  list with two elements: student pairs (the confirmed session pairs), and history (the updated pairing history).
#' @export
#'
#' @examples
#' interactive_pairing(present_students, pair_history, "DATASCIENCE101", 1, 1, "group_of_3")
interactive_pairing <- function(present_roster, pair_history, Class, Semester, Week, odd_student_strategy) {
  repeat {
    # Step 1: Generate session pairs
    session_df <- generate_session_pairs(
      present_roster = present_roster,
      pair_history = pair_history,
      Class = Class,
      Semester = Semester,
      Week = Week,
      odd_student_strategy = odd_student_strategy
    )
    
    # Step 2: Show pAirs
    print(session_df)
    
    # Step 3: Ask user to confirm pairs
    answer <- readline("Do you want to accept these pairs? (y/n): ")
    
    if (tolower(answer) == "y") {
      # Step 4: Commit to history and return
      pair_history <- commit_pairs_to_history(pair_history, session_df)
      message("Pairs committed to history.")
      return(list(pairs = session_df, history = pair_history))
    } 
    else {
      message("Pairs discarded: Generating new set...")
    }
  }
}