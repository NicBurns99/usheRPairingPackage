#' Convert pairings list to data frame
#'
#' Converts the output of create_pairs() into a data frame with the columns Week (integer), Group (integer), and Pairs (Character)
#'
#' @param pairings A list object returned from create_pairs()
#'
#' @returns A data frame with the columns Week, Group, and Pairs
#' @export
#'
#' @examples
#' df <- pairings_to_df(pairings)
#'
pairings_to_df <- function(pairings) {
  rows <- list() #list for accumulating rows of final df
  
  #Loop through list to extract each weeks data (1 to however many weeks etc)
  for (week in seq_along(pairings)) { # seq_along() - index for each week
    week_data <- pairings[[week]] # get groups for that week
    
    #Loop through each pair in each week and split students?
    for (group in seq_along(week_data)) { # loop each group in that week
        rows <- append(rows, list(data.frame(
          Week = week,
          Group = group,
          Pair = week_data[group],
          stringsAsFactors = F
        )))
      }
    }
  
  do.call(rbind.data.frame, rows) #stack rows list and converts to df
}