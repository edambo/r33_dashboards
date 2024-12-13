#' Generate Stacked Pair Data
#'
#' @description Wrapper function that modifies the performance of the
#' fastlink::getMatches() function in order to extract and view matched pairs
#' between two data sets, including cases where matching was performed within
#' the same data set. This facilitates manual review of matches as well as
#' the assignment of unique subject IDs for pairs using the "stack_ids()"
#' function. Modified from the original version written by Dr. MBC.
#'   
#'   Dependencies: dplyr, fastLink
#'   
#'   Built: R (4.4.1); dplyr (1.1.4); fastlink (0.6.1); purrr 1.0.2
#'
#' @param dfA_pack A list containing the following named arguments for
#'                 data frame A in the fastLink fuzzy matching: 
#'                 $df (the original data frame A); $ids (list of identifiers
#'                 for data frame A, with a row-unique ID first); 
#'                 $suffix (a suffix to specify that a row came from 
#'                 data frame A, when reviewing stacked pairs)
#' @param dfB_pack A list containing the following named arguments for
#'                 data frame B in the fastLink fuzzy matching: 
#'                 $df (the original data frame B); $ids (list of identifiers
#'                 for data frame B, with a row-unique ID first); 
#'                 $suffix (a suffix to specify that a row came from 
#'                 data frame B, when reviewing stacked pairs)
#' @param match_vars A list or vector of variables utilized in the fastLink 
#'                   matching
#' @param fastlink_out A fastLink object created as output from fastLink 
#'                     matching 
#'
#' @return "pair_data", a data frame wherein each row represents a fastLink 
#'         match. These matches include posterior probability, the values of
#'         identifiers, and the contents of each match variable stacked for
#'         a "side by side" comparison. This includes a gamma value for the
#'         comparison of each variable value.
#' @export
#'

get_pair_data <- function(dfA_pack, dfB_pack, match_vars, fastlink_out){

  # =========================================================================
  # Check for Proper FastLink Input, Unpacking Sets
  # =========================================================================
  if (!("fastLink" %in% class(fastlink_out))) {
    stop('.x must be of class "fastLink"')
  }
  
  # Unpack all dfA values for stack creation
  dfA <- dfA_pack$df
  dfA_ids <- dfA_pack$ids
  dfA_vars <- c()
  
  # Unpack all dfB values for stack creation  
  dfB <- dfB_pack$df
  dfB_ids <- dfB_pack$ids
  dfB_vars <- c()
  
  # If dfA_pack and dfB_pack are identical....
  if (identical(dfA_pack, dfB_pack)){
    
    # Alter suffixes of matching variables to have "a" and "b", respectively
    dfA_suffix <- "a"
    dfB_suffix <- "b"
    # Rename ID and extra variables
    
  }
  # Otherwise, just use their packed suffixes
  if (!identical(dfA_pack, dfB_pack)){
    dfA_suffix <- dfA_pack$suffix
    dfB_suffix <- dfB_pack$suffix
  }
  
  
  # =========================================================================
  # Reduce DFs to Variables of Interest, Prep for Export
  # =========================================================================
  
  # Extract Match Variable Gammas, rename to match data set
  gammas <- fastlink_out$patterns
  
  colnames(gammas) <- paste(match_vars, "gamma", sep = "_")
  
  # Reduce Data Frames to desired columns only, use a "standard name" for
  # the row number/index value column name
  dfA <- dfA |> 
    dplyr::select(all_of(dfA_ids)) 
  
  colnames(dfA)[1] <- 'dfA_id'
  
  dfB <- dfB |>
    dplyr::select(dplyr::all_of(dfB_ids)) 
  
  colnames(dfB)[1] <- 'dfB_id'
  
  # Make list of desired variable names from each input data frame
  
  dfA_vars <- c('dfA_id')
  dfB_vars <- c('dfB_id')
  
  for (var in match_vars){
    dfA_vars <- c(dfA_vars, paste(var, dfA_suffix, sep="_"))
  }
  for (var in match_vars){
    dfB_vars <- c(dfB_vars, paste(var, dfB_suffix, sep="_"))
  }
  
  # Add any remaining desired variables listed in the packed data
  
  dfA_vars <- c(
    dfA_vars,
    paste(setdiff(colnames(dfA)[-1],match_vars), dfA_suffix, sep = "_")
  )
  
  dfB_vars <- c(
    dfB_vars,
    paste(setdiff(colnames(dfB)[-1],match_vars), dfB_suffix, sep = "_")
  )
  
  # Organize final output column names, for the extracted data
  
  col_order <- paste(rep(dfA_ids[1],2), c(dfA_suffix, dfB_suffix), sep = "_")
  
  col_order <- c('pair', 'posterior_probability', col_order)
  
  for (var in match_vars){
    col_order <- c(
      col_order, 
      paste(var, c(dfA_suffix, dfB_suffix, "gamma"), sep = "_")
    )
  }
  
  col_order <- c(
    col_order, 
    setdiff(dfA_vars[-1],col_order), 
    setdiff(dfB_vars[-1], col_order)
  )
  
  # =========================================================================
  # Create data frame of potential matches to compare
  # =========================================================================
  
  # Extract necessary variables (indices in both data sets for each pair, and
  # the pair's posterior probability)
  
  potential_matches <- tibble::tibble(
    row_a = fastlink_out$matches$inds.b,
    row_b = fastlink_out$matches$inds.a,
    posterior_probability = fastlink_out$posterior
  )
  
  potential_matches <- cbind(potential_matches, gammas)
  
  potential_matches <- potential_matches |>
    dplyr::mutate(
      combo = purrr::map2_chr(
        .x = row_a,
        .y = row_b,
        .f = function(x, y) {
          min <- min(x, y)
          max <- max(x, y)
          out <- paste(min, max, sep = "_")
          out
        }
      ),
      dup = duplicated(combo)
    ) |>
    dplyr::filter(!dup) |>
    dplyr::select(-combo, -dup)
  
  # Assign a number for each pair
  stacked_potential_matches <- tibble::tibble(
    row = c(rbind(
      potential_matches[["row_a"]], potential_matches[["row_b"]]
    )),
    pair = rep(seq(1, length(row) / 2), each = 2),
    posterior_probability = rep(
      potential_matches[["posterior_probability"]], each = 2
    )
  )
  
  # Re-add gammas
  stacked_potential_matches <- cbind(
    stacked_potential_matches,
    potential_matches |>
      dplyr::select(dplyr::all_of(ends_with("gamma"))) |>
      dplyr::mutate(row_num = row_number()) |>
      dplyr::slice(rep(row_number(), 2)) |>
      dplyr::arrange(row_num) |>
      dplyr::select(-row_num)
  )
  
  # Pivot to side-by-side comparison of values within pairs
  stacked_potential_matches <- stacked_potential_matches |> 
    dplyr::group_by(pair) |> 
    dplyr::mutate(
      row_a = row[1],
      row_b = row[2]
    ) |>
    dplyr::select(-row) |>
    dplyr::distinct() |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dfA_id = dfA$dfA_id[[row_a]],
      dfB_id = dfB$dfB_id[[row_b]]
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-row_a, -row_b)
  
  # =========================================================================
  # Add Variables of Interest to Pair Data Frame
  # =========================================================================    
  
  pair_data <- stacked_potential_matches
  
  for (i in 1:(length(dfA_ids)-1)){
    # Match dfA variable data for each match
    dfA_cols <- subset(dfA, select = c('dfA_id', dfA_ids[i+1]))
    names(dfA_cols)[2] <- paste(dfA_ids[i+1], dfA_suffix, sep = "_")
    pair_data <- dplyr::left_join(
      pair_data, dfA_cols, by='dfA_id', relationship = "many-to-one"
    )
  }
  for (i in 1:(length(dfB_ids)-1)){
    # Match dfB variable data for each match
    dfB_cols <- subset(dfB, select = c('dfB_id', dfB_ids[i+1]))
    names(dfB_cols)[2] <- paste(dfB_ids[i+1], dfB_suffix, sep = "_")
    pair_data <- dplyr::left_join(
      pair_data, dfB_cols, by='dfB_id', relationship = "many-to-one"
    )
  }
  
  # If dfA and dfB have the same name for their unique row ID variable, 
  # add "a" and "b" suffixes to original unique row id names
  if (identical(dfA_ids[1], dfB_ids[1])){
    dfA_vars[1] <- paste(dfA_ids[1], "a", sep = "_")
    dfB_vars[1] <- paste(dfB_ids[1], "b", sep = "_")
  }
  
  # Reordered the paired data
  pair_data <- pair_data |>
    rename_at(
      c('dfA_id', 'dfB_id'),
      ~c(dfA_vars[1], dfB_vars[1])
    )
  
  pair_data <- pair_data[, col_order]
  
  # =========================================================================
  # Return output
  # =========================================================================    
  
  pair_data
  
}