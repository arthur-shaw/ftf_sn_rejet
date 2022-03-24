# =============================================================================
# Check that necessary objects exist
# =============================================================================

objects_needed <- c(
    # issues and attributes
    "attribs",
    "issues_plus_miss_and_suso",
    # to reject
    "to_reject_ids",
    "to_reject_issues",
    "to_reject_api",
    # to review
    "to_review_ids",
    "to_review_issues",
    "to_review_api"
    # to follow up

)

check_exists(objects_needed)

# =============================================================================
# Correct French accents in error messages
# =============================================================================

#' Correct French accents
#' 
#' Replace gibberish with correct Unicode accents
#' 
#' @param df Data frame.
#' @param msg_var Bare variable name.
#' 
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @importFrom rlang `{{`
correct_accents <- function(
    df,
    msg_var
) {

    dplyr::mutate(.data = df,
        {{msg_var}} := stringr::str_replace({{msg_var}}, "[0-9]+ questions ont", "XX questions ont"),
        {{msg_var}} := stringr::str_replace_all({{msg_var}}, pattern = "Ã©", replacement = "é"),
        {{msg_var}} := stringr::str_replace_all({{msg_var}}, pattern = "Ã¢", replacement = "â"),
        {{msg_var}} := stringr::str_replace_all({{msg_var}}, pattern = "Ã¨", replacement = "è"),
        {{msg_var}} := stringr::str_replace_all({{msg_var}}, pattern = "Ã", replacement = "à"),
        {{msg_var}} := stringr::str_replace_all({{msg_var}}, pattern = '"', replacement = "")    
    )

}

# to_reject
to_reject_api <- correct_accents(df = to_reject_api, msg_var = reject_comment)
to_reject_issues <- correct_accents(df = to_reject_issues, msg_var = issue_comment)

# to_review
to_review_api <- correct_accents(df = to_review_api, msg_var = reject_comment)
to_review_issues <- correct_accents(df = to_review_issues, msg_var = issue_comment)

# to_follow_up
to_follow_up_api <- correct_accents(df = to_follow_up_api, msg_var = reject_comment)
to_follow_up_issues <- correct_accents(df = to_follow_up_issues, msg_var = issue_comment)

# =============================================================================
# Write decisions to disk in Excel and Stata format
# =============================================================================

# -----------------------------------------------------------------------------
# Define function
# -----------------------------------------------------------------------------

write_to_excel_and_stata <- function(
    data,
    dir,
    name = deparse(substitute(data))
) {

    # Excel
    writexl::write_xlsx(x = data, path = paste0(dir, name, ".xlsx"), col_names = TRUE)

    # Stata
    # - first, truncate character length for Stata strings
    # - then, save truncated data
    data |>
    dplyr::mutate(
        dplyr::across(
            .cols = where(is.character),
            .fns = ~ stringr::str_trunc(.x, width = 500)
        )
    ) |>
    haven::write_dta(path = paste0(dir, name, ".dta"))

}

# -----------------------------------------------------------------------------
# Attributes
# -----------------------------------------------------------------------------

# data
write_to_excel_and_stata(
    data = attribs, 
    dir = output_dir, 
    name = "attributes"
)

# -----------------------------------------------------------------------------
# Issues
# -----------------------------------------------------------------------------

# data
write_to_excel_and_stata(
    data = issues_plus_miss_and_suso, 
    dir = output_dir, 
    name = "issues"
)

# -----------------------------------------------------------------------------
# To reject
# -----------------------------------------------------------------------------

purrr::walk2(
    .x = list(to_reject_ids, to_reject_issues, to_reject_api),
    .y = c("to_reject_ids", "to_reject_issues", "to_reject_api"),
    .f = ~ write_to_excel_and_stata(.x, dir = output_dir, name = .y)
)

# -----------------------------------------------------------------------------
# To review
# -----------------------------------------------------------------------------

purrr::walk2(
    .x = list(to_review_ids, to_review_issues, to_review_api),
    .y = c("to_review_ids", "to_review_issues", "to_review_api"),
    .f = ~ write_to_excel_and_stata(.x, dir = output_dir, name = .y)
)

# -----------------------------------------------------------------------------
# To follow up
# -----------------------------------------------------------------------------

purrr::walk2(
    .x = list(to_follow_up_ids, to_follow_up_issues, to_follow_up_api),
    .y = c("to_follow_up_ids", "to_follow_up_issues", "to_follow_up_api"),
    .f = ~ write_to_excel_and_stata(.x, dir = output_dir, name = .y)
)

