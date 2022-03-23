# =============================================================================
# Ingérer les fichiers modifiés
# =============================================================================

# -----------------------------------------------------------------------------
# Définir la fonction
# -----------------------------------------------------------------------------

# ingérer les fichiers modifiés

#' Ingérer les fichiers modifiés
#' 
#' @param dir Caractère. Répertoire du fichier
#' @param file Caractère. Nom du fichier, sans extension
#' 
#' @importFrom readxl read_excel
#' @importFrom rlang global_env
update_from_excel <- function(
    dir,
    file
) {

    assign(
        x = file,
        value = readxl::read_excel(paste0(dir, file, ".xlsx")),
        envir = rlang::global_env()
    )

}

# -----------------------------------------------------------------------------
# Exécuter la fonction
# -----------------------------------------------------------------------------

purrr::walk(
    .x = c("to_reject_ids", "to_reject_issues", "to_reject_api"),
    .f = ~ update_from_excel(dir = output_dir, file = .x)
)

# =============================================================================
# Lancer les rejets sur les
# =============================================================================

# post comments for interviews to reject
susoreview::post_comments(
    df_to_reject = to_reject_ids,
    df_issues = to_reject_issues
)

# implement rejection with rejection message
purrr::pwalk(
    .l = to_reject_api,
    .f = susoreview::reject_interview,
    statuses_to_reject = statuses_to_reject,
    workspace = workspace
)
