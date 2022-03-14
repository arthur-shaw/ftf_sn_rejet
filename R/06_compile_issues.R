# =============================================================================
# Check that necessary objects exist
# =============================================================================

objects_needed <- c(
    "attribs",
    # "calories_par_item",
    "suso_errors"
)

check_exists(objects_needed)

# =============================================================================
# Load necessary libraries
# =============================================================================

library(susoreview)
library(haven)
library(purrr)
library(rlang)

# =============================================================================
# Flag errors
# =============================================================================

# -----------------------------------------------------------------------------
# Mesures anthro sans décimale
# -----------------------------------------------------------------------------

issue_anthro_femme_sans_decimale <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "w_sans_decimale",
    where = w_sans_decimale == TRUE,
    type = 1,
    desc = "Mesure anthro femme sans décimale",
    comment = paste0(
        "ERREUR: Mesure anthro pour une femme sans décimale. ",
        "Les mesures anthro doivent être renseignées avec la précision d'une place après la virgule (eg, 8,0 au lieu de 8) ",
        "Pour au moins une des femmes mesurées, l'une des mesures n'a pas de décimale. ",
        "Veuillez corriger la mesure. Si la décimale est absente, veuillez l'ajouter ou l'insérer."
    )
)

issue_anthro_femme_sans_decimale_comm <- susoreview::make_issue_in_roster(
    df = anthro_sans_decimale,
    where = w_sans_decimale == TRUE,
    roster_vars = "PERSONS__id",
    type = 2,
    desc = "Mesure anthro femme sans décimale",
    comment = "Au moins un des mesures pour cette femme n'a pas la précision nécessaire / une place après la décimale.",
    issue_vars = "v406_1"
)

issue_anthro_enfant_sans_decimale <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "c_sans_decimale",
    where = c_sans_decimale == TRUE,
    type = 1,
    desc = "Mesure anthro enfant sans décimale",
    comment = paste0(
        "ERREUR: Mesure anthro pour un enfant sans décimale. ",
        "Les mesures anthro doivent être renseignées avec la précision d'une place après la virgule (eg, 8,0 au lieu de 8) ",
        "Pour au moins un des enfants mesurés, l'une des mesures n'a pas de décimale. ",
        "Veuillez corriger la mesure. Si la décimale est absente, veuillez l'ajouter ou l'insérer."
    )
)

issue_anthro_enfant_sans_decimale_comm <- susoreview::make_issue_in_roster(
    df = anthro_sans_decimale,
    where = c_sans_decimale == TRUE,
    roster_vars = "PERSONS__id",
    type = 2,
    desc = "Mesure anthro enfant sans décimale",
    comment = "Au moins un des mesures pour cet enfant n'a pas la précision nécessaire / une place après la décimale.",
    issue_vars = "v516_1"
)

# -----------------------------------------------------------------------------
# Consommation alimentaire
# -----------------------------------------------------------------------------

# aucune consommation alimentaire au sein du ménage
issue_no_home_food_cons <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "repas_a_domicile",
    where = repas_a_domicile == 0,
    type = 1,
    desc = "Aucune consommation alimentaire au ménage",
    comment = paste0(
        "ERREUR: Aucune consommation alimentaire déclarée. ",
        "Le ménage n'a pas consommé des aliments au sein du ménage. ",
        "Ceci est très peu probable. ",
        "Veuillez confirmer à nouveau toutes les questions de la section 8.1."
    )
)

# aucune consommation alimentaire--ni au sein ni en dehors du ménage
issue_no_food_cons <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("repas_a_domicile", "repas_dehors_menage"),
    where = repas_a_domicile == 0 & repas_dehors_menage == 0,
    type = 1,
    desc = "Aucune consommation alimentaire",
    comment = paste0(
        "ERREUR: Aucune consommation alimentaire déclarée. ",
        "Le ménage n'a consommé aucune alimentation--ni au sein ni en dehors du ménage. ",
        "C'est impossible. ",
        "Veuillez confirmer à nouveau la consommation au sein du ménage (8.1 - REPAS PRIS....) et en dehors du ménage (8.1 CONSOMMATION ALIMENTAIRE)."
    )
)

# -----------------------------------------------------------------------------
# Consommation non-alimentaire
# -----------------------------------------------------------------------------

# aucune consommation non-alimentaire
issue_no_non_food_cons <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("depense_7d", "depense_30d", "depense_3m", "depense_12m"),
    where = (
        depense_7d == 0 &
        depense_30d == 0 &
        depense_3m == 0 &
        depense_12m == 0         
    ),
    type = 1,
    desc = "Aucune consommation non-alimentaire",
    comment = "ERREUR: Aucune consommation non-alimentaire déclarée (8.2 à 8.7)"
)

# -----------------------------------------------------------------------------
# Calories
# -----------------------------------------------------------------------------

# # calories totales trop élevées
# issue_calories_tot_high <- susoreview::create_issue(
#     df_attribs = attribs,
#     vars = c("calories_totales_elevees", "p_calcule_ok"),
#     where = calories_totales_elevees == 1 & p_calcule_ok == 1,
#     type = 1,
#     desc = "Calories totales trop élevées",
#     comment = paste0(
#         "ERREUR: La consommation alimentaire déclarée est trop élevée. ",
#         "D'abord, vérifier les quantités et les unités déclarées ",
#         "pour chaque produit dans 8.1b. ",
#         "Ensuite, confirmer que les déclarations concernent la consommation ",
#         "et non pas l'acquisition."    
#     )
# )

# # calories totales trop faible
# issue_calories_tot_low <- susoreview::create_issue(
#     df_attribs = attribs,
#     vars = c("calories_totales_faibles", "p_calcule_ok"),
#     where = calories_totales_faibles == 1 & p_calcule_ok == 1,
#     type = 1,
#     desc = "Calories totales trop faibles",
#     comment = paste0(
#         "ERREUR: La consommation alimentaire déclarée est trop faible. ",
#         "D'abord, confirmer que tous les produits consommés ont été renseignés. ",
#         "Ensuite, vérifier que les quantités et unités de consommation sont correctes"   
#     )
# )

# # calories trop élevées pour un item
# issue_calories_item_high <- susoreview::create_issue(
#     df_attribs = attribs,
#     vars = c("calories_item_elevees", "p_calcule_ok"),
#     where = calories_item_elevees == 1 & p_calcule_ok == 1,
#     type = 1,
#     desc = "Calories trop élevées pour un item",
#     comment = paste0(
#         "ERREUR. Trop de calories tirées d'un seul produit. D'abord, chercher le ",
#         "produit avec la plus grande quantité ou la plus grande unité de ",
#         "consommation. ",
#         "Ensuite, confirmer la consommation de celui-ci."
#     )
# )

# # items pour lesqules les calories sont trop élevées
# produit_codes <- c(
#     "aliment__id %in% c(1:26, 166:169)",
#     "aliment__id %in% c(27:39, 170, 171)",
#     "aliment__id %in% c(40:51, 172, 173)",
#     "aliment__id %in% c(52:60, 174)",
#     "aliment__id %in% c(61:70, 175)",
#     "aliment__id %in% c(71:87, 176)",
#     "aliment__id %in% c(88:108, 177)",
#     "aliment__id %in% c(109:133, 178)",
#     "aliment__id %in% c(134:138)",
#     "aliment__id %in% c(139:154, 179)",
#     "aliment__id %in% c(155:165, 180)"
# )

# produit_noms <- c(
#     "_1", # cereales
#     "_2", # viandes
#     "_3", # poissons
#     "_4", # laitier
#     "_10", # huiles
#     "_5", # fruits
#     "_6", # legumes
#     "_7", # legtub
#     "_8", # sucreries
#     "_9", # epices
#     "_11", # boissons
# )

# issues_where_calories_item_high <- purrr::map2_dfr(
#     .x = produit_codes,
#     .y = produit_noms,
#     .f = ~ susoreview::make_issue_in_roster(
#         df = dplyr::filter(calories_par_item,         
#             !!rlang::parse_quo(
#                 glue::glue("{.x}"),
#                 rlang::global_env()
#             )        
#         ),
#         where = calories_par_produit > 1500,
#         roster_vars = "aliment__id",
#         type = 2,
#         desc = "Calories trop élevées pour un item",
#         comment = "Calories trop élevées pour cet item",
#         issue_vars = glue::glue("v8103a_{.y}")
#     )
# )


# =============================================================================
# Flag critical inconsistencies
# =============================================================================

# -----------------------------------------------------------------------------
# Biens v. dépenses
# -----------------------------------------------------------------------------

# portable, mais pas crédits de communication
issue_portable_sans_communication <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("possede_portable", "depenses_credit_comm"),
    where = possede_portable == 1 & depenses_credit_comm == 0,
    type = 1,
    desc = "Possède portable, mais pas de crédit de communication",
    comment = paste0(
        "ERREUR: possède portable sans frais de communication. ",
        "Le ménage possède un téléphone portable selon la section 8.7, ",
        "mais ne déclare aucun dépense de crédit de communication dans la ",
        "section 8.2. ",
        "Ceci est peu problable. ",
        "Veuillez vérifier quelle information est correcte."
    )
)

# crédits de communication, mais aucun portable
issue_communication_sans_portable <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("possede_portable", "depenses_credit_comm"),
    where = depenses_credit_comm == 1 & possede_portable == 0,
    type = 1,
    desc = "crédits de communication, mais aucun portable",
    comment = paste0(
        "ERREUR: dépenses de communications sans portable. ",
        "Le ménage déclare des dépenses de communication dans la section 8.2, ",
        "mais ne possède pas de portable dans la section 8.7. ",
        "Ceci est peu probable. ",
        "Veuillez vérifier quelle information est correcte."
    )
)

# dépenses d'entretien de vélo, mais aucun vélo
issue_entretien_velo_sans_velo <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("depenses_velo", "possede_velo"),
    where = depenses_velo == 1 & possede_velo == 0,
    type = 1,
    desc = "dépenses d'entretien de vélo, mais aucun vélo",
    comment = paste0(
        "ERREUR: dépenses d'entretien de vélo sans vélo. ",
        "Le ménage déclare des dépenses d'entretien de vélo la section 8.3, ",
        "mais ne possède pas de vélo dans la section 8.7. ",
        "Ceci est peu probable. ",
        "Veuillez vérifier quelle information est correcte."
    )
)

# dépense d'entretien de véhicule motorisé, mais aucun véhicule
issue_entretien_vehicle_sans_vehicule <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("depenses_vehic_moteur", "possede_vehic_motorise"),
    where = depenses_vehic_moteur == 1 & possede_vehic_motorise == 0,
    type = 1,
    desc = "dépense d'entretien de véhicule motorisé, mais aucun véhicule",
    comment = paste0(
        "ERREUR: dépenses d'entretien de véhicule sans véhicule. ",
        "Le ménage déclare des dépenses d'entretien de véhicule motorisé la section 8.3, ",
        "mais ne possède pas de véhicule dans la section 8.7 (e.g., moto, voiture, minibus, camion). ",
        "Ceci est peu probable. ",
        "Veuillez vérifier quelle information est correcte."        
    )
)

# dépense de carburant, mais aucun véhicule motorisé (et aucun groupe électrogène)
issue_carburant_sans_vehicule <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("depenses_carburant", "possede_vehic_motorise", "possede_groupe"),
    where = depenses_carburant == 1 & possede_vehic_motorise == 0 & possede_groupe == 0,
    type = 1,
    desc = "dépense de carburant, mais aucun véhicule motorisé (et aucun groupe électrogène)",
    comment = paste0(
        "ERREUR: dépenses de carburant sans véhicule et sans groupe électrogène. ",
        "Le ménage déclare des dépenses de carburant la section 8.3 (e.g., essence, diesel), ",
        "mais ne possède ni de véhicule (e.g., moto, voiture, minibus, camion) ni de groupe électrogène ",
        "dans la section 8.7. Ceci est peu probable. ",
        "Veuillez vérifier quelle information est correcte."             
    )
)


# -----------------------------------------------------------------------------
# CONSOMMATION ALIMENTAIRE 7 JOURS V. PRODUCTION
# -----------------------------------------------------------------------------

# consommé de son propre riz, mais sans en produire
issue_cons_propre_riz_sans_prod <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("conso7d_propre_riz", "parcelle_riz"),
    where = conso7d_propre_riz == 1 & parcelle_riz == 0,
    type = 1,
    desc = "consommé de son propre riz, mais sans en produire",
    comment = paste0(
        "ERREUR: consomme du riz de sa propre production sans en produire. ",
        "Dans la section 8.1, le ménage déclare consommer du riz provenant de sa propre production. ",
        "Or, dans les sections 2 et 7.91, le ménage ne déclare aucune parcelle de riz. ",
        "Ceci est peu probable. ",
        "Veuillez vérifier quelle information est correcte."  
    )
)

# consommé de sa propre patate douce, mais sans en produire
issue_cons_propre_patate_douce_sans_prod <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("conso7d_propre_patate_douce", "parcelle_patate_douce"),
    where = conso7d_propre_patate_douce == 1 & parcelle_patate_douce == 0,
    type = 1,
    desc = "consommé de sa propre patate douce, mais sans en produire",
    comment = paste0(
        "ERREUR: consomme du riz de sa propre production sans en produire. ",
        "Dans la section 8.1, le ménage déclare consommer de la patate douce provenant de sa propre production. ",
        "Or, dans les sections 2 et 7.91, le ménage ne déclare aucune parcelle de patate douce. ",
        "Ceci est peu probable. ",
        "Veuillez vérifier quelle information est correcte."  
    )
)

# consommé de son propre mouton, mais sans en produire
issue_cons_propre_mouton_sans_prod <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("conso7d_propre_mouton", "eleve_moutons"),
    where = conso7d_propre_mouton == 1 & eleve_moutons == 0,
    type = 1,
    desc = "consommé de sa propre mouton, mais sans en produire",
    comment = paste0(
        "ERREUR: consomme du mouton de sa propre production sans en produire. ",
        "Dans la section 8.1, le ménage déclare consommer de la viande de mouton provenant de sa propre production. ",
        "Or, dans les sections 2 et 7.52, le ménage déclare ne posséder aucun mouton. ",
        "Ceci est peu probable. ",
        "Veuillez vérifier quelle information est correcte."  
    )
)


# -----------------------------------------------------------------------------
# CONSOMMATION 24H V. 7 DERNIERS JOURS
# -----------------------------------------------------------------------------


# TODO: pick up from line 241 of compileIssues from Nigeria

    # "c24_abats_dom",
    # "c24_abats_sauvage",
    # "c24_laitier",

c_vars <- c(
    "c24_cereales",
    "c24_legumes_oranges",
    "c24_tubercules_blanches",
    "c24_bananes",
    "c24_feuilles_vertes",
    "c24_autres_legumes",
    "c24_fruits_vit_a",
    "c24_autres_fruits",
    "c24_viande_dom",
    "c24_viande_sauvage",
    "c24_oeufs",
    "c24_poissons",
    "c24_legumineuses",
    "c24_noix_graines",
    "c24_huiles",
    "c24_viandes_transformees",
    "c24_fritures",
    "c24_sucreries",
    "c24_condiments",
    "c24_palme_rouge"
)

    # "w24_abats_dom",
    # "w24_abats_sauvage",
    # "w24_laitier",

w_vars <- c(
    "w24_cereales",
    "w24_legumes_oranges",
    "w24_tubercules_blanches",
    "w24_bananes",
    "w24_feuilles_vertes",
    "w24_autres_legumes",
    "w24_fruits_vit_a",
    "w24_autres_fruits",
    "w24_viande_dom",
    "w24_viande_sauvage",
    "w24_oeufs",
    "w24_poissons",
    "w24_legumineuses",
    "w24_noix_graines",
    "w24_huiles",
    "w24_viandes_transformees",
    "w24_fritures",
    "w24_boissons_sucrees",
    "w24_sucreries",
    "w24_condiments",
    "w24_palme_rouge"
)

# cons7j_abats
# cons7jw_laitier
# cons7jc_laitier

cons_vars <- c(
    "cons7j_cereales",
    "cons7j_legumes_orange",
    "cons7j_tubercules_blanches",
    "cons7j_bananes",
    "cons7j_feuilles_vertes",
    "cons7j_autres_legumes",
    "cons7j_fruits_vit_a",
    "cons7j_autres_fruits",
    "cons7j_viande_dom",
    "cons7j_viand_sauvage",
    "cons7j_oeufs",
    "cons7j_poisson",
    "cons7j_legumineuses",
    "cons7j_noix_graines",
    "cons7j_huiles",
    "cons7j_viandes_transformees",
    "cons7j_fritures",
    "cons7j_boissons_sucrees",
    "cons7j_sucreries",
    "cons7j_condiments",
    "cons7j_palme_rouge"
)

alim_descs <- c(
    "des céréales",
    "des légumes à la chair orange",
    "des tubercules à la chair blanche",
    "des bananes plantains ou des bananes vertes",
    "des légumes à feuilles vert foncé",
    "d'autres légumes (e.g., des haricots verts, des aubergines, des gombos, des oignons, etc.)",
    "des fruits riches en vitamine A (e.g., mangues mûres, papayes mûres, cantaloup, etc.)",
    "d'autres fruits",
    "de la viande d'animaux domestiqués (e.g., tels que bœuf, mouton, le chèvre, poulet, etc)",
    "de la viande d'animaux sauvage (i.e., gibier)",
    "des œufs",
    "du poisson frais, séché, ou fumé",
    "des aliments fait à base de haricots, de pois, de lentille, d'arachides, ou d'autres légumineuses",
    "des aliments à base de noix ou de graines (e.g., noix de cajou, graines de sésame, etc)",
    "de l'huile ou des matières grasses (e.g., huile d'arachide, beurre, etc)",
    "de la viande transformée (e.g., saucissons, conserves de viande, etc)",
    "des fritures (e.g., beignets, pommes frites, etc)",
    "des boissons sucrées (e.g., boissons gazeuses/sodas, jus sucrés, etc)",
    "des sucreries (e.g.,  des chocolats, des bonbons, des pâtisseries, des gâteaux)",
    "des consdiments (e.g., cubes, sauce soja, poudre de poisson, etc)",
    "des aliments à base d'huile de palme rouge, de noix de palme rouge ou de sauce graine"
)

# remove boissons sucrées from list
enf_cons_vars <- cons_vars[! cons_vars %in% c("cons7j_sucreries")]
enf_alim_desc <- alim_descs[! alim_descs %in% c("des boissons sucrées (e.g., boissons gazeuses/sodas, jus sucrés, etc)")]

issue_conso_enfant_24h_v_7j <- purrr::pmap_dfr(
    .l = list(enf_alim_desc, c_vars, enf_cons_vars),
    .f = ~ susoreview::create_issue(
        df_attribs = attribs,
        vars = c(..2, ..3),
        where = !!rlang::parse_quo(
            glue::glue("{..2} == 1 & {..3} == 0"),
            rlang::global_env()
        ),
        type = 1,
        desc = glue::glue("consommation de {..1} par un enfant dans les dernier 24h sans consommation par le ménage dans les 7 derniers jours"),
        comment = paste0(
            glue::glue("ERREUR: consommation de {..1} par un enfant dans les dernières 24h sans consommation par le ménage dans les 7 derniers jours. "),
            glue::glue("Dans la section 5, l'un des enfants a consommée {..1} dans les dernières 24 heures. "),
            glue::glue("Or dans la section 8.1, le ménage déclare n'avoir pas consommé {..1} dans les 7 derniers jours. "),
            "Les dernières 24 rentrent dans les 7 derniers jours. ",
            "Ceci est peu probable. ",
            "Veuillez confirmer quelle information est correcte."
        )
    )
)

issue_conso_femme_24h_v_7j <- purrr::pmap_dfr(
    .l = list(alim_descs, w_vars, cons_vars),
    .f = ~ susoreview::create_issue(
        df_attribs = attribs,
        vars = c(..2, ..3),
        where = !!rlang::parse_quo(
            glue::glue("{..2} == 1 & {..3} == 0"),
            rlang::global_env()
        ),
        type = 1,
        desc = glue::glue("consommation de {..1} par une femme dans les dernier 24h sans consommation par le ménage dans les 7 derniers jours"),
        comment = paste0(
            glue::glue("ERREUR: consommation de {..1} par une femme dans les dernières 24h sans consommation par le ménage dans les 7 derniers jours."),
            glue::glue("Dans la section 4, l'une des femmes a consommée {..1} dans les dernières 24 heures. "),
            glue::glue("Or dans la section 8.1, le ménage déclare n'avoir pas consommé {..1} dans les 7 derniers jours. "),
            "Les dernières 24 rentrent dans les 7 derniers jours. ",
            "Ceci est peu probable. ",
            "Veuillez confirmer quelle information est correcte."
        )
    )
)

# =============================================================================
# Combine all issues
# =============================================================================

# combine all issues
issues <- dplyr::bind_rows(mget(ls(pattern = "^issue_")))

# remove intermediary objects to lighten load on memory
rm(list = ls(pattern = "^issue_"))

# =============================================================================
# Add issues from interview metadata
# =============================================================================

# -----------------------------------------------------------------------------
# ... if questions left unanswered
# -----------------------------------------------------------------------------

# extract number of questions unanswered
interview_stats <- suso_diagnostics %>%
    # rename to match column names from GET /api/v1/interviews/{id}/stats
    dplyr::rename(
        NotAnswered = n_questions_unanswered,
        WithComments = questions__comments,
        Invalid = entities__errors
    ) %>%
    dplyr::select(interview__id, interview__key, NotAnswered, WithComments, Invalid)

# prepare number of legit missing file
# TODO: see if any legit unanswered
# num_legit_miss <- num_legit_miss %>%
#     rename(n_legit_miss = numLegitMiss) %>%
#     select(interview__id, interview__key, n_legit_miss)

# add error if interview completed, but questions left unanswered
# returns issues data supplemented with unanswered question issues
issues_plus_unanswered <- susoreview::add_issue_if_unanswered(
    df_cases_to_review = cases_to_review,
    df_interview_stats = interview_stats,
    df_issues = issues,
    n_unanswered_ok = 1, # to prevent supervisor-scope question from flagging
    issue_desc = "Questions laissés sans réponse",
    issue_comment = glue::glue("ERREUR: L'entretien a été marqué comme achevé, mais {NotAnswered} questions ont été laissées sans réponse. Veuillez renseigner ces questions.")
)

# -----------------------------------------------------------------------------
# ... if any SuSo errors
# -----------------------------------------------------------------------------

# add issue if there are SuSo errors
issues_plus_miss_and_suso <- susoreview::add_issues_for_suso_errors(
    df_cases_to_review = cases_to_review,
    df_errors = suso_errors,
    issue_type = 3,
    df_issues = issues_plus_unanswered
)
