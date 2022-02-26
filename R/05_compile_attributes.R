# =============================================================================
# Check that necessary objects exist
# =============================================================================

objects_needed <- c(
    "menages",
    "membres",
    "parcelles",
    "moutons", 
    "repos_hors_menage", 
    "emploi_temps"
)

check_exists(objects_needed)

# =============================================================================
# Load necessary libraries
# =============================================================================

library(dplyr)
library(susoreview)
library(purrr)
library(rlang)

# =============================================================================
# Create attributes
# =============================================================================

# -----------------------------------------------------------------------------
# Ménage
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2 : CARACTÉRISTIQUES DU MÉNAGE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# cultive du riz
attrib_cultive_riz <- susoreview::create_attribute(
    df = menages,
    condition = v234 == 1,
    attrib_name = "cultive_riz",
    attrib_vars = "v234"
)

# cultive de la patate douce
attrib_cultive_patate_douce <- susoreview::create_attribute(
    df = menages,
    condition = v236 == 1,
    attrib_name = "cultive_patate_douce",
    attrib_vars = "v236"
)

# élève des moutons
attrib_moutons <- susoreview::create_attribute(
    df = menages,
    condition = v226e > 0,
    attrib_name = "eleve_moutons",
    attrib_vars = "v226e"
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 8.1 : REPAS PRIS À L'EXTÉRIEUR DU MÉNAGE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# en dehors du ménage
attrib_repas_dehors_menage <- susoreview::any_vars(
    df = menages,
    var_pattern = "v8101a_mafh",
    var_val = 1,
    attrib_name = "repas_dehors_menage"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 8.1 : CONSOMMATION ALIMENTAIRE DANS LES 7 DERNIERS JOURS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# au sein du ménage (section 7B)
attrib_repas_a_domicile <- susoreview::any_vars(
    df = menages,
    var_pattern = "^v8101_",
    var_val = 1,
    attrib_name = "repas_a_domicile"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 8.2 à 8.7 : CONSOMMATION NON-ALIMENTAIRE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 7 jours
attrib_depense_7d <- susoreview::any_vars(
    df = menages,
    var_pattern = "v8202",
    var_val = 1,
    attrib_name = "depense_7d"
)

# 30 jours
attrib_depense_30d <- susoreview::any_vars(
    df = menages,
    var_pattern = "v8302",
    var_val = 1,
    attrib_name = "depense_30d"
)

# 3 mois
attrib_depense_3m <- susoreview::any_vars(
    df = menages,
    var_pattern = "v8402",
    var_val = 1,
    attrib_name = "depense_3m"
)

# 12 mois
attrib_depense_12m <- susoreview::any_vars(
    df = menages,
    var_pattern = "v8502",
    var_val = 1,
    attrib_name = "depense_12m"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 8.2 à 8.7 : DÉPENSES INDIQUANT UN BIEN
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# dépenses d'entretien de vélo
attrib_depenses_velo <- susoreview::create_attribute(
    df = menages,
    condition = v8302__8219 == 1,
    attrib_name = "depenses_velo",
    attrib_vars = "v8302"
)

# dépense d'entretien de véhicule à moteur
attrib_depenses_vehic_moteur <- susoreview::create_attribute(
    df = menages,
    condition = v8302__8218 == 1,
    attrib_name = "depenses_vehic_moteur",
    attrib_vars = "v8302"
)

# dépense de carburant pour un véhicle motorisé
attrib_depenses_carburant <- susoreview::create_attribute(
    df = menages,
    condition = v8302__8217 == 1,
    attrib_name = "depenses_carburant",
    attrib_vars = "v8302"
)

# dépense en crédits de communication
attrib_depenses_credit_comm <- susoreview::create_attribute(
    df = menages,
    condition = v8202__8196 == 1,
    attrib_name = "depenses_credit_comm",
    attrib_vars = "v8202"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 8.7 : BIENS DURABLES
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# possède un vélo
attrib_possede_velo <- susoreview::create_attribute(
    df = menages,
    condition = v8701__8335 == 1,
    attrib_name = "possede_velo",
    attrib_vars = "v8701"
)

# possède un véhicule motorisé
attrib_possede_vehic_motorise <- susoreview::create_attribute(
    df = menages,
    condition = (
        v8701__8337 == 1 |  # moto
        v8701__8338 == 1 |  # voiture
        v8701__8339 == 1 |  # minibus
        v8701__8340 == 1    # camion
    ),
    attrib_name = "possede_vehic_motorise",
    attrib_vars = "v8701"
)

# possède un téléphone portable
attrib_possede_portable <- susoreview::create_attribute(
    df = menages,
    condition = v8701__8328 == 1,
    attrib_name = "possede_portable",
    attrib_vars = "v8701"
)

# possède un groupe électrogène
attrib_possede_groupe <- susoreview::create_attribute(
    df = menages, 
    condition = v8701__8352 == 1,
    attrib_name = "possede_groupe",
    attrib_vars = "v8701"
)

# -----------------------------------------------------------------------------
# Membres
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4 : DIVERSITÉ ALIMENTAIRE DES FEMMES
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

w_vals <- c(
    409, # Nourriture faite à base de céréales telles que le maïs, mil, riz ou fonio y compris du pain, des nouilles, des pâtes alimentaires (spagetti, macaroni, etc), du couscous, de la bouillie, de l'avoine (Quaker), du popcorn ou thiakry?.............409
    410, # Citrouilles, carottes, courges ou patates douces qui sont jaunes ou oranges à l'intérieur?.........................................................................................................................................................410
    411, # Pommes de terre blanches, ignames blanches, manioc, taro, navet, ou tout autre aliment à base de racines?..........................................................................................................................................411
    4111, # Des bananes plantains ou vertes?...................................................................................................................................................................................................................4111
    412, # Des légumes à feuilles vert foncé tels que le moringa, les feuilles de baobab, les feuilles de patate douce, les feuilles de manioc, les feuilles d'hibiscus, les feuilles d'amaranthe, les épinards ou d'autres légumes à feuilles vert foncé ?...412
    413, # D'autres légumes, par exemple des haricots verts, des aubergines, des gombos, des oignons, des choux, des tomates ?................................................................................................................................413
    414, # Mangues mûres, papayes mûres, cantaloup ou d'autres aliments riches en vitamine A ?................................................................................................................................................................................414
    415, # D'autres fruits?...................................................................................................................................................................................................................................415
    416, # Du foie, des reins, du cœur ou d'autres abats provenant d'animaux domestiques tels que la vache, le mouton, la chèvre, le poulet ou le canard ?....................................................................................................416
    417, # De la viande d'animaux domestiques, tels que bœuf, mouton, le chèvre,  poulet, ou canard?..........................................................................................................................................................417
    418, # Du foie, des reins, du cœur ou d'autres abats provenant d'animaux sauvages tels que le lièvre, les lézards ou les cerfs ?..........................................................................................................................418
    419, # De la viande d'animaux sauvages tels que le lièvre, le lézard, ou le cerf?.........................................................................................................................................................................419
    420,  # Des œufs?..........................................................................................................................................................................................................................................420
    421, # Poissons, crustacés ou fruits de mer frais, fumés, ou séchés ?.....................................................................................................................................................................................421
    422, # Des aliments à base de haricots, de pois, de lentilles ou d'arachides tels que le niébé, le ragout d'arachide, le beurre d'arachide?...............................................................................................................422
    423, # Des aliments à base de noix ou de graines telles que la noix de cajou, les noix sauvages ou les graines de sésame ?................................................................................................................................423
    424, # Lait, fromage, yaourt ou autres produits laitiers tels que le lait et les céréales ou le thiakry?..................................................................................................................................................424
    425, # De l'huile, des matières grasses, du beurre ou des aliments préparés avec l'un de ces ingrédients?.................................................................................................................................................425
    4251, # Viandes transformées...............................................................................................................................................................................................................................4251
    4252, # Aliments frits et salés tels que les chips, beignets, fatayas, pomme de terre frite................................................................................................................................................................4252
    4253, # Boissons gazeuses/sodas, jus sucrés, boissons à saveur de chocolat.................................................................................................................................................................................4253
    426, # Des aliments sucrés tels que des chocolats, des sucreries, des bonbons, des pâtisseries, des gâteaux ou des biscuits?..............................................................................................................................426
    427, # Des condiments pour la saveur, tels que des cubes, des piments, des épices, sauce soja, de la poudre de poisson, etc?..............................................................................................................................427
    428, # Des larves, des escargots ou des insectes tels que les criquets ou les termites?...................................................................................................................................................................428
    429 # Des aliments à base d'huile de palme rouge, de noix de palme rouge ou de sauce graine?.............................................................................................................................................................429
)

w_names <- c(
    "w24_cereales",
    "w24_legumes_oranges",
    "w24_tubercules_blanches",
    "w24_bananes",
    "w24_feuilles_vertes",
    "w24_autres_legumes",
    "w24_fruits_vit_a",
    "w24_autres_fruits",
    "w24_abats_dom",
    "w24_viande_dom",
    "w24_abats_sauvage",
    "w24_viande_sauvage",
    "w24_oeufs",
    "w24_poissons",
    "w24_legumineuses",
    "w24_noix_graines",
    "w24_laitier",
    "w24_huiles",
    "w24_viandes_transformees",
    "w24_fritures",
    "w24_boissons_sucrees",
    "w24_sucreries",
    "w24_condiments",
    "w24_insectes",
    "w24_palme_rouge"
)

attrib_woman_24h <- purrr::map2(
        .x = w_vals,
        .y = w_names,
        .f = ~ susoreview::any_obs(
            df = membres,
            where = !!rlang::parse_quo(
                paste0("v409a__", .x, " == 1"),
                rlang::global_env()
            ),
            attrib_name = .y,
            attrib_vars = "v409a"
        )
    ) %>%
    dplyr::bind_rows()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5 : DIVERSITÉ ALIMENTAIRE DES ENFANTS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

c_vals <- c(
    539, # Nourriture faite à base de céréales telles que le maïs, mil, riz ou fonio y compris du pain, des nouilles, des pâtes alimentaires (spagetti, macaroni, etc), du couscous, de la bouillie, de l'avoine (Quaker), du popcorn ou thiakry?.............
    540, # Citrouilles, carottes, courges ou patates douces qui sont jaunes ou oranges à l'intérieur?.........................................................................................................................................................
    541, # Pommes de terre blanches, ignames blanches, manioc, taro, navet, ou tout autre aliment à base de racines?..........................................................................................................................................
    5411, # Des bananes plantains ou vertes?...................................................................................................................................................................................................................
    542, # Des légumes à feuilles vert foncé tels que le moringa, les feuilles de baobab, les feuilles de patate douce, les feuilles de manioc, les feuilles d'hibiscus, les feuilles d'amaranthe, les épinards ou d'autres légumes à feuilles vert foncé ?...
    543, # D'autres légumes, par exemple des haricots verts, des aubergines, des gombos, des oignons, des choux, des tomates ?................................................................................................................................
    544, # Mangues mûres, papayes mûres, cantaloup ou d'autres aliments riches en vitamine A..................................................................................................................................................................
    545, # D'autres fruits?...................................................................................................................................................................................................................................
    546, # Du foie, des reins, du cœur ou d'autres abats provenant d'animaux domestiques tels que la vache, le mouton, la chèvre, le poulet ou le canard ?....................................................................................................
    547, # De la viande d'animaux domestiques, tels que bœuf, mouton, le chèvre,  poulet, ou canard?..........................................................................................................................................................
    548, # Du foie, des reins, du cœur ou d'autres abats provenant d'animaux sauvages tels que le lièvre, les lézards ou les cerfs ?..........................................................................................................................
    549, # De la viande d'animaux sauvages tels que le lièvre, le lézard, ou le cerf?.........................................................................................................................................................................
    550, # Des œufs?..........................................................................................................................................................................................................................................
    551, # Poissons, crustacés ou fruits de mer frais, fumés, ou séchés ?.....................................................................................................................................................................................
    552, # Des aliments à base de haricots, de pois, de lentilles ou d'arachides tels que le niébé, le ragout d'arachide, le beurre d'arachide?...............................................................................................................
    553, # Des aliments à base de noix ou de graines telles que la noix de cajou, les noix sauvages ou les graines de sésame ?................................................................................................................................
    554, # Fromage dur ou mou?
    555, # De l'huile, des matières grasses, du beurre ou des aliments préparés avec l'un de ces ingrédients?.................................................................................................................................................
    5491, # Viandes transformées...............................................................................................................................................................................................................................
    5571, # Aliments frits et salés tels que les chips, beignets, fatayas, pomme de terre frite................................................................................................................................................................
    556, # Des aliments sucrés tels que des chocolats, des sucreries, des bonbons, des pâtisseries, des gâteaux ou des biscuits?..............................................................................................................................
    557, # Des condiments pour la saveur, tels que des cubes, des piments, des épices, sauce soja, de la poudre de poisson, etc?..............................................................................................................................
    558, # Des larves, des escargots ou des insectes tels que les criquets ou les termites?...................................................................................................................................................................
    559 # Des aliments à base d'huile de palme rouge, de noix de palme rouge ou de sauce graine?.............................................................................................................................................................
)

c_names <- c(
    "c24_cereales",
    "c24_legumes_oranges",
    "c24_tubercules_blanches",
    "c24_bananes",
    "c24_feuilles_vertes",
    "c24_autres_legumes",
    "c24_fruits_vit_a",
    "c24_autres_fruits",
    "c24_abats_dom",
    "c24_viande_dom",
    "c24_abats_sauvage",
    "c24_viande_sauvage",
    "c24_oeufs",
    "c24_poissons",
    "c24_legumineuses",
    "c24_noix_graines",
    "c24_laitier",
    "c24_huiles",
    "c24_viandes_transformees",
    "c24_fritures",
    "c24_sucreries",
    "c24_condiments",
    "c24_insectes",
    "c24_palme_rouge"
)

attrib_child_24h <- purrr::map2(
        .x = c_vals,
        .y = c_names,
        .f = ~ susoreview::any_obs(
            df = membres,
            where = !!rlang::parse_quo(
                paste0("v539a__", .x, " == 1"),
                rlang::global_env()
            ),
            attrib_name = .y,
            attrib_vars = "v539a"
        )
    ) %>%
    dplyr::bind_rows()

# -----------------------------------------------------------------------------
# CONSOMMATION ALIMENTAIRE 7 DERNIERS JOURS
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DE SA PROPRE PRODUCTION
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# riz
attrib_conso7d_propre_riz <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id %in% c(1, 2) & v8106 > 0,
    attrib_name = "conso7d_propre_riz",
    attrib_vars = "v8101_1|v8106"
)

# patate douce
attrib_conso7d_propre_patate_douce <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id == 128 & v8106 > 0,
    attrib_name = "conso7d_propre_patate_douce",
    attrib_vars = "v8101_7|v8106"
)

# mouton
attrib_conso7d_propre_mouton <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id == 29 & v8106 > 0,
    attrib_name = "conso7d_propre_mouton",
    attrib_vars = "v8101_2|v8106"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DES ITEMS DU RAPPEL DE 24H
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# céréales
# Nourriture faite à base de céréales telles que le maïs, mil, riz ou fonio y compris du pain, des nouilles, des pâtes alimentaires (spagetti, macaroni, etc), du couscous, de la bouillie, de l'avoine (Quaker), du popcorn ou thiakry?.............
attrib_cons7j_cereales <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id %in% c(1:23),
    attrib_name = "cons7j_cereales",
    attrib_vars = "v8101_1"
)

# légumes à la chair orange
# Citrouilles, carottes, courges ou patates douces qui sont jaunes ou oranges à l'intérieur?.........................................................................................................................................................
attrib_cons7j_legumes_orange <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            90, # carottes
            94, # Courge/Courgette
            128 # Patate douce
        )    
    ),
    attrib_name = "cons7j_legumes_orange",
    attrib_vars = "v8101_6|v8101_7"
)

# tubercules à la chair blanche
# Pommes de terre blanches, ignames blanches, manioc, taro, navet, ou tout autre aliment à base de racines?..........................................................................................................................................
attrib_cons7j_tubercules_blanches <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            123, # Manioc
            124, # igname
            126, # Pomme de terre
            127, # Taro, macabo
            129 # Autres tubercules n.d.a
        )
    ),
    attrib_name = "cons7j_tubercules_blanches",
    attrib_vars = "v8101_7"
)

# bananes plantain ou bananes douce
# Des bananes plantains ou des bananes vertes?...................................................................................................................................................................................................................
attrib_cons7j_bananes <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            76, # Banane douce
            125 # Plantain
        )
    ),
    attrib_name = "cons7j_bananes",
    attrib_vars = "v8101_5|v8101_7"
)


# feuilles vertes
# Des légumes à feuilles vert foncé tels que le moringa, les feuilles de baobab, les feuilles de patate douce, les feuilles de manioc, les feuilles d'hibiscus, les feuilles d'amaranthe, les épinards ou d'autres légumes à feuilles vert foncé ?...
attrib_cons7j_feuilles_vertes <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            102, # Feuilles d'oseille (bissap)
            103, # Feuilles de baobab/ "laalo"
            104, # Feuilles de haricot/ "niébé"
            105, # Feuilles nébédaye (moringa)
            106 # Autres légumes en feuilles (manioc, taro,  etc.)
        )
    ),
    attrib_name = "cons7j_feuilles_vertes",
    attrib_vars = "v8101_6"

)


# d'autres légumes
# D'autres légumes, par exemple des haricots verts, des aubergines, des gombos, des oignons, des choux, des tomates ?................................................................................................................................
attrib_cons7j_autres_legumes <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            88, # Salade (laitue)
            89, # Choux
            91, # Haricot vert
            92, # Concombre
            93, # Aubergine
            95, # Poivron frais
            96, # Tomate fraîche
            97, # Tomate séchée
            98, # Gombo frais
            99, # Gombo sec
            100, # Oignon frais
            101, # Ail
            107, # Autre légumes frais n.d.a
            108 # Concentré de tomate
        )
    ),
    attrib_name = "cons7j_autres_legumes",
    attrib_vars = "v8101_6"
)

# fruits riches en vitamine A
# Mangues mûres, papayes mûres, cantaloup ou d'autres aliments riches en vitamine A..................................................................................................................................................................
attrib_cons7j_fruits_vit_a <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            71, # Mangue
            84 # Papaye
        )
    ),
    attrib_name = "cons7j_fruits_vit_a",
    attrib_vars = "v8101_5"
)

# d'autres fruits
# D'autres fruits?...................................................................................................................................................................................................................................
attrib_cons7j_autres_fruits <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            72, # Ananas
            73, # Orange
            74, # Citrons
            75, # Autres agrumes (mandarine, pamplemousse, etc.)
            77, # Avocats
            78, # Pastèque
            79, # Melon
            80, # Dattes
            83, # Pommes
            85, # Fruit de baobab
            86, # Néré
            87 # Autres fruits (tamarin noir, liane sauvage, raisin, fraise, etc.)
        )        
    ),
    attrib_name = "cons7j_autres_fruits",
    attrib_vars = "v8101_5"
)

# des abats
# Du foie, des reins, du cœur ou d'autres abats provenant d'animaux domestiques tels que la vache, le mouton, la chèvre, le poulet ou le canard ?....................................................................................................
# Du foie, des reins, du cœur ou d'autres abats provenant d'animaux sauvages tels que le lièvre, les lézards ou les cerfs ?..........................................................................................................................
attrib_cons7j_abats <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id == 31, # Abats et tripes (foie, rognon, etc.)
    attrib_name = "cons7j_abats",
    attrib_vars = "v8101_2"
)

# viande d'animaux domestiqués
# De la viande d'animaux domestiques, tels que bœuf, mouton, le chèvre,  poulet, ou canard?..........................................................................................................................................................
attrib_cons7j_viande_dom <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            27:30, # Viande de bœuf, Viande de chameau, Viande de mouton, Viande de chèvre
            32:35, # Viande de porc, Poulet sur pied, Viande de poulet, Viande d'autres volailles domestiques
            37, # Viande séchée (boeuf, mouton, chameau)
            39 # Autres viandes n.d.a
        )
    ),
    attrib_name = "cons7j_viande_dom",
    attrib_vars = "v8101_2"
)

# viande d'animaux sauvage
# De la viande d'animaux sauvages tels que le lièvre, le lézard, ou le cerf?.........................................................................................................................................................................
attrib_cons7j_viand_sauvage <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            38, # Gibiers
            39 # Autres viandes n.d.a
        )
    ),
    attrib_name = "cons7j_viand_sauvage",
    attrib_vars = "v8101_2"
)

# des oeufs
# Des œufs?..........................................................................................................................................................................................................................................
attrib_cons7j_oeufs <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id == 60, # Œufs
    attrib_name = "cons7j_oeufs",
    attrib_vars = "v8101_4"
)

# poissons
# Poissons, crustacés ou fruits de mer frais, fumés, ou séchés ?.....................................................................................................................................................................................
attrib_cons7j_poissons <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id %in% c(40:51),
    attrib_name = "cons7j_poisson",
    attrib_vars = "v8101_3"
)

# légumineuses
# Des aliments à base de haricots, de pois, de lentilles ou d'arachides tels que le niébé, le ragout d'arachide, le beurre d'arachide?...............................................................................................................
attrib_cons7j_legumineuses <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            109:112, # Petits pois, Petit pois secs, Autres légumes secs n.d.a, Niébé/Haricots secs
            113:118, # Arachides fraîches en coques, Arachides séchées en coques, Arachides décortiquées, Arachides pilées ("Nguérté noflaye"), Arachide grillée, Pâte d'arachide
            119 # Fromage à base de soja
        )
    ),
    attrib_name = "cons7j_legumineuses",
    attrib_vars = "v8101_7"
)

# noix et graines
# Des aliments à base de noix ou de graines telles que la noix de cajou, les noix sauvages ou les graines de sésame ?................................................................................................................................
attrib_cons7j_noix_graines <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            120, # Sésame
            121, # Noix de cajou
            122 # Noix de karité
        )
    ),
    attrib_name = "cons7j_noix_graines",
    attrib_vars = "v8101_7"
)

# produits laitiers
# FEMME: Lait, fromage, yaourt ou autres produits laitiers?							
attrib_7jw_laitier <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id %in% c(52:57, 59),
    attrib_name = "cons7jw_laitier",
    attrib_vars = "v8101_4"
)

# ENFANT: Fromage dur ou mou?
attrib_7jc_laitier <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id %in% c(57, 59),
    attrib_name = "cons7jc_laitier",
    attrib_vars = "v8101_4"
)

# huiles et matière grasse
# De l'huile, des matières grasses, du beurre ou des aliments préparés avec l'un de ces ingrédients?.................................................................................................................................................
attrib_cons7j_huiles <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id %in% c(61:70),
    attrib_name = "cons7j_huiles",
    attrib_vars = "v8101_10"
)

# viande transformée
# Viandes transformées...............................................................................................................................................................................................................................
attrib_cons7j_viandes_transformees <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            36, # Charcuterie (jambon, saucisson), conserves de viandes
            39 # Autres viandes n.d.a
        )
    ),
    attrib_name = "cons7j_viandes_transformees",
    attrib_vars = "v8101_2"
)

# fritures
# Aliments frits et salés tels que les chips, beignets, fatayas, pomme de terre frite................................................................................................................................................................
attrib_cons7j_fritures <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id == 26, # Beignets, galettes
    attrib_name = "cons7j_fritures",
    attrib_vars = "v8101_1"
)

# boissons sucrées
# Boissons gazeuses/sodas, jus sucrés, boissons à saveur de chocolat.................................................................................................................................................................................
attrib_cons7j_boissons_sucrees <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            158, # Chocolat en poudre
            160, # Jus de fruits (orange, bissap, gingembre, jus de cajou,etc.)
            162, # Boissons gazeuses (coca, etc.)
            163, # Jus en poudre
            54 # Lait concentré sucré
        )
    ),
    attrib_name = "cons7j_boissons_sucrees",
    attrib_vars = "v8101_4|v8101_11"
)

# sucreries
# Des aliments sucrés tels que des chocolats, des sucreries, des bonbons, des pâtisseries, des gâteaux ou des biscuits?..............................................................................................................................
attrib_cons7j_sucreries <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id %in% c(134:138),
    attrib_name = "cons7j_sucreries",
    attrib_vars = "v8101_8"
)

# condiments
# Des condiments pour la saveur, tels que des cubes, des piments, des épices, sauce soja, de la poudre de poisson, etc?..............................................................................................................................
attrib_cons7j_condiments <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = aliment__id %in% c(139:152),
    attrib_name = "cons7j_condiments",
    attrib_vars = "v8101_9"
)

# palme rouge
# Des aliments à base d'huile de palme rouge, de noix de palme rouge ou de sauce graine?.............................................................................................................................................................
attrib_cons7j_palme_rouge <- susoreview::any_obs(
    df = consommation_alimentaire_7d,
    where = (
        aliment__id %in% c(
            63, # Huile de palme rouge
            68, # Huile de palme raffinée
            70 # Autres huiles n.d.a. (maïs, soja, huile palmiste, etc.)
        )
    ),
    attrib_name = "cons7j_palme_rouge",
    attrib_vars = "v8101_10"
)

# -----------------------------------------------------------------------------
# Parcelles
# -----------------------------------------------------------------------------

# !!! TODO: confirm data structure for multi-select combo box

# parcelle de riz
attrib_parcelle_riz <- susoreview::any_obs(
    df = parcelles,
    where = v79_n5__3 == 1,
    attrib_name = "parcelle_riz",
    attrib_vars = "v79_n5"
)

# parcelle de patate douce
attrib_parcelle_patate_douce <- susoreview::any_obs(
    df = parcelles,
    where = v79_n5__15 == 1,
    attrib_name = "parcelle_patate_douce",
    attrib_vars = "v79_n5"
)

# -----------------------------------------------------------------------------
# Calories totales
# -----------------------------------------------------------------------------

# # pourcentage de produits consommées qui ont été valorisés en calories
# attrib_p_aliments_valorises <- susoreview::create_attribute(
#     df = calories_totales,
#     condition = p_calcule > 0.7,
#     attrib_name = "p_calcule_ok",
#     attrib_vars = "^v8101_|^v8103a_|^v8103a_|^v8103a_"
# )

# # trop de calories
# attrib_calories_elevees <- susoreview::create_attribute(
#     df = calories_totales,
#     condition = calories_totales > 4000,
#     attrib_name = "calories_totales_elevees",
#     attrib_vars = "^v8101_|^v8103a_|^v8103a_|^v8103a_"
# )

# # trop peu de calories
# attrib_calories_faibles <- susoreview::create_attribute(
#     df = calories_totales,
#     condition = calories_totales <= 800 &  p_calcule > 0.7,
#     attrib_name = "calories_totales_faibles",
#     attrib_vars = "^v8101_|^v8103a_|^v8103a_|^v8103a_"
# )

# -----------------------------------------------------------------------------
# Calories par item
# -----------------------------------------------------------------------------

# # trop de calories déclarée pour un seul item
# attrib_calories_elevees_item <- susoreview::any_obs(
#     df = calories_par_item,
#     where = calories_par_produit > 1500,
#     attrib_name = "calories_item_elevees",
#     attrib_vars = "^v8103a_|^v8103a_|^v8103a_"
# )

# =============================================================================
# Rassembler les attributs
# =============================================================================

# combine all attribute data sets whose names match the pattern below
attribs <- dplyr::bind_rows(mget(ls(pattern = "^attrib_")))

# remove intermediary objects to lighten load on memory
rm(list = ls(pattern = "^attrib_"))
