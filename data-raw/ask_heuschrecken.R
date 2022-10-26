#' ---
#' title: "Extract Heuschrecken data from the ASK database"
#' author: "RS-eco"
#' ---

# Load libraries
library(tidyverse)

# Load database
source("R/load_database.R")
ask_art <- load_database(name="ask_art", res="quadrant")

# Extract data for certain species
ask_heuschrecken <- ask_ins %>% filter(art %in% toupper(c("Oecanthus pellucens", "Calliptamus italicus", "Oedipoda caerulescens", "Phanoptera nana", "Chorthippus vagans",
                                                          "Mecostethus parapleurus", "Stethophyma grossum", "Omocestus viridulus", "Pseudochorthippus montanus", 
                                                          "Chorthippus mollis", "Gomphocerippus rufus", "Euthystira brachyptera", "Tettigonia cantans", "Meconema meridionale", 
                                                          "Decticus verrucivorus", "Miramella alpina"))) %>% 
  select(-c(ora_nachweis_id, ora_fuo_id, id, satznr, ord, datestamp, bemerkung, quelle, projekt, qbe, bearbeiter, bestimmer, sammler, auftraggeber, projektgattung, 
            scientificNameStd, kingdom, phylum, class, order, family, zahlarb, bestand_bp_min, bestand_bp, paar, verbleib, objnr, lr_pkt, vorl_obn, datum, meld, von_ttmm, von_jahr, bis_ttmm, bis_jahr, 
            quadrant, hoehe, objekt, merkmale, svcode1, svcode1n, bemerkung1, massstab, fachstell))

colnames(ask_heuschrecken)
head(ask_heuschrecken)

readr::write_csv(ask_heuschrecken, "ask_heuschrecken.csv.xz")
