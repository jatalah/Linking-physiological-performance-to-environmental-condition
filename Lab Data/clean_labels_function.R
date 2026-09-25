clean_labels <- function(x) {
  x |>
    str_replace_all("_", " ") |>
    str_to_title() |>
    str_replace_all("\\bVo2 Per G 6wks\\b", "VO2 (6 weeks)") |>
    str_replace_all("\\bVo2 Per G 12wks\\b", "VO2 (12 weeks)") |>
    str_replace_all("\\bC18 1n9c Oleic Acid Gonad\\b", "Oleic Acid (Gonad)") |>
    str_replace_all("\\bOmega 3 Gonad\\b", "Omega-3 (Gonad)") |>
    str_replace_all("\\bOmega 6 Dg\\b", "Omega-6 (DG)") |>
    str_replace_all("\\bPufa Dg\\b", "PUFA (DG)") |>
    str_replace_all("\\bCondition Score\\b", "Condition Score") |>
    str_replace_all("\\bGlutathione Reductase\\b", "Glutathione Reductase") |>
    str_replace_all("\\bHspa5 Bi P\\b", "HSPA5 (BiP)") |>
    str_replace_all("\\bPcna Proliferating Cell Nuclear Antigen\\b", "PCNA") |>
    str_replace_all("\\bFatty Acid Synthase Fas\\b", "Fatty Acid Synthase") |>
    str_replace_all("\\bCathepsin D\\b", "Cathepsin D") |>
    str_replace_all("\\bCopper Transporter Slc31a1\\b", "Copper Transporter") |>
    str_replace_all("\\bSodium Calcium Exchanger Slc8a\\b", "Na+/Ca2+ Exchanger") |>
    str_replace_all("\\bSodium Dependent Multivitamin Transporter Slc5a6\\b", "Multivitamin Transporter") |>
    str_replace_all("\\bGrowth Arrest And Dna Damage Inducible Protein Gadd45\\b", "GADD45") |>
    str_replace_all("\\bDonson Protein Downstream Neighbour Of Son\\b", "DONSON Protein") |>
    str_replace_all("\\bC22 5n3 Docosapentaenoic Acid Dpa Gonad\\b", "DPA (Gonad)") |>
    str_replace_all("\\bSodium Hydrogen Exchanger Slc9a10\\b", "Sodium Hydrogen Exchanger") |>
    str_trim()
}

