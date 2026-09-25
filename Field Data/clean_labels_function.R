library(stringr)

clean_labels <- function(x) {
  x |>
    str_remove_all("^sum_of_") |>  # remove 'sum_of_' prefix
    str_replace_all("_", " ") |>  # replace underscores with spaces
    str_to_title() |>  # title case
    str_replace_all(c(
      "Vo2 Per G 6wks" = "VO2 (6 weeks)",
      "Vo2 Per G 12wks" = "VO2 (12 weeks)",
      "C18 1n9c Oleic Acid Gonad" = "Oleic Acid (Gonad)",
      "C18 1n9c Oleic Acid Dg" = "Oleic Acid (DG)",
      "C22 5N3 Docosapentaenoic Acid Dpa Gonad" = "DPA (Gonad)",
      "C18 3N3 Alpha Linolenic Acid Ala Gonad" = "ALA (Gonad)",
      "Fat Percent Dg" = "Fat % (DG)",
      "Fat Percent Gonad" = "Fat % (Gonad)",
      "N 3 Pufa Non Gonadal" = "n-3 PUFA (Non-Gonadal)",
      "N 6 Pufa Gonad" = "n-6 PUFA (Gonad)",
      "Pufa Dg" = "PUFA (DG)",
      "Mufa Gonad" = "MUFA (Gonad)",
      "Sfa Dg" = "SFA (DG)",
      "Pufa Non Gonadal" = "PUFA (Non-Gonadal)",
      "Omega 3 Gonad" = "Omega-3 (Gonad)",
      "Omega 6 Dg" = "Omega-6 (DG)",
      "Condition Score" = "Condition Score",
      "Glutathione Reductase" = "Glutathione Reductase",
      "Hspa5 Bi P" = "HSPA5 (BiP)",
      "Pcna Proliferating Cell Nuclear Antigen" = "PCNA",
      "Fatty Acid Synthase Fas" = "Fatty Acid Synthase",
      "Cathepsin D" = "Cathepsin D",
      "Copper Transporter Slc31A1" = "Copper Transporter",
      "Sodium Calcium Exchanger Slc8A" = "Na+/Ca2+ Exchanger",
      "Sodium Dependent Multivitamin Transporter Slc5a6" = "Multivitamin Transporter",
      "Growth Arrest And Dna Damage Inducible Protein Gadd45" = "GADD45",
      "Donson Protein Downstream Neighbour Of Son" = "DONSON Protein",
      "Sodium Hydrogen Exchanger Slc9A10" = "Sodium Hydrogen Exchanger"
    )) |> 
    str_trim()
}
