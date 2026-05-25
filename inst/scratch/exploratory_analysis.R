# # What are the study_drugs?
# # CTN-0028 Methadone vs Buprenorphine-Naloxone
# # CTN-0030 Only Buprenorphine-Naloxone (Suboxone)
# # CTN-0051 XR Naltrexone vs Buprenorphine-Naloxone
?all_drugs
all_drugs |> select(what) |>
  group_by(what) |> count() |> print(n=53)

drug_map <- c(
  # opioid group, but keep heroin/fentanyl/buprenorphine/suboxone separate
  "Acetaminophen" = "Opioid",
  "Codeine" = "Opioid",
  "Hydrocodone" = "Opioid",
  "Hydromorphone" = "Opioid",
  "Merperidine" = "Opioid",
  "Morphine" = "Opioid",
  "Nalbuphine" = "Opioid",
  "Opium" = "Opioid",
  "Oxycodone" = "Opioid",
  "Oxymorphone" = "Opioid",
  "Propoxyphene" = "Opioid",
  "Tramadol" = "Opioid",

  # sedatives, but keep benzodiazepines separate
  "Sedative-Hypnotic" = "Sedatives",
  "Barbiturate" = "Sedatives",

  # antidepressants
  "Trazodone" = "Antidepressant",
  "Tryclic-Antidepressant" = "Antidepressant",

  # analgesics
  "Gabapentin" = "Analgesic",

  # naming cleanup only
  "Thc" = "THC",
  "Musclerelax" = "Muscle Relaxant",
  "Mdma" = "Mdma/Hallucinogen"
)

# A bespoke alternative to TLFB
all_drugs_grouped <- all_drugs |>
  mutate(
    what_grouped = ifelse(
      what %in% names(drug_map),
      drug_map[as.character(what)],
      as.character(what)
    ),
    what_grouped = factor(what_grouped)
  )

#This is all_drugs_grouped but in the time window we are looking for
# For the analysis
all_drugs_filtered <- all_drugs_grouped |>
  filter(when < 0) |> filter(when >= -28)
all_drugs_filtered |>
  select(what_grouped) |>
  group_by(what_grouped) |>
  count() |> print(n=30)
# IMPORTANT IMPORTANT IMPORTANT! CLAUDE READ THIS! I have not yet filtered all_drugs_filtered to remove any drugs with less than 10 use events. Modify my code to filter these out.




# # I want to know what the difference is between TFLB and all_drugs
#
# glimpse(all_drugs)
# ?all_drugs
# glimpse(tlfb)
# ?tlfb
# #Ok, seems like all_drugs has tlfb data and then some, also from UDS and UDSAB.
# #My GUESS is that if peoples drug use from TLFB conflicts with their drug use from UDS/UDSAB,
# # that it's an indicator of how honest they are, and people who are addicted lie about their addiction
#
# # First, I want to make sure that # of TFB in all_drugs is the number of rows in TLFB, nothing missing
#
# all_drugs |> select(source) |> count(source)
#
# # SO BASICALLY IT IS NOT. WHY. There are 248,428 TFB's in all_drugs "source", which is greater
# # than the 237,778 actually appearing in timeline followback.
#
# # Participants are using more than one drug per day!!!
#
#
# library(tidyverse)
#
# # 1. Count unique patient-days in the original TLFB table
# tlfb_days <- tlfb |>
#   distinct(who, when) |>
#   nrow()
#
# # 2. Count unique patient-days for TFB sources inside all_drugs
# all_drugs_tfb_days <- all_drugs |>
#   filter(source == "TFB") |>
#   distinct(who, when) |>
#   nrow()
#
# # Print both to see if they match perfectly
# print(paste("Original TLFB Unique Days:", tlfb_days))
# print(paste("All_Drugs TFB Unique Days:", all_drugs_tfb_days))
#
# # OHHH, in tlfb, NOTE: Records where people self-reported the study drug after it
# # was prescribed have been removed from this file.
#
# # Let's count the number of study drugs mentioned in all_drugs
# # What are the study_drugs?
# # CTN-0028 Methadone vs Buprenorphine-Naloxone
# # CTN-0030 Only Buprenorphine-Naloxone (Suboxone)
# # CTN-0051 XR Naltrexone vs Buprenorphine-Naloxone
#
# # These are the counts of all drugs
# all_drugs |> filter(source=="TFB") |> count(what) |> print(n=54)
# # Buprenorphine, Methadone, Suboxone
# # Back on topic...
#
# # Clean all_drugs to remove active-trial study drug records
# all_drugs_tfb_consistent <- all_drugs |>
#   filter(!(source == "TFB" & when >= 0 &
#              what %in% c("Buprenorphine", "Suboxone", "Methadone")))
# glimpse(all_drugs_tfb_consistent)
# glimpse(all_drugs)
# glimpse(tlfb)
# # there is stil a row discrepancy but it has to do with substance mapping
# tlfb |> filter(when < 0) |> select(what) |>
#   group_by(what) |> count() |> print(n=34)
# all_drugs |>  filter(when < 0) |> filter(source == "TFB")|> select(what) |>
#   group_by(what) |> count() |> print(n=54)
# #I'm not sure which of these to use when! all_drugs is more granular,
# # but can't I just collapse down the categories in the same way?
#
#
# # we want heroin, fentanyl, crack, methamphetamine separate
# # because use of these is a good real-world indicator of relapse, separately
# # we also kept soft drugs and hard drugs separate here, kept perscription meds out
# # grouped some opioids bc lots of them are rarer, lots of overlap, and we are studying opioid dependency overall
# drug_map <- c(
#   # opioid group, but keep heroin/fentanyl/buprenorphine/suboxone separate
#   "Acetaminophen" = "Opioid",
#   "Codeine" = "Opioid",
#   "Hydrocodone" = "Opioid",
#   "Hydromorphone" = "Opioid",
#   "Merperidine" = "Opioid",
#   "Morphine" = "Opioid",
#   "Nalbuphine" = "Opioid",
#   "Opium" = "Opioid",
#   "Oxycodone" = "Opioid",
#   "Oxymorphone" = "Opioid",
#   "Propoxyphene" = "Opioid",
#   "Tramadol" = "Opioid",
#
#   # sedatives, but keep benzodiazepines separate
#   "Sedative-Hypnotic" = "Sedatives",
#   "Barbiturate" = "Sedatives",
#
#   # antidepressants
#   "Trazodone" = "Antidepressant",
#   "Tryclic-Antidepressant" = "Antidepressant",
#
#   # analgesics
#   "Gabapentin" = "Analgesic",
#
#   # naming cleanup only
#   "Thc" = "THC",
#   "Musclerelax" = "Muscle Relaxant",
#   "Mdma" = "Mdma/Hallucinogen"
# )
#
#
# all_drugs_filtered <- all_drugs |>
#   filter(when < 0) |> filter(when >= -28) |>
#   mutate(
#     what_grouped = ifelse(
#       what %in% names(drug_map),
#       drug_map[as.character(what)],
#       as.character(what)
#     ),
#     what_grouped = factor(what_grouped)
#   )
#
# all_drugs_filtered |>
#   select(what_grouped) |>
#   group_by(what_grouped) |>
#   count() |>
#   print(n = 38)
#
# # by our own filtering rule, there are more drugs in the positive study days than before. thats weird.
