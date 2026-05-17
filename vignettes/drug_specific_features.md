# Drug-Specific Feature Selection

For each drug category expected in `all_drugs_filtered` (days −28 to −1,
≥10 use events in the window), this document records which of the three feature
types to include and the justification.

**Feature types:**
- **days** — total days (any source) in the window with this substance
- **streak** — longest consecutive-day run with this substance
- **binary** — any use at all in the window (0/1)

Verify the exact list of surviving categories by running:
```r
all_drugs_filtered |> count(what_grouped) |> arrange(desc(n))
```
before finalising the analysis tibble. Categories not present there should
be skipped regardless of this document.

---

## Drug Map Changes (since initial version)

The following changes to `drug_map` have been made and are reflected in this
document. Update feature names and section headers accordingly.

| Original name in `what` | New `what_grouped` value | Change type |
|---|---|---|
| `"Thc"` | `"Cannabinoids"` | Rename + group |
| `"K2"` | `"Cannabinoids"` | Merged into Cannabinoids |
| `"Mdma"` | `"MDMA/Hallucinogen"` | Capitalisation fix |
| `"Hallucinogen"` | `"MDMA/Hallucinogen"` | Merged into MDMA/Hallucinogen |
| `"Alcohol"` | `"Alcohol Missing Amnt"` | Rename (amount unknown records) |
| `"Musclerelax"` | `"Muscle Relaxant"` | Unchanged (already existed) |

**Secondary filter applied:** Illicit drug categories with < 1% prevalence
(users / total participants) are dropped from `all_drugs_filtered`. Prescribed
and legal categories are exempt. PCP (19 events) is the known casualty of this
filter; MDMA/Hallucinogen (~100 events after merge) may also be dropped —
flagged below.

**Nicotine:** Not found in the pre-study window. Run the following to
investigate before assuming it is absent:
```r
all_drugs |>
  filter(grepl("nic|tobacco|smok|cig", what, ignore.case = TRUE)) |>
  count(what)
```
`nicotine_binary` is unconditionally included if any nicotine records exist;
`nicotine_days` is conditional on the ≥10 events filter. If nicotine is truly
absent from `all_drugs`, skip both features and rely on the Fagerstrom score.

---

## Hard / Illicit Drugs

### Heroin
| Feature | Include? | Justification |
|---|---|---|
| `heroin_days` | ✅ Yes | Strongest single predictor of OUD relapse; graded dose-response with outcome |
| `heroin_streak` | ✅ Yes | Streak captures compulsive dependent use, not just occasional use |
| `heroin_binary` | ✅ Yes | Any heroin use vs. none is a clinically meaningful threshold; useful for linear models |

### Fentanyl
| Feature | Include? | Justification |
|---|---|---|
| `fentanyl_days` | ✅ Yes | Increasingly prevalent; pharmacologically distinct from heroin (faster onset, higher potency) |
| `fentanyl_streak` | ✅ Yes | Same rationale as heroin |
| `fentanyl_binary` | ✅ Yes | Any fentanyl use is clinically meaningful (overdose risk, tolerance) |

### Cocaine
| Feature | Include? | Justification |
|---|---|---|
| `cocaine_days` | ✅ Yes | Stimulant co-use strongly predicts opioid relapse (polydrug severity marker) |
| `cocaine_streak` | ✅ Yes | Binge-pattern cocaine use captured by streak; addictive |
| `cocaine_binary` | ✅ Yes | Any vs. no cocaine use is a meaningful severity split |

### Crack cocaine
| Feature | Include? | Justification |
|---|---|---|
| `crack_days` | ✅ Yes | 2889 events in the window; passes filter by a wide margin |
| `crack_streak` | ✅ Yes | Same rationale as cocaine; binge pattern |
| `crack_binary` | ✅ Yes | Any vs. no crack use is a meaningful severity split |

**Note:** With 2889 events, crack is not sparse. The composite `stimulant_days` option is not needed — keep cocaine and crack as separate features.

### Methamphetamine
| Feature | Include? | Justification |
|---|---|---|
| `methamphetamine_days` | ✅ Yes | Increasingly common in OUD populations; high addiction potential |
| `methamphetamine_streak` | ✅ Yes | Binge/run use pattern; streak meaningful |
| `methamphetamine_binary` | ✅ Yes | Any vs. none split is clinically relevant |

### Amphetamine
| Feature | Include? | Justification |
|---|---|---|
| `amphetamine_days` | ✅ Yes | 844 events; passes filter. Treat same as methamphetamine — stimulant with high abuse potential. Could be Adderall Rx but likely illicit in this context |
| `amphetamine_streak` | ✅ Yes | Same rationale as methamphetamine |
| `amphetamine_binary` | ✅ Yes | Any vs. none split clinically relevant |

**Note:** Amphetamine was not in the original `drug_map` and keeps its
original name in `what_grouped`. It was only discovered after inspecting the
filtered data. Not in the original drug_specific_features.md.

---

## Sedatives / CNS Depressants

### Benzodiazepine
| Feature | Include? | Justification |
|---|---|---|
| `benzodiazepine_days` | ✅ Yes | Cross-addiction with opioids; one of the strongest predictors of overdose and relapse |
| `benzodiazepine_streak` | ✅ Yes | Physical dependence develops with sustained use; streak reflects severity |
| `benzodiazepine_binary` | ✅ Yes | Any benzo use at intake is a major clinical flag |

### Sedatives (grouped: Sedative-Hypnotic + Barbiturate)
| Feature | Include? | Justification |
|---|---|---|
| `sedatives_days` | ✅ Yes (263 events observed) | Non-benzo CNS depressants; barbiturates are largely out of use but hypnotics (Ambien etc.) remain common |
| `sedatives_streak` | ✅ Yes (263 events observed) | Physical dependence potential; streak meaningful |
| `sedatives_binary` | ✅ Yes (263 events observed) | Any use flag |

**Note:** 263 events observed in the window — comfortably passes the ≥10
filter. Updated from conditional ⚠️ to unconditional ✅.

---

## Cannabis

### Cannabinoids (formerly THC; now includes K2)
| Feature | Include? | Justification |
|---|---|---|
| `cannabinoids_days` | ✅ Yes | Cannabis use has a complex bidirectional relationship with opioid use; include to let the model find the direction. K2 merged in (31 events, same pharmacological class) |
| `cannabinoids_streak` | ✅ Yes | Cannabis use disorder is real; streak captures habitual use vs. occasional |
| `cannabinoids_binary` | ✅ Yes | Any use vs. none |

**Note:** Previously named `thc_days` / `thc_streak` / `thc_binary`. K2
(synthetic cannabinoid, 31 events) was merged into this group via `drug_map`.

---

## Alcohol

### Alcohol (hard / heavy drinking)
| Feature | Include? | Justification |
|---|---|---|
| `alcohol_hard_days` | ✅ Yes | Heavy drinking is a robust predictor of opioid relapse; distinct from moderate drinking |
| `alcohol_hard_streak` | ✅ Yes | Heavy drinking streaks (bingeing) predict worse outcomes than isolated events |
| `alcohol_hard_binary` | ✅ Yes | Any heavy drinking vs. none |

### Alcohol (light / moderate drinking)
| Feature | Include? | Justification |
|---|---|---|
| `alcohol_light_days` | ✅ Yes | Include separately from heavy; captures overall drinking behavior and restraint when used together |
| `alcohol_light_streak` | ❌ No | Light drinking streaks have low clinical significance for OUD prediction; omit to reduce noise |
| `alcohol_light_binary` | ✅ Yes | Any light drinking vs. abstinence |

**Note:** The "restraint" feature (`alcohol_hard_days − alcohol_light_days`)
is a derived feature computed at tibble-building time. Keep raw counts as
separate features too.

### Alcohol (missing amount)
| Feature | Include? | Justification |
|---|---|---|
| `alcohol_missing_amnt_days` | ✅ Yes | 1449 events; records where alcohol was logged but not classified as heavy or light. Keeps information rather than discarding it; unknown amount means unknown severity |
| `alcohol_missing_amnt_streak` | ❌ No | Severity unknown; streak semantics unclear for unclassified alcohol. Omit to avoid spurious signal |
| `alcohol_missing_amnt_binary` | ✅ Yes | Any unclassified alcohol use — captures people who drank but whose amount was not recorded |

**Note:** These records were originally named `"Alcohol"` in `what` and had
no classification into heavy/light. Renamed `"Alcohol Missing Amnt"` in
`drug_map`. 1449 events; clearly passes filter.

---

## Study Medications (pre-randomisation exposure)

### Buprenorphine
| Feature | Include? | Justification |
|---|---|---|
| `buprenorphine_days` | ✅ Yes | Pre-study MOUD exposure; duration may predict readiness/tolerance |
| `buprenorphine_streak` | ❌ No | A long streak = consistent medication-taking (desirable), not compulsive use. Streak is semantically inverted here |
| `buprenorphine_binary` | ✅ Yes | Any prior buprenorphine use (see "Took THEIR study drug" feature) |

### Suboxone
| Feature | Include? | Justification |
|---|---|---|
| `suboxone_days` | ✅ Yes | Treat as buprenorphine-equivalent. May combine with `buprenorphine_days` at tibble-building time |
| `suboxone_streak` | ❌ No | Same reasoning as buprenorphine |
| `suboxone_binary` | ✅ Yes | Combined with `buprenorphine_binary` for the "Took THEIR study drug" feature |

### Methadone
| Feature | Include? | Justification |
|---|---|---|
| `methadone_days` | ✅ Yes | Pre-study methadone exposure is relevant for CTN-0028 participants |
| `methadone_streak` | ❌ No | Same reasoning as buprenorphine |
| `methadone_binary` | ✅ Yes | Any prior methadone use |

---

## Prescription / Non-Addictive Medications

### Opioid group (prescription: Codeine, Hydrocodone, Oxycodone, etc.)
| Feature | Include? | Justification |
|---|---|---|
| `opioid_days` | ✅ Yes | Prescription opioid misuse before study; distinct from heroin/fentanyl in route and access pattern |
| `opioid_streak` | ✅ Yes | Compulsive prescription opioid use is addictive; streak meaningful |
| `opioid_binary` | ✅ Yes | Any prescription opioid misuse vs. none |

**Warning:** Acetaminophen is mapped into this group but is not an opioid —
it likely appears as the APAP component of combination pills (Vicodin,
Percocet). Verify whether Acetaminophen rows co-occur with opioid rows on the
same person-day before finalising. If they do, remove `"Acetaminophen" =
"Opioid"` from `drug_map` to avoid double-counting.

### Analgesic — Gabapentin
| Feature | Include? | Justification |
|---|---|---|
| `analgesic_days` | ✅ Yes (23 events observed) | Gabapentin misuse is increasing in OUD populations; include if sufficient events |
| `analgesic_streak` | ❌ No | Not established as a substance with compulsive-use streaks in OUD context; omit |
| `analgesic_binary` | ✅ Yes (23 events observed) | Any gabapentin use (prescription adherence / misuse flag) |

**Note:** 23 events observed — passes the ≥10 filter. Updated from
conditional ⚠️ to unconditional ✅. Consider renaming to `"Gabapentin"`
in `drug_map` (see Features_To_Include_Main.Rmd warning).

### Antidepressant (Trazodone + Tricyclic)
| Feature | Include? | Justification |
|---|---|---|
| `antidepressant_days` | ⚠️ Conditional (≥10 events) | Functions as a prescription medication adherence marker, not a substance of abuse |
| `antidepressant_streak` | ❌ No | Streak in antidepressants = medication compliance (desirable); semantically inverted |
| `antidepressant_binary` | ⚠️ Conditional (≥10 events) | "On antidepressants at baseline" binary may carry psychiatric comorbidity signal |

**Note:** Antidepressant was not visible in the observed events table — check
whether it survived the filter. If absent, skip.

### Antiemetic
| Feature | Include? | Justification |
|---|---|---|
| `antiemetic_days` | ✅ Yes (16 events; prescribed — exempt from secondary filter) | Anti-nausea medication (e.g. ondansetron). Rare but kept as standalone; also feeds rx composite |
| `antiemetic_streak` | ❌ No | Prescription medication; streak semantics inverted |
| `antiemetic_binary` | ✅ Yes | Any antiemetic use flag; also feeds `rx_any_binary` composite |

**Note:** Antiemetic was not in the original drug_map and keeps its original
name. Preserved by the `prescribed_or_legal` exemption in the secondary filter
despite only 16 events.

### Muscle Relaxant
| Feature | Include? | Justification |
|---|---|---|
| `muscle_relaxant_days` | ❌ Not standalone | Kept in `all_drugs_filtered` but does not generate its own feature columns; contributes to `rx_any_binary` / `rx_days` / `rx_categories` composite only |
| `muscle_relaxant_streak` | ❌ No | |
| `muscle_relaxant_binary` | ❌ Not standalone | |

**Note:** 92 events. Kept in `all_drugs_filtered` at user request to
operationalise the "takes medications on schedule" hypothesis via the rx
composite. Not a standalone predictor of OUD relapse.

### MDMA / Hallucinogen
| Feature | Include? | Justification |
|---|---|---|
| `mdma_days` | ⚠️ Flagged for deletion | ~100 events after merging Hallucinogen in; weak link to OUD relapse; may be dropped by secondary filter (< 1% users) |
| `mdma_streak` | ❌ No | |
| `mdma_binary` | ⚠️ Flagged for deletion | |

**Note:** Originally `"Mdma/Hallucinogen"` (from `"Mdma"` in drug_map) plus
a separate `"Hallucinogen"` category — both merged into `"MDMA/Hallucinogen"`.
Combined ~100 events. Kept in code with a deletion flag; remove after
confirming < 1% prevalence or at next pipeline cleanup.

---

## Nicotine

### Nicotine
| Feature | Include? | Justification |
|---|---|---|
| `nicotine_days` | ⚠️ Conditional (≥10 events; may not exist) | Comorbid nicotine dependence predicts OUD outcomes; raw days count adds information beyond Fagerstrom score |
| `nicotine_streak` | ❌ No | Almost all smokers smoke every day; streak will be near-constant and low-variance |
| `nicotine_binary` | ⚠️ Conditional (only if nicotine records exist) | Ever-smoker binary is high value — but if nicotine is absent from `all_drugs` entirely, rely on Fagerstrom score alone |

**Note:** Nicotine was not visible in the filtered events table. Run the
investigation query in the Drug Map Changes section above. If truly absent,
skip both features.

---

## Caffeine

### Caffeine
| Feature | Include? | Justification |
|---|---|---|
| `caffeine_days` | ❌ Omit | No established relationship with OUD relapse; near-universal use creates near-zero variance |
| `caffeine_streak` | ❌ No | |
| `caffeine_binary` | ❌ Omit | |

---

## Summary table

| Drug | days | streak | binary | Notes |
|---|---|---|---|---|
| Heroin | ✅ | ✅ | ✅ | |
| Fentanyl | ✅ | ✅ | ✅ | |
| Opioid (rx group) | ✅ | ✅ | ✅ | Acetaminophen warning — verify |
| Cocaine | ✅ | ✅ | ✅ | |
| Crack | ✅ | ✅ | ✅ | Updated from ⚠️; 2889 events |
| Methamphetamine | ✅ | ✅ | ✅ | |
| Amphetamine | ✅ | ✅ | ✅ | New; 844 events |
| Benzodiazepine | ✅ | ✅ | ✅ | |
| Sedatives | ✅ | ✅ | ✅ | Updated from ⚠️; 263 events |
| Cannabinoids | ✅ | ✅ | ✅ | Formerly THC; K2 merged in |
| Alcohol (hard) | ✅ | ✅ | ✅ | |
| Alcohol (light) | ✅ | ❌ | ✅ | |
| Alcohol Missing Amnt | ✅ | ❌ | ✅ | New; 1449 events |
| Buprenorphine | ✅ | ❌ | ✅ | |
| Suboxone | ✅ | ❌ | ✅ | |
| Methadone | ✅ | ❌ | ✅ | |
| Analgesic (Gabapentin) | ✅ | ❌ | ✅ | Updated from ⚠️; 23 events |
| Antidepressant | ⚠️ | ❌ | ⚠️ | Check if in filtered data |
| Antiemetic | ✅ | ❌ | ✅ | New; 16 events; also feeds rx composite |
| Muscle Relaxant | ❌ | ❌ | ❌ | rx composite only |
| MDMA/Hallucinogen | ⚠️ | ❌ | ⚠️ | Flagged for deletion |
| Nicotine | ⚠️ | ❌ | ⚠️ | Not found in data — investigate |
| Caffeine | ❌ | ❌ | ❌ | |

✅ Include | ⚠️ Conditional or flagged | ❌ Omit
