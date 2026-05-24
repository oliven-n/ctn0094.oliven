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

## Alcohol

All three alcohol categories are renamed in `drug_map` to begin with "Alcohol"
so they sort together alphabetically. No aggregation occurs — they remain
distinct features.

### Alcohol Heavy Amnt
| Feature | Include? | Justification |
|---|---|---|
| `alcohol_heavy_amnt_days` | ✅ Yes | Heavy drinking is a robust predictor of opioid relapse; distinct from moderate drinking |
| `alcohol_heavy_amnt_streak` | ✅ Yes | Heavy drinking streaks (bingeing) predict worse outcomes than isolated events |
| `alcohol_heavy_amnt_binary` | ✅ Yes | Any heavy drinking vs. none |

### Alcohol Light Amnt
| Feature | Include? | Justification |
|---|---|---|
| `alcohol_light_amnt_days` | ✅ Yes | Include separately from heavy; captures overall drinking behaviour and restraint when used together |
| `alcohol_light_amnt_streak` | ❌ No | Light drinking streaks have low clinical significance for OUD prediction; omit to reduce noise |
| `alcohol_light_amnt_binary` | ✅ Yes | Any light drinking vs. abstinence |

### Alcohol Missing Amnt
| Feature | Include? | Justification |
|---|---|---|
| `alcohol_missing_amnt_days` | ✅ Yes | Days where alcohol was detected but quantity was not recorded; captures data gaps and may carry its own under-reporting signal |
| `alcohol_missing_amnt_streak` | ❌ No | Streak has no interpretable meaning when quantity (heavy vs. light) is unknown. TODO: re-evaluate if usage patterns closely track heavy drinking |
| `alcohol_missing_amnt_binary` | ✅ Yes | Presence of unquantified alcohol records may correlate with under-reporting behaviour |

**Note:** The "restraint" feature (`alcohol_heavy_amnt_days − alcohol_light_amnt_days`)
is a derived feature computed separately at tibble-building time (section 6).
Keep the raw counts as separate features too.

---

## Amphetamine

### Amphetamine
| Feature | Include? | Justification |
|---|---|---|
| `amphetamine_days` | ✅ Yes | Stimulant co-use; distinct from methamphetamine in form, access pattern, and demographic associations |
| `amphetamine_streak` | ✅ Yes | Addictive with binge-pattern use; streak captures compulsive escalation |
| `amphetamine_binary` | ✅ Yes | Any vs. none is clinically meaningful |

---

## Analgesic

### Analgesic (Gabapentin + Acetaminophen)
| Feature | Include? | Justification |
|---|---|---|
| `analgesic_days` | ✅ Yes | Prescription non-opioid pain/withdrawal management; functions as a medication engagement marker |
| `analgesic_streak` | ❌ No | Not established as a substance with compulsive-use streaks; streak in non-addictive Rx is semantically inverted (streak = compliance, not escalation) and would contaminate streak aggregations |
| `analgesic_binary` | ✅ Yes | Any gabapentin/acetaminophen use = baseline prescription engagement signal |

**Note:** Gabapentin has borderline misuse potential in OUD populations but is grouped here because its primary role in this dataset is as a legally prescribed non-opioid analgesic. Acetaminophen piggybacks on gabapentin's event count to survive the primary filter (5 + 23 = 28 combined events).

---

## Antiemetic

### Antiemetic
| Feature | Include? | Justification |
|---|---|---|
| `antiemetic_days` | ✅ Yes | Proxy for withdrawal symptom severity and medication adherence |
| `antiemetic_streak` | ❌ No | Streak in non-addictive Rx is semantically inverted and would contaminate streak aggregations with a positive-valence signal |
| `antiemetic_binary` | ✅ Yes | "On antiemetics at baseline" indicates withdrawal management engagement |

---

## Benzodiazepine

### Benzodiazepine
| Feature | Include? | Justification |
|---|---|---|
| `benzodiazepine_days` | ✅ Yes | Cross-addiction with opioids; one of the strongest predictors of overdose and relapse |
| `benzodiazepine_streak` | ✅ Yes | Physical dependence develops with sustained use; streak reflects severity |
| `benzodiazepine_binary` | ✅ Yes | Any benzo use at intake is a major clinical flag |

---

## Buprenorphine

### Buprenorphine
| Feature | Include? | Justification |
|---|---|---|
| `buprenorphine_days` | ✅ Yes | Pre-study MOUD exposure; duration may predict readiness/tolerance |
| `buprenorphine_streak` | ❌ No | A long streak = consistent medication-taking (desirable), not compulsive use. Streak is semantically inverted here and would contaminate streak aggregations |
| `buprenorphine_binary` | ✅ Yes | Any prior buprenorphine use |

---

## Cannabinoids

### Cannabinoids (THC + K2)
| Feature | Include? | Justification |
|---|---|---|
| `cannabinoids_days` | ✅ Yes | Cannabis use has a complex bidirectional relationship with opioid use; include to let the model find the direction |
| `cannabinoids_streak` | ✅ Yes | Cannabis use disorder is real; streak captures habitual use vs. occasional |
| `cannabinoids_binary` | ✅ Yes | Any use vs. none |

**Note:** THC and K2 (synthetic cannabinoid) are combined into one category via `drug_map`.

---

## Cocaine

### Cocaine
| Feature | Include? | Justification |
|---|---|---|
| `cocaine_days` | ✅ Yes | Stimulant co-use strongly predicts opioid relapse (polydrug severity marker) |
| `cocaine_streak` | ✅ Yes | Binge-pattern cocaine use captured by streak; addictive |
| `cocaine_binary` | ✅ Yes | Any vs. no cocaine use is a meaningful severity split |

---

## Crack

### Crack cocaine
| Feature | Include? | Justification |
|---|---|---|
| `crack_days` | ✅ Yes (if ≥10 events) | Distinct use pattern from powder cocaine |
| `crack_streak` | ✅ Yes (if ≥10 events) | Same rationale as cocaine |
| `crack_binary` | ✅ Yes (if ≥10 events) | |

**Note:** If both `cocaine_days` and `crack_days` survive the filter but
`crack_days` is very sparse (10–30 events), consider whether a composite
`stimulant_days = cocaine_days + crack_days` is more useful. Leave this
decision to the ML pipeline unless counts are extremely low.

---

## Fentanyl

### Fentanyl
| Feature | Include? | Justification |
|---|---|---|
| `fentanyl_days` | ✅ Yes | Increasingly prevalent; pharmacologically distinct from heroin (faster onset, higher potency) |
| `fentanyl_streak` | ✅ Yes | Same rationale as heroin |
| `fentanyl_binary` | ✅ Yes | Any fentanyl use is clinically meaningful (overdose risk, tolerance) |

---

## Heroin

### Heroin
| Feature | Include? | Justification |
|---|---|---|
| `heroin_days` | ✅ Yes | Strongest single predictor of OUD relapse; graded dose-response with outcome |
| `heroin_streak` | ✅ Yes | Streak captures compulsive dependent use, not just occasional use |
| `heroin_binary` | ✅ Yes | Any heroin use vs. none is a clinically meaningful threshold; useful for linear models |

---

## MDMA/Hallucinogen

### MDMA / Hallucinogen
| Feature | Include? | Justification |
|---|---|---|
| `mdma_hallucinogen_days` | ✅ Yes | Rare but present (passed primary filter); retained since secondary filter is not applied |
| `mdma_hallucinogen_streak` | ❌ No | Weak established link to OUD relapse; streak unlikely to carry additional signal |
| `mdma_hallucinogen_binary` | ✅ Yes | Any use flag retained for completeness |

---

## Methadone

### Methadone
| Feature | Include? | Justification |
|---|---|---|
| `methadone_days` | ✅ Yes | Pre-study methadone exposure is relevant for CTN-0028 participants |
| `methadone_streak` | ❌ No | Same reasoning as buprenorphine — streak = compliance, semantically inverted |
| `methadone_binary` | ✅ Yes | Any prior methadone use |

---

## Methamphetamine

### Methamphetamine
| Feature | Include? | Justification |
|---|---|---|
| `methamphetamine_days` | ✅ Yes | Increasingly common in OUD populations; high addiction potential |
| `methamphetamine_streak` | ✅ Yes | Binge/run use pattern; streak meaningful |
| `methamphetamine_binary` | ✅ Yes | Any vs. none split is clinically relevant |

---

## Muscle Relaxant

### Muscle Relaxant
| Feature | Include? | Justification |
|---|---|---|
| `muscle_relaxant_days` | ✅ Yes | Passed primary filter; prescription engagement signal analogous to analgesic/antiemetic |
| `muscle_relaxant_streak` | ❌ No | Non-addictive Rx; streak semantically inverted and would contaminate streak aggregations |
| `muscle_relaxant_binary` | ✅ Yes | Any use flag; prescription adherence marker |

---

## Opioid

### Opioid group (prescription: Codeine, Hydrocodone, Oxycodone, etc.)
| Feature | Include? | Justification |
|---|---|---|
| `opioid_days` | ✅ Yes | Prescription opioid misuse before study; distinct from heroin/fentanyl in route and access pattern |
| `opioid_streak` | ✅ Yes | Compulsive prescription opioid use is addictive; streak meaningful |
| `opioid_binary` | ✅ Yes | Any prescription opioid misuse vs. none |

---

## PCP

### PCP (Phencyclidine)
| Feature | Include? | Justification |
|---|---|---|
| `pcp_days` | ✅ Yes | Frequency of use is a meaningful severity signal in a polydrug OUD context |
| `pcp_streak` | ✅ Yes | PCP has psychological dependence and habituation potential; without a withdrawal driver, long streaks reflect compulsive/habitual use rather than physical necessity — still clinically meaningful |
| `pcp_binary` | ✅ Yes | Any PCP use vs. none is a relevant behavioural marker in this population |

---

## Sedatives

### Sedatives (Sedative-Hypnotic + Barbiturate)
| Feature | Include? | Justification |
|---|---|---|
| `sedatives_days` | ✅ Yes (if ≥10 events) | Non-benzo CNS depressants; hypnotics (Ambien etc.) remain common |
| `sedatives_streak` | ✅ Yes (if ≥10 events) | Physical dependence potential; streak meaningful if events present |
| `sedatives_binary` | ✅ Yes (if ≥10 events) | Any use flag |

---

## Suboxone

### Suboxone
| Feature | Include? | Justification |
|---|---|---|
| `suboxone_days` | ✅ Yes | Treat as buprenorphine-equivalent; may combine with `buprenorphine_days` at modelling time |
| `suboxone_streak` | ❌ No | Same reasoning as buprenorphine |
| `suboxone_binary` | ✅ Yes | Combined with `buprenorphine_binary` for the "Took THEIR study drug" feature |

---

## Excluded categories

### Antidepressant (Trazodone + Tricyclic)
Dropped by primary filter: 3 events across 3 people in the pre-randomisation window. Will be revisited as part of a future legal/prescription medicine engagement composite feature built from raw `all_drugs` labels before filtering.

### Caffeine
Not included. No established relationship with OUD relapse; near-universal use creates near-zero variance.

---

## Summary table

| Drug | days | streak | binary |
|---|---|---|---|
| Alcohol Heavy Amnt | ✅ | ✅ | ✅ |
| Alcohol Light Amnt | ✅ | ❌ | ✅ |
| Alcohol Missing Amnt | ✅ | ❌ | ✅ |
| Amphetamine | ✅ | ✅ | ✅ |
| Analgesic (Gabapentin + Acetaminophen) | ✅ | ❌ | ✅ |
| Antiemetic | ✅ | ❌ | ✅ |
| Benzodiazepine | ✅ | ✅ | ✅ |
| Buprenorphine | ✅ | ❌ | ✅ |
| Cannabinoids (THC + K2) | ✅ | ✅ | ✅ |
| Cocaine | ✅ | ✅ | ✅ |
| Crack | ✅ | ✅ | ✅ |
| Fentanyl | ✅ | ✅ | ✅ |
| Heroin | ✅ | ✅ | ✅ |
| MDMA/Hallucinogen | ✅ | ❌ | ✅ |
| Methadone | ✅ | ❌ | ✅ |
| Methamphetamine | ✅ | ✅ | ✅ |
| Muscle Relaxant | ✅ | ❌ | ✅ |
| Opioid (rx group) | ✅ | ✅ | ✅ |
| PCP | ✅ | ✅ | ✅ |
| Sedatives | ✅ | ✅ | ✅ |
| Suboxone | ✅ | ❌ | ✅ |

✅ Include | ❌ Omit
