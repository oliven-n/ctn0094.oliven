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
| `sedatives_days` | ⚠️ Conditional (≥10 events) | Non-benzo CNS depressants; barbiturates are largely out of use but hypnotics (Ambien etc.) remain common |
| `sedatives_streak` | ⚠️ Conditional (≥10 events) | If events are present, streak is meaningful (physical dependence potential) |
| `sedatives_binary` | ⚠️ Conditional (≥10 events) | Any use flag |

---

## Cannabis

### THC
| Feature | Include? | Justification |
|---|---|---|
| `thc_days` | ✅ Yes | Cannabis use has a complex bidirectional relationship with opioid use; include to let the model find the direction |
| `thc_streak` | ✅ Yes | Cannabis use disorder is real; streak captures habitual use vs. occasional |
| `thc_binary` | ✅ Yes | Any use vs. none |

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
proposed by Nat is a derived feature to be computed separately at tibble-
building time. Keep the raw counts as separate features too.

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
| `suboxone_days` | ✅ Yes | Per Nat's note, treat as buprenorphine-equivalent. May combine with `buprenorphine_days` at tibble-building time |
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

### Analgesic — Gabapentin
| Feature | Include? | Justification |
|---|---|---|
| `analgesic_days` | ⚠️ Conditional (≥10 events) | Gabapentin misuse is increasing in OUD populations; include if sufficient events |
| `analgesic_streak` | ❌ No | Not established as a substance with compulsive-use streaks in OUD context; omit |
| `analgesic_binary` | ⚠️ Conditional (≥10 events) | Any gabapentin use (prescription adherence / misuse flag) |

### Antidepressant (Trazodone + Tricyclic)
| Feature | Include? | Justification |
|---|---|---|
| `antidepressant_days` | ⚠️ Conditional (≥10 events) | Functions as a prescription medication adherence marker, not a substance of abuse |
| `antidepressant_streak` | ❌ No | Streak in antidepressants = medication compliance (desirable); semantically inverted |
| `antidepressant_binary` | ⚠️ Conditional (≥10 events) | "On antidepressants at baseline" binary may carry psychiatric comorbidity signal |

### Muscle Relaxant
| Feature | Include? | Justification |
|---|---|---|
| `muscle_relaxant_days` | ❌ Likely omit | Low clinical relevance for OUD relapse; likely very sparse and dropped by filter |
| `muscle_relaxant_streak` | ❌ No | |
| `muscle_relaxant_binary` | ❌ Likely omit | |

### Mdma / Hallucinogen
| Feature | Include? | Justification |
|---|---|---|
| `mdma_days` | ❌ Likely omit | Rare in this population; likely dropped by filter; weak established link to OUD relapse |
| `mdma_streak` | ❌ No | |
| `mdma_binary` | ❌ Likely omit | |

---

## Nicotine

### Nicotine
| Feature | Include? | Justification |
|---|---|---|
| `nicotine_days` | ⚠️ Conditional (≥10 events) | Comorbid nicotine dependence predicts OUD outcomes; raw days count adds information beyond Fagerstrom score |
| `nicotine_streak` | ❌ No | Almost all smokers smoke every day; streak will be near-constant and low-variance |
| `nicotine_binary` | ✅ Yes | Ever-smoker binary is high value regardless of days count; include unconditionally |

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

| Drug | days | streak | binary |
|---|---|---|---|
| Heroin | ✅ | ✅ | ✅ |
| Fentanyl | ✅ | ✅ | ✅ |
| Opioid (rx group) | ✅ | ✅ | ✅ |
| Cocaine | ✅ | ✅ | ✅ |
| Crack | ⚠️ | ⚠️ | ⚠️ |
| Methamphetamine | ✅ | ✅ | ✅ |
| Benzodiazepine | ✅ | ✅ | ✅ |
| Sedatives | ⚠️ | ⚠️ | ⚠️ |
| THC | ✅ | ✅ | ✅ |
| Alcohol (hard) | ✅ | ✅ | ✅ |
| Alcohol (light) | ✅ | ❌ | ✅ |
| Buprenorphine | ✅ | ❌ | ✅ |
| Suboxone | ✅ | ❌ | ✅ |
| Methadone | ✅ | ❌ | ✅ |
| Analgesic (Gabapentin) | ⚠️ | ❌ | ⚠️ |
| Antidepressant | ⚠️ | ❌ | ⚠️ |
| Muscle Relaxant | ❌ | ❌ | ❌ |
| Mdma/Hallucinogen | ❌ | ❌ | ❌ |
| Nicotine | ⚠️ | ❌ | ✅ |
| Caffeine | ❌ | ❌ | ❌ |

✅ Include | ⚠️ Conditional on ≥10 events in the −28 to −1 window | ❌ Omit
