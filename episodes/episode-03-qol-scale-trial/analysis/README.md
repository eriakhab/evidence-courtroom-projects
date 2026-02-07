# Episode 03 — Psychometric Analysis Documentation

This folder contains statistical scripts used to evaluate the
measurement validity and invariance of a quality-of-life scale
in hypertensive patients.

## Objectives

The primary goals of this analysis were to:

- Assess construct validity using Confirmatory Factor Analysis (CFA)
- Evaluate item properties using Item Response Theory (IRT)
- Test measurement invariance across sex and race groups
- Compare estimation methods and model stability

## Methods

### Confirmatory Factor Analysis (CFA)

- Estimation: Full Information Maximum Likelihood (FIML)
- Script: cfa_fiml.txt
- Purpose: Evaluate latent factor structure and model fit

### Item Response Theory (IRT)

- Estimation methods:
  - FIML
  - WLSMV
- Scripts:
  - irt_fiml.txt
  - irt_wlsmv.txt
- Purpose: Assess item discrimination and difficulty

### Measurement Invariance

- Groups tested:
  - Sex
  - Race
- Scripts:
  - invariance_sex.txt
  - invariance_race.txt
- Purpose: Evaluate cross-group comparability

## Model Evaluation

Models were assessed using:

- Global fit indices (CFI, TLI, RMSEA, SRMR)
- Information criteria (when applicable)
- Parameter stability
- Nested model comparisons

## Reproducibility

All analyses are reproducible using:

- Datasets in /data
- Scripts in this folder
- Technical interpretation in /writeups

This structure enables independent verification of results.
