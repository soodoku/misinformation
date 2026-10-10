# Misinformation about Misinformation? Of Headlines and Survey Design

Robert C. Luskin, Gaurav Sood, Yul Min Park, and Joshua Blank

[Paper](ms/main.pdf) · [Data](data/README.md)

## Question and motivation

How much apparent misinformation comes from the way survey questions are asked?
Media-poll items often invite opinions and omit an explicit “don't know” option.
Those features can turn guesses into answers that headlines present as false
beliefs. The companion paper, [The Waters of Casablanca](https://github.com/finite-sample/know_casablanca),
develops the distinction between misinformation and less confident belief and
the 0–10 measure used here.

## Data and research design

The study codes the wording and response options of media-poll misinformation
items, then tests question design in a randomized survey experiment. Mechanical
Turk respondents received one of five versions of nine items: media wording,
versions changing don't-know options and claim cues, or a confidence scale.
The corpus describes polling practice; random assignment identifies the effects
of the tested question versions within the experimental sample. The sample is
not representative of the U.S. public.

## Key findings

Offering and encouraging “don't know” substantially reduces incorrect answers.
A confidence scale finds still less misinformation. Adding background and
repeating the false claim has little average effect in the tested comparisons.
Incorrect multiple-choice answers therefore overstate confidently held false
beliefs in these experiments.

![Incorrect answers by question version](figs/arms.png)

Points show the share answering each item incorrectly, with 95% Wilson intervals.
For the scale, an incorrect answer means choosing the false endpoint. Don't-know
and skipped answers count as not incorrect.

## Reproduce

```
make restore
make check
```

`make restore` installs the package versions in `renv.lock`. `make check` rebuilds the analysis and manuscript, lints the R code, and runs the tests. R 4.6 and XeLaTeX with `latexmk` are required. `make ci-docker` runs these checks in the project's standard Rocker image.

## Files and pipeline

| Path | Contents |
|---|---|
| `data/raw/` | The media-poll corpus, Roper toplines, and the survey experiment; see [data/README.md](data/README.md) |
| `docs/` | Item-level decisions, answer keys, the questionnaire, and how each citation was checked |
| `R/` | Reusable functions: source validation, corpus scoring, experimental estimates, and output writers. These files do not run the analysis. |
| `scripts/` | Configuration and numbered execution stages, listed below. |
| `data/derived/` | Generated intermediate data; ignored by Git. |
| `ms/` | `main.tex`, `references.bib`, and the compiled `main.pdf` |
| `tests/testthat/` | Reproductions of the 2018 draft's numbers, tests of the answer keys, and a check that the survey data carry no direct identifiers |

### Execution stages

`make analysis` runs `scripts/99_run_all.R`. It loads the configuration and reusable functions in `R/`, then executes stages 01–04 in separate environments. Stages pass results through files.

| File | Purpose |
|---|---|
| `scripts/00_config.R` | Paths, source hashes, labels, colors, `theme_paper()`, figure sizes, and `table_style`. |
| `scripts/01_prepare_data.R` | Verify source hashes, read and score the inputs, and save `data/derived/prepared_data.rds`. |
| `scripts/02_estimate.R` | Read the prepared data and write estimates to `tabs/*.csv`. |
| `scripts/03_figures.R` | Read the estimates and write PDF and PNG figures to `figs/`. |
| `scripts/04_tables.R` | Generate LaTeX tables, manuscript numbers, and `tabs/style.tex`. |
| `scripts/99_run_all.R` | Run all stages in order. |

Change plot and table defaults in `00_config.R`. The manuscript applies the generated `\TableStyle` to each table. Statistical and coding functions stay in `R/corpus.R` and `R/experiment.R`, where tests can call them without running the pipeline.

