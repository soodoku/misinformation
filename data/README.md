# Data

| File | Contents | Source |
|---|---|---|
| `raw/media_poll_items_2018.csv` | 180 misinformation items from media polls, 2003-2017, with the authors' original 2018 coding | Compiled by the authors from the Roper Center's iPoll archive |
| `raw/roper_toplines.csv` | Full response distributions for 133 of those items, one row per response category, with Roper question IDs | Roper Center iPoll |
| `raw/mturk_july_2017.csv` | Survey experiment, Amazon Mechanical Turk, 9 July 2017 (Qualtrics export) | Same file as `data/turk/mam_mturk_070917.csv` in `soodoku/partisan-gaps` |

`R/sources.R` checks each file against a SHA-256 hash before any analysis.

The original coding in `media_poll_items_2018.csv` is kept as it was; the
analysis recodes every item. `docs/item_decisions.csv` records which items are
kept and why the rest are dropped, whether a don't-know option was offered, and
whether the stem repeats the false claim. `docs/response_keys.csv` and
`docs/numeric_keys.csv` score each response category in the toplines, and
`docs/manual_shares.csv` gives the shares, with their basis, for the 26 kept
items that have no topline file.

The survey file is anonymized: IP addresses, latitude and longitude, city, and
postal code are blank. The Qualtrics questionnaire is in
`docs/questionnaire_mturk_july_2017.pdf`.

Roper Center data are used under the Center's terms; the question texts and
toplines here are the minimum needed to reproduce the scoring.
