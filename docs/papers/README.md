---
title: "Literature store: papers that inform the model"
role: index
workstream: literature
status: current
updated: 2026-10-28
sot: self
supersedes: []
superseded_by: null
---

# `docs/papers/`: papers that inform the model

This folder holds papers whose findings or parameters bear on the model, along with what we
extracted from each. Before you hard-code a number taken from a paper (an elasticity, a
take-up rate, a calibration target), look for it in `parameters.csv`. If it is not there,
ingest the paper first, then cite the row.

## Layout

| Path | What it holds |
|---|---|
| `manifest.csv` | One row per paper: bibliographic fields, PDF path and sha256, status, summary, model areas touched |
| `parameters.csv` | One row per extracted quantity, in long format, with its exact source location |
| `pdfs/<paper_id>.pdf` | The paper, committed and renamed to its `paper_id` |
| `summaries/<paper_id>.md` | One structured reading per paper: findings, implications for the model with file:line references, model comparison, caveats |
| `tables/<paper_id>_<name>.csv` | Multi-row tables worth keeping whole (e.g. per-state law). Each one gets a pointer row in `parameters.csv` |
| `benchmarks/<paper_id>.R` | Optional. Compares model output against the paper's numbers |
| `tools/check_papers.R` | Validator. Run it after any change here |

Scripts here are never sourced by `src/`, and `main.R` does not walk this folder.

## `paper_id`

A snake_case citekey: `<surnames>_<year>_<keyword>`, e.g.
`comey_gallagher_gorman_2025_salt_workarounds`. With more than three authors, use the first
surname plus `etal`. The citekeys in John's references-library `master.bib` use the same
snake_case scheme.

## `manifest.csv`

| Column | Content |
|---|---|
| `paper_id` | as above |
| `title, authors, year, version, venue, url` | `authors` are `;`-separated; `version` says which draft (working papers change) |
| `source_filename` | the file name the PDF arrived with |
| `pdf_file, sha256` | repo-root-relative path and hash. If the PDF is replaced with a new draft, update both and re-verify `parameters.csv` |
| `added, added_by` | date and initials |
| `status` | `registered` (PDF and row only) → `read` (summary written) → `extracted` (`parameters.csv` rows) → `verified` (every row re-checked against the text) |
| `summary_file` | repo-root-relative path |
| `topics` | `;`-separated keywords |
| `model_areas` | `;`-separated repo paths the paper bears on |

## `parameters.csv`

| Column | Content |
|---|---|
| `param_id` | `<paper_id>.<slug>`, unique; what code and docs cite |
| `paper_id` | must exist in the manifest |
| `quantity` | plain-language description |
| `value, unit, se` | the number as printed (`se` blank if none) |
| `tax_year, population` | what the number describes |
| `source_loc` | table/figure/page, e.g. `Table 3 Panel B, p.28`. Required |
| `use` | `calibration_target` (the model should hit it), `parameter` (the model could use it directly), `validation_benchmark` (compare against it), or `context` |
| `model_target` | the variable, function or YAML key the number bears on |
| `verified` | `y` only after a separate pass re-found the number in the text |
| `note` | derivations, inconsistencies, caveats |

## How the model cites a paper

Put a comment next to the YAML key or R line that uses or is calibrated to the number:

```yaml
# lit: comey_gallagher_gorman_2025_salt_workarounds.ptet_ded_indiv_2024
```

`grep -rn "# lit:"` lists every place the literature reaches the model.
`grep <param_id> docs/papers/parameters.csv` finds the number and its source. State YAML
`reference:` fields still cite statute and official guidance, not papers.

## Ingesting a paper

Follow `.claude/skills/ingest-paper/SKILL.md`. In short:
1. Move the PDF to `pdfs/<paper_id>.pdf` and add the manifest row.
2. Extract the text.
3. Write the summary.
4. Extract the parameters with their `source_loc`.
5. Verify each number in a separate pass.
6. Map the implications onto the code with file:line references.
7. Run the validator:

```bash
Rscript docs/papers/tools/check_papers.R
```

## Papers

| `paper_id` | Bears on | Summary |
|---|---|---|
| `comey_gallagher_gorman_2025_salt_workarounds` | PTET / SALT cap workaround (`do_salt_workaround_baseline()`), state PTET credits | [summary](summaries/comey_gallagher_gorman_2025_salt_workarounds.md) |
