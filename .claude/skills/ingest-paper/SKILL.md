---
name: ingest-paper
description: Ingest an academic or agency paper into docs/papers/ (manifest row, structured summary, extracted parameters with source locations, verification pass, model implications with file:line). Use when a PDF is dropped into docs/papers/, when the user asks to read a paper for what it implies for the model, or before hard-coding a number taken from the literature.
argument-hint: <path to PDF, or paper_id to re-verify>
---

Ingest the paper at: $ARGUMENTS

Read `docs/papers/README.md` first: it defines the columns, the `paper_id` scheme, and
the `# lit:` citation convention. The pilot entry,
`comey_gallagher_gorman_2025_salt_workarounds`, is the worked example for every step.

## Steps

1. **Register.**
   - Pick the `paper_id` and move the PDF to `docs/papers/pdfs/<paper_id>.pdf`.
   - Compute its sha256 (`Rscript -e "cat(digest::digest(file='<pdf>', algo='sha256'))"`).
   - Add the `manifest.csv` row with `status = registered`, keeping the arriving file name in
     `source_filename`.
   - If a row for the same paper already exists, this is a new draft: update `version`,
     `pdf_file` and `sha256`, set `status = registered`, and re-verify everything below.

2. **Extract text.**
   - Write the text to the session scratchpad, not to the repo:
     `Rscript -e "writeLines(pdftools::pdf_text('<pdf>'), '<scratch>/<paper_id>.txt')"`.
   - Prefix each page so `source_loc` can cite page numbers.
   - Read the whole paper, including the appendix tables. Headline numbers often live in
     appendix tables or footnotes.

3. **Summarize.**
   - Write `docs/papers/summaries/<paper_id>.md`. Copy the front matter and section order
     from the pilot summary: Question · Data and method · Headline findings (table with
     locations) · Implications for the model · Model comparison · Follow-ups · Caveats and
     inconsistencies in the paper · Revision history.
   - Then set `status = read`.

4. **Extract parameters.**
   - Add one `parameters.csv` row per number the model could use, be calibrated to, or be
     checked against.
   - Fill `source_loc` with table/panel/page every time.
   - Keep a multi-row table (by state, by year, by percentile) whole in
     `docs/papers/tables/<paper_id>_<name>.csv`, with one pointer row in `parameters.csv`.
   - Then set `status = extracted`.

5. **Verify, as a separate pass.**
   - Re-find every value in the extracted text and check the arithmetic that links table
     rows (shares sum, products reproduce totals, log effects convert to percentages).
   - Check multi-row tables by script against the text, not by eye.
   - Set `verified = y` only for numbers you re-found.
   - Record any internal inconsistencies of the paper in the summary and in the row's
     `note`. Do not silently pick one.
   - When every row is `y`, set `status = verified`.

6. **Map onto the model.**
   - Grep `src/`, `config/` and `research/` for the mechanism, variables and YAML keys the
     paper touches.
   - Write each implication with file:line references, and say what the model does today
     before saying what the paper implies.
   - Fill `model_target` in `parameters.csv` and `model_areas` in the manifest.
   - Do not change model code or parameters as part of ingestion. List those changes as
     follow-ups for the user to approve.

7. **Benchmark (optional, when the paper reports aggregates the model produces).**
   - Write `docs/papers/benchmarks/<paper_id>.R`, which reads detail files from a named
     runscript's output and prints model against paper by `param_id`.
   - Smoke-test it on synthetic detail files in the scratchpad.
   - Model runs need the cluster's dependency data, so record in the summary whether the
     benchmark has run, with vintage, sample fraction and commit.

8. **Validate and index.**
   - Run `Rscript docs/papers/tools/check_papers.R`. It must print `OK`.
   - Add the paper to the table at the bottom of `docs/papers/README.md`.

## Rules

- Numbers come from the PDF text, never from memory or from a citing paper.
- If a number looks wrong, go back to the source and check it. Do not explain it away.
- A state YAML `reference:` cites statute or official guidance, not a paper. A paper can
  point you to the statute.
- Do not stage blind. Show `git status` for the files you added and let the user decide
  whether to commit.
