---
name: run-sim
description: Run a Tax-Simulator simulation. Use when the user wants to execute a runscript on SLURM or locally via Rscript. Handles validation, argument construction, and job submission.
disable-model-invocation: true
argument-hint: <runscript_path> [options like "10% sample", "on slurm", "scenario=X", etc.]
---

Run a Tax-Simulator simulation based on the user's request: $ARGUMENTS

## Execution Steps

### 1. Parse the request

Extract from the user's request (use defaults for anything not specified):

| Parameter | Default | Notes |
|-----------|---------|-------|
| `runscript` | (required) | Path relative to `config/runscripts/`, no `.csv` extension |
| `scenario_id` | `NULL` | Single scenario to run, or NULL for all |
| `user_id` | `user_test` | |
| `local` | `1` | 0 = production, 1 = local |
| `vintage` | `NULL` | Custom output folder name, or NULL for timestamp |
| `pct_sample` | `1` | Fraction of records, 0 to 1 |
| `stacked` | `1` | 1 = generate stacked reports |
| `baseline_vintage` | `NULL` | Reuse existing baseline output, or NULL to re-run |
| `delete_detail` | `0` | 1 = delete detail files after completion |
| `multicore` | `none` | Only used for Rscript mode: `none`, `scenario`, or `year` |
| `execution_mode` | auto-detect | `slurm` or `rscript` (see auto-detection below) |

**Auto-detect execution mode:** Read `config/interfaces/output_roots.yaml`. If the `local` path contains `/vast/palmer` or `/gpfs/gibbs` (Yale HPC cluster paths), default to `slurm`. Otherwise default to `rscript`. The user can override by saying "on slurm", "locally via rscript", "not on slurm", etc.

### 2. Validate before running

Read the runscript CSV at `config/runscripts/{runscript}.csv` and perform ALL of the following checks. If any check fails, report the problem clearly and do NOT submit the job.

#### 2a. File existence checks
- Confirm the runscript CSV exists
- For every row, confirm `config/scenarios/tax_law/{tax_law}/` directory exists
- For every behavior module listed (space-delimited in `behavior` column), confirm `config/scenarios/behavior/{module}.R` exists

#### 2b. MTR consistency checks
- For every row, confirm `mtr_vars` and `mtr_types` have the same number of space-delimited tokens
- **CRITICAL: If ANY non-baseline scenario has behavior modules, the baseline row MUST also have `mtr_vars` and `mtr_types` populated.** This is the most common failure mode. If baseline is missing MTR vars that are needed by behavior modules, error with a clear explanation.
- Check that behavior modules have the MTR variables they need. Cross-reference:
  - `charity/*` needs `char_cash`
  - `kg/*` needs `kg_lt`
  - `entity_shifting/*` needs `kg_lt` and `part_active`
  - `employment/*` needs `wages1` and `wages2` (extensive type)
  - `child_earnings/*` needs `wages1` and `wages2`
  - `ot/*` needs `ot1` and `ot2`
  - `tips/*` needs `tips1` and `tips2`
  - `capital_income/*` needs `kg_lt`

#### 2c. Parallelization safety
- **If `child_earnings/34` appears in any scenario's behavior column, `multicore=year` is UNSAFE.** If the user requested year-level parallelization, refuse and explain why. For SLURM mode this is not an issue (SLURM handles dependencies correctly via phase ordering).

#### 2d. Year range check
- If the user described a policy starting in year t, check that the `years` column starts at t-1 or earlier. If not, warn (but don't block -- just flag it).

#### 2e. Baseline vintage check
- If `baseline_vintage` is specified and not NULL, confirm the directory exists under the output root.

### 3. Build and execute the command

#### SLURM mode
```bash
cd /gpfs/gibbs/project/sarin/jar335/Repositories/Tax-Simulator && bash slurm_run.sh {runscript} {scenario_id} {user_id} {local} {vintage} {pct_sample} {stacked} {baseline_vintage} {delete_detail}
```

No need to `module load R` -- `slurm_run.sh` handles module loading in its sbatch scripts.

#### Rscript mode
```bash
cd /gpfs/gibbs/project/sarin/jar335/Repositories/Tax-Simulator && module load R/4.4.1-foss-2022b && Rscript src/main.R {runscript} {scenario_id} {user_id} {local} {vintage} {pct_sample} {stacked} {baseline_vintage} {delete_detail} {multicore}
```

**Important:** Pass literal string `NULL` (not empty) for null arguments.

### 4. Post-submission (SLURM mode)

After submitting, report:
- The SLURM job IDs for each phase
- The expected output location (output root + vintage folder)
- Remind the user they can ask you to check status anytime

If the user asks to check on a running simulation, use `sacct -u $USER --name=taxsim-baseline,taxsim-cf,taxsim-agg,taxsim-postproc,taxsim-stacked --starttime=today -o JobID,JobName,State,Elapsed,MaxRSS --parsable2` or `squeue -u $USER` to show progress.

### 5. Common shorthand the user may use

- "10% sample" or "sample=0.1" -> `pct_sample=0.1`
- "full sample" -> `pct_sample=1`
- "30 years" or "30 year" -> adjust `years` column expectation (but years are set in the runscript, so just note if there's a mismatch)
- "production" or "prod" -> `local=0`
- "no stacking" -> `stacked=0`
- "scenario parallel" or "scenario multicore" -> `multicore=scenario`
- "reuse baseline from X" -> `baseline_vintage=X`
- "delete detail" or "clean up after" -> `delete_detail=1`
