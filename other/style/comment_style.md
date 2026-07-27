# Comment and documentation style

The reference for this style is the pre-2026 code on `main`: `src/data/tax_law.R`,
`src/calc/do_taxes.R`, `src/data/post_processing/distribution.R`. When in doubt,
open one of those and match it.


## File headers

Three lines. Name the file, say what is in it, stop.

```r
#-------------------------------------------------------------------------------
# wealth_dynamics.R
#
# Contains functions to simulate saving responses to tax changes and their
# effect on the estate tax base
#-------------------------------------------------------------------------------
```

Where the file rests on an assumption a reader could not recover from the code,
add one short paragraph below the banner stating the assumption and its number.
Model it on the fringe-benefit note in `do_taxes.R`.

```r
# Assume that a share s of any increase in taxes paid during life is financed
# out of wealth rather than consumption. The shortfall compounds over time and
# reduces the estate at death. s is set by the scenario's financing profile.
# Conventional runs only.
```

Nothing else belongs in a header: no design rationale, no history, no pointers
to plans, no list of what the file does not do.


## Function documentation

Open with a verb in the third person. One or two sentences. Then the parameter
and return blocks in the existing format.

```r
  #----------------------------------------------------------------------------
  # Calculates payroll and individual income taxes for all tax units.
  #
  # Parameters:
  #   - tax_units (df) : tibble of tax units, exogenous variables only
  #
  # Returns: tibble of tax units with new columns for calculated tax variables
  #          (df).
  #----------------------------------------------------------------------------
```


## Inline comments

Name the action the code takes. One line, verb first.

```r
  # Read baseline YAML files
  # Overwrite baseline subparams with specified changes
  # Loop over years
  # Add age cuts
```

Two or three lines are fine where a number or an assumption needs stating. Past
that, cut it.


## Citations and formulas

Keep the number and the source, flat, in one or two sentences. Put a formula on
its own indented line. Do not add the range of estimates, the reason another
paper is the wrong one, or a discussion of what the choice implies.

```r
# Assume an elasticity of reported estates with respect to the net-of-tax
# rate of 0.16 (Kopczuk and Slemrod 2001).
#
#   retained = ((1 - tau_S) / (1 - tau_B)) ^ report_eps
```


## What to cut

- Capital letters for emphasis. Rewrite the sentence instead.
- Stacked hyphenated modifiers. "the net above-baseline during-life after-tax
  cash-flow shock" is four modifiers and one noun.
- References a reader cannot follow: `(D7)`, `(P1/F4)`, `spec section 3.2`,
  `DESIGN_LOCK R6`, plan filenames.
- Line numbers in other files. They go stale immediately.
- Invented terms used as though they were established. Name the thing plainly
  the first time and every time after.
- Changelogs. When a value was re-pinned, or that a refactor came back
  identical, belongs in the commit message.
- Reassurance: "verified", "byte-identical", "clean", "exact".
- Closing flourishes that restate the point as a principle, of the form "which
  is what makes X a Y rather than a Z".
- Notation in running prose. A formula on its own line reads; the same symbols
  inside a sentence do not.
- Anything that says what the code does not do, unless a reader would otherwise
  reach for it.


## Length

If a comment block runs past about six lines, the content probably belongs in a
memo under `other/`, with a one-line pointer from the code.
