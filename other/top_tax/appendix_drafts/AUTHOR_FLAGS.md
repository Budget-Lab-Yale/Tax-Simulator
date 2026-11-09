# Flags for the author — from the appendix drafting pass (2026-07-16)

Findings surfaced while grounding the appendix drafts in the code. None have been
applied to `methodology_memo.md`; the appendix drafts follow the code where the
two disagree.

## Memo corrections recommended

1. **Table 1 label error (all three formats).** "Corporate incidence constants
   (e.g., supernormal share 0.375)" — in the code and design docs, 0.375 is the
   **normal-return** share; the supernormal share is 0.625 (consistent with the
   OTA 63% / TPC 60% supernormal anchors). Should read "normal-return share
   0.375" or "supernormal share 0.625".
2. **§2 realization, "cohorts by age and wealth."** The realization model's
   cells are age-only; wealth enters through gain-weighted cell parameters and
   wealth-graded mortality (the age × wealth-percentile cells belong to the
   financing channel). Suggest softening, e.g. "representative age cohorts,
   with cell parameters weighted toward the wealthy households who hold the
   gains."
3. **§2 conversion, "depends on the gap."** The module responds to the
   reform-induced *change* in the ordinary-vs-deferred-gain wedge, recomputed
   each year, not to its level. The appendix states the change form.
4. **§2 avoidance, concealment removes income "entirely."** Retirement-account
   distributions are deliberately never concealed (third-party information
   reporting). The appendix states the exception; the memo sentence overstates.
5. **§2 underreporting.** Farm income (Schedule F) also responds, with the
   Schedule C elasticity; the memo's list omits it. The appendix folds it into
   "self-employment."

## Presentation nits (no action strictly needed)

6. **"16 percent" vs "−17".** §2 prose states effects (1 − e^−0.17 ≈ 16%) while
   Table 1 states the semi-elasticity (−17). Consistent, but easy to misread as
   a mismatch; a table footnote could preempt it.

## Code/records issues found along the way (not memo issues)

7. **Kopczuk–Slemrod misattribution in a code comment.** The estate avoidance
   module cites the 0.16 elasticity to "Dying to Save Taxes" (the death-timing
   paper); the correct source is the Brookings chapter "The Impact of the
   Estate Tax on Wealth Accumulation and Avoidance Behavior." The appendix
   cites the chapter.
8. **Charity module mismatch on the σ watch-list.** The σ staleness check
   references the −1.0 charity module while the top-tax runscripts use −0.5
   (matching Table 1). Worth confirming the July σ derivation ran with −0.5.
9. **η drift.** The estate-margins build measured +3.7% drift and recommends
   re-pinning η to ≈ 2.4901; Table 1 correctly reflects the shipped 2.4825.
10. **Documented, unbuilt gap** (from the concealment design note §4.2): in a
    package combining deemed realization at death with a wealth tax, concealed
    assets do not currently escape the deemed gains tax. Omitted from the
    appendix per the no-limitations-register rule; needs an author decision if
    such a package is published.

## Citations the author should verify (from Appendix B)

11. ~~DeBacker, Heim & Yuskavage (2025)~~ — **RESOLVED 2026-07-16**: author
    supplied the NTA slide deck (Nov 6, 2025). Values verified exactly (0.046
    sole-prop / 0.052 partnership cross-section subsamples, federal MTR; 0.040
    = Kansas DiD Schedule E, insignificant). Fixed the Treasury-OTA
    misattribution (DeBacker is South Carolina, Heim is Central Florida, only
    Yuskavage is OTA) in Appendix B, the reference entry, and the module
    comment. The reported-income application convention matches the authors'
    own OBBBA illustration (slide 21). Note kept for the record: their
    regression LHS is the audit adjustment (underreported income), so
    "elasticity of reported income" is the authors' convention, not a base
    conversion.
12. **Pearce & Prisinzano (2018) author order** — PWBM W2018-2's landing page
    hides the byline; one index lists "Prisinzano and Pearce." Kept the memo's
    order; verify against the PDF.
