# GPT Review Notes

_Reviewed 2026-07-02 after the external-review cleanup pass._

The original five review findings were mostly addressed. Remaining issues:

1. **High: the rewritten conservation invariant still is not safe as a hard test.**
   `FORMAL_MODEL.md:411-434` says every term is sign-clean, but `B_accr_t` is explicitly
   allowed to turn negative in recovery years, and the D15 `Delta rho` residual can also be
   offsetting. Also `B_res_t = theta_res * w_t` is too rigid if record-scaled household
   flows, retirement/internal-account flows, and residual ownership are measured from
   different sources. Treat this as a reconciliation diagnostic first, not a hard-error
   invariant, until `B_accr_t` and residual signs are fully specified.

2. **Medium: "hard stop, fall back to the smear" is contradictory.**
   `CONSIDERATIONS.md:448-457` and `FORMAL_MODEL.md:77-82` say absent metadata means both
   hard stop and fallback. Those are different behaviors. The smear is also a distribution
   fallback, not a revenue fallback. Better wording: either "disable on-model corporate
   channel and continue with status quo off-model corporate receipts + distribution smear"
   or "stop the run." Pick one.

3. **Medium: DB cleanup is incomplete.**
   Phase 0 was fixed, but `CONSIDERATIONS.md:407-411` still says DC/DB retirement get the
   direct capitalization hit, and `CONSIDERATIONS.md:467-468` still asks for "DC/DB equity
   share." D10 says DB is residual-only, so those should become DC-only plus DB-residual
   sizing.

4. **Medium: V14 is now stale in the formal verdict table.**
   `FORMAL_MODEL.md:706` still says stale sections should be marked superseded before
   implementation, even though the new update says those fixes were applied. Either
   downgrade V14 to "resolved in external-review pass" or leave only the remaining stale
   items.

5. **Low: the corrected "second-order" language is still easy to misread.**
   `CONSIDERATIONS.md:719-723` still prints the old "second-order" sentence before the
   correction. Since this is implementation-sensitive, replace the sentence outright rather
   than append a correction.

## Bottom Line

The docs are much closer. The remaining serious issue is the conservation/checking story:
it needs to distinguish "economic accounting identity," "diagnostic reconciliation," and
"hard implementation invariant." Once that is cleaned up, the material is probably ready
for a Phase 0 stakes script and a real implementation design pass.
