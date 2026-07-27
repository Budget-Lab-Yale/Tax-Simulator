# The timeable-share pin: what the control vintage settles

*2026-07-27, when measure_timeable.R was run for the first time.*

Measured at the SAME share (0.2542), one vintage from July under sigma 0.16 and one
from today under sigma 0.2002:

| | short-run E_full | long-run E_full |
|---|---|---|
| `form_tmbl_logs_0p2542` (July, sigma 0.16) | +5.2631 | -2.6766 |
| `form_timeable_logs_25` (today, sigma 0.2002) | +5.2636 | -2.6792 |
| target | +5.04 | -2.52 |

Three things follow, and the first two matter beyond this parameter.

## 1. Sigma and the gains bathtub really are orthogonal

The two runs agree to four significant figures across a 25% change in sigma. That
orthogonality had been an assertion in `conversion.yaml`'s note -- disjoint bases,
conversion on ordinary income and the bathtub on realizations -- and it is the reason
eta was NOT re-derived after sigma moved on 2026-07-26. It is now measured rather
than argued.

## 2. The long-run gap is pre-existing, not something the rebuild did

The long-run moment sits 6.2% from its -2.52 target, and it sat there in July too.
This leg is a +2pp permanent shock while eta_logs was pinned on a +5pp one, and the
net-of-tax response form is not linear, so the two shocks need not return the same
semi-elasticity. Worth a look sometime; not a regression.

## 3. The shipped 0.2542 never hit its own target, and that is the whole drift

At 0.2542 the short-run moment is +5.26 against a +5.04 target, 4.4% high. The
original pin was hand-iterated to within about 5% and stopped there, which was the
stated tolerance. Interpolating the grid onto the target gives **0.2452**, which hits
+5.04 by construction.

So the 0.2542 -> 0.2452 move is NOT the model changing, the data changing, or the
calibration losing identification -- the three things a drift banner is meant to warn
about. It is the difference between a value iterated into a tolerance band and a value
solved. Same model, same data, better arithmetic.

The grid was monotone (+2.6877 / +5.2636 / +8.8133) and the long-run moment was flat
across it (0.96% spread), so both guards in `measure_timeable.R` passed and the
sequential identification they exist to check is holding.
