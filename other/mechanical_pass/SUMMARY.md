# The third estimate: what changed and what it needs

2026-07-29. Branch `mechanical-pass`, seven commits, off `wealth`.

## What the model does differently

For each policy the model used to report two numbers: the tax raised before anyone
reacts, and the tax raised after everyone does. The gap between them mixed two
things that are worth telling apart.

Some of that gap is automatic. A corporate tax increase lands on shareholders'
dividends and share prices. Tax paid out of savings leaves less wealth behind, so
there is less capital income and a smaller estate later. When the employer's
payroll tax rises, wages fall to absorb it, which cuts income tax too. None of
this involves anyone deciding anything.

The rest of the gap is people deciding things: selling stock sooner or later,
giving less to charity, shifting income between forms, concealing assets.

The model now reports three numbers instead of two, with the new one in the
middle. It carries the automatic effects and not the decisions. So the old gap
splits cleanly, and it splits in a fixed order: the automatic effects happen
first, and people decide what to do in the world those effects have already
created. That was already the order the model computed things in, so nothing
about the answer depends on the choice.

## An example

A capital gains rate increase, run over four years, tax raised in billions:

| | savings channel off | savings channel on |
|---|---|---|
| before anyone reacts | 324 | 324 |
| plus the automatic effects | 324 | 321 |
| plus people reacting | 145 | 144 |

With the savings channel switched off there are no automatic effects, so the
first two rows agree and the whole 180 billion drop is people realizing fewer
gains. With it on, we can now say that 3.7 billion of the loss is tax being
financed out of wealth and draining later years' bases, and the rest is the
realization response. Before, those were one number.

## One rule was in the wrong place

The employer payroll adjustment — wages falling to absorb an employer tax
increase — was being applied in the first estimate, as though it were part of the
law. It is not; it is one tax base moving because another one did, the same as
the corporate and savings effects. It has moved to the middle estimate, which
makes the three automatic effects consistent with each other for the first time.

This matters beyond tidiness. It was the reason an uncap-the-taxmax policy could
not be shown honestly next to the others.

The distribution tables still show the same burden they did before. The worker's
income tax saving from his own wage cut used to be baked invisibly into the
numbers; it is now a labeled line, and it adds back to exactly what it was.

## What was checked

Three full-size runs, comparing the model before and after the change.

Policies that touch no employer payroll parameter give identical answers. Policies
that do move in the direction they should: payroll revenue up, because the base is
no longer shaved before being taxed, and income tax up, because the offset has
moved out of that estimate. A policy changing only the employee's side of the same
tax does not move at all, which is the check that the new rule fires on the right
thing.

The distribution tables reproduce their old values exactly, to the last digit, on
every column they share.

The detail behind all of this is in `regression_notes.md` in this folder.

## What it needs before it can be merged

There is a parameter governing how strongly people respond on capital gains. It
was measured against information that this change moves, so it needs measuring
again. Until that happens, any capital gains run stops with a message explaining
why. That is deliberate: the alternative is quietly using a number that no longer
describes the model.

Re-measuring means three full-size thirty-year runs and then a short calculation.
Several hours, almost all of it waiting.

The change added a diagnostic that estimates how much this actually matters before
paying for those runs. The answer is under one percent, so re-measuring should
confirm the existing number rather than replace it.

## Two places this departed from the agreed plan

Both are places where following the plan's recipe would not have produced what the
plan said it wanted.

The plan measured the payroll adjustment for the distribution tables as the
difference between the middle estimate and the first one. But that difference also
contains the savings drawdown, which the same plan says to keep out of burden
tables, on the grounds that the later income dip is an echo of tax already counted
as burden when it was paid. The adjustment is measured a slightly different way
that leaves the drawdown out.

One case still cannot be separated: a policy that changes employer payroll *and*
the corporate rate. There the two automatic effects are tangled and the corporate
one is already counted elsewhere, so that combination gets no payroll line and a
warning saying so, rather than counting the corporate effect twice.

Second, where a policy has no automatic effects at all, the middle estimate equals
the first. The plan said to write nothing. It is written anyway, as a copy, so
every policy always carries all three estimates and no report needs a special
case.

## Two things found on the way

The wage adjustment finished by recomputing total wages as the sum of the two
earners' wages, which is not quite identical to the total the data supplies. It
ran on the reform side but not the baseline side, so the two disagreed slightly.
Taking the adjustment out of the first estimate removes that disagreement. This is
the artifact recorded in the wages-residual note; it was small, about a
hundred-millionth of a dollar per record, but it was there.

A test script guarding the cluster job ordering had been failing since late July,
when an argument it passes was retired. It failed immediately, every time, and the
failure went to a discarded output stream, so it appeared to pass. Fixed, and
extended to cover the new job ordering.
