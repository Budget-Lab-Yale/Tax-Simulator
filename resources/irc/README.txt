Internal Revenue Code - Key Sections for Tax-Simulator
======================================================

Full statutory text extracted from Cornell LII (law.cornell.edu/uscode/text/26/).
These are the IRC sections most relevant to the provisions modeled by Tax-Simulator.

Downloaded: 2026-03-17

Files:
  section_1.txt     - Tax imposed (rates, brackets, capital gains)        -> ord.yaml, pref.yaml
  section_11.txt    - Tax imposed on corporations                         -> corp.yaml
  section_21.txt    - Child and dependent care credit                     -> cdctc.yaml
  section_24.txt    - Child tax credit                                    -> ctc.yaml
  section_25A.txt   - American Opportunity / Lifetime Learning credits    -> ed.yaml
  section_25B.txt   - Saver's credit (elective deferrals/IRA)            -> savers.yaml
  section_32.txt    - Earned income tax credit                            -> eitc.yaml
  section_55.txt    - Alternative minimum tax                             -> amt.yaml
  section_62.txt    - Adjusted gross income defined                       -> agi.yaml
  section_63.txt    - Taxable income / standard deduction                 -> std.yaml
  section_68.txt    - Overall limitation on itemized deductions (Pease)   -> item.yaml
  section_86.txt    - Social Security benefit taxation                    -> ss.yaml
  section_151.txt   - Personal exemptions                                 -> pe.yaml
  section_152.txt   - Dependent defined                                   -> (cross-ref: ctc, eitc, cdctc, pe)
  section_163.txt   - Interest (mortgage interest deduction)              -> item.yaml
  section_164.txt   - Taxes (SALT deduction)                              -> item.yaml
  section_170.txt   - Charitable contributions                            -> char.yaml
  section_199A.txt  - Qualified business income deduction                 -> qbi.yaml
  section_224.txt   - Tips deduction                                      -> below.yaml
  section_225.txt   - Overtime deduction                                  -> below.yaml
  section_1202.txt  - Qualified small business stock exclusion            -> pref.yaml
  section_1401.txt  - Self-employment tax rate (SECA)                     -> pr.yaml
  section_1411.txt  - Net investment income tax                           -> niit.yaml
  section_3101.txt  - FICA employee tax rates                             -> pr.yaml
  section_3111.txt  - FICA employer tax rates                             -> pr.yaml
  section_7703.txt  - Determination of marital status                     -> filing.yaml

To add more sections, run from repo root:
  curl -sL "https://www.law.cornell.edu/uscode/text/26/{SECTION}" | python3 /tmp/extract_irc.py > resources/irc/section_{SECTION}.txt
  (extract_irc.py script is in the git history or can be recreated)
