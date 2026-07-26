# Explicit comparison selector for the historical Pearce--Prisinzano
# entity-shifting deferral proxy.  The shared implementation detects this
# module path and uses beta_legacy = 0.25 even when kg_dynamics is active.
sys.source('./src/behavior/entity_shifting/pearce_prisinzano.R',
           envir = environment())
