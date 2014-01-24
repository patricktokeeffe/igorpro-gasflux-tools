Change Log
==========

Version to-be-released
----------------------

API changes:
- `ResampleXY()` now only sets X-scaling if optional parameter `setScales`
  is non-zero

Enhancements:
- NEW: IntervalGradient() - calculate vertical gradients using finite-
  difference approximation

Bug fixes:
- Fix `Cov` so it returns NAN if source waves are multidimensional instead of
  just printing error and continuing



