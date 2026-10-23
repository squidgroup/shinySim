# shinySim (development version)

Version 0.0.0.9001, on the `development` branch. The stable version on `main` is
0.0.0.9000. Check which one you have with `packageVersion("shinySim")`.

## New features

* **Exact expected mean and variance.** The expected mean and variance of the
  response, and how the variance divides among levels and predictors, are now
  calculated exactly for interactions of any order (including variables with
  non-zero means) and include the covariances created by fixed categorical
  effects and covariates, taken from the data structure. On `main`, only two-way
  interactions allowed for covariance between their variables; interactions of
  three or more variables assumed the variables were independent.
* **Built-in checks.** Before a change is accepted, the code the app would print
  is run through squidSim and compared with the app's own parameters. The app
  also runs a quick squidSim simulation and warns if its mean or variance differs
  from the expected values by more than 10%.
* **Input checks.** The app refuses a variance-covariance matrix squidSim cannot
  simulate from, invalid or duplicate variable names (issue #25), names that
  clash with data-structure columns, fixed categorical names that don't match the
  factor's levels, and non-numeric grouping columns (issue #23).
* **Clearer interactions.** The interaction pickers only offer variables that
  still exist, grouped and labelled by component. Renaming a variable
  carries the new name into its interactions, and deleting one removes the
  interactions that used it.
* The app stops when its last browser tab is closed (a page refresh keeps it
  running). Set `shiny::shinyOptions(shinySim.stop_on_close = FALSE)` to keep it
  running regardless.
* Colour-blind-friendly colours (Okabe-Ito) for components, which no longer run
  out on models with many components.

## Bug fixes

* The printed squidSim code could fail or simulate something different from
  what was entered:
  * group and variable names were left out for fixed and covariate components,
    or guessed, so interactions could bind to the wrong variable;
  * a component with beta 1, variance 1 and a non-zero mean printed neither
    `beta` nor `vcov`, which squidSim needs;
  * `vcov` was not printed when it differed from squidSim's default.
* Choosing interactions, or deleting a variable used in one, could crash the
  app (issue #20).
* Only one interaction could be added; all interactions are now kept together in
  squidSim's single `interactions` component (issue #22).
* Editing a component took its group and type from the "Add" tab instead of
  keeping its own.
* Editing one covariance did not update its mirror, leaving the matrix
  asymmetric.
* The "Add" form's tables kept old values after being reopened or cleared
  (issue #24).
* The variance bar plot failed with "figure margins too large" in a small
  window.
* `shinySim()` no longer writes `data.struc` into your global environment.

## Other changes

* The documentation, DESCRIPTION and README now say what the app does: it
  shows the squidSim code to copy into R, the model equation, and the expected
  mean and variance. It does not build data structures or return simulated data.
* `bslib`, `dplyr`, `stringr`, `tippy` and `utils` are no longer needed.
