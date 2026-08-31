<p align="center">
  <img src="inst/shinySim/www/squidSim_logo.png" width = "200"/>
</p>

<div align="center">
 <h1>shinySim</h1>
</div>

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/license-MIT-green)](LICENSE.md)
<!-- badges: end -->

shinySim is a **point-and-click interface to the
[squidSim](https://github.com/squidgroup/squidSim) package**. It lets you build
structured simulations — hierarchical data structures, predictors, and variance
components across levels — without writing the simulation call by hand.

## Features

- **Build a data structure interactively.** Define nested and crossed levels,
  sample sizes, and repeat observations, or pass in a structure you already have.
- **Set variance components per level.** Specify predictors, betas, and
  (co)variances for each grouping factor.
- **See the equations as you go.** The app writes out the model being simulated,
  so what you are building stays legible.
- **Inspect simulated variances.** Check that the realised variance components
  match what you asked for before you commit to a design.
- **Get the code back.** The equivalent `squidSim` call is returned alongside the
  data, so simulations remain reproducible outside the app.
- **Runs locally.** Nothing is uploaded anywhere.

## Installation

shinySim is not on CRAN, but you can install the development version from GitHub
using the devtools package:

```r
install.packages("devtools")
devtools::install_github("squidgroup/shinySim")
library(shinySim)
```

This will also install [squidSim](https://github.com/squidgroup/squidSim), which
is not on CRAN and is declared via `Remotes:`.

## Usage

The only function is `shinySim()`. It can be run without any arguments:

```r
library(shinySim)
shinySim()
```

Or you can provide a data structure created in `squidSim` (or elsewhere):

```r
library(shinySim)

data_test <- squidSim::make_structure(
  "sex(2)/individual(10)",
  repeat_obs  = 2,
  level_names = list(sex = c("F", "M"))
)

shinySim(data.struc = data_test)
```

## Arguments

| Argument | Purpose |
| :------- | :------ |
| `data.struc` | A data structure created with `squidSim::make_structure()` or an equivalent data frame. Optional — leave empty to build one in the app. |

## Bug reports and contributions

Please file issues and feature requests at
<https://github.com/squidgroup/shinySim/issues>. Pull requests are welcome.

Issues with the underlying simulation engine belong upstream at
[squidgroup/squidSim](https://github.com/squidgroup/squidSim).

## Related

- [**squidSim**](https://github.com/squidgroup/squidSim) — the simulation engine
  this app drives
- [**SQuID**](https://squidgroup.org/) — the Statistical Quantification of
  Individual Differences group

## Citation

Please cite the package (adjust the year as needed):

> Ivimey-Cook, E. R., & Pick, J. L. (2026). *shinySim: Shiny interface to the
> squidSim simulation package.* R package version 0.0.0.9000.
> <https://github.com/squidgroup/shinySim>

A machine-readable [`CITATION.cff`](CITATION.cff) is included, so GitHub's
"Cite this repository" button gives formatted APA and BibTeX.

## License

Released under the [MIT License](LICENSE.md).
