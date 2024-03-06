# shinySim
shiny app interface to squidSim

#### Installation

Currently the shinySim package is not on CRAN, but you can install the development version from GitHub using the devtools pacakge:

    # install.packages("devtools")
    devtools::install_github("squidgroup/shinySim")
    library(shinySim)

#### Running shinySim

The only functionin shinySim is `shinySim()`. This can be run without any arguments, or you can provide a data structure, e.g.
```{r}
library(shinySim)
data_test <- squidSim::make_structure("sex(2)/individual(10)",repeat_obs=2,level_names=list(sex=c("F","M")))
shinySim(data.struc = data_test)
```
