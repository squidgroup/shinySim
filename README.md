# shinySim
shiny app that helps to generate code for the squidSim R pacakge, by focussing on inputting parameters

#### Installation

Currently the shinySim package is not on CRAN, but you can install the current version from GitHub using the devtools package. You will also need to have aleady installed the squidSim package:

```{r}
    install.packages("devtools")
    devtools::install_github("squidgroup/squidSim")
    devtools::install_github("squidgroup/shinySim")
    library(shinySim)
```

#### Running shinySim

The only function shinySim is `shinySim()`. This can be run without any arguments, or you can provide a data structure, e.g.
```{r}
library(shinySim)
data_test <- squidSim::make_structure("sex(2)/individual(10)",repeat_obs=2,level_names=list(sex=c("F","M")))
shinySim(data.struc = data_test)
```
