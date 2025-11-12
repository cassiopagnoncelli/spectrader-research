## Install packages

```r
renv::remove("fets")
renv::install(paste0("packages/", dir("packages")[1]))
```
