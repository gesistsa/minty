# Minty Benchmarks


``` r
suppressPackageStartupMessages(library(minty))
suppressPackageStartupMessages(library(readr))
Sys.time()
```

    [1] "2025-01-05 12:42:59 CET"

Under 200 rows, simple

``` r
iris_chr <- as.data.frame(lapply(iris, as.character))
bench::mark(minty::type_convert(iris_chr),
            suppressMessages(readr::type_convert(iris_chr)),
            iterations = 10)
```

    # A tibble: 2 × 6
      expression                            min  median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>                       <bch:tm> <bch:t>     <dbl> <bch:byt>    <dbl>
    1 minty::type_convert(iris_chr)    393.95µs 416.7µs     2302.  702.53KB        0
    2 suppressMessages(readr::type_co…   2.14ms   2.2ms      368.    1.97MB        0

Many rows

``` r
flights_chr <- as.data.frame(lapply(nycflights13::flights, as.character))
bench::mark(minty::type_convert(flights_chr, guess_integer = TRUE),
            suppressMessages(readr::type_convert(flights_chr, guess_integer = TRUE)),
            iterations = 5)
```

    Warning: Some expressions had a GC in every iteration; so filtering is
    disabled.

    # A tibble: 2 × 6
      expression                             min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>                           <bch> <bch:>     <dbl> <bch:byt>    <dbl>
    1 minty::type_convert(flights_chr, gu… 1.12s  1.17s     0.859     189MB     15.8
    2 suppressMessages(readr::type_conver… 1.03s  1.03s     0.946     153MB     17.4

Many row, guess_max

``` r
bench::mark(minty::type_convert(flights_chr, guess_integer = TRUE, guess_max = 500),
            suppressMessages(readr::type_convert(flights_chr, guess_integer = TRUE)),
            iterations = 5)
```

    Warning: Some expressions had a GC in every iteration; so filtering is
    disabled.

    # A tibble: 2 × 6
      expression                           min   median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>                      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    1 minty::type_convert(flights_ch… 543.66ms 551.12ms     1.76      153MB     19.4
    2 suppressMessages(readr::type_c…    1.04s    1.04s     0.940     153MB     17.3

``` r
sessionInfo()
```

    R version 4.4.2 (2024-10-31)
    Platform: x86_64-pc-linux-gnu
    Running under: Ubuntu 22.04.5 LTS

    Matrix products: default
    BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.20.so;  LAPACK version 3.10.0

    locale:
     [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
     [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
     [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
     [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
     [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

    time zone: Europe/Berlin
    tzcode source: system (glibc)

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
    [1] readr_2.1.5 minty_0.0.5

    loaded via a namespace (and not attached):
     [1] crayon_1.5.3       vctrs_0.6.5        cli_3.6.3          knitr_1.49        
     [5] rlang_1.1.4        xfun_0.49          bench_1.1.3        jsonlite_1.8.9    
     [9] glue_1.8.0         htmltools_0.5.8.1  hms_1.1.3          rmarkdown_2.29    
    [13] evaluate_1.0.1     tibble_3.2.1       tzdb_0.4.0         fastmap_1.2.0     
    [17] yaml_2.3.10        profmem_0.6.0      lifecycle_1.0.4    compiler_4.4.2    
    [21] pkgconfig_2.0.3    nycflights13_1.0.2 digest_0.6.37      R6_2.5.1          
    [25] utf8_1.2.4         pillar_1.10.0      magrittr_2.0.3     tools_4.4.2       
