# Minty Benchmarks


``` r
suppressPackageStartupMessages(library(minty))
suppressPackageStartupMessages(library(readr))
Sys.time()
```

    [1] "2024-06-10 09:57:34 CEST"

Under 200 rows, simple

``` r
iris_chr <- as.data.frame(lapply(iris, as.character))
bench::mark(minty::type_convert(iris_chr), iterations = 10)
```

    # A tibble: 1 × 6
      expression                         min   median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>                    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    1 minty::type_convert(iris_chr)    368µs    399µs     2383.     695KB        0

``` r
bench::mark(suppressMessages(readr::type_convert(iris_chr)), iterations = 10)
```

    # A tibble: 1 × 6
      expression                             min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>                          <bch:> <bch:>     <dbl> <bch:byt>    <dbl>
    1 suppressMessages(readr::type_conve… 2.11ms 2.15ms      376.    1.81MB        0

Many rows

``` r
flights_chr <- as.data.frame(lapply(nycflights13::flights, as.character))
bench::mark(x <- minty::type_convert(flights_chr, guess_integer = TRUE), iterations = 5)
```

    Warning: Some expressions had a GC in every iteration; so filtering is
    disabled.

    # A tibble: 1 × 6
      expression                             min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>                           <bch> <bch:>     <dbl> <bch:byt>    <dbl>
    1 x <- minty::type_convert(flights_ch… 991ms  1.05s     0.961     189MB     17.5

``` r
bench::mark(y <- suppressMessages(readr::type_convert(flights_chr, guess_integer = TRUE)), iterations = 5)
```

    Warning: Some expressions had a GC in every iteration; so filtering is
    disabled.

    # A tibble: 1 × 6
      expression                             min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>                           <bch> <bch:>     <dbl> <bch:byt>    <dbl>
    1 y <- suppressMessages(readr::type_c… 991ms  1.05s     0.970     153MB     17.5

``` r
all.equal(x, y)
```

    [1] TRUE

``` r
sessionInfo()
```

    R version 4.4.0 (2024-04-24)
    Platform: x86_64-pc-linux-gnu
    Running under: Ubuntu 22.04.4 LTS

    Matrix products: default
    BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0 
    LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0

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
    [1] readr_2.1.5 minty_0.0.2

    loaded via a namespace (and not attached):
     [1] crayon_1.5.2       vctrs_0.6.5        cli_3.6.2          knitr_1.46        
     [5] rlang_1.1.4        xfun_0.43          bench_1.1.3        jsonlite_1.8.8    
     [9] glue_1.7.0         htmltools_0.5.8.1  hms_1.1.3          fansi_1.0.6       
    [13] rmarkdown_2.26     evaluate_0.23      tibble_3.2.1       tzdb_0.4.0        
    [17] fastmap_1.1.1      yaml_2.3.8         profmem_0.6.0      lifecycle_1.0.4   
    [21] compiler_4.4.0     pkgconfig_2.0.3    nycflights13_1.0.2 digest_0.6.35     
    [25] R6_2.5.1           utf8_1.2.4         pillar_1.9.0       magrittr_2.0.3    
    [29] tools_4.4.0       
