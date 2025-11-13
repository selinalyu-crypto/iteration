rmd file
================
Selina Lyu

## Lists

You can put anything in a list

``` r
l = list (
vec_numeric = 5:8,
vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
mat = matrix(1:8, nrow = 2, ncol = 4),
summary = summary(rnorm(100))
)

l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.2147 -0.4942  0.1139  0.1089  0.6915  2.4016

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[[1]][1:3]
```

    ## [1] 5 6 7

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

``` r
list_norms = 
  list(
    a = rnorm(20, 3, 1),
    b = rnorm(20, 0, 5),
    c = rnorm(20, 10, .2),
    d = rnorm(20, -3, 1)
  )

is.list(list_norms)
```

    ## [1] TRUE

go back to old function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

I can apply that function to each list element

``` r
mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.11 0.814

``` r
mean_and_sd(list_norms[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.09  3.34

``` r
mean_and_sd(list_norms[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.92 0.237

``` r
mean_and_sd(list_norms[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.52 0.974

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}
```

## Let’s try map

``` r
output = map(list_norms, mean_and_sd)

list_norms
```

    ## $a
    ##  [1] 2.379633 3.042116 2.089078 3.158029 2.345415 4.767287 3.716707 3.910174
    ##  [9] 3.384185 4.682176 2.364264 2.538355 4.432282 2.349304 2.792619 2.607192
    ## [17] 2.680007 2.720887 3.494188 2.822670
    ## 
    ## $b
    ##  [1] -2.5297873  6.7151941 -1.0728970 -0.8977827 -0.5009537  3.5633315
    ##  [7] -0.3678220 -0.1881709 -3.4083024 -1.6213514  0.3008022 -2.9444724
    ## [13]  2.6574810 -7.5919704  1.5327893 -7.6822491 -1.5048806 -2.6413995
    ## [19] -3.2604739 -0.2844839
    ## 
    ## $c
    ##  [1]  9.617128 10.235317  9.667006  9.907294  9.776816  9.849836 10.417433
    ##  [8] 10.003479  9.742740  9.671879 10.090037  9.996288  9.936386  9.814128
    ## [15]  9.702508  9.784962 10.200006  9.875747  9.723115 10.373858
    ## 
    ## $d
    ##  [1] -2.5748996 -3.2386471 -1.9415170 -2.1135773 -3.6192430 -0.7938975
    ##  [7] -3.2550270 -4.4244947 -3.1443996 -2.7924617 -0.6920216 -2.8941976
    ## [13] -2.5430012 -3.0771529 -3.3340008 -3.0347260 -2.2123604 -0.9247550
    ## [19] -1.9726076 -1.7920916
