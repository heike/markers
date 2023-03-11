
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tmaRks

<!-- badges: start -->

[![R-CMD-check](https://github.com/heike/tmaRks/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/heike/tmaRks/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/heike/markers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/heike/markers/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of tmaRks is to …

## Installation

You can install the development version of tmaRks like so:

``` r
remotes::install_github("heike/tmaRks")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tmaRks)
#> 
#> Attaching package: 'tmaRks'
#> The following object is masked from 'package:utils':
#> 
#>     data
## basic example code
```

1.  Load toolmarks

``` r
data(toolmarks)
head(toolmarks)
#> # A tibble: 6 × 11
#>    tool size  side  direction angle  mark TID            x     y   value signa…¹
#>   <int> <chr> <chr> <chr>     <int> <int> <chr>      <dbl> <dbl>   <dbl>   <dbl>
#> 1     1 S     A     F            80     1 T01SA-F80…  2.00  3.62 -0.0816 0.00195
#> 2     1 S     A     F            80     1 T01SA-F80…  2.01  3.62 -0.0816 0.00193
#> 3     1 S     A     F            80     1 T01SA-F80…  2.02  3.62 -0.0816 0.00184
#> 4     1 S     A     F            80     1 T01SA-F80…  2.02  3.62 -0.0817 0.00173
#> 5     1 S     A     F            80     1 T01SA-F80…  2.03  3.62 -0.0817 0.00166
#> 6     1 S     A     F            80     1 T01SA-F80…  2.04  3.62 -0.0817 0.00157
#> # … with abbreviated variable name ¹​signature
```

2.  Align by set

``` r
library(tidyverse, quietly = TRUE)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.0     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.0
#> ✔ ggplot2   3.4.1     ✔ tibble    3.2.0
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

reps <- toolmarks %>% filter(tool <= 3, size == "L") %>% group_by(tool, side, size) %>%
  tidyr::nest()
reps <- reps %>% mutate(
  data = data %>% purrr::map(sig_align_set, value=signature, group = mark, min.overlap = 500)
) %>% tidyr::unnest(cols=data)
```

3.  Visualize

``` r
reps <- reps %>% 
  unite("id", tool, side, size, remove = FALSE) %>%
  mutate(
  plot = purrr::map2(aligned_set, id, .f = function(d, id) {
    gg <- d %>% ggplot(aes(x = aligned_x, y = signature, colour = factor(mark))) + geom_line() +
      theme_bw() + 
      ggtitle(id[1]) +
      theme(legend.position="none")
    gg
  })
)

marks_01_A_S <- reps %>% filter(tool==01, side=="A", size=="S")

length(reps$data)

dim(reps$data[[1]])

table(reps$data[[1]]$direction)
table(reps$data[[2]]$direction)


table(reps$data[[3]]$angle)


reps$data[[3]]$angle



dim(marks_01_A_S$data[1])


str(reps)
names(reps)




table(reps$tool)
table(reps$side)
table(reps$size)

reps$data[[1]]$y

reps$



#reps$aligned_set[[3]] %>% summary()
#reps$aligned_set[[3]] %>% names()
#reps[3,]

library(readr)
saveRDS(reps, "data/reps.rds")

reps <- readRDS("data/reps.rds")






library(gridExtra)
do.call(marrangeGrob, list(reps$plot[1:8], nrow=4, ncol=2))


ml = do.call(marrangeGrob, list(reps$plot, nrow=4, ncol=2))
ggsave(plot=ml, filename="figures/multipage.pdf")       
```

Download [pdf](multipage.pdf) with multiple pages of figures.

The steps so far are:

1.  Convert stl files to x3p files and save in an appropriate folder

``` r
stls <- dir(pattern="stl", path="data/stl_files", full.names = TRUE)


for (file in stls) {
  stl_tool <- rgl::readSTL(file, plot=FALSE)
  x3p <- stl_to_x3p(stl_tool)
  x3p %>% x3p_write(file = gsub(".stl", ".x3p", file)) # not that this will re-name EVERY x3p by stl, i.e. no need to deal with stl_files to x3p_files separately
}
```

2.  Function `assert_name_pattern(files, pattern)` checks all file names
    in vector `files` for compliance with the pattern given in
    `pattern`.

- signature extraction – I’m just filtering the signal from a set value
  of x to another. I bet this makes it so we lose some of the signal,
  but it’s simple. adding metadata –– So far I haven’t run this code, so
  the x3p files don’t have the correct metadata. Nina made a spreadsheet
  with this information, so all it would take is to run it, but it’s
  slow.

- aligning signatures –– I’m still doing this my initial way instead of
  using your “tmaRks” way because I added a piece in which all marks are
  the same length, 900 points, with NA fillers on either side. I figured
  this would make modeling easier. One thing I haven’t done is to align
  the signals based on two criteria: the subset by tool (marks at
  different angles) and then by angle. When I tried aligning the marks
  made at different angles the alignment didn’t look good.

- binning –– So far I’ve done this by choosing an arbitrary number of
  bins (18, so that each bin has 50 points in it). It would be nice if I
  could just change that number easily, e.g. b=22, and then the code
  would just run. The way it’s written now it’s way too dependent on the
  18 bins.

- modeling –– The main task here has been calculating means by tool, and
  then the variance across tools. I couldn’t figure out how to do this
  in general so you’ll see it’s quite specific.

One more point: - concatenating sides –– I’d like to start doing the
whole analysis by using the signals from sides A and B of each tool
concatenated. I think that is a more honest description of what the data
are. That would have to be done after aligning, I think.
