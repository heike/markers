
<!-- README.md is generated from README.Rmd. Please edit that file -->

# markers

<!-- badges: start -->

[![R-CMD-check](https://github.com/heike/markers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/heike/markers/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of markers is to …

## Installation

You can install the development version of markers like so:

``` r
remotes::install_github("heike/markers")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(markers)
## basic example code
```

1.  Load toolmarks

``` r
data(toolmarks)
```

2.  Align by set

``` r
library(tidyverse)
reps <- toolmarks %>% group_by(tool, side, size) %>%
  tidyr::nest()
reps <- reps %>% mutate(
  aligned_set = data %>% purrr::map(sig_align_set, value=signature, group = mark, min.overlap = 1000)
)
```

3.  Visualize

``` r
reps <- reps %>% 
  unite("id", tool, side, size, remove = FALSE) %>%
  mutate(
  plot = purrr::map2(aligned_set, id, .f = function(d, id) {
    gg <- d %>% ggplot(aes(x =x, y = aligned, colour = factor(mark))) + geom_line() +
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
  using your “markers” way because I added a piece in which all marks
  are the same length, 900 points, with NA fillers on either side. I
  figured this would make modeling easier. One thing I haven’t done is
  to align the signals based on two criteria: the subset by tool (marks
  at different angles) and then by angle. When I tried aligning the
  marks made at different angles the alignment didn’t look good.

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
