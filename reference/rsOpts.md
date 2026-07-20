# Set global options for RStoolbox

shortcut to options(RStoolbox.\*)

## Usage

``` r
rsOpts(verbose = NULL, idxdb = NULL)
```

## Arguments

- verbose:

  Logical. If `TRUE` many functions will print status messages about the
  current processing step. By default verbose mode is disabled.

- idxdb:

  List. The list conatins the formal calculation of spectral indices.
  Modify this list to pipe your own spectral index through the internal
  C++ calculation of RStoolbox.

## Value

No return, just a setter for the verbosiness and the index-database of
the RStoolbox package. For latter, see the example of
Rstoolbox::spectralIndices()

## Examples

``` r
rsOpts(verbose=TRUE)
```
