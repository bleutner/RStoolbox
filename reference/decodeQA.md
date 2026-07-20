# Decode QA flags to bit-words

Intended for use with Landsat 16-bit QA bands. Decodes pixel quality
flags from integer to bit-words.

## Usage

``` r
decodeQA(x)
```

## Arguments

- x:

  Integer (16bit)

## Value

Returns the decoded QA values from an integer

## See also

[encodeQA](https://bleutner.github.io/RStoolbox/reference/encodeQA.md)

## Examples

``` r
decodeQA(53248)
#> [1] "1101000000000000"
```
