# Extract validation results from superClass objects

Extract validation results from superClass objects

## Usage

``` r
getValidation(x, from = "testset", metrics = "overall")
```

## Arguments

- x:

  superClass object or caret::confusionMatrix

- from:

  Character. 'testset' extracts the results from independent validation
  with testset. 'cv' extracts cross-validation results.

- metrics:

  Character. Only relevant in classification mode (ignored for
  regression models). Select 'overall' for overall accuracy metrics,
  'classwise' for classwise metrics, 'confmat' for the confusion matrix
  itself and 'caret' to return the whole caret::confusionMatrix object.

## Value

Returns a data.frame with validation results. If metrics = 'confmat' or
'caret' will return a table or the full caret::confusionMatrix object,
respectively.

## Examples

``` r
library(pls)
#> 
#> Attaching package: ‘pls’
#> The following object is masked from ‘package:caret’:
#> 
#>     R2
#> The following object is masked from ‘package:stats’:
#> 
#>     loadings
## Fit classifier (splitting training into 70\% training data, 30\% validation data)
train <- readRDS(system.file("external/trainingPoints_rlogo.rds", package="RStoolbox"))
SC   <- superClass(rlogo, trainData = train, responseCol = "class",
                    model="pls", trainPartition = 0.7)
## Independent testset-validation
getValidation(SC)
#>   model validation  Accuracy     Kappa AccuracyLower AccuracyUpper AccuracyNull
#> 1   pls    testset 0.8888889 0.8333333     0.5175035     0.9971909    0.3333333
#>   AccuracyPValue McnemarPValue
#> 1      0.0009653           NaN
getValidation(SC, metrics = "classwise")
#>   model validation class Sensitivity Specificity Pos.Pred.Value Neg.Pred.Value
#> 1   pls    testset     A   1.0000000   0.8333333           0.75      1.0000000
#> 2   pls    testset     B   0.6666667   1.0000000           1.00      0.8571429
#> 3   pls    testset     C   1.0000000   1.0000000           1.00      1.0000000
#>   Precision    Recall        F1 Prevalence Detection.Rate Detection.Prevalence
#> 1      0.75 1.0000000 0.8571429  0.3333333      0.3333333            0.4444444
#> 2      1.00 0.6666667 0.8000000  0.3333333      0.2222222            0.2222222
#> 3      1.00 1.0000000 1.0000000  0.3333333      0.3333333            0.3333333
#>   Balanced.Accuracy
#> 1         0.9166667
#> 2         0.8333333
#> 3         1.0000000
## Cross-validation based 
getValidation(SC, from = "cv")
#>   model validation  Accuracy     Kappa AccuracyLower AccuracyUpper AccuracyNull
#> 1   pls         cv 0.8571429 0.7857143      0.636576      0.969511    0.3333333
#>   AccuracyPValue McnemarPValue
#> 1   1.101588e-06           NaN
```
