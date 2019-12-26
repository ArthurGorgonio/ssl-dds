# FlexCon-C1(s)
This is a semi-supervised algorithm based on self-training algorithm. This [algorithm][1] train a classifier to setup the confidence value of next iteration. Also, this algorithm generate a memory of the all samples, in orther words, it stores all the predicitons data.

# Usage
To use this algorithm, you need to download R and download this repository.

Assume that the R working directory it isn't the same of this application use
```R
setwd("pathToThisFold")
```
In Windowns you need a \\ to split folds names.
in Linux you just use a / to split folds names.

so you are able to run this application by 2 forms:
1)
```R
source("src/MainCephas.R")
```
or
2) Open the MainCephas script and run using RStudio.

# References
[1]: https://ieeexplore.ieee.org/document/8489128/.
