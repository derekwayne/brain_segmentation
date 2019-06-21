# Image Segmentation

## Purpose

While considering the applications of two dimensional image segmentation I thought it would be interesting to apply the procedures to medical images such as an MRI scan. These results are a part of the pipeline for tumor extraction  and can be used for surgical preparations. Segmentation is equivalently a classification problem, where the labels represent tissues present in the scan. Specifically I chose to work with three dimensional brain MR images. This is a very interesting extension of techniques used in flat images. The goal is to explore some of the modern literature on the topic and attempt to use neruoimaging packages in R to achieve an accurate segmentation.


## Tools and Packages

1. [R](https://www.r-project.org/): For statistical computing
2. [mritc](https://cran.r-project.org/web/packages/mritc/index.html): Various methods for MRI tissue classification.
3. [imager](https://cran.r-project.org/web/packages/imager/index.html): Image processing library.
4. [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html): Data visualizations.
5. [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html): Data manipulation.
6. [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html): Functions for graphics with grid layouts.

## Author

* **Derek Wayne** - *Initial work* - [Portfolio](https://derekwayne.github.io)

## Acknowledgments and References

The methods used within this project are thanks to the work done by Yongyue Zhang et al. in the paper [Segmentation of Brain MR Images Through a Hidden Markov Random Field Model and the Expectation-Maximization Algorith](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.200.3832&rep=rep1&type=pdf).
