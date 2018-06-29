# R Packages for SML2018 Labs

Students will be required to work through programming exercises and complete assignments using the [R statistical programming language](https://www.r-project.org/). It is strongly recommended that students take time to familiarise themselves with the fundamentals of the language before class commences.

Labs will run from weeks 2 to 11 of the semester and cover the following subjects:

Week  | Subject
------|----------------------------------
1-2   | Tidyverse fundamentals
3     | Kernel methods
4-5   | Neural networks
6-7   | Recommender systems/sparse matrices
8-9   | Text mining
10-11 | Social networks

A brief description of each subject is provided below.

## Tidyverse fundamentals

Loading, cleaning and tidying data is one of the most important and often overlooked tasks in data science. An often heard refrain is that 80% of a data scientists time is spent cleaning and rearranging data. We can reduce the amount of time spent on this tedious task by adhering to tidy data principles and using some of the fantastic data munging packages available to us in R. These first of these two labs will introduce the [tidyverse](https://www.tidyverse.org/) and focus on the following packages:

* `readr` for reading data into R.
* `dplyr` for calculating new variables and summarising data.
* `tidyr` for reshaping dataframes.
* `purrr` for dealing with lists and fitting models.
* `lubridate` allows us to quickly parse dates.
* `stringr` allows us to extract and find strings.

Our second lab will introduce the fantastic `ggplot2` library for plotting. We will look at how to produce useful visualisations such as:

* Various types of plots (scatter, line, histogram, violin, etc.).
* Controlling aesthetics of a plot.
* Adding useful statistics to a plot.
* Facetting plots by variables.
* Creating animated (`gganimate`) and interactive plots (`plotly`).

This is a lot to cover in two labs, but the main focus here will be to give those that haven't used packages from tidyverse an adequate starting point. As we progress through the semester you will be able to hone your skills with these packages while continuing to learn new material.

## Kernel methods

__TODO__

Some ideas:

* Have student create their own SVM classifier from scratch? Similar to [this](http://enhancedatascience.com/2018/05/23/create-your-machine-learning-library-from-scratch-with-r-3-5-knn/) series of blog posts (incidentally they have an SVM post coming up next which could prove useful as a reference).
* Play around with kernel trick?
* Explore overfitting model selection criteria. See G. C. Cawley and N. L. C. Talbot, Over-fitting in model selection and subsequent selection bias in performance evaluation, Journal of Machine Learning Research, 2010. Research, vol. 11, pp. 2079-2107, July 2010.
* Use it to do classification on a data set
* Compare different kernel performance
* Compare to the existing R packages for SVM classification. Maybe only do a simple SVM implementation from scratch and use existing packages for more complicated experiments? Just in case students can't finish the implementation? Or leave implementation as final exercise?

## Neural networks

We will be using Keras to fit our neural networks. Keras is a high level deep learning framework capable of creating deep architectures such as convolutional neural networks and recurrent networks. It is easy to use and is capable of using different back-ends (we will be using TensorFlow).

## Recommender systems/sparse matrices

Need more info on class material. The `recosystem` package looks quite promising. An introduction is provided [here](https://cran.r-project.org/web/packages/recosystem/vignettes/introduction.html). It contains bindings for the `LIBMF` C++ library.

## Text mining

We will use the packages:

* `tidytext`
* `stringr`.

Our focus will be on sentiment analysis, tf-idf and constructing features such as n-grams. Maybe look at producing word embeddings using a vector space model. Can do something like a simple word2vec using Keras?

## Social networks

Can use the following packages for network visualisations:

* `ggnet`
* `igraph`.

Have a look at [this paper](https://journal.r-project.org/archive/2017/RJ-2017-023/RJ-2017-023.pdf) for a discussion of network visualisation using `ggplot2`.