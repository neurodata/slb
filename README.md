# Statistical Learning Benchmarks for R (slbR)

[![Travis-CI Build Status](https://travis-ci.org/neurodata/slbR.svg?branch=master)](https://travis-ci.org/neurodata/slbR) [![Codecov status](https://codecov.io/gh/neurodata/slbR/branch/master/graph/badge.svg)](https://codecov.io/gh/neurodata/slbR)

## Contents

- [Overview](#overview)
- [Repo Contents](#repo-contents)
- [System Requirements](#system-requirements)
- [Installation Guide](#installation-guide)
- [Demo](#demo)
- [Issues](https://github.com/ebridge2/slbR/issues)

# Overview

In assessing the generalizability of a statistical learning algorithm, it is vital to consider a variety of diverse, feature-rich datasets. In this package, we develop a simple interface to many common benchmark datasets, including the Penn Machine Learning Benchmarks Olson (2017) <arXiv:1703.00512>, allowing users to examine performance across many disparate contexts.

# Repo Contents

- [R](./R): `R` package code.
- [docs](./docs): package documentation, and usage of the `slbR` package on many real and simulated data examples.
- [man](./man): package manual for help in R session.
- [tests](./tests): `R` unit tests written using the `testthat` package.
- [vignettes](./vignettes): `R` vignettes for R session html help pages.


# System Requirements

## Hardware Requirements

The `slbR` package requires only a standard computer with enough RAM to support the operations defined by a user. For minimal performance, this will be a computer with about 2 GB of RAM. For optimal performance, we recommend a computer with the following specs:

RAM: 16+ GB
CPU: 4+ cores, 3.3+ GHz/core

The runtimes below are generated using a computer with the recommended specs (16 GB RAM, 4 cores@3.3 GHz) and internet of speed 25 Mbps.

## Software Requirements

### OS Requirements

The package development version is tested on *Linux* operating systems. The developmental version of the package has been tested on the following systems:

Linux: Ubuntu 16.04
Mac OSX:
Windows:

#### Installing R version 3.4.2 on Ubuntu 16.04

the latest version of R can be installed by adding the latest repository to `apt`:

```
sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt-get update
sudo apt-get install r-base r-base-dev
```

which should install in about 20 seconds.

# Installation Guide


## Package dependencies

Users should install the following packages prior to installing `slbR`, from an `R` terminal:

```
install.packages(c('readr', 'httr'))
```

which will install in about 15 seconds on a recommended machine.

If you are having an issue that you believe to be tied to software versioning issues, please drop us an [Issue](https://github.com/neurodata/slbR/issues).

### Package Installation

From an `R` session, type:

```
require(devtools)
install_github('neurodata/slbR', force=TRUE)  # install slbR
```

The package should take approximately 20 seconds to install on a recommended computer.

# Demo

As an example, load all classification datasets from the `PMLB` repository:
```
require(slbR)
data <- load.datasets(repositories="PMLB", task="classiciation")
```
