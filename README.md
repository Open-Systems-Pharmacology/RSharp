
# rSharp

<!-- README.md is generated from README.Rmd. Please edit that file -->

The **RSharp** R package provides access to .NET libraries from R. It
allows to create .NET objects, access their fields, and call their
methods.

This package is based on the [rClr](https://github.com/rdotnet/rClr)
package and utilizes some of its code base.

## Installation

### Windows

Start your preferred R environment and run the following R commands from
within the `rSharp` folder to install the package using prebuilt
binaries

    install.packages("devtools")
    devtools::install_github("https://github.com/Open-Systems-Pharmacology/rSharp")

### Ubuntu

Run the following commands to get set up to install and use the R
package using prebuilt binaries

    sudo apt-get install dotnet-runtime-8.0 libcurl4-openssl-dev libssl-dev libxml2-dev 
    sudo apt-get install libfontconfig1-dev libharfbuzz-dev libfribidi-dev
    sudo apt-get install libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
    sudo ln -s /usr/lib/x86_64-linux-gnu/libdl.so.2 /usr/lib/x86_64-linux-gnu/libdl.so

Symlink libdl.so.2 to libdl.so

    sudo ln -s /usr/lib/x86_64-linux-gnu/libdl.so.2 /usr/lib/x86_64-linux-gnu/libdl.so

Start your preferred R environment and run the following R commands from
within the `rSharp` folder

    install.packages("devtools")
    devtools::install_github("https://github.com/Open-Systems-Pharmacology/rSharp")

## Build

### Windows

Download and install R <https://cran.r-project.org/bin/windows/base/>

Open your preferred R environment and run the following R commands from
within the `rSharp` folder

    install.packages("devtools")
    devtools::build()

To optionally set up to build the binaries make sure
`Desktop development with C++` workload is installed in Visual Studio.

Create an environment variable `R_INSTALL_PATH` and set the value to the
path where R is installed

    set R_INSTALL_PATH = "C:\Program Files\R\R-4.3.3"

Start Visual Studio and open the `rSharp.sln` solution file and build
the solution.

Then start your preferred R environment and run the following R commands
from within the `rSharp` folder

    devtools::install()

### Ubuntu

Run the following commands to get set up to build the R package

    git clone https://github.com/Open-Systems-Pharmacology/rSharp.git
    sudo apt-get install dotnet-runtime-8.0 libcurl4-openssl-dev libssl-dev libxml2-dev 
    sudo apt-get install libfontconfig1-dev libharfbuzz-dev libfribidi-dev
    sudo apt-get install libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

Symlink libdl.so.2 to libdl.so

    sudo ln -s /usr/lib/x86_64-linux-gnu/libdl.so.2 /usr/lib/x86_64-linux-gnu/libdl.so

Optionally set up to build the binaries

    sudo apt-get install dotnet-sdk-8.0
    sudo apt-get install nuget

Start your preferred R environment and run the following R commands from
within the `rSharp` folder

    install.packages("devtools")
    devtools::build()

To build the binaries change to the `rSharp\shared` directory and run

    make

Then change back to the `rSharp` directory start your preferred R
environment and run the following R commands

    devtools::install()

## User guide

Examples of interacting with .NET assemblies using this package are
detailed in `vignette('user-guide')`. Some useful tips around using the
package are available in the `vignette('knowledge-base')`.

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community
(codebases, issue trackers, chat rooms, mailing lists etc…) is expected
to follow the Open Systems Pharmacology [code of
conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution

We encourage contribution to the Open Systems Pharmacology community.
Before getting started please read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
If you are contributing code, please be familiar with the [coding
standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

The `{RSharp}` package is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
