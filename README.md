# eyelinkReader

R package to import eye tracking recording generated by SR Research Eyelink eye tracker from  EDF-files. It includes options to import events and/or recorded samples and extract individual events such as saccades, fixatoins, blinks, and recorded variables.

This is a developmental version of the package. It is feature-complete and stable, as far as  importing itself is concerned. However, it does lack final touches of a CRAN-worthy package such as failsafes during the installation (I am a newby to Makevars), tests, good example datasets, vignettes, etc. 

## Installation from Github

Please note that, in order to work, this package requires EDF API library. It is included in Eyelink  Developers Kit, available from [www.sr-support.com](https://www.sr-support.com). Because the package needs to be compiled during the installation, it will fail if no library was found. Currently, it assumes that on Windows x64 the EDF library is at its default location (`c:/Program Files (x86)/SR Research/EyeLink/EDF_Access_API/lib/win64`).

To install, run
```
library("devtools")
options(devtools.install.args = "--no-multiarch") # only relevant for Windows 
install_github("alexander-pastukhov/eyelinkReader", dependencies=TRUE)
```

At the moment, this works for me on Linux and 64-bit Windows, but not on 32-bit Windows. Also, currently I do not have an access to Mac OS X, so I would appreciate any help with that platform.

## Manuals

I have attempted to document the package as thoroughly as I could. However, for any question about specific attributes I would refer to the EDF API manual, which is supplied by SR Research alongside the library.

## License

The code is licensed under the [MIT License](http://www.opensource.org/licenses/mit-license.php). However, the following additional conditions apply to header files in `src/SRResearch` subfolder.  All EyeLink® related files, including compiled files may be made available to SR Research licensed users only and may not otherwise be redistributed in any manner.
