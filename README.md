# LNFL

LNFL converts an ASCII line file parameter database (available at [RTWeb](http://rtweb.aer.com/line_param_download.php)) to the unformatted binary the LBLRTM expects as input. This is called a `TAPE3`.

# Cloning 

Assuming the output directory should be `LNFL`:

```
% git clone --recursive git@github.com:AER-RC/LNFL.git
```

`--recursive` is important, because this repository is linked with our [common FORTRAN modules repository](https://github.com/AER-RC/aer_rt_utils) that are required in the model builds. If this keyword is forgotten, one can do:

```
git submodule init
git submodule update
```

in the `LNFL` directory.

Currently, the latest release is LNFL v3.2, and it is recommended that this be the version that users clone and checkout (rather than the `master` branch). To do this, one needs to simply checkout the `v3.2` tag:

```
git checkout tags/v3.2
```

No releases before v3.2 are available via GitHub, but they can be requested by emailing <aer_lblrtm@aer.com>. For information on previous releases, please visit the [What's New Wiki page](https://github.com/AER-RC/LNFL/wiki/What's-New).

# Building LNFL

LNFL has been built extensively only in single precision. It is not recommended that users attempt to build in double precision. To start, descend into the `build` directory:

```
cd build
make -f make_lnfl $TARGET
```

The `TARGET` environment variable depends on the user's operating system, compiler, and desired precision. Available targets are:

| Target | Description | Compiler |
| :--- | :--- | :--- |
| aixIBMsgl | IBM/AIX OS using IBM fortran,single precision| `xlf90` |
| linuxPGIsgl | Linux OS using PGI fortran,single precision |  `pgf90` |
| linuxGNUsgl | Linux OS using GNU fortran,single precision | `gfortran` |
| linuxG95sgl | Linux OS using G95 fortran,single precision | `g95` |
| inuxINTELsgl | Linux OS using Intel fortran,single precision | `ifort` |
| mingwGNUsgl | Windows unix shell environment using gfortran,single precision | `gfortran` |
| osxABSOFTsgl | Mac OS_X using Absoft Pro fortran,singleprecision | `f90` |
| osxGNUsgl | Mac OS_X using GNU fortran,singleprecision | `gfortran` | 
| osxIBMsgl | Mac OS_X using IBM XL fortran,singleprecision | `xlf90` |
| osxINTELsgl | Mac OS_X using Intel fortran,single precision | `ifort` |
| sunSUNsgl | Sun/Solaris OS using Sun fortran,single precision | `sunf90` |
| sgiMIPSsgl | SGI/IRIX64 OS using MIPS fortran,single precision | `f90` |

# LNFL Inputs

# Running LNFL

# LNFL Outputs
