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

# Running LNFL

LNFL now accommodates the 160 character `.par` format adopted for HITRAN_2004. However, the default format for LNFL will remain the 100 character format. The rationale for this choice is that the vibrational data as stored in the 100 character format is more amenable to utilization in LBLRTM, particularly for LBLRTM Non-LTE calculation than that associated with the 160 character format for which the vibrational information had been expanded to facilitate association of the lines with the quantum mechanical vibrational designations.

The capability to accommodate the 160 character format is restricted to `TAPE1`. The `TAPE2` files should be in 100 character format - you can generate this from the 160 character format by running LNFL with the 160 cahacter file as `TAPE1`, and then use the 100 character `TAPE7` file that LNFL produces as the `TAPE2` for the next run.

A few notes:

1. To facilitate the change to HITRAN2004, some of the LNFL flags on Record 3 in older releases have been distributed to Records 3 and 4. On Record 3, the flags include those that are general for the LNFL run and those that are relevant to `TAPE1`.  On Record 4, the flags include those relevant to `TAPE2` only.

2. The default for blocking has been changed.  No blocking is now the default. Flags `NBLK1` and `NBLK2` are no longer necessary and are ignored.

3. The writing of internally stored line coupling information to `TAPE2` is no longer supported. Line coupling information may be included explicitly on `TAPE1` or `TAPE2` by the user.

4. As of Dec. 2013, LNFL requires additional broadening parameter input files. These files are listed in the [LNFL Inputs Section](#inputs), and example files are included in the LNFL package. These files are only needed and used if the `IBRD` flag is set in the `TAPE5`.

5. As of Jan. 2015, LNFL allows one to specify the name of the ASCII line file at the command line rather than having to link it to `TAPE1`. For example, if one wanted to use aer_v_3.6 as the ASCII line file, `lnfl_v3.1_linux_pgi_sgl aer_v_3.6` could be used at the command line.

## LNFL Inputs <a name="inputs"></a>

| File Name | Description |
| :--- | :--- |
| `TAPE1` | ASCII line file (e.g., HITRAN database) |
| `TAPE2` | Supplemenatry ASCII Line File to be merged with `TAPE1` |
| `TAPE5` | [LNFL Input File](#TAPE5) |
| `co2_co2_brd_param`| CO<sub>2</sub> self broadening parameters |
| `co2_h2o_brd_param` | CO<sub>2</sub> transitions broadened by H<sub>2</sub>O |
| `o2_h2o_brd_param` | O<sub>2</sub> transitions broadened by H<sub>2</sub>O |
| `wv_co2_brd_param` | H<sub>2</sub>O transitions broadened by CO<sub>2</sub> |
| `spd_dep_param` | Speed-dependent Voigt parameters |

Special reduced microwave ASCII line files, corresponding to the old `spectral_lines.dat` of MonoRTM v4.2, are provided to be used as the `TAPE1` inputs of MonoLNFL in the microwave. The calculational accuracy of these reduced line lists at any frequency between 0 and 899.4 GHz (0-30 cm<sup>-1</sup>) with respect to the corresponding full line list (aer_v_3.3) is:

* spectral_lines.dat.0_55.v5.0_fast: 0.1 K upwelling, 0.2 K downwelling
* spectral_lines.dat.0_55.v5.0_veryfast: 0.5 K upwelling, 1.0 K downwelling

A special reduced near-IR ASCII line file for the ASCENDS region (6300-6700 cm<sup>-1</sup>) containing the first 7 HITRAN molecules is also included (`spectral_lines.dat.6300_6700.v5.0`). These line parameters are consistent with aer_v_3.2 and aer_v_3.3 (which are identical for wavenumbers greater than 55 cm<sup>-1</sup>). See files headers and MonoRTM v5.0 release notes for more details.

Users who wish to run MonoRTM v5.0 in other spectral regions should use the full AER line file (aer_v_3.5), with a caution that the full line file can take a long time to run.

`TAPE2` *must* be in older 100 character HITRAN format.

Note that LBLRTM ignores the speed dependence parameters, but they are used by MonoRTM v5.0 and later.

### LNFL TAPE5 <a name="TAPE5"></a>

## LNFL Outputs

| File Name | Description |
| :--- | :--- |
| `TAPE3` | Binary output file (input for LBLRTM and MonoRTM) |
| `TAPE6` | Log file |
| `TAPE7` | ASCII version of `TAPE3` in older 100 character HITRAN format, does not include extra broadening parameters) |
