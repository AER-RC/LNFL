# LNFL

---
**Contents**

1. [Cloning](#cloning)
2. [LNFL Docker Image](#docker)
3. [Building LNFL](#build)
4. [Running LNFL](#run)

LNFL converts an ASCII line file parameter database (available at [RTWeb](http://rtweb.aer.com/line_param_download.php)) to the unformatted binary the LBLRTM expects as input. This is called a `TAPE3`.

# Cloning <a name="cloning"></a>

Assuming the output directory should be `LNFL`:

```
git clone --recursive git@github.com:AER-RC/LNFL.git
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

# LNFL Docker Image <a name="docker"></a>

If users would like to bypass having to setup and build LNFL on their own and have [Docker](https://www.docker.com/) installed on their system, they can use the Docker Hub image for LNFL that the AER-RC group has made [publicly available](https://hub.docker.com/repository/docker/aerradclim/lnfl). Alternatively, the same image is available in the [LNFL GitHub Packages](https://github.com/AER-RC/LNFL/packages/200491). To run an LNFL container with the image (only one `pull` is necessary):

```
docker pull aerradclim/lnfl # from Docker Hub
docker tag aerradclim/lnfl lnfl

docker pull docker.pkg.github.com/aer-rc/lnfl/lnfl:latest # from GitHub
docker tag docker.pkg.github.com/aer-rc/lnfl/lnfl:latest

docker run --name lnfl --rm -v ~/Work/RC/TAPE5_2000-3250:/LNFL/TAPE5 -v ~/Work/RC/LNFL_output:/LNFL/LNFL_Out lnfl
```

Not all of the arguments in the previous code snippet are required. A simple `docker run aerradclim/lnfl` will suffice, but the user would not be providing any inputs or getting any outputs. Consequently, there are two "volume mounts" -- one for the only LNFL input ([the TAPE5](#TAPE5)), and one for the [outputs](#output). The former is a single file, and the latter is a directory. In volume mounts, the path on the left side of the `:` is with respect to the "host" (i.e., the local machine of the user), and the path to the right of the `:` is the path inside the container. LNFL users should alter their host paths accordingly. Host paths should be absolute and not relative. The output directory should already exist on the host, otherwise it will be written as root and the files underneath it will only be accessible by root. BOTH VOLUME MOUNTS ARE NECESSARY TO RUN THE MODEL AND RETRIEVE ITS OUTPUT.

The following are optional container arguments:

* `name`: name of the container
* `--rm`: removes container after LNFL is finished running and staging outputs

# Building LNFL <a name="build"></a>

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

# Running LNFL <a name="run"></a>

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

#### _Record 1_

Required (but ignored); 72 characters (formatted as 9 8-character strings, or `9A8`) of user identification; think of this record as a documentation comment

#### _Record 2_

Required; Specifies the spectral range of the line file, with:

| Variable Name | Column Number Range | String Format | Notes |
| :--- | :---: | :---: | :--- |
| _v<sub>min</sub>_ | 1-10 | `F10.3` | <ul><li>low wavenumber limit for the line file</li><li>should be 25 cm<sup>-1</sup> less than _v<sub>1</sub>_ for LBLRTM calculation</li></ul> |
| _v<sub>max</sub>_ | 11-20 | `F10.3` | <ul><li>high wavenumber limit for the line file</li><li>should be 25 cm<sup>-1</sup> greater than _v<sub>2</sub>_ for LBLRTM calculation</li></ul> |


#### _Record 3_

Required; Specifies the molecules for which the line file is created and additional LNFL options

| Variable Name | Column Number Range | String Format | Notes |
| :--- | :---: | :---: | :--- |
| `MOLIND1` | 1-47 | `47I1` | <ul><li>Molecular INDicator for Molecule `M` from line data on file `TAPE1`</li><li>0  molecule `M` is not selected, 1 molecule `M` is selected</li><li>See [Available Species Table](#molecules)</li><li>Note that molecule number and column number are the same</li></ul> |
| `HOLIND1` | 52-100 | `A49` | <ul><li>HOLlerith INDicator to select general LNFL options and specific options for `TAPE1`</li><li>See [LNFL Options Table](#options1)</ul> |

**Available Molecular Species** <a name="molecules"></a>

| Molecule Number `N` | Species Chemical Formula | Molecule Number `N` | Species Chemical Formula | Molecule Number `N` | Species Chemical Formula | Molecule Number `N` | Species Chemical Formula | Molecule Number `N` | Species Chemical Formula | Molecule Number `N` | Species Chemical Formula | Molecule Number `N` | Species Chemical Formula |
| :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| 1 | H<sub>2</sub>O | 2 | CO<sub>2</sub> | 3 | O<sub>3</sub>| 4 | N<sub>2</sub>O | 5 | CO | 6 | CH<sub>4</sub> | 7 | O<sub>2</sub> |
| 8 | NO | 9 | SO<sub>2</sub> | 10 | NO<sub>2</sub> | 11 | NH<sub>3</sub> | 12 | HNO<sub>3</sub> | 13 | OH | 14 | HF |
| 15 | HCl | 16 | HBr | 17 | HI | 18 | ClO | 19 | OCS | 20 | H<sub>2</sub>CO | 21 | HOCl |
| 22 | N<sub>2</sub> | 23 | HCN | 24 | CH<sub>3</sub>Cl | 25 | H<sub>2</sub>O<sub>2</sub> | 26 | C<sub>2</sub>H<sub>2</sub> | 27 | C<sub>2</sub>H<sub>6</sub> | 28 | PH<sub>3</sub> |
| 29 | COF<sub>2</sub> | 30 | SF<sub>6</sub> | 31 | H<sub>2</sub>S | 32 | HCOOH | 33 | HO<sub>2</sub> | 34 | O | 35 | ClONO<sub>2</sub> |
| 36 | NO<sup>+</sup> | 37 | HOBr | 38 | C<sub>2</sub>H<sub>4</sub> | 39 | C<sub>3</sub>HOH | 40 | CH<sub>3</sub>Br | 41 | CH<sub>3</sub>CN | 42 | CF<sub>4</sub> |
| 43 | C<sub>4</sub>H<sub>2</sub>| 44 | HC<sub>3</sub>N | 45 | H<sub>2</sub> | 46 | CS | 47 | SO<sub>3</sub> |  | | | |

**LNFL Options** <a name="options1"></a>

| Option | Description |
| :--- | :--- |
| `LNOUT` | <ul><li>Selects option to provide formatted representation of `TAPE3` on file `TAPE7`</li><li>Representation is identical to `TAPE1` and `TAPE2`</li><li>One transition per record</li><li>**CAUTION** this option may produce a VERY LARGE output file</li></ul> |
| `NOCPL` | <ul><li>Suppresses all line coupling information on `TAPE3` and `TAPE7`</li></ul> |
| `NLTE` | <ul><li>Reserves transition parameters (quantum numbers) for LBLRTM Non Local Thermodynamic Equilibrium (NLTE) option</li></ul> |
| `REJ` | <ul><li>Selects line rejection and requires input data for strength rejection (record 5)</li></ul> |
| `MRG2` | <ul><li>Line parameters on `TAPE2` are to be merged with those on `TAPE1`</li></ul> |
| `F160` | <ul><li>Selects the 160 character format for `TAPE1` (e.g. HITRAN_2004)</li></ul> |
| `BLK1` | <ul><li>indicates `TAPE1` is blocked (Note: `NBLK1` is ignored)</li></ul> |
| `EXBRD` | <ul><li>Uses extra broadeneing parameter files</li></ul> |

#### _Record 4_

Required if `MRG2` set in Record 3; `TAPE2` molecules and options

| Variable Name | Column Number Range | String Format | Notes |
| :--- | :---: | :---: | :--- |
| `MOLIND2` | 1-47 | `47I1` | <ul><li>Molecular INDicator for Molecule `M` from line data on file `TAPE2`</li><li>0  molecule `M` is not selected, 1 molecule `M` is selected</li><li>See [Available Species Table](#molecules)</ul> |
| `HOLIND2` | 52-100 | `A49` | <ul><li>HOLlerith INDicator to select general LNFL options and specific options for `TAPE2`</li><li>See [Line Coupling Options Table](#options2)</ul> |

**Line Coupling Options** <a name="options2"></a>

| Option | Description |
| :--- | :--- |
| `BLK2` | <ul><li>indicates `TAPE2` is blocked (Note: `NBLK2` is ignored)</li></ul> |
| `F80` | <ul><li>Replacement or supplemental line data in 80-character format from `TAPE2` merged with line data from `TAPE1`.</li><li>`TAPE3` parameters required for LBLRTM from 80-character formatted data are set to default parameters by LNFL (1982 HITRAN format)</li></ul> |

#### _Record 5_

Required if line rejection option (`REJ`) selected in Record 3

* `SR(M=1,NMOL)`: Strength rejection value for molecule `M`
* `8E10.3` Format
*  lines with strength values less than `SR(M)` will not be included on LBLRTM line file (`TAPE3`)
*  negative value for `SR(M)` uses internally stored default value based on tangent path through the Earth's atmosphere
*  a value of 0 for `SR(M)` implies no rejection for molecule `M`, all lines for that molecule are retained
*  values for `SR(M)` are relative to LBLRTM line strengths, which are equal to HITRAN strengths / ( _v_ * (1-e<sup>_v_/_kT_</sup>))
*  `NMOL` is 7 or the number of the highest molecule selected, whichever is larger

## LNFL Outputs <a name="output"></a>

| File Name | Description |
| :--- | :--- |
| `TAPE3` | Binary output file (input for LBLRTM and MonoRTM) |
| `TAPE6` | Log file |
| `TAPE7` | ASCII version of `TAPE3` in older 100 character HITRAN format, does not include extra broadening parameters |
