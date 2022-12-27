[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
![build](https://github.com/Mikolaj-A-Kowalski/endfio/actions/workflows/build.yml/badge.svg)

# endfio - ENDF-6 I/O Library for Rust

[ENDF-6](https://www-nds.iaea.org/public/endf/endf-manual.pdf) is the
standard format for distribution of nuclear data files. `endfio` aims to be
a small library to allow reading, modifying and creating the nuclear data files
in that format.

Each ENDF file is considered a `Tape`, which can contain multiple `Materials`,
which are a collection of `Files`, which finally store `Sections`. Only at the
level of section is the actual data stored as a sequence of `Records`.

At the moment `endfio` supports reading and writing the following records:
- [x] TEXT record
- [x] CONT record
- [x] HEAD record
- [x] DIR record
- [x] LIST Record
- [x] TAB1 Record
- [x] TAB2 Record
- [ ] INTG Record

# Format Warnings

## Ambigious 'Record' meaning in MF=1 MT=451

Header section of each `Material` (MF=1 MT=451) terminates with a 'Directory
entry', which lists the contents in a format like e.g.:
```
                                1        451        164          3
                               23        501        677          3
                               23        502        124          3
                             <mf>       <mt>       <nc>     <nmod>
```
Where:
- `<mf>` - The number specifying type of a file
- `<mt>` - The number specifying type of a section (e.g. reaction type)
- `<nc>` - Number of *Records* in the section
- `<nmod>` - Number of times the data in the section was changed since
             originally published

Historically the ENDF files seem to interpret *Records* in the context of
`<nc>` as the 'number of lines'. However, since a single ENDF record can
span multiple lines (TAB1, TAB2, LIST, INTG), it can be argued that this
interpretation is incorrect. In fact, some recently added sections in e.g.:
[ENDF/B VIII.0](https://www.nndc.bnl.gov/endf-b8.0/)(e.g. MF=33 of H-1),
seem to interpret *Record* as 'ENDF Record'.

To resolve this problem `endfio` will read the files with the stricter
interpretation of *Record* in `<nc>` without error, but will use the
historical 'number of lines' interpretation on output.

## HSUB Overture
ENDF-6 format requires that first three lines of the text *comment* section
of the Material header (MF=1 MT=451) follow special format:
```
---<Library Name>      MATERIAL <MAT>      REVISION <N>
----<Descriptive sub-library name>
-----ENDF-6
```
There is a bit of ambiguity regarding the format of different
entries. `endfio` makes the following assumptions:
 - `<Library Name>` contains name from NLIB number and arabic version e.g. `JEFF 3` or `ENDF/B 8`.
 - `REVISION` keyword is present if release number if not 0. E.g. JEFF 3.3. will contain `REVISION 3`.
 - Descriptive Sub-Library names are altered wrt. to manual to be easier to generate automatically: E.g.
   endfio will use `Incident-Photon Data` instead of `Photo-Nuclear Data`
 - 3rd line does not contain `FORMAT` keyword (it is not mentioned in the ENDF Manual)

Note that the *overture* is only generated by endfio for Materials that are
created from scratch. If the MaterialHeader was loaded from an existing file
the *overture* will not be changed when re-printed!