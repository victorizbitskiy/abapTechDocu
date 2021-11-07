[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/victorizbitskiy/zru_number_validation/blob/main/LICENSE)
![ABAP 7.4+](https://img.shields.io/badge/ABAP-7.4%2B-brightgreen)

<img src="https://github.com/victorizbitskiy/abapTechDocu/blob/main/logo/logo.png" height="100px"/>\
<a href="https://www.flaticon.com/authors/itim2101">Designed by itim2101/Flaticon</a>

**Work in progress...**

Translations:
- [:ru: На русском языке](https://github.com/victorizbitskiy/abapTechDocu/blob/main/translations/ru/) 

## `abapTechDocu`
A tool to help you create technical documentation

# Table of contents
1. [What it is?](#what-it-is)
2. [What is this for?](#what-is-this-for)
3. [Installation](#installation)
4. [Usage](#usage)
5. [Supported Object Type](supported-object-type)

## What it is?
A tool for creating technical documentation

## What is this for
Often, when creating technical documentation, it is required to indicate objects created or modified in the system. Doing it manually is a chore, so I wanted to find a way to automate this. I found a tool to help create documentation about <a href="https://github.com/victorizbitskiy/CUSTTOOL"> settings</a>, but did not find a tool that automates the search for created development objects. So I created it myself.

## Installation
Installation is done with [abapGit](http://www.abapgit.org).

## Usage
Just run the **`ztechdocu`** report and specify the transport requests or packages.

![screen](https://github.com/victorizbitskiy/abapTechDocu/blob/main/docs/img/sel_scr.png)

As a result, you will get a list of all relevant objects. Like this:

![result](https://github.com/victorizbitskiy/abapTechDocu/blob/main/docs/img/example_1.png)

Then you can, for example, upload this data to a file.

## Supported Object Type
Currently, the output of the object title is supported only for the following objects.

| Object type | Object type name             |
| :-----| :----------------------------------|
|  CLAS | Class (ABAP Objects)               |
|  DEVC | Package                            |
|  DOMA | Domain                             |
|  DTEL | Data Element                       |
|  FUGR | Function Group                     |
|  INTF | Interface (ABAP Objects)           |
|  MSAG | Message Class                      |
|  PROG | Program                            |
|  SFPF | Form Object: Interface             |
|  SFPI | Form Object: Form                  |
|  SHLP | Search Help                        |
|  SXCI | Business Add-Ins - Implementations |
|  TABL | Table                              |
|  TRAN | Transaction                        |
|  TTYP | Table Type                         |
|  VIEW | View                               |

