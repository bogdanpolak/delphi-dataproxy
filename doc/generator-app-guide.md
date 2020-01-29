# User Guide: Proxy Generator

## Overview

The **Proxy Generator for FireDAC** application is a supportive tool included in TProxyDataSet project. It allows to create Delphi source code for proxy class together with code of fake dataset code. Generator is using a provided SQL command (SELECT statement) and an internal dataset to output results. 

![](../doc/resources/generator-app.png)

The project is stored in a folder `tools/generator-app`. Generator App is using FireDAC connection definitions to connect to the RDBMS server and execute query. Potentially it is possible to support to other Delphi DAC components, different then FireDAC, but this functionality in not available in current release.

Generator App goals are:
* Provide / build a SQL statement 
  * Connects to RDBMS database with `FireDAC`, paste, enter or edit a SQL statement
  * Checks a structure and data in the result data set
* Generate a proxy and fake dataset
  * Using a SQL statement and internal TDataSet component application is generating code of:
     - **proxy** - class derived from `TProxyDataSet` containing all data fields from query
     - **fake** - code generating in memory dataset (TClientDataSet or TFDMemTable) with the same structure and with code appending the live data fetched from SQL command.

## Build notes

To compile `Generator App` this tool developer requires any modern Delphi IDE (XE8 or newer) with FireDAC. FireDAC components are avaliable in all Delphi tiers including Professional and Community edition, check the Embarcadero documentation for version limitations. Project should be able to compile in older versions without any or with some minor changes - please register any problems or inform about success.

## Setup

1) Clone this repository
1) Load GeneratorApp project (`/tools/generator-app/ProxyGenerator.dpr`)
1) Build the generator project
1) Check you FireDAC Connection Definitions and add new definition if it is required. Definition management are available through:
   - `FireDAC Explorer` - external tool distributed with IDE
   - Directly in Delphi IDE using `Data Explorer` tool window

## First steps

1) Run generator
1) Select your connection definition
1) Connect to the database (server)
1) Copy or type a SQL command or use the query builder (early preview)
1) Execute this statement
1) Generate the code
