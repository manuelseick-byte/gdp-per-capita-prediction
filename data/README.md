# Data

The raw source files used in the project are not redistributed here. To run the analysis, download the corresponding public source data and place the files in the structure below.

## World Bank World Development Indicators

Create:

```text
data/World Bank/
```

and place the following files there:

```text
API_NY.GDP.MKTP.KD_DS2_en_csv_v2_216051.csv
API_NY.GNP.ATLS.CD_DS2_en_csv_v2_6564.csv
API_SP.POP.TOTL_DS2_en_csv_v2_130083.csv
API_SL.TLF.TOTL.IN_DS2_en_csv_v2_127995.csv
API_NY.GNS.ICTR.CD_DS2_en_csv_v2_224.csv
API_NY.GSR.NFCY.CD_DS2_en_csv_v2_6580.csv
```

These correspond to the World Bank WDI series used in the project:

- GDP (constant 2015 US$), `NY.GDP.MKTP.KD`
- GNI, Atlas method (current US$), `NY.GNP.ATLS.CD`
- Population, total, `SP.POP.TOTL`
- Labour force, total, `SL.TLF.TOTL.IN`
- Gross savings (current US$), `NY.GNS.ICTR.CD`
- Net primary income from abroad (current US$), `NY.GSR.NFCY.CD`

The files used for the original project were downloaded from the World Bank World Development Indicators and show a source update date of 7 October 2025.

## Penn World Table

Create:

```text
data/PWT/
```

and place the project extract there as:

```text
2025-11-16T20-56_export.csv
```

The analysis uses the Penn World Table variables `hc` (human capital index) and `rtfpna` (TFP at constant national prices).

Because database exports can change over time, reproducing the project most closely requires the same vintage and variable selection used in the original analysis.
