## XdfBinCompare: A tunerpro XDF and bin comparison utility

### Motivation

The idea is to compare bins, according to an XDF so that you can
see the changes you or another tuner have made to a bin relative to some starting point.

This helps to take stock of your changes before flashing and in reverse engineering
changes made by others.

### Usage
```shell
Usage: xbc [options]

--help                   Display usage text
--xdf <value>            XDF model common to both bins
--base-bin <value>       Filename of the starting bin file
--mod-bin <value>        Filename of the bin to compare with the base
--table-exclusions <value> A comma separated list of substrings used to exclude tables. 
                         E.g. '(FF), (FF#2)' => exclude flex fuel tables.
--category-exclusions <value> A comma separated list of exact names used to exclude categories. 
                         E.g. 'MHD+ Suite, MHD+ Config' => exclude these two categories.
--report <value>         Filename of an existing report from which to extract notes and table ordering. Enables re-running of a diff without having to rework notes and table re-ordering.
--output <value>         Output filename for difference report.
```

For example, the following command
```shell
xbc --table-exclusions="(Antilag),(Map 2),(Map 3),(Map 4),(FF),(FF#2)" --report "stage-0-vs-stage-1.txt" --xdf "BMW-XDFs\F G series B58\00003076501103.xdf" --base-bin "stage-0.bin" --mod-bin "stage-1.bin"
```
will compare a stage 0 and stage 1 bin, ignoring flexfuel, antilag and multimap tables.

This will produce a text report such as:
```shell
Base bin: stage-0.bin
Modified bin: stage-1.bin
XDF: BMW-XDFs\F G series B58\00003076501103-jmt.xdf

Table (scalar): Load limit factor by rich for component protection
Description: K_FRFMXBS_MN
Categories: Limits
Unit info: -
Base:
    0.500

Difference:
    0.500

Modified:
    1.000

Notes:
    Disables this load limit multiplier.

Table (vector): Max naturally aspirated volumetric efficiency
Description: For naturally aspirated operation, should not exceed 100% KL_LAMX
Categories: Limits
Unit info: 1/min --> %
Breakpoints: Max naturally aspirated volumetric efficiency X (autogen)
Base:
      100      120      163      175      225      250      275      300      325      350      450      475      500      520      540      600      675      695
    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00    92.00

Difference:
      100      120      163      175      225      250      275      300      325      350      450      475      500      520      540      600      675      695
     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00     8.00

Modified:
      100      120      163      175      225      250      275      300      325      350      450      475      500      520      540      600      675      695
   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00   100.00

Notes:
    Adjust volumetric efficiency to simplify calculations
    
...
```

The `--report` option is designed to help with iterative map development. 
It will make the report generator read an existing report and extract notes and tables orderings from it.
Thus, if you have an earlier report with notes and some table ordering that makes sense, but then need to
re-run the diff after map changes, the existing notes and orderings will be maintained in the new report.

The tail of the report contains listings of tables that are not changed and those have been excluded 
due to `--table-exclusions` and `--category-exclusions`.

### Thoughts on future features

1. Map simulation
This would allow arithmetic expressions referencing table lookups and allow the user
to feed in ranges of values to see the response from the map. For example, calculate
the torque request given the values in 'relative torque request' and 'torque request ceiling'
maps for a range of accelerator pedal inputs. Determine if any inputs fall outside the 
map breakpoints.

2. Nicer rendering
Text works okay but HTML rendering with coloured graphs would be nicer.
