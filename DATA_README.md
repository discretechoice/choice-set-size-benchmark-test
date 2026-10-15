# Replication data

`PrefMatWideFormatFullBiogeme.txt` is a tab-separated numeric file with a header, readable using `read.table(..., header=TRUE)`. It contains 18,141 rows for 1,512 study respondents before the scripts apply the analysis restrictions. The public copy retains 52 fields needed for the supplied scripts or block-share calculation. Unused survey fields, original survey-record identifiers and auxiliary fields have been removed. Retained values and row order have not been changed.

| Field | Use |
|---|---|
| `id` | Sequential study respondent identifier, 1–1,512; groups repeated choice tasks |
| `treat` | Treatment 1–5 corresponds to 2Alt–6Alt, including the SQ |
| `task` | Choice-task number; analysis retains tasks 1–8 |
| `choibio` | Chosen alternative, numbered from 1; 1 is the SQ |
| `choice0`–`choice5` | Alternative-specific choice indicators; suffix 0 denotes the SQ |
| `clar0`–`clar5` | Water clarity coding; scripts apply the required recoding |
| `fish0`–`fish5` | Fish-stock coding; scripts apply the required recoding |
| `bio0`–`bio5` | Biodiversity coding; scripts apply the required recoding |
| `coast0`–`coast5` | Coastal-protection coding; scripts apply the required recoding |
| `lit0`–`lit5` | Litter coding; scripts apply the required recoding |
| `cost0`–`cost5` | Annual per-person cost in euros; SQ cost is zero |
| `block` | Original experimental-block identifier |
| `blockNG` | Within-treatment block number, 1–4 |
| `v1` | Baltic Sea visit indicator used by descriptives: value 1 is coded as yes |
| `v371` | Birth year; age is calculated as 2016 minus this value |
| `v372` | Gender code; scripts code value 1 as female and other values as zero |
| `v375` | Education code; code 6 is excluded; code 4 defines the university indicator in Table 2 |

Raw attribute codes are retained. The analysis scripts implement the conversion to 0–3 for water clarity, biodiversity and litter, and 0–1 for fish stocks and coastal protection. Retain the full data file until these transformations are applied.

## Analysis sample

Keep `task <= 8` and `v375 != 6`, selecting the appropriate treatment. The resulting samples contain 305, 301, 305, 304 and 281 respondents (2Alt–6Alt): 1,496 respondents and 11,968 choices in total. Each retained respondent completes eight tasks.

Table 2 and Table 4 use respondent-level denominators. Table 3 uses choice-level denominators. The pooled Table 4 total is weighted by respondent counts.

## Table 6 block shares

The following base-R commands, run from the repository root, calculate the respondent-level shares:

```r
d <- read.table("PrefMatWideFormatFullBiogeme.txt", header = TRUE)
d <- d[d$task <= 8 & d$v375 != 6 & d$treat %in% 1:5, ]
u <- d[!duplicated(d$id), ]
round(prop.table(table(u$blockNG, u$treat), margin = 2), 3)
```

This describes the variables used by the replication code; it is not a full questionnaire codebook. The original questionnaire wording and substantive attribute definitions are provided in the manuscript.
