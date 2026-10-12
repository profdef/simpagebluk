# simpagebluk

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23311470.svg)](https://doi.org/10.5281/zenodo.23311470)

R code for the figure and the illustrative simulation in

> Djafri D. **The PAGEBLUK framework: an integrated epidemiological model of civilisational fractures.** Manuscript in preparation for *BMJ Global Health*.

*Pagebluk* is the Javanese word for a deadly epidemic. In the PAGEBLUK framework, the classical epidemiological triad sits at the core: host (**P**, *pejamu*), agent (**A**, *agen*) and environment (**L**, *lingkungan*). Around it is a ring of interdisciplinary action and behavioural feedback (**U**, *upaya lintas-ilmu dan umpan balik*). In the outer layer, civilization opens four fractures:

| Letter | Fracture | Main parameter of the reproduction number |
|---|---|---|
| **G** | *Gerak*: mobility | ι (importation), spatial coupling |
| **E** | *Eksposur*: exposure density | *c* (contact rate) |
| **K** | *Kesenjangan*: inequity | *S/N*, *D*, case fatality |
| **B** | *Berita dan bias nalar*: information disorder and reasoning bias | multiplier on *c*, *D*, *S/N* and ι |

The fractures are mapped onto

$$R_t = \beta \times c \times D \times \frac{S}{N} \; (+\ \iota)$$

## Contents

```
R/
  figure1.R     Figure 1: structure of the framework (panel A) and fracture-to-parameter mapping (panel B)
  simulasi.R    Online supplementary material 1: deterministic illustration (analyses S1–S4, figure S1)
output/         Files produced by the scripts (committed for convenience)
```

## How to reproduce

Requires R (≥ 4.3) and the packages `ggplot2`, `patchwork`, `scales`, `ragg` and `systemfonts`.

```r
install.packages(c("ggplot2", "patchwork", "scales", "ragg", "systemfonts"))
```

From the repository root:

```bash
Rscript R/figure1.R
Rscript R/simulasi.R
```

The figures were produced with the Helvetica Neue font on macOS. On other systems the scripts fall back to the default sans-serif font, so the layout may differ slightly. All numbers are unaffected.

The published outputs were generated with R 4.6.1, ggplot2 4.0.3, patchwork 1.3.2, ragg 1.5.2, scales 1.4.0 and systemfonts 1.3.2.

## The simulation (`R/simulasi.R`)

The simulation is deterministic and needs no data or random seed. All parameters are illustrative, so the results show what the multiplicative structure of the framework implies. They are not empirical estimates and do not validate the framework.

| Symbol | Meaning | Value |
|---|---|---|
| β, *c*, *D* | Transmission probability per contact, contacts per day, infectious days | 0.05, 12, 5 (so *R*₀ = 3) |
| ε | Vaccine efficacy against infection (all-or-nothing) | 0.90 |
| *V*\* | Planned vaccine coverage (S2) | 80% |
| — | Planned reduction in contacts (S2) | 30% |
| *m* | Proportion of the population holding misinformation (S2) | 0–60% |
| *r* | Reduction in uptake and adherence among those holding misinformation (S2) | 0.5, 1.0 |
| ι | Imported cases per generation (S3) | 1, 5, 20 |
| IFR | Infection fatality ratio, timely *v* delayed care (S4) | 0.5%, 1.0% |

| Analysis | Question | Method | Output |
|---|---|---|---|
| **S1** | How much must each fracture narrow? (proposition 2) | Reduction per term needed to reach *R~t~* = 1: *x~k~* = 1 − *R*₀^(−1/*k*) | `S1-reduction-per-term.csv`, figure S1a |
| **S2** | Does information disorder act as a multiplier? (proposition 4) | Misinformation prevalence at which *R~t~* = 1, when it affects vaccination and distancing (`both`) or vaccination only (`vaccine only`) | `S2-misinformation-threshold.csv`, figure S1b |
| **S3** | How does mobility interact with the other fractures? | Discrete generations, *I*~*g*+1~ = *R~t~ I~g~* + ι; equilibrium ι/(1 − *R~t~*) | `S3-importation-multiplier.csv`, figure S1c |
| **S4** | How does inequity shape who dies? (proposition 3) | Final size relation *z* = *s*₀[1 − exp(−*R*₀*z*)], *s*₀ = 1 − ε*V*, with deaths = *z* × IFR | `S4-inequity-groups.csv`, figure S1d |

Main results at *R*₀ = 3:

- **S1.** A single term must fall by 67%, or each of three terms by 31%.
- **S2.** *R~t~* returns above 1 at 22% misinformation prevalence when both measures are affected, against 27% when only vaccination is affected (*r* = 1).
- **S3.** Each imported case yields 20 cases per generation at *R~t~* = 0.95.
- **S4.** The two groups suffer 495 *v* 35 deaths per 100 000.

`hasil-simulasi.rds` holds all results as an R list. Some variable and column names are in Indonesian: `jalur` = pathway, `mati` = deaths, `hasil` = results.

## References for the model

- Anderson RM, May RM. Population biology of infectious diseases: Part I. *Nature* 1979;280:361–7. <https://doi.org/10.1038/280361a0>
- Ma J, Earn DJD. Generality of the final size formula for an epidemic of a newly invading infectious disease. *Bull Math Biol* 2006;68:679–702. <https://doi.org/10.1007/s11538-005-9047-7>

## Author

Defriman Djafri, Faculty of Public Health, Universitas Andalas, Padang, Indonesia ([ORCID 0000-0002-2119-9616](https://orcid.org/0000-0002-2119-9616)). Correspondence: defrimandjafri@ph.unand.ac.id

The code was written with the assistance of an AI tool (Claude, Anthropic), as disclosed in the manuscript. The author reviewed and verified all code and results.

## How to cite

If you use this code, please cite the article and the software:

> Djafri D. *simpagebluk: code for the PAGEBLUK framework article* (version v1.0.1) [Software]. Zenodo; 2026. <https://doi.org/10.5281/zenodo.23313623>

The DOI above refers to version v1.0.1, which reproduces the results reported in the article. The concept DOI [10.5281/zenodo.23311470](https://doi.org/10.5281/zenodo.23311470) always resolves to the latest version.

## Licence

The code in this repository is released under the [MIT License](LICENSE). If you use it, please cite the article above.
