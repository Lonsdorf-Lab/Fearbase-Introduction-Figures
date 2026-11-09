# FEARBASE publication figures

This repository contains the reproducible code for figures 2-5 of the following manuscript:\
<br>

```{=html}
<div align="center"\>

**The Fearbase, a growing database, living analysis platform, and collaborative consortium for human fear conditioning**

Maria Bruntsch1,2*, Tina B. Lonsdorf1,2*, Fritz Becker1, Annalena Witte1, Conrad Alting1, Joel Beckmann3, Katarina Blask3, Mana R. Ehlers1 & the Fearbase Consortium
</div>
```

1 Bielefeld University, Department of Psychology, Biological Psychology and Cognitive Neuroscience, Bielefeld, Germany </br> 2 University Medical Center Hamburg-Eppendorf, Department of Systems Neuroscience, Hamburg, Germany </br> 3 Leibniz Institute for Psychology, Trier, Germany </br>

- These authors contributed equally

## Reproducible environment: rix

The `default.nix` snapshot was generated using `generate_env.R` and [`rix`](https://docs.ropensci.org/rix/). It contains information on the exact R version and packages used to create the figures.

The interested reader/user only needs Nix to reproduce the environment from default.nix.

### Reproduce all figures

#### From the terminal

If you are on Windows with a general-purpose WSL: Install Nix first.

Following that, you can reproduce all figures on Linux, macOS and Windows with the following commands:

``` sh
nix-build
nix-shell --pure --run "Rscript run_figures.R"
```

#### From an R session

Important: `rix` and Nix must be available.

Run the following command: `rix::nix_build(project_path = ".")`. This will produce all figures, which are written to `figures/` in both PDF and TIFF (LZW-compressed) formats. The TIFF files are rendered at 600 dpi using the physical dimensions declared in the export section at the end of `run_figures.R`.

## Fallback

You can also run the figure pipeline using your current installation of R and the `run_figures.R` script like this:

``` powershell
& 'C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe' run_figures.R
```

Alternatively, simply open and run the `run_figures.R` script in your R IDE of choice.

## Project structure

- `run_figures.R`: The complete figure pipeline. The script reads the inputs, prepares all figure data, builds figures 2-5, and exports the figures as PDF and TIFF files.
- `data/input/`: data used for creating the figures. NOT INCLUDED. Harmonised data is IP of the Fearbase and not available upon request. See manuscript for details.
- `figures/`: Exported manuscript figures (TIFF and PDF)

## Figure outputs

| Output | Contents | Size |
|----------------------|----------------------|---------------------------:|
| `figure_02` | Publication Year, Sample Size, Age, and Sex | 15 × 15 cm |
| `figure_03` | SCR Quantification, Reinforcement, Instructions, and Stimuli | 16 × 15 cm |
| `figure_04` | Measure and Phase Co-occurrence | 15 × 21 cm |
| `figure_05` | Phase Lengths | 10 × 10 cm |

Each output is produced as `.pdf` and `.tiff`.