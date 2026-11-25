# 2027 RA Update — Quarto book (final data pass)

Place these files at the ROOT of the `reasonable-assurance-analysis` RStudio project, then render
the whole book with `quarto::quarto_render()`. Output goes to `/docs` (GitHub Pages source).

## Chapters (in _quarto.yml order)
- index.qmd        — cover (TBNMC banner) + intro
- overview.qmd     — Sections 1–3; Figs 1–10 + Fig 11; Table 1; compliance Tables 2–6 (native)
- tables.qmd       — Tables 7–9 (native, from current APDB export) + Tables 10–14 (images)
- summary.qmd      — Sections 4–6 + Appendix 6 (native, 1985–2025)
- appendices.qmd   — Appendices 1–5 (native, load-reporting Assessment tables, 2022–2025)
- provenance.qmd   — code/data provenance

## This pass updated
- Figure 2  -> through-2025 (figs/fig02_chltrends_thru2025.png)
- Figures 8, 9 -> final renders (figs/)
- Figure 11 (chlorophyll-a attainment grid) -> ADDED (figs/fig11_attainment.png)
- Tables 7–9 -> native, from APDB_Tables_as_of_2026-11 (data/table7-9_*.csv)
- Appendices 1–5 -> native, load-reporting show_aaassess() 2022–2025 (data/appendix*_*.csv)
- Appendix 6 -> native, 1985–2025 (reads data/loadratios1985-2025.csv)

## R packages required to render
flextable, here, dplyr, tidyr, knitr  (all already used by your pipeline)

## Still as images (exact fidelity, not code-generated here)
- Figs 3, 5, 7, 10 (figs/) and allocation Tables 10–14 (tbls/)
- Figs 1, 4, 6 use your repo's figs/ outputs (lulcmap, sgsegtrend, wbidattainment)
