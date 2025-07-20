# 🗂️ Sequence qualitative coding app

![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Microsoft Excel](https://img.shields.io/badge/Microsoft_Excel-217346?style=for-the-badge&logo=microsoft-excel&logoColor=white)
[![License](https://img.shields.io/github/license/Ileriayo/markdown-badges?style=for-the-badge)](./LICENSE)

A Shiny app for labeling rows in an Excel file using a custom set of categories. You can click multiple labels in any order, and the labels are saved as a semicolon-separated list in a new column.

![Screenshot](screenshot.png)

## Features
- Upload `.xlsx` file and choose a column to annotate
- Apply multiple labels per cell
- Reset label for any row
- Preview each row with **Markdown rendering**
- Export the labeled Excel file anytime

## Getting Started

1. Install R and R packages:
```r
install.packages(c("shiny", "readxl", "writexl", "RColorBrewer", "markdown"))
```
2. Configure your labels in `label_list`.
3. Run the app:
```r
shiny::runApp()
```
