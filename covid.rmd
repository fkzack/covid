---
title: "Covid Plots"
output: github_document:
  fig_width: 11
  fig_height: 8.5
  html_preview: true
---

```{r}
ca <- getCounties(getStateUrl('California'))


print(addGrid(covidPlot(cases~date | county, data=ca, group=county, main="California Counties")))

```

