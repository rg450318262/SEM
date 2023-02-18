---
title: "COS-D419 Factor Analysis and Structural Equation Models 2023, Assignment 4"
author: "Rong Guang"
date: "2023-02-18"
output:
  bookdown::pdf_document2:
    latex_engine: lualatex
---



**\textcolor{blue}{The texts that reflect my understanding have been highlighted in} \textcolor{red}{red color}.** 

# Task description

The first section is task description, which is copied from the assignment5.rmd. It is for communicating with future "me". Please skip it.

## Exercise 5.1

Specify and estimate the initial baseline models for the two groups. 

Present a brief summary of the model fit and make the first step of the modification by including **(exceptionally, at the same time!)** all the four parameters known to be required for improving the model fit of both models.

Fine-tune the models step by step following the guidelines given in the lecture material, i.e., implement the modifications **(as usually, one change at a time)** testing and studying each step. 

Present the final baseline models of each group and draw the graphs

# Preparation

##Read in the data set:

Start by downloading the **two data files** from Moodle to your Project folder!


```r
#install the necessary pakages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, 
               expss, 
               tidyverse, 
               janitor,
               knitr, 
               qualtRics, 
               arules, 
               arulesViz, 
               sjlabelled,
               DT,
               stringr,
               labelled,
               ggstatsplot,
               ggcorplot)

library(tidyverse)
library(readr)

#This week's file name
latest.name1 <- "MBIELM1.CSV"
latest.name2 <- "MBISEC1.CSV"
#read in the data
mbi.elm <-  #elementary school
  read_csv(
    file.path(
      here(),
      'data',
      latest.name1
      )
    )

mbi.sec <- #secondary school
  read_csv(
    file.path(
      here(),
      'data',
      latest.name2
      )
    )
```

## Write functions

To control length of reports, codes already shown in the previous homework were not showing in the current report. Yet they are available in .rmd report.

### To generate a function for calculating chi square difference was defined.



### to generate CFA results with improved readability



### Write a function to simplify plotting of merged tables for multi-group fit indicies


```r
multi.fit.tab <- function(data, title, more.footnote = NULL){
data <- data |> 
  rename(p = 'p value',
         p2 = 'RMSEA p value',
         chi = 'chi square') |> 
  mutate(df = as.numeric(df) |> round(0),
         p = case_when(
           as.numeric(p) < 0.001 ~ "<0.001",
           as.numeric(p) >= 0.001 ~ p
           ),
         p2 = case_when(
           as.numeric(p2) < 0.001 ~ "<0.001",
           as.numeric(p2) >= 0.001 ~ p2
           )
         ) |>
  mutate('Chi square (df, p)' = 
           paste0(chi, "(", df,", ", p, ")"),
         'RMSEA(p)'           = 
           paste0(RMSEA, "(", p2, ")"
                  )
         ) |> 
  select(
    Model,
    'Chi square (df, p)', 
    CFI, TLI,
    'RMSEA(p)', 
    SRMR, 
    'CSF*'= CSF
    ) 
#print the combined table with adjustment of aesthetics
data |> 
  kable(booktabs = T, 
        #format = "markdown", 
        caption = 
          title,
        align = "lrrrrrr"
        ) |> 
  kable_styling(full_width = T) |> 
  footnote(symbol = 
             c("Chi square scaling factor", 
               more.footnote)
           ) |>
  column_spec(1, width = "3.5cm") |> 
  column_spec(2, width = "4cm")|> 
  column_spec(3, width = "1cm")|> 
  column_spec(4, width = "1cm")|> 
  column_spec(5, width = "2.5cm")|> 
  column_spec(6, width = "1cm") |> 
  column_spec(7, width = "1cm") 
}
```

### Write a function to simplify plotting aligned residual variance and co-variance tables


```r
align.table <- function(data, num.no.header.col, title){

data  |> 
  kable(
    digits = 3,
    booktabs = T,
    #format = "markdown",
    caption = title,
    linesep = ""
    ) |>  
  add_header_above(c(" " = num.no.header.col, 
                     "Elementary level" = 5,
                     "Secondary level" = 5
                     )
                   ) |> 
  kable_styling(
    latex_options = "striped"
  ) |> 
  footnote(
           symbol = c(
             "Un-standardized estimates",
             "Standardized estimates"
                      )
           )
}
```

### Write a function for correlation matrix with numbers



### to generate a function for histogram overlapping with density plot



### to generate a function for violin overlapping with box plot



### To generate a function describing continuous data set



### Write a function describing continuous data set



### Write a function for histogram overlapping with density plot



### Write a function to generate dot distribution plot


```r
dot.dist <- 
  function(data, type, title){
    data |>
      t() |> 
      as.data.frame() %>% 
      mutate(Item = rownames(.)) |> 
      rowwise() |> 
      mutate(Median = eval(parse(text = type))(V1:V580)) |> 
      ggstatsplot::ggdotplotstats(
        point.args = list(color = "red", size = 3, shape = 13),
        xlab = paste(type, "ratings"),
        title = title,
        x = Median,
        y = Item
      )
    }
```

### Write a fuction to generate correlation matrix with statistical test


```r
mycor <- 
  function(data, cols, title){
  mbi.elm |> 
      select(all_of(cols)) |> 
      ggstatsplot::ggcorrmat(
        colors = c("#B2182B", "white", "#4D4D4D"),
        title = "(a) Items on emotional exhaustion, 
        elementary school teacher",
        matrix.type  = "lower"
      )
    }
```

# Inspect the data

## Distribution


```r
#generate the plots, by subgroup of teachers
p.dist.elm <- 
  corr.density(
    mbi.elm, 
    fig.num = "1(a)", 
    group = "elementary school teacher"
    )
p.dist.sec <- 
  corr.density(
    mbi.sec, 
    fig.num = "1(b)",
    group = "secondary school teacher"
    )
#print the plot
library(patchwork); p.dist.elm/p.dist.sec
```



\begin{center}\includegraphics{Assignment5_RongGuang_files/figure-latex/unnamed-chunk-14-1} \end{center}

 

```r
#generate plot by subgroups of teachers
p.dot.elm <- 
  dot.dist(
    data = mbi.elm, 
    type = "median", 
    title = "(a) Elementary school teacher"
    )
p.dot.sec <- 
  dot.dist(
    data = mbi.sec, 
    type = "median", 
    title = "(b) Secondary school teacher"
    )
#plot layout
patchwork <- p.dot.elm|p.dot.sec
#print the plot with a genral title
patchwork+plot_annotation(
    title = 
      'Figure 2 Distributions of median rating for each item',
    theme = 
      theme(plot.title = 
              element_text(
                size = 16,
                face = "bold",
                vjust = -1.5,
                hjust =0.5)
            )
    )
```



\begin{center}\includegraphics{Assignment5_RongGuang_files/figure-latex/unnamed-chunk-15-1} \end{center}



```r
fa.ee <- c("ITEM1", "ITEM3", "ITEM6", "ITEM8", "ITEM13", "ITEM14", "ITEM16", "ITEM20")
fa.dp <- c("ITEM5", "ITEM10", "ITEM11", "ITEM15", "ITEM22")
fa.pa <- c("ITEM4", "ITEM7", "ITEM9", "ITEM12", "ITEM17", "ITEM18", "ITEM19", "ITEM21")
#generate 6 plots, 3 factors X 2 subgroups of teachers
p.cor.elm.ee <- 
       mycor(
         data= mbi.elm, 
         cols = fa.ee, 
         "(a) Items on emotional exhaustion, 
         elementary school teacher"
         )
p.cor.sec.ee <- 
       mycor(
         data = mbi.sec, 
         cols = fa.ee, 
         "(b) Items on emotional exhaustion, 
          secondary school teacher"
         )
p.cor.elm.dp <- 
       mycor(
         data = mbi.elm, 
         cols = fa.dp, 
         "(c) Items on depersonalization,
          elementary school teacher"
         )
p.cor.sec.dp <- 
       mycor(
         data = mbi.sec, 
         cols = fa.dp, 
         "(d) Items on depersonalization,
          secondary school teacher"
         )
p.cor.elm.pa <- 
       mycor(
         data = mbi.elm, 
         cols = fa.pa, 
         "(e) Items on personal accomplishment,
         secondary school teacher"
         )
p.cor.sec.pa <- 
       mycor(
         data = mbi.sec ,
         cols = fa.pa, 
         "(f) Items on personal accomplishment,
          secondary school teacher"
         )
#plot sub-figure layout
patchwork <- 
  p.cor.elm.ee/p.cor.elm.dp/p.cor.elm.pa|p.cor.sec.ee/p.cor.sec.dp/p.cor.sec.pa 
#print the plot with a gernal title
patchwork+
  plot_annotation(
    title = 
      'Figure 3 Correlalogram for items on each factor for two groups of teachers',
    theme = 
      theme(plot.title = 
              element_text(
                size = 16,
                face = "bold",
                vjust = -1.5,
                hjust =0.5)
            )
    )
```



\begin{center}\includegraphics{Assignment5_RongGuang_files/figure-latex/unnamed-chunk-16-1} \end{center}

# Testing the factorial invariance of MBI inventory between elementary and secondary school teachers  

## Define and estimate initial models for both subgroups

\textcolor{blue}{The postulated three-factor structure of the MBI that was tested in the previous assignments were re-tested as the initial model for establishing a baseline model. }

### Define the initial model 


```r
library(lavaan)
# Define a CFA model using the lavaan (Latent Variable Analysis) syntax:
# see https://lavaan.ugent.be/tutorial/syntax1.html
initial.model <- '
# CFA model for the burnout, the baseline model:
    EE =~ ITEM1 + ITEM2 + ITEM3 + ITEM6 + ITEM8 + 
          ITEM13 + ITEM14 + ITEM16 + ITEM20
    DP =~ ITEM5 + ITEM10 + ITEM11 + ITEM15 +ITEM22
    PA =~ ITEM4 + ITEM7 + ITEM9 + ITEM12 + 
          ITEM17 + ITEM18 + ITEM19 + ITEM21
          '
```

Cited from Byrne: *It is important to note that measuring instruments are often group specific in the way they operate, and, thus, it is possible that baseline models may not be completely identical across groups.*

### Estimate indices to examine factorial validity 

(1) Estimate factorial validity for the elementary teacher subgroup


```r
cfa.elm <- 
  cfa(
    initial.model, 
    data = mbi.elm,  
    estimator = "MLM",
    mimic = "Mplus"
    )
```

(2) Estimate factorial validity for the secondary teacher subgroup


```r
cfa.sec <- 
  cfa(
    initial.model, 
    data = mbi.sec,  
    estimator = "MLM",
    mimic = "Mplus"
    )
```

### Evaluate model

(1) Fit indices


```r
library(knitr);library(kableExtra)
#combine fit indices of both levels
initial.elm.fit <- 
  cfa.summary.mlm.a(cfa.elm) |> 
  t() |> 
  as.data.frame()

initial.sec.fit <- 
  cfa.summary.mlm.a(cfa.sec) |> 
  t() |> 
  as.data.frame()

initial.both <- 
  rbind(
    initial.elm.fit[2,], 
    initial.sec.fit[2,]
    ) 

names(initial.both) <- 
  initial.elm.fit[1,]

rownames(initial.both) <- NULL

initial.both <- 
  initial.both |> 
  mutate(Model = c("Elementary level",
    "Secondary level")) |> 
  select(Model, everything())

#print the table
multi.fit.tab(initial.both, "Fit indices for two subgroups, basline models")
```

\begin{table}

\caption{(\#tab:unnamed-chunk-20)Fit indices for two subgroups, basline models}
\centering
\begin{tabu} to \linewidth {>{\raggedright\arraybackslash}p{3.5cm}>{\raggedleft\arraybackslash}p{4cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{2.5cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{1cm}}
\toprule
Model & Chi square (df, p) & CFI & TLI & RMSEA(p) & SRMR & CSF*\\
\midrule
Elementary level & 826.573(206, <0.001) & 0.857 & 0.840 & 0.072(<0.001) & 0.068 & 1.225\\
Secondary level & 999.359(206, <0.001) & 0.836 & 0.816 & 0.075(<0.001) & 0.077 & 1.284\\
\bottomrule
\multicolumn{7}{l}{\rule{0pt}{1em}\textsuperscript{*} Chi square scaling factor}\\
\end{tabu}
\end{table}

\textcolor{blue}{See table 1. Goodness-of-fit statistics for this baseline model (three factor) reveals that the indices are less than optimal for both elementary (MLM Chi-square[206] = 826.573; CFI = 0.857; RMSEA = 0.072 ; SRMR = 0.068) and secondary (MLM Chi-square[206] = 999.359; CFI = 0.836; RMSEA = 0.075; SRMR = 0.077) levels.}

(2) factor loading

Factor loading of elementary level were extracted.


```r
fl.elm <- cfa.summary.b (cfa.elm) #fl  is for factor loading)
colnames(fl.elm)[2] <- "Beta*"
```

Factor loading of secondary level were extracted.


```r
fl.sec <- cfa.summary.b (cfa.sec) #fl is for factor loading
colnames(fl.sec) <- c("Parameter",
                     "Beta* ",
                     "SE ",
                     "Z ",
                     "p-value ")
```

Factor loading of both levels were merged in one table and printed.


```r
fl.both <- left_join(fl.elm, 
                     fl.sec, 
                     by = "Parameter")
fl.both |> 
  kable(
    digits = 3,
    booktabs = T,
    #format = "markdown",
    caption = "Factor loadings for both levels",
    linesep = ""
    ) |>  
  add_header_above(c(" " = 1, 
                     "Elementary level" = 4,
                     "Secondary level" = 4
                     )
                   ) |> 
  kable_styling() |> 
  row_spec(1:9, 
           background = "#E5E4E2"
           ) |> 
  row_spec(15:22, 
           background = "#E5E4E2"
           ) |> 
  row_spec(c(1,10,15), bold = T) |> 
  footnote(general = 
             "Rows with coeffcient estimates fixed to 1 are highligted in bold ",
           symbol = c(
             "Standardized estimates"
                      )
           )
```

\begin{table}

\caption{(\#tab:unnamed-chunk-23)Factor loadings for both levels}
\centering
\begin{tabular}[t]{lrrrlrrrl}
\toprule
\multicolumn{1}{c}{ } & \multicolumn{4}{c}{Elementary level} & \multicolumn{4}{c}{Secondary level} \\
\cmidrule(l{3pt}r{3pt}){2-5} \cmidrule(l{3pt}r{3pt}){6-9}
Parameter & Beta* & SE & Z & p-value & Beta*  & SE  & Z  & p-value \\
\midrule
\textbf{\cellcolor[HTML]{E5E4E2}{EE→ITEM1}} & \textbf{\cellcolor[HTML]{E5E4E2}{0.776}} & \textbf{\cellcolor[HTML]{E5E4E2}{0.000}} & \textbf{\cellcolor[HTML]{E5E4E2}{NA}} & \textbf{\cellcolor[HTML]{E5E4E2}{NA}} & \textbf{\cellcolor[HTML]{E5E4E2}{0.756}} & \textbf{\cellcolor[HTML]{E5E4E2}{0.000}} & \textbf{\cellcolor[HTML]{E5E4E2}{NA}} & \textbf{\cellcolor[HTML]{E5E4E2}{NA}}\\
\cellcolor[HTML]{E5E4E2}{EE→ITEM2} & \cellcolor[HTML]{E5E4E2}{0.754} & \cellcolor[HTML]{E5E4E2}{0.032} & \cellcolor[HTML]{E5E4E2}{28.561} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.736} & \cellcolor[HTML]{E5E4E2}{0.031} & \cellcolor[HTML]{E5E4E2}{30.236} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{EE→ITEM3} & \cellcolor[HTML]{E5E4E2}{0.740} & \cellcolor[HTML]{E5E4E2}{0.045} & \cellcolor[HTML]{E5E4E2}{21.984} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.722} & \cellcolor[HTML]{E5E4E2}{0.043} & \cellcolor[HTML]{E5E4E2}{24.030} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{EE→ITEM6} & \cellcolor[HTML]{E5E4E2}{0.631} & \cellcolor[HTML]{E5E4E2}{0.051} & \cellcolor[HTML]{E5E4E2}{16.064} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.626} & \cellcolor[HTML]{E5E4E2}{0.046} & \cellcolor[HTML]{E5E4E2}{18.669} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{EE→ITEM8} & \cellcolor[HTML]{E5E4E2}{0.855} & \cellcolor[HTML]{E5E4E2}{0.042} & \cellcolor[HTML]{E5E4E2}{28.448} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.833} & \cellcolor[HTML]{E5E4E2}{0.046} & \cellcolor[HTML]{E5E4E2}{25.968} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{EE→ITEM13} & \cellcolor[HTML]{E5E4E2}{0.754} & \cellcolor[HTML]{E5E4E2}{0.045} & \cellcolor[HTML]{E5E4E2}{22.474} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.762} & \cellcolor[HTML]{E5E4E2}{0.045} & \cellcolor[HTML]{E5E4E2}{23.619} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{EE→ITEM14} & \cellcolor[HTML]{E5E4E2}{0.655} & \cellcolor[HTML]{E5E4E2}{0.046} & \cellcolor[HTML]{E5E4E2}{19.939} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.634} & \cellcolor[HTML]{E5E4E2}{0.045} & \cellcolor[HTML]{E5E4E2}{20.685} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{EE→ITEM16} & \cellcolor[HTML]{E5E4E2}{0.640} & \cellcolor[HTML]{E5E4E2}{0.047} & \cellcolor[HTML]{E5E4E2}{15.992} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.596} & \cellcolor[HTML]{E5E4E2}{0.047} & \cellcolor[HTML]{E5E4E2}{15.261} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{EE→ITEM20} & \cellcolor[HTML]{E5E4E2}{0.734} & \cellcolor[HTML]{E5E4E2}{0.045} & \cellcolor[HTML]{E5E4E2}{18.371} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.707} & \cellcolor[HTML]{E5E4E2}{0.048} & \cellcolor[HTML]{E5E4E2}{17.421} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\textbf{DP→ITEM5} & \textbf{0.576} & \textbf{0.000} & \textbf{NA} & \textbf{NA} & \textbf{0.453} & \textbf{0.000} & \textbf{NA} & \textbf{NA}\\
DP→ITEM10 & 0.794 & 0.115 & 11.968 & <0.001 & 0.820 & 0.188 & 10.259 & <0.001\\
DP→ITEM11 & 0.793 & 0.122 & 11.588 & <0.001 & 0.808 & 0.197 & 9.666 & <0.001\\
DP→ITEM15 & 0.505 & 0.072 & 9.287 & <0.001 & 0.472 & 0.098 & 10.295 & <0.001\\
DP→ITEM22 & 0.351 & 0.091 & 6.997 & <0.001 & 0.447 & 0.131 & 8.226 & <0.001\\
\textbf{\cellcolor[HTML]{E5E4E2}{PA→ITEM4}} & \textbf{\cellcolor[HTML]{E5E4E2}{0.447}} & \textbf{\cellcolor[HTML]{E5E4E2}{0.000}} & \textbf{\cellcolor[HTML]{E5E4E2}{NA}} & \textbf{\cellcolor[HTML]{E5E4E2}{NA}} & \textbf{\cellcolor[HTML]{E5E4E2}{0.340}} & \textbf{\cellcolor[HTML]{E5E4E2}{0.000}} & \textbf{\cellcolor[HTML]{E5E4E2}{NA}} & \textbf{\cellcolor[HTML]{E5E4E2}{NA}}\\
\cellcolor[HTML]{E5E4E2}{PA→ITEM7} & \cellcolor[HTML]{E5E4E2}{0.516} & \cellcolor[HTML]{E5E4E2}{0.148} & \cellcolor[HTML]{E5E4E2}{7.308} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.545} & \cellcolor[HTML]{E5E4E2}{0.221} & \cellcolor[HTML]{E5E4E2}{7.495} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{PA→ITEM9} & \cellcolor[HTML]{E5E4E2}{0.581} & \cellcolor[HTML]{E5E4E2}{0.280} & \cellcolor[HTML]{E5E4E2}{6.629} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.681} & \cellcolor[HTML]{E5E4E2}{0.365} & \cellcolor[HTML]{E5E4E2}{7.432} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{PA→ITEM12} & \cellcolor[HTML]{E5E4E2}{0.611} & \cellcolor[HTML]{E5E4E2}{0.303} & \cellcolor[HTML]{E5E4E2}{6.214} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.586} & \cellcolor[HTML]{E5E4E2}{0.283} & \cellcolor[HTML]{E5E4E2}{7.398} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{PA→ITEM17} & \cellcolor[HTML]{E5E4E2}{0.681} & \cellcolor[HTML]{E5E4E2}{0.185} & \cellcolor[HTML]{E5E4E2}{7.796} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.546} & \cellcolor[HTML]{E5E4E2}{0.187} & \cellcolor[HTML]{E5E4E2}{7.486} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{PA→ITEM18} & \cellcolor[HTML]{E5E4E2}{0.628} & \cellcolor[HTML]{E5E4E2}{0.276} & \cellcolor[HTML]{E5E4E2}{6.628} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.698} & \cellcolor[HTML]{E5E4E2}{0.294} & \cellcolor[HTML]{E5E4E2}{7.431} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{PA→ITEM19} & \cellcolor[HTML]{E5E4E2}{0.643} & \cellcolor[HTML]{E5E4E2}{0.255} & \cellcolor[HTML]{E5E4E2}{6.844} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.706} & \cellcolor[HTML]{E5E4E2}{0.324} & \cellcolor[HTML]{E5E4E2}{7.565} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\cellcolor[HTML]{E5E4E2}{PA→ITEM21} & \cellcolor[HTML]{E5E4E2}{0.425} & \cellcolor[HTML]{E5E4E2}{0.187} & \cellcolor[HTML]{E5E4E2}{7.018} & \cellcolor[HTML]{E5E4E2}{<0.001} & \cellcolor[HTML]{E5E4E2}{0.410} & \cellcolor[HTML]{E5E4E2}{0.242} & \cellcolor[HTML]{E5E4E2}{6.808} & \cellcolor[HTML]{E5E4E2}{<0.001}\\
\bottomrule
\multicolumn{9}{l}{\rule{0pt}{1em}\textit{Note: }}\\
\multicolumn{9}{l}{\rule{0pt}{1em}Rows with coeffcient estimates fixed to 1 are highligted in bold }\\
\multicolumn{9}{l}{\rule{0pt}{1em}\textsuperscript{*} Standardized estimates}\\
\end{tabular}
\end{table}

the cross-loading involved the loading of Item 12 on Factor 1 (Emotional Exhaustion) in addition to its targeted Factor 3 (Personal Accomplishment)

(3) Variance 

Variance of elementary level were extracted.


```r
var.elm <- cfa.summary.c(cfa.elm, fa.num = 3, item.num = 22)
names(var.elm)[3] <- "Beta*"
names(var.elm)[4]<- "Beta†"
```

Variance of secondary level were extracted.


```r
var.sec <- cfa.summary.c(cfa.sec, fa.num = 3, item.num = 22)
var.sec <- var.sec[,-1]
names(var.sec) <- 
  c("Indicator", 
    "Beta* ", 
    "Beta† ",
    "SE ", 
    "Z ", 
    "p-value "
    )
```

Variance of both levels were merged in one table and printed.


```r
var.both <- left_join(var.elm, 
                     var.sec, 
                     by = "Indicator")

align.table(data = var.both, 
            num.no.header.col = 2, 
            title = "Residual variance for both levels")
```

\begin{table}

\caption{(\#tab:unnamed-chunk-26)Residual variance for both levels}
\centering
\begin{tabular}[t]{llrrrrlrrrrl}
\toprule
\multicolumn{2}{c}{ } & \multicolumn{5}{c}{Elementary level} & \multicolumn{5}{c}{Secondary level} \\
\cmidrule(l{3pt}r{3pt}){3-7} \cmidrule(l{3pt}r{3pt}){8-12}
Parameter & Indicator & Beta* & Beta† & SE & Z & p-value & Beta*  & Beta†  & SE  & Z  & p-value \\
\midrule
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM1} & \cellcolor{gray!6}{1.095} & \cellcolor{gray!6}{0.398} & \cellcolor{gray!6}{0.062} & \cellcolor{gray!6}{17.641} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{1.078} & \cellcolor{gray!6}{0.429} & \cellcolor{gray!6}{0.056} & \cellcolor{gray!6}{19.329} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM2 & 1.067 & 0.432 & 0.063 & 16.832 & <0.001 & 1.071 & 0.459 & 0.053 & 20.373 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM3} & \cellcolor{gray!6}{1.322} & \cellcolor{gray!6}{0.452} & \cellcolor{gray!6}{0.089} & \cellcolor{gray!6}{14.773} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{1.383} & \cellcolor{gray!6}{0.479} & \cellcolor{gray!6}{0.083} & \cellcolor{gray!6}{16.704} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM6 & 1.655 & 0.602 & 0.098 & 16.924 & <0.001 & 1.656 & 0.609 & 0.084 & 19.730 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM8} & \cellcolor{gray!6}{0.886} & \cellcolor{gray!6}{0.269} & \cellcolor{gray!6}{0.068} & \cellcolor{gray!6}{13.044} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{0.890} & \cellcolor{gray!6}{0.306} & \cellcolor{gray!6}{0.061} & \cellcolor{gray!6}{14.560} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM13 & 1.281 & 0.431 & 0.087 & 14.663 & <0.001 & 1.167 & 0.419 & 0.075 & 15.574 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM14} & \cellcolor{gray!6}{1.897} & \cellcolor{gray!6}{0.571} & \cellcolor{gray!6}{0.113} & \cellcolor{gray!6}{16.728} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{1.883} & \cellcolor{gray!6}{0.599} & \cellcolor{gray!6}{0.110} & \cellcolor{gray!6}{17.084} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM16 & 1.363 & 0.591 & 0.066 & 20.746 & <0.001 & 1.353 & 0.645 & 0.071 & 19.024 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM20} & \cellcolor{gray!6}{0.954} & \cellcolor{gray!6}{0.461} & \cellcolor{gray!6}{0.093} & \cellcolor{gray!6}{10.210} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{0.983} & \cellcolor{gray!6}{0.500} & \cellcolor{gray!6}{0.057} & \cellcolor{gray!6}{17.125} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM5 & 1.459 & 0.669 & 0.119 & 12.289 & <0.001 & 1.711 & 0.795 & 0.100 & 17.052 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM10} & \cellcolor{gray!6}{0.806} & \cellcolor{gray!6}{0.370} & \cellcolor{gray!6}{0.094} & \cellcolor{gray!6}{8.530} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{0.803} & \cellcolor{gray!6}{0.328} & \cellcolor{gray!6}{0.090} & \cellcolor{gray!6}{8.944} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM11 & 0.848 & 0.372 & 0.101 & 8.404 & <0.001 & 0.854 & 0.347 & 0.095 & 9.013 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM15} & \cellcolor{gray!6}{0.934} & \cellcolor{gray!6}{0.745} & \cellcolor{gray!6}{0.119} & \cellcolor{gray!6}{7.870} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{1.562} & \cellcolor{gray!6}{0.778} & \cellcolor{gray!6}{0.112} & \cellcolor{gray!6}{13.964} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM22 & 2.086 & 0.877 & 0.143 & 14.538 & <0.001 & 2.052 & 0.800 & 0.124 & 16.598 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM4} & \cellcolor{gray!6}{0.696} & \cellcolor{gray!6}{0.800} & \cellcolor{gray!6}{0.066} & \cellcolor{gray!6}{10.568} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{1.074} & \cellcolor{gray!6}{0.884} & \cellcolor{gray!6}{0.104} & \cellcolor{gray!6}{10.372} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM7 & 0.562 & 0.734 & 0.058 & 9.605 & <0.001 & 0.907 & 0.703 & 0.064 & 14.108 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM9} & \cellcolor{gray!6}{1.176} & \cellcolor{gray!6}{0.662} & \cellcolor{gray!6}{0.115} & \cellcolor{gray!6}{10.247} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{1.194} & \cellcolor{gray!6}{0.536} & \cellcolor{gray!6}{0.097} & \cellcolor{gray!6}{12.297} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM12 & 1.039 & 0.627 & 0.079 & 13.108 & <0.001 & 1.177 & 0.657 & 0.076 & 15.418 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM17} & \cellcolor{gray!6}{0.418} & \cellcolor{gray!6}{0.536} & \cellcolor{gray!6}{0.048} & \cellcolor{gray!6}{8.653} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{0.649} & \cellcolor{gray!6}{0.701} & \cellcolor{gray!6}{0.063} & \cellcolor{gray!6}{10.319} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM18 & 0.894 & 0.606 & 0.109 & 8.170 & <0.001 & 0.703 & 0.512 & 0.068 & 10.329 & <0.001\\
\cellcolor{gray!6}{Residual} & \cellcolor{gray!6}{ITEM19} & \cellcolor{gray!6}{0.753} & \cellcolor{gray!6}{0.587} & \cellcolor{gray!6}{0.062} & \cellcolor{gray!6}{12.153} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{0.847} & \cellcolor{gray!6}{0.501} & \cellcolor{gray!6}{0.080} & \cellcolor{gray!6}{10.595} & \cellcolor{gray!6}{<0.001}\\
Residual & ITEM21 & 1.360 & 0.819 & 0.124 & 10.949 & <0.001 & 1.889 & 0.832 & 0.111 & 17.056 & <0.001\\
\cellcolor{gray!6}{Total} & \cellcolor{gray!6}{EE} & \cellcolor{gray!6}{1.657} & \cellcolor{gray!6}{1.000} & \cellcolor{gray!6}{0.114} & \cellcolor{gray!6}{14.585} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{1.436} & \cellcolor{gray!6}{1.000} & \cellcolor{gray!6}{0.097} & \cellcolor{gray!6}{14.854} & \cellcolor{gray!6}{<0.001}\\
Total & DP & 0.723 & 1.000 & 0.111 & 6.515 & <0.001 & 0.442 & 1.000 & 0.085 & 5.188 & <0.001\\
\cellcolor{gray!6}{Total} & \cellcolor{gray!6}{PA} & \cellcolor{gray!6}{0.174} & \cellcolor{gray!6}{1.000} & \cellcolor{gray!6}{0.046} & \cellcolor{gray!6}{3.814} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{0.141} & \cellcolor{gray!6}{1.000} & \cellcolor{gray!6}{0.034} & \cellcolor{gray!6}{4.108} & \cellcolor{gray!6}{<0.001}\\
\bottomrule
\multicolumn{12}{l}{\rule{0pt}{1em}\textsuperscript{*} Un-standardized estimates}\\
\multicolumn{12}{l}{\rule{0pt}{1em}\textsuperscript{\dag} Standardized estimates}\\
\end{tabular}
\end{table}

(3) Co-variance

Co-variance of elementary level were extracted.


```r
cov.elm <- cfa.summary.d(cfa.elm, fa.num = 3, item.num = 22)
colnames(cov.elm)[2:3] <- c("Beta*", "Beta†")
```

Co-variance of secondary level were extracted.


```r
cov.sec <- cfa.summary.d(cfa.sec, fa.num = 3, item.num = 22)
colnames(cov.sec) <- c("Parameter", "Beta* ", "Beta† ", "SE ", "Z ", "p-value ")
```

Co-variance of both levels were merged in one table and printed.


```r
cov.both <- left_join(cov.elm, 
                     cov.sec, 
                     by = "Parameter")

align.table(data = cov.both, 
            num.no.header.col = 1, 
            title = "Residual co-variance for both levels")
```

\begin{table}

\caption{(\#tab:unnamed-chunk-29)Residual co-variance for both levels}
\centering
\begin{tabular}[t]{lrrrrlrrrrl}
\toprule
\multicolumn{1}{c}{ } & \multicolumn{5}{c}{Elementary level} & \multicolumn{5}{c}{Secondary level} \\
\cmidrule(l{3pt}r{3pt}){2-6} \cmidrule(l{3pt}r{3pt}){7-11}
Parameter & Beta* & Beta† & SE & Z & p-value & Beta*  & Beta†  & SE  & Z  & p-value \\
\midrule
\cellcolor{gray!6}{EE ←→ DP} & \cellcolor{gray!6}{0.688} & \cellcolor{gray!6}{0.628} & \cellcolor{gray!6}{0.075} & \cellcolor{gray!6}{9.171} & \cellcolor{gray!6}{<0.001} & \cellcolor{gray!6}{0.451} & \cellcolor{gray!6}{0.566} & \cellcolor{gray!6}{0.057} & \cellcolor{gray!6}{7.928} & \cellcolor{gray!6}{<0.001}\\
EE ←→ PA & -0.254 & -0.473 & 0.037 & -6.952 & <0.001 & -0.177 & -0.393 & 0.029 & -6.193 & <0.001\\
\bottomrule
\multicolumn{11}{l}{\rule{0pt}{1em}\textsuperscript{*} Un-standardized estimates}\\
\multicolumn{11}{l}{\rule{0pt}{1em}\textsuperscript{\dag} Standardized estimates}\\
\end{tabular}
\end{table}

### Model re-specification

(1) Search for mis-specified parameters

\textcolor{blue}{To establish baseline models for both panels of teachers that represent good model fit and parsimony, I further investigated the modification indices of the hypothesized models, respectively for two levels. }

MIs of elementary level panel were calculated.


```r
#extract needed variables
initial.MI.elm <- 
  modindices(cfa.elm,
             standardized = TRUE,
             sort. = TRUE,
             maximum.number = 10)
```

MIs of secondary level panel were calculated.


```r
#extract needed variables
initial.MI.sec <- 
  modindices(cfa.sec,
             standardized = TRUE,
             sort. = TRUE,
             maximum.number = 10)
```

MI tables with 10 largest MI parameters was printed in descending order of MI. Potential mis-specification of most concerns were highlighted in red.


```r
MI.both <- rbind(initial.MI.elm, initial.MI.sec)

MI.both    |> 
  mutate(
    op = case_when(op == "~~"~"←→",
                   op == "=~"~"→"), 
    Parameter = 
           paste(lhs, op, rhs)
         ) |>
  select(Parameter, 
         MI = mi, 
         EPC = epc, 
         "std EPC" = sepc.all
         )|>
  kable(digits = 3,
        booktab = T,
        linesep = "",
        caption = 
          "Selected modification indices for determining baseline model") |>
  kable_styling(
    latex_options = "striped"
    ) |>
  row_spec(
    c(1:4, 11:14), 
    color = "red"
    ) |> 
  footnote(general = 
             "Rows highlighted in red are of special concerns") |> 
  pack_rows(index = c(
    "Elementary level" = 10,
    "Secondary level" = 10
    )
    )
```

\begin{table}

\caption{(\#tab:unnamed-chunk-32)Selected modification indices for determining baseline model}
\centering
\begin{tabular}[t]{llrrr}
\toprule
  & Parameter & MI & EPC & std EPC\\
\midrule
\addlinespace[0.3em]
\multicolumn{5}{l}{\textbf{Elementary level}}\\
\hspace{1em}\textcolor{red}{\cellcolor{gray!6}{183}} & \textcolor{red}{\cellcolor{gray!6}{ITEM6 ←→ ITEM16}} & \textcolor{red}{\cellcolor{gray!6}{180.298}} & \textcolor{red}{\cellcolor{gray!6}{0.893}} & \textcolor{red}{\cellcolor{gray!6}{0.595}}\\
\hspace{1em}\textcolor{red}{120} & \textcolor{red}{ITEM1 ←→ ITEM2} & \textcolor{red}{103.177} & \textcolor{red}{0.534} & \textcolor{red}{0.494}\\
\hspace{1em}\textcolor{red}{\cellcolor{gray!6}{84}} & \textcolor{red}{\cellcolor{gray!6}{EE → ITEM12}} & \textcolor{red}{\cellcolor{gray!6}{81.319}} & \textcolor{red}{\cellcolor{gray!6}{-0.400}} & \textcolor{red}{\cellcolor{gray!6}{-0.400}}\\
\hspace{1em}\textcolor{red}{285} & \textcolor{red}{ITEM10 ←→ ITEM11} & \textcolor{red}{67.743} & \textcolor{red}{0.688} & \textcolor{red}{0.832}\\
\hspace{1em}\cellcolor{gray!6}{348} & \cellcolor{gray!6}{ITEM18 ←→ ITEM19} & \cellcolor{gray!6}{43.669} & \cellcolor{gray!6}{0.279} & \cellcolor{gray!6}{0.340}\\
\hspace{1em}323 & ITEM4 ←→ ITEM7 & 42.833 & 0.184 & 0.294\\
\hspace{1em}\cellcolor{gray!6}{175} & \cellcolor{gray!6}{ITEM3 ←→ ITEM12} & \cellcolor{gray!6}{28.187} & \cellcolor{gray!6}{-0.287} & \cellcolor{gray!6}{-0.245}\\
\hspace{1em}275 & ITEM5 ←→ ITEM15 & 25.815 & 0.273 & 0.234\\
\hspace{1em}\cellcolor{gray!6}{96} & \cellcolor{gray!6}{DP → ITEM16} & \cellcolor{gray!6}{25.652} & \cellcolor{gray!6}{0.459} & \cellcolor{gray!6}{0.257}\\
\hspace{1em}185 & ITEM6 ←→ ITEM5 & 23.753 & 0.337 & 0.217\\
\addlinespace[0.3em]
\multicolumn{5}{l}{\textbf{Secondary level}}\\
\hspace{1em}\textcolor{red}{\cellcolor{gray!6}{1201}} & \textcolor{red}{\cellcolor{gray!6}{ITEM1 ←→ ITEM2}} & \textcolor{red}{\cellcolor{gray!6}{171.647}} & \textcolor{red}{\cellcolor{gray!6}{0.627}} & \textcolor{red}{\cellcolor{gray!6}{0.583}}\\
\hspace{1em}\textcolor{red}{2851} & \textcolor{red}{ITEM10 ←→ ITEM11} & \textcolor{red}{135.841} & \textcolor{red}{1.181} & \textcolor{red}{1.426}\\
\hspace{1em}\textcolor{red}{\cellcolor{gray!6}{1831}} & \textcolor{red}{\cellcolor{gray!6}{ITEM6 ←→ ITEM16}} & \textcolor{red}{\cellcolor{gray!6}{127.756}} & \textcolor{red}{\cellcolor{gray!6}{0.686}} & \textcolor{red}{\cellcolor{gray!6}{0.458}}\\
\hspace{1em}\textcolor{red}{841} & \textcolor{red}{EE → ITEM12} & \textcolor{red}{118.156} & \textcolor{red}{-0.468} & \textcolor{red}{-0.419}\\
\hspace{1em}\cellcolor{gray!6}{2751} & \cellcolor{gray!6}{ITEM5 ←→ ITEM15} & \cellcolor{gray!6}{77.216} & \cellcolor{gray!6}{0.580} & \cellcolor{gray!6}{0.355}\\
\hspace{1em}296 & ITEM11 ←→ ITEM15 & 60.947 & -0.485 & -0.420\\
\hspace{1em}\cellcolor{gray!6}{147} & \cellcolor{gray!6}{ITEM2 ←→ ITEM20} & \cellcolor{gray!6}{53.024} & \cellcolor{gray!6}{-0.324} & \cellcolor{gray!6}{-0.316}\\
\hspace{1em}274 & ITEM5 ←→ ITEM11 & 48.297 & -0.446 & -0.369\\
\hspace{1em}\cellcolor{gray!6}{339} & \cellcolor{gray!6}{ITEM9 ←→ ITEM19} & \cellcolor{gray!6}{46.617} & \cellcolor{gray!6}{0.360} & \cellcolor{gray!6}{0.358}\\
\hspace{1em}77 & EE → ITEM10 & 45.623 & -0.394 & -0.302\\
\bottomrule
\multicolumn{5}{l}{\rule{0pt}{1em}\textit{Note: }}\\
\multicolumn{5}{l}{\rule{0pt}{1em}Rows highlighted in red are of special concerns}\\
\end{tabular}
\end{table}

\textcolor{blue}{See table 5. Three exceptionally large residual co-variances and one cross-loading contributed to the misfit of the model for both teacher panels. The residual co-variances involved Items 1 and 2, Items 6 and 16, and Items 10 and 11; the cross-loading involved the loading of Item 12 on Factor 1 (Emotional Exhaustion) in addition to its targeted Factor 3 (Personal Accomplishment).}

 In reviewing both the MIs and expected parameter change (EPC) statistics for elementary teachers (table 5, upper part), it is clear that all four parameters are contributing substantially to model misfit, with the residual covariance between Item 6 and Item 16 exhibiting the most profound effect.
 
 We see precisely the same pattern on secondary teachers, albeit the effect would appear to be even more pronounced than it was for elementary teachers. One slight difference between the two groups of teachers regards the impact of these four parameters on model misfit. Whereas the residual covariance between Items 6 and 16 was found to be the most seriously misfitting parameter for elementary teachers; for secondary teachers, the residual covariance between Items 1 and 2 was most pronounced. 

(2) Re-specified both models 

\textcolor{blue}{The good practice is relaxing one parameter each time. Nonetheless, according to the knowledge derived from our previous work, I included all four mis-specified parameters in a post-hoc model (common to the groups).}

First, the 4 parameters were relaxed in model statement.


```r
respecified4 <- 'EE =~ ITEM12
                 ITEM6 ~~ ITEM16
                 ITEM10 ~~ ITEM11
                 ITEM1 ~~ ITEM2
                 '
model2 <- paste(initial.model, respecified4)
```

Then, the model fit were re-estimated for both group, respectively


```r
#for elementary
cfa2.elm <- 
  cfa(
    model2, 
    data = mbi.elm,  
    estimator = "MLM",
    mimic = "Mplus"
    )
#for secondary
cfa2.sec <- 
  cfa(
    model2, 
    data = mbi.sec,  
    estimator = "MLM",
    mimic = "Mplus"
    )
```

### Examine Model 2 

(1) Inspect fit indices of model2 (comparing to initial model)


```r
#combine fit indices of both levels
model2.elm.fit <- 
  cfa.summary.mlm.a(
    cfa2.elm
    ) |> 
  t() |> 
  as.data.frame()

model2.sec.fit <- 
  cfa.summary.mlm.a(
    cfa2.sec
    ) |> 
  t() |> 
  as.data.frame()

model2.both <- 
  rbind(
    model2.elm.fit[2,], 
    model2.sec.fit[2,]
    ) 

names(model2.both) <- model2.elm.fit[1,]

rownames(model2.both) <- NULL

model2.both <- 
  model2.both |> 
  mutate(Model = c("Elementary level",
    "Secondary level")) |> 
  select(Model, everything())

#combine model 1 and 2 tables
compare12 <- rbind(initial.both, model2.both)

#print the table
multi.fit.tab(compare12, 
              "Fit indices for two subgroups, model 2, comparing to initial model") |> 
  pack_rows(index = c(
    "Initial model" = 2,
    "Model 2" = 2
  )
  )
```

\begin{table}

\caption{(\#tab:unnamed-chunk-35)Fit indices for two subgroups, model 2, comparing to initial model}
\centering
\begin{tabu} to \linewidth {>{\raggedright\arraybackslash}p{3.5cm}>{\raggedleft\arraybackslash}p{4cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{2.5cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{1cm}}
\toprule
Model & Chi square (df, p) & CFI & TLI & RMSEA(p) & SRMR & CSF*\\
\midrule
\addlinespace[0.3em]
\multicolumn{7}{l}{\textbf{Initial model}}\\
\hspace{1em}Elementary level & 826.573(206, <0.001) & 0.857 & 0.840 & 0.072(<0.001) & 0.068 & 1.225\\
\hspace{1em}Secondary level & 999.359(206, <0.001) & 0.836 & 0.816 & 0.075(<0.001) & 0.077 & 1.284\\
\addlinespace[0.3em]
\multicolumn{7}{l}{\textbf{Model 2}}\\
\hspace{1em}Elementary level & 477.667(202, <0.001) & 0.936 & 0.927 & 0.049(  0.679) & 0.050 & 1.224\\
\hspace{1em}Secondary level & 587.538(202, <0.001) & 0.920 & 0.909 & 0.053(  0.168) & 0.056 & 1.278\\
\bottomrule
\multicolumn{7}{l}{\rule{0pt}{1em}\textsuperscript{*} Chi square scaling factor}\\
\end{tabu}
\end{table}

\textcolor{blue}{Estimation of this re-specified model, for each teacher group, yielded greatly improved model fit statistics than initial model. See table 6. However, we should note that several statistics, albeit improved comparing to initial model, still fall below the preferable value. For example, CFI from both groups were <0.95.}

(2) Modification indices of model 2

\textcolor{blue}{To establish baseline models for both panels of teachers that represent good model fit and parsimony, I further investigated the modification indices of model 2, respectively for two groups, to decide if there was any more model mis-fit and mis-specification }

MIs of elementary level panel were calculated.


```r
#extract needed variables
model2.MI.elm <- 
  modindices(cfa2.elm,
             standardized = TRUE,
             sort. = TRUE,
             maximum.number = 10)
```

MIs of secondary level panel were calculated.


```r
#extract needed variables
model2.MI.sec <- 
  modindices(cfa2.sec,
             standardized = TRUE,
             sort. = TRUE,
             maximum.number = 10)
```

MI tables with 10 largest MI parameters was printed in descending order of MI. Potential mis-specification of most concerns were highlighted in red.


```r
MI2.both <- rbind(model2.MI.elm, model2.MI.sec)

MI2.both    |> 
  mutate(
    op = case_when(op == "~~"~"←→",
                   op == "=~"~"→"), 
    Parameter = 
           paste(lhs, op, rhs)
         ) |>
  select(Parameter, 
         MI = mi, 
         EPC = epc, 
         "std EPC" = sepc.all
         )|>
  kable(digits = 3,
        booktab = T,
        linesep = "",
        caption = 
          "Selected modification indices for determining baseline model") |>
  kable_styling(
    latex_options = "striped"
    ) |>
  row_spec(
    c(1:2, 11:12), 
    color = "red"
    ) |> 
  footnote(general = 
             "Rows highlighted in red are of special concerns") |> 
  pack_rows(index = c(
    "Elementary level" = 10,
    "Secondary level" = 10
    )
    )
```

\begin{table}

\caption{(\#tab:unnamed-chunk-38)Selected modification indices for determining baseline model}
\centering
\begin{tabular}[t]{llrrr}
\toprule
  & Parameter & MI & EPC & std EPC\\
\midrule
\addlinespace[0.3em]
\multicolumn{5}{l}{\textbf{Elementary level}}\\
\hspace{1em}\textcolor{red}{\cellcolor{gray!6}{323}} & \textcolor{red}{\cellcolor{gray!6}{ITEM4 ←→ ITEM7}} & \textcolor{red}{\cellcolor{gray!6}{38.931}} & \textcolor{red}{\cellcolor{gray!6}{0.174}} & \textcolor{red}{\cellcolor{gray!6}{0.284}}\\
\hspace{1em}\textcolor{red}{348} & \textcolor{red}{ITEM18 ←→ ITEM19} & \textcolor{red}{38.744} & \textcolor{red}{0.266} & \textcolor{red}{0.333}\\
\hspace{1em}\cellcolor{gray!6}{115} & \cellcolor{gray!6}{PA → ITEM14} & \cellcolor{gray!6}{24.435} & \cellcolor{gray!6}{0.864} & \cellcolor{gray!6}{0.205}\\
\hspace{1em}177 & ITEM3 ←→ ITEM12 & 23.978 & -0.250 & -0.227\\
\hspace{1em}\cellcolor{gray!6}{227} & \cellcolor{gray!6}{ITEM13 ←→ ITEM12} & \cellcolor{gray!6}{20.493} & \cellcolor{gray!6}{0.231} & \cellcolor{gray!6}{0.211}\\
\hspace{1em}147 & ITEM2 ←→ ITEM14 & 16.441 & 0.245 & 0.163\\
\hspace{1em}\cellcolor{gray!6}{99} & \cellcolor{gray!6}{DP → ITEM16} & \cellcolor{gray!6}{15.733} & \cellcolor{gray!6}{0.310} & \cellcolor{gray!6}{0.197}\\
\hspace{1em}216 & ITEM13 ←→ ITEM14 & 14.838 & 0.281 & 0.180\\
\hspace{1em}\cellcolor{gray!6}{82} & \cellcolor{gray!6}{EE → ITEM11} & \cellcolor{gray!6}{14.750} & \cellcolor{gray!6}{0.250} & \cellcolor{gray!6}{0.206}\\
\hspace{1em}105 & DP → ITEM17 & 12.788 & -0.173 & -0.188\\
\addlinespace[0.3em]
\multicolumn{5}{l}{\textbf{Secondary level}}\\
\hspace{1em}\textcolor{red}{\cellcolor{gray!6}{821}} & \textcolor{red}{\cellcolor{gray!6}{EE → ITEM11}} & \textcolor{red}{\cellcolor{gray!6}{67.177}} & \textcolor{red}{\cellcolor{gray!6}{0.472}} & \textcolor{red}{\cellcolor{gray!6}{0.339}}\\
\hspace{1em}\textcolor{red}{339} & \textcolor{red}{ITEM9 ←→ ITEM19} & \textcolor{red}{43.690} & \textcolor{red}{0.355} & \textcolor{red}{0.357}\\
\hspace{1em}\cellcolor{gray!6}{276} & \cellcolor{gray!6}{ITEM5 ←→ ITEM15} & \cellcolor{gray!6}{35.576} & \cellcolor{gray!6}{0.416} & \cellcolor{gray!6}{0.310}\\
\hspace{1em}296 & ITEM11 ←→ ITEM15 & 29.016 & -0.297 & -0.206\\
\hspace{1em}\cellcolor{gray!6}{247} & \cellcolor{gray!6}{ITEM16 ←→ ITEM20} & \cellcolor{gray!6}{28.900} & \cellcolor{gray!6}{0.227} & \cellcolor{gray!6}{0.201}\\
\hspace{1em}98 & DP → ITEM14 & 22.145 & -0.490 & -0.239\\
\hspace{1em}\cellcolor{gray!6}{345} & \cellcolor{gray!6}{ITEM17 ←→ ITEM18} & \cellcolor{gray!6}{21.583} & \cellcolor{gray!6}{0.147} & \cellcolor{gray!6}{0.219}\\
\hspace{1em}335 & ITEM7 ←→ ITEM21 & 21.370 & 0.247 & 0.191\\
\hspace{1em}\cellcolor{gray!6}{346} & \cellcolor{gray!6}{ITEM17 ←→ ITEM19} & \cellcolor{gray!6}{20.742} & \cellcolor{gray!6}{-0.159} & \cellcolor{gray!6}{-0.217}\\
\hspace{1em}149 & ITEM2 ←→ ITEM20 & 20.020 & -0.171 & -0.162\\
\bottomrule
\multicolumn{5}{l}{\rule{0pt}{1em}\textit{Note: }}\\
\multicolumn{5}{l}{\rule{0pt}{1em}Rows highlighted in red are of special concerns}\\
\end{tabular}
\end{table}

\textcolor{blue}{See table 7. In reviewing this information for elementary teachers, we observe two MIs larger than all other MIs (ITEM7 with ITEM4; ITEM19 with ITEM18); both represent residual co-variances. I followed Byrne's step in addressing these parameters.} According to Byrne, of the two, only the residual covariance between Items 7 and 4 is substantively viable in that there is a clear overlapping of item content. In contrast, the content of Items 19 and 18 exhibits no such redundancy, and, thus, there is no reasonable justification for including this parameter in a succeeding Model 3.

\textcolor{blue}{However, in checking the MI for secondary teachers, the decision was made: more work is needed in establishing an appropriate baseline model.} Two parameters were of special concern due to their large MI and substantive meaningfulness. They are Item 11 cross-loads onto factor EE, and item 19 co-varies with item 9. This time I operated by the good practice of specifying one parameter each time. Given the substantially large MI representing the cross-loading of Item 11 on factor EE, this parameter alone was included in our next post-hoc model (Model 3 for secondary teachers).

Byrne noted the reasons for making this decision (to further re-specifying model secondary teachers), which I quoted here for future reflection: (a) The model does not yet reflect a satisfactorily good fit to the data (CFI = 0.920); and (b) in reviewing the MIs in Table 7.2, we observe one very large mis-specified parameter representing the loading of Item 11 on Factor 1 (F1 by ITEM11), as well as another substantially large MI representing a residual covariance between Items 19 and 9, both of which can be substantiated as substantively meaningful parameters.

(3) Model re-specification for model 3 


```r
respecified3 <- 'EE =~ ITEM11
                 '
model3 <- paste(model2, respecified3)
```

Then, the model fit were re-estimated for both group, separately.


```r
#for elementary
cfa3.elm <- 
  cfa(
    model3, 
    data = mbi.elm,  
    estimator = "MLM",
    mimic = "Mplus"
    )
#for secondary
cfa3.sec <- 
  cfa(
    model3, 
    data = mbi.sec,  
    estimator = "MLM",
    mimic = "Mplus"
    )
```

### Examine Model 3

(1) Inspect fit indices of model3 (comparing to model 2)


```r
#combine fit indices of both levels
model3.elm.fit <- 
  cfa.summary.mlm.a(
    cfa3.elm
    ) |> 
  t() |> 
  as.data.frame()

model3.sec.fit <- 
  cfa.summary.mlm.a(
    cfa3.sec
    ) |> 
  t() |> 
  as.data.frame()

model3.both <- 
  rbind(
    model3.elm.fit[2,], 
    model3.sec.fit[2,]
    ) 

names(model3.both) <- model3.elm.fit[1,]

rownames(model3.both) <- NULL

model3.both <- 
  model3.both |> 
  mutate(Model = c("Elementary level",
    "Secondary level")) |> 
  select(Model, everything())

#combine model 1 and 2 tables
compare123 <- rbind(initial.both, model2.both, model3.both)

#print the table
multi.fit.tab(compare123, 
              "Fit indices for two subgroups, model 3, comparing to preceding models") |> 
  pack_rows(index = c(
    "Initial model" = 2,
    "Model 2" = 2,
    "Model 3" =2
  )
  )
```

\begin{table}

\caption{(\#tab:unnamed-chunk-41)Fit indices for two subgroups, model 3, comparing to preceding models}
\centering
\begin{tabu} to \linewidth {>{\raggedright\arraybackslash}p{3.5cm}>{\raggedleft\arraybackslash}p{4cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{2.5cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{1cm}}
\toprule
Model & Chi square (df, p) & CFI & TLI & RMSEA(p) & SRMR & CSF*\\
\midrule
\addlinespace[0.3em]
\multicolumn{7}{l}{\textbf{Initial model}}\\
\hspace{1em}Elementary level & 826.573(206, <0.001) & 0.857 & 0.840 & 0.072(<0.001) & 0.068 & 1.225\\
\hspace{1em}Secondary level & 999.359(206, <0.001) & 0.836 & 0.816 & 0.075(<0.001) & 0.077 & 1.284\\
\addlinespace[0.3em]
\multicolumn{7}{l}{\textbf{Model 2}}\\
\hspace{1em}Elementary level & 477.667(202, <0.001) & 0.936 & 0.927 & 0.049(  0.679) & 0.050 & 1.224\\
\hspace{1em}Secondary level & 587.538(202, <0.001) & 0.920 & 0.909 & 0.053(  0.168) & 0.056 & 1.278\\
\addlinespace[0.3em]
\multicolumn{7}{l}{\textbf{Model 3}}\\
\hspace{1em}Elementary level & 466.722(201, <0.001) & 0.939 & 0.930 & 0.048(  0.761) & 0.050 & 1.223\\
\hspace{1em}Secondary level & 535.759(201, <0.001) & 0.931 & 0.920 & 0.049(  0.629) & 0.053 & 1.275\\
\bottomrule
\multicolumn{7}{l}{\rule{0pt}{1em}\textsuperscript{*} Chi square scaling factor}\\
\end{tabu}
\end{table}

See table 8. Results from the estimation of Model 3 for elementary teachers yielded goodness-of-fit statistics that represented a satisfactorily good fit to the data (MLM chi square[201] = 466.722; CFI = 0.939; RMSEA = 0.048; SRMR = 0.050). Although a review of Table 9 (find below) reveals several additional moderately large MIs, for balancing goodness-of-fit and parsimony, \textcolor{blue}{the decision was model 3 can serve as the baseline model for elementary teachers.}

Results from the estimation of Model 3 for secondary teachers, on the other hand, further substantiated the residual covariance between Items 19 and 9 as representing an acutely mis-specified parameter in the model. \textcolor{blue}{Thus, for secondary teachers only, model 4 was put to the test with this residual covariance specified as a freely estimated parameter.}

(2) Modification indices of model 3

MIs of model 3 for each groups were calculated.


```r
#elementary
model3.MI.elm <- 
  modindices(cfa3.elm,
             standardized = TRUE,
             sort. = TRUE,
             maximum.number = 10)
#secondary
model3.MI.sec <- 
  modindices(cfa3.sec,
             standardized = TRUE,
             sort. = TRUE,
             maximum.number = 10)
```


MI tables with 10 largest MI parameters was printed in descending order of MI. Potential mis-specification of most concerns were highlighted in red.


```r
MI3.both <- rbind(model3.MI.elm, model3.MI.sec)

MI3.both    |> 
  mutate(
    op = case_when(op == "~~"~"←→",
                   op == "=~"~"→"), 
    Parameter = 
           paste(lhs, op, rhs)
         ) |>
  select(Parameter, 
         MI = mi, 
         EPC = epc, 
         "std EPC" = sepc.all
         )|>
  kable(digits = 3,
        booktab = T,
        linesep = "",
        caption = 
          "Selected modification indices for determining baseline model") |>
  kable_styling(
    latex_options = "striped"
    ) |>
  row_spec(
    c(1:2, 11), 
    color = "red"
    ) |> 
  footnote(general = 
             "Rows highlighted in red are of special concerns") |> 
  pack_rows(index = c(
    "Elementary level" = 10,
    "Secondary level" = 10
    )
    )
```

\begin{table}

\caption{(\#tab:unnamed-chunk-43)Selected modification indices for determining baseline model}
\centering
\begin{tabular}[t]{llrrr}
\toprule
  & Parameter & MI & EPC & std EPC\\
\midrule
\addlinespace[0.3em]
\multicolumn{5}{l}{\textbf{Elementary level}}\\
\hspace{1em}\textcolor{red}{\cellcolor{gray!6}{323}} & \textcolor{red}{\cellcolor{gray!6}{ITEM4 ←→ ITEM7}} & \textcolor{red}{\cellcolor{gray!6}{38.958}} & \textcolor{red}{\cellcolor{gray!6}{0.174}} & \textcolor{red}{\cellcolor{gray!6}{0.284}}\\
\hspace{1em}\textcolor{red}{348} & \textcolor{red}{ITEM18 ←→ ITEM19} & \textcolor{red}{38.557} & \textcolor{red}{0.265} & \textcolor{red}{0.333}\\
\hspace{1em}\cellcolor{gray!6}{115} & \cellcolor{gray!6}{PA → ITEM14} & \cellcolor{gray!6}{24.388} & \cellcolor{gray!6}{0.863} & \cellcolor{gray!6}{0.204}\\
\hspace{1em}177 & ITEM3 ←→ ITEM12 & 23.577 & -0.248 & -0.225\\
\hspace{1em}\cellcolor{gray!6}{227} & \cellcolor{gray!6}{ITEM13 ←→ ITEM12} & \cellcolor{gray!6}{20.753} & \cellcolor{gray!6}{0.232} & \cellcolor{gray!6}{0.212}\\
\hspace{1em}147 & ITEM2 ←→ ITEM14 & 16.397 & 0.244 & 0.162\\
\hspace{1em}\cellcolor{gray!6}{99} & \cellcolor{gray!6}{DP → ITEM16} & \cellcolor{gray!6}{16.061} & \cellcolor{gray!6}{0.289} & \cellcolor{gray!6}{0.189}\\
\hspace{1em}216 & ITEM13 ←→ ITEM14 & 14.721 & 0.280 & 0.179\\
\hspace{1em}\cellcolor{gray!6}{105} & \cellcolor{gray!6}{DP → ITEM17} & \cellcolor{gray!6}{13.316} & \cellcolor{gray!6}{-0.173} & \cellcolor{gray!6}{-0.194}\\
\hspace{1em}98 & DP → ITEM14 & 11.816 & -0.353 & -0.192\\
\addlinespace[0.3em]
\multicolumn{5}{l}{\textbf{Secondary level}}\\
\hspace{1em}\textcolor{red}{\cellcolor{gray!6}{339}} & \textcolor{red}{\cellcolor{gray!6}{ITEM9 ←→ ITEM19}} & \textcolor{red}{\cellcolor{gray!6}{42.687}} & \textcolor{red}{\cellcolor{gray!6}{0.351}} & \textcolor{red}{\cellcolor{gray!6}{0.355}}\\
\hspace{1em}247 & ITEM16 ←→ ITEM20 & 28.275 & 0.223 & 0.199\\
\hspace{1em}\cellcolor{gray!6}{345} & \cellcolor{gray!6}{ITEM17 ←→ ITEM18} & \cellcolor{gray!6}{21.951} & \cellcolor{gray!6}{0.148} & \cellcolor{gray!6}{0.221}\\
\hspace{1em}335 & ITEM7 ←→ ITEM21 & 21.602 & 0.248 & 0.192\\
\hspace{1em}\cellcolor{gray!6}{346} & \cellcolor{gray!6}{ITEM17 ←→ ITEM19} & \cellcolor{gray!6}{20.837} & \cellcolor{gray!6}{-0.160} & \cellcolor{gray!6}{-0.218}\\
\hspace{1em}84 & EE → ITEM22 & 20.306 & 0.321 & 0.225\\
\hspace{1em}\cellcolor{gray!6}{981} & \cellcolor{gray!6}{DP → ITEM14} & \cellcolor{gray!6}{20.142} & \cellcolor{gray!6}{-0.404} & \cellcolor{gray!6}{-0.210}\\
\hspace{1em}1471 & ITEM2 ←→ ITEM14 & 19.895 & 0.239 & 0.155\\
\hspace{1em}\cellcolor{gray!6}{149} & \cellcolor{gray!6}{ITEM2 ←→ ITEM20} & \cellcolor{gray!6}{18.463} & \cellcolor{gray!6}{-0.164} & \cellcolor{gray!6}{-0.155}\\
\hspace{1em}333 & ITEM7 ←→ ITEM18 & 18.163 & -0.159 & -0.202\\
\bottomrule
\multicolumn{5}{l}{\rule{0pt}{1em}\textit{Note: }}\\
\multicolumn{5}{l}{\rule{0pt}{1em}Rows highlighted in red are of special concerns}\\
\end{tabular}
\end{table}

(3) Re-specification for model 3 (only for secondary teacher)

The parameter ITEM9 ~~ ITEM19 was relaxed for estimation.


```r
respecified4 <- 'ITEM9 ~~ ITEM19
                 '
model4 <- paste(model3, respecified4)
```

Then, the model fit were re-estimated for secondary group, only


```r
cfa4.sec <- 
  cfa(
    model4, 
    data = mbi.sec,  
    estimator = "MLM",
    mimic = "Mplus"
    )
```

### Examine Model 4

\textcolor{blue}{Note that at this point I had taken model 3 as the baseline model for elementary teachers, and model 4 was to achieve the baseline model for secondary teachers.}

(1) Inspect fit indices of model4 (comparing to 3)


```r
model4.sec.fit <- 
  cfa.summary.mlm.a(
    cfa4.sec
    ) |> 
  t() |> 
  as.data.frame()

names(model4.sec.fit ) <- model4.sec.fit[1,]

model4.sec.fit <- model4.sec.fit [-1,]
model4.sec.fit  <- 
  model4.sec.fit  |> 
  mutate(Model = "Secondary level") |> 
  select(Model, everything())

rownames(model4.sec.fit ) <- NULL

#combine model 1 and 2 tables
model3.both[1,1] <- "Elementary level†"
model4.sec.fit[1,1] <- "Secondary level†"
compare1234 <- 
  rbind(initial.both,
        model2.both, 
        model3.both, 
        model4.sec.fit )

#print the table
multi.fit.tab(compare1234, 
              "Fit indices for two subgroups, model 4, comparing to preceding models",
              c("Baseline model for elementary teachers", 
                "Baseline model for secondary teachers")) |> 
  pack_rows(index = c(
    "Initial model" = 2,
    "Model 2" = 2,
    "Model 3" =2,
    "Model 4" =1
  )
  ) |> 
  row_spec(c(5,7), 
           color = "red"
           )
```

\begin{table}

\caption{(\#tab:unnamed-chunk-46)Fit indices for two subgroups, model 4, comparing to preceding models}
\centering
\begin{tabu} to \linewidth {>{\raggedright\arraybackslash}p{3.5cm}>{\raggedleft\arraybackslash}p{4cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{2.5cm}>{\raggedleft\arraybackslash}p{1cm}>{\raggedleft\arraybackslash}p{1cm}}
\toprule
Model & Chi square (df, p) & CFI & TLI & RMSEA(p) & SRMR & CSF*\\
\midrule
\addlinespace[0.3em]
\multicolumn{7}{l}{\textbf{Initial model}}\\
\hspace{1em}Elementary level & 826.573(206, <0.001) & 0.857 & 0.840 & 0.072(<0.001) & 0.068 & 1.225\\
\hspace{1em}Secondary level & 999.359(206, <0.001) & 0.836 & 0.816 & 0.075(<0.001) & 0.077 & 1.284\\
\addlinespace[0.3em]
\multicolumn{7}{l}{\textbf{Model 2}}\\
\hspace{1em}Elementary level & 477.667(202, <0.001) & 0.936 & 0.927 & 0.049(  0.679) & 0.050 & 1.224\\
\hspace{1em}Secondary level & 587.538(202, <0.001) & 0.920 & 0.909 & 0.053(  0.168) & 0.056 & 1.278\\
\addlinespace[0.3em]
\multicolumn{7}{l}{\textbf{Model 3}}\\
\textcolor{red}{\hspace{1em}Elementary level†} & \textcolor{red}{466.722(201, <0.001)} & \textcolor{red}{0.939} & \textcolor{red}{0.930} & \textcolor{red}{0.048(  0.761)} & \textcolor{red}{0.050} & \textcolor{red}{1.223}\\
\hspace{1em}Secondary level & 535.759(201, <0.001) & 0.931 & 0.920 & 0.049(  0.629) & 0.053 & 1.275\\
\addlinespace[0.3em]
\multicolumn{7}{l}{\textbf{Model 4}}\\
\textcolor{red}{\hspace{1em}Secondary level†} & \textcolor{red}{505.831(200, <0.001)} & \textcolor{red}{0.937} & \textcolor{red}{0.927} & \textcolor{red}{0.047(  0.859)} & \textcolor{red}{0.052} & \textcolor{red}{1.273}\\
\bottomrule
\multicolumn{7}{l}{\rule{0pt}{1em}\textsuperscript{*} Chi square scaling factor}\\
\multicolumn{7}{l}{\rule{0pt}{1em}\textsuperscript{\dag} Baseline model for elementary teachers}\\
\multicolumn{7}{l}{\rule{0pt}{1em}\textsuperscript{\ddag} Baseline model for secondary teachers}\\
\end{tabu}
\end{table}









```r
chisq_mlm <- function(fit_nested, fit_parent) {
    # scaling correction factors
      c0 <- fitMeasures(fit_nested, "chisq.scaling.factor") %>% as.numeric()
      c1 <- fitMeasures(fit_parent, "chisq.scaling.factor") %>% as.numeric()
    # scaling correction of the difference test
      d0 <- fitMeasures(fit_nested, "df") %>% as.numeric()
      d1 <- fitMeasures(fit_parent, "df") %>% as.numeric()
      cd <- ((d0 * c0) - (d1 * c1))/(d0 - d1)
    # MLM chi-square difference test
      T0 <- fitMeasures(fit_nested, "chisq.scaled") %>% as.numeric()
      T1 <- fitMeasures(fit_parent, "chisq.scaled") %>% as.numeric()
      TRd <- (T0*c0 - T1*c1)/cd
    # degrees of freedom
      df = d0 - d1
    return(c("TR_d" = round(TRd,3), 
             "df" = round(df,0), 
             "p_value" = pchisq(TRd, df, lower.tail = FALSE) |> round(3)))
}
```








xie































































































