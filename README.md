# Persuasive Analytical Report

This is an example repository used for class to explain the aspects of a persuasive report using analytics.

# Help with embedded HTML files

The file `Example_Report_embedded_choropleths.Rmd` has an attempt within it to embed HTML files, specifically animated choropleths from the [choroplethr](https://cran.r-project.org/web/packages/choroplethr/vignettes/g-animated-choropleths.html) package.

I could not seem to successfully embed these files.  Part of the code can be found at line 313:

    <<insertHTML:[./SPM/animated_choropleth.html]

    ```{r, echo=FALSE}
    htmltools::includeHTML("./SPM/animated_choropleth.html")
    ```

and the remainder of it can be found at `Example_Presentation_lib.R`, in the `subHtmlRender` function starting at line 104.



