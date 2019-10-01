# drafts

This directory contains drafts related to the paper.

## Using `redoc` in this directory

I'm including some steps I've found useful in using `redoc`, since I use it infrequently. First, I created a draft document that uses the `redoc::redoc` output format as well as some options. This also uses the `reference_docx` option from the `word_document` output format; the reference is `my_docx_styles.docx`.

The key steps to using `redoc` in this directory are:

* knit the .Rmd to produce a .docx file
* turn on track changes in the .docx file
* distribute to coauthors
* address comments / edits in Word (or leave alone to be pulled into the .Rmd)

To update the .Rmd, use

```{r}
redoc::dedoc(
  docx = here::here("drafts", "201910_DS.docx"), 
  to = "201910_DS.Rmd", dir = "drafts", 
  overwrite = TRUE, 
  track_changes = "criticmarkup")
```
