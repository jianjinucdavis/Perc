---
title: "README_updates"
author: "JJ"
date: "August 20, 2015"
output: pdf_document
---

**Updates to Perc**

Perc_0.0.9099 fixed an error in computing imputed-win matrix in the function `conductance` that exists in old versions of Perc packages. Updates to the codes can be found at: https://github.com/jianjinucdavis/Perc/commit/d01d3a072f26a61172d09855db8f1e92cd5d9330

Perc_0.0.9100 updated transitivity and conductance function by adding an argument `strict` to allow defining a transitive triangle. `strict = TRUE` is used when a transitive triangle is defined as all pathways in the triangle go to the same direction. `strict = FALSE` is used when a transitive triangle is defined as PRIMARY pathways in the triangle go to the same direction. github commits: 6cf790b39a978b8fee0202337e6d80140630eb02



Note: you'll need to register a github account and email me your username to access the link above.
