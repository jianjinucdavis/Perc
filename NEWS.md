---
title: "README_updates"
author: "JJ"
date: "August 20, 2015"
output: pdf_document
---

**Updates to Perc**

Perc_0.1.2 updated `as.conflictmat`. In `as.conflictmat`, when raw data includes self-loops or incorrect class of data, `as.conflictmat` fixes the data automatically and return a warning which notifies users about the auto-fixes. 

Perc_0.1.1 updated `as.conflictmat` and `simRankOrder`. In `as.conflictmat`, when raw data used is a matrix, the updated codes sorted the matrix by its colnames and export the sorted matrix. This update is made so that the output is consistent for both edgelist raw data and matrix raw data. In `simRankOrder`, the codes used to assign IDs to rank orders were updated so that the process of assigning IDs did not depend on the order of the column/row names of the input for `simRankOrder`. This solved the problem of assigning wrong IDs to rank orders when raw data imported from `as.conflictmat` was a matrix. github commits: https://github.com/jianjinucdavis/Perc/commit/bc4143fd21a4052285f6c245e4717d3ff28da6c8

Perc_0.0.9100 updated transitivity and conductance function by adding an argument `strict` to allow defining a transitive triangle. `strict = TRUE` is used when a transitive triangle is defined as all pathways in the triangle go to the same direction. `strict = FALSE` is used when a transitive triangle is defined as PRIMARY pathways in the triangle go to the same direction. github commits: 6cf790b39a978b8fee0202337e6d80140630eb02

Perc_0.0.9099 fixed an error in computing imputed-win matrix in the function `conductance` that exists in old versions of Perc packages. Updates to the codes can be found at: https://github.com/jianjinucdavis/Perc/commit/d01d3a072f26a61172d09855db8f1e92cd5d9330
