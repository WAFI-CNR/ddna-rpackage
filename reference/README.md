# Scripts in this folder

List and description of the scripts contained in this folder

## ddna.r

Ste has no idea.

## plot_sequences.r

Script used to plot sequences by color. This script was embedded into a Web application, therefore it requires some modifications before being used.

## compute_seq_entropy.r

Script that computes the intra-sequence Shannon entropy. It is a standalone version so it can be used straightaway.
This script loads sequence data from the following CSV file: `100_sintetici_genbot.csv`.
It does NOT compute the inter-sequence entropy (that we need).
It does not plot anything.

## plot_entropy.r

Script that computes both the inter-sequence and intra-sequence Shannon entropies. It also plots them. This script was embedded into a Web application, therefore it requires some modifications before being used.

## plot_bases_distribution.r

Script that computes and plots the statistical distribution of the DNA bases in a group of sequences. This script was embedded into a Web application, therefore it requires some modifications before being used.

## LCSplot_log.r

Script that loads data about an LCS curve and plots it. It plots both a linear plot and a logarithmic one, as an inset to the linear plot. It is a standalone version so it can be used straightaway.
This script loads LCS data from the following file: `popularity_3_human-filtered.mat`.

## 100_sintetici_genbot.csv

Input file for the script: `compute_seq_entropy.r`.

## popularity_3_human-filtered.mat

Input file for the script: `LCSplot_log.r`.