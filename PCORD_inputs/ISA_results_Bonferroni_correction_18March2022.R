# --------------------------------------------------
# correcting indicator species analysis results from PC-ORD with Bonferroni
# 18 Mar 2022
# HH
# --------------------------------------------------
#

?p.adjust
#will be using the Bonferroni correction

sapling_pvals <- read.csv("PCORD_inputs/saplings_ISA_fromPCORD_treatmentgroup_standardmethod_17March2022.csv")
View(sapling_pvals)

p_adj <- p.adjust(sapling_pvals$p, method="bonferroni")
sum(p_adj<=0.05)

p_adj_fdr <- p.adjust(sapling_pvals$p, method="fdr")
sum(p_adj_fdr<=0.05)
