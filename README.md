# Intell_PLE_Pathway
Codes for Gene-Environment Pathways to Cognitive Intelligence and Psychotic-Like Experiences in Children, Park et al.(2023), eLife
https://doi.org/10.7554/eLife.88117.1


## LMM
Main analysis: R codes for linear mixed model analysis


## Euro
Sensitivity analysis: Adjustment for ethnic confounding
R codes for analysis with European ancestry samples.


## Null
Sensitivity analysis: Adjustment for unobserved confounders
R codes for null treatment approach (Miao et al., 2022). 


## Schizo
Sensitivity analysis: Adjustment for schizophrenia polygenic scores


## Dataset
Due to ABCD Study's policy on data sharing, we share a synthetic dataset instead of actual, real observations.
The synthetic dataset is made from our final samples (N=6,602), using CTGAN (Xu et al., 2019).
After hyperparameters optimization using Optuna (Akiba et al., 2019), the created synthetic dataset from the CTGAN model showed an overall quality score of 84.15%.
That is, the synthetic dataset is approximately 84% similar to the original dataset that we used in our study.

Please note that the analyses results have slight differences from those published in the paper in terms of effect sizes & statistical significance, as the synthetic dataset is not completely identical to the original dataset.


## References
Akiba, T., Sano, S., Yanase, T., Ohta, T., & Koyama, M. (2019, July). Optuna: A next-generation hyperparameter optimization framework. _In Proceedings of the 25th ACM SIGKDD international conference on knowledge discovery & data mining_ (pp. 2623-2631).

Miao, W., Hu, W., Ogburn, E. L., & Zhou, X. H. (2022). Identifying effects of multiple treatments in the presence of unmeasured confounding. _Journal of the American Statistical Association_, 1-15.

Xu, L., Skoularidou, M., Cuesta-Infante, A., & Veeramachaneni, K. (2019). Modeling tabular data using conditional gan. _Advances in Neural Information Processing Systems_, 32.
