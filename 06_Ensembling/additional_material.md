# Combining Pattern Classifiers: Methods and Algorithms
Book by Ludmila I. Kuncheva, available at https://onlinelibrary.wiley.com/doi/book/10.1002/9781118914564

This book reviews many theoretical findings about combining classification algorithms and includes a thorough discussion of bagging and boosting. Unfortunately, it only contains limited experimental results and does not explicitly discuss the ensemble selection method we focused on in the lecture. Within the limited experimental results it however includes a forward selection, which performs best, and appears to be a simplified version of ensemble selection (c.f. Chapter 8.4.3 on Page 276).

# Diversity in Ensembling
Measuring the diversity of models and acting upon this "has been refererred to as the holy grail of ensemble learning" [1] and it is also thoroughly discussed in Kuncheva's book (see entry above). However, thorough experiments do not provide evidence of it being a helpful factor in building ensembles [2]. Recent research [1] has demonstrated that diversity is an additional term in the bias-variance decomposition of the ensemble loss, and thus not something to be optimized, but rather managed in a bias-variance-diversity trade-off.

[1] Zhi-Hua Zhou. Ensemble methods: foundations & algorithms. Chapman & Hall/CRC, 2012
[2] Q(D)O-ES: Population-based Quality (Diversity) Optimisation for Post Hoc Ensemble Selection in AutoML. Lennart Purucker, Lennart Schneider, Marie Anastacio, Joeran Beel, Bernd Bischl, Holger Hoos. Proceedings of the Second International Conference on Automated Machine Learning 2
[3] A Unified Theory of Diversity in Ensemble Learning. Danny Wood, Tingting Mu, Andrew Webb, Henry Reeve, Mikel Luján, Gavin Brown. https://arxiv.org/abs/2301.03962

# Failures of Stacking
Stacking can fail in case of a very low number of positive samples [4] and in multi-layer stacking [5]. Both cases are not too well studied, and it might be possible to detect this issue using an outer layer of cross-validation.

[4] Perlich and Swirszcz. On Cross-Validation and Stacking: Building seemingly predictive models on random data. SigKDD Explorations. Volume 12-2 (2011)
[5] Erickson. https://github.com/autogluon/autogluon/issues/2779#issuecomment-3923539465 
