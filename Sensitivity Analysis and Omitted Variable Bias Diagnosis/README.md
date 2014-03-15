****************
This project replicates part of Blattman, Christopher (2009) “From Violence to Voting: War and Political Participation
in Uganda.” American Political Science Review. 103(2): 231–247.
****************

Blattman(2009) explores the effect of people's experience of being abducted by rebels on political and non-political behaviors in their later life stage in Uganda. According to the author, the random rebel recruitment generated a quasiexperiment of abduction on individuals, thus facilitating the identification of abduction effect.

I first replicate Table 3 in this paper using weighted regression with cluster-robust standard error. In R, svydesign and svyglm (read the documentation of the package "survey" (read http://cran.fhcrc.org/web/packages/survey/survey.pdf to learn the functions of these two commends) will suffice for the purpose. 

A more important step of the empirical strategy in this paper is the sensitivity analysis that serves to alleviate readers' concern about omitted variable bias with respect to the causal relationship between abduction experience and political behaviors. Before practicing sensitivity analysis, let me briefly introduce this method. The underlying reason for us to adopt the sensitivity analysis is there might exist some unobserved covariate correlated with both the treatment and the outcome variables that could substantially reduece the treatment effect we want to measure. Therefore we want to check the values of the correlations that are necessary to change the results obtained under non-omitted variable bias. If the required correlation is very high for overturning the estimated result, we are in a relatively safe position to say that the obtained estimation is likely to be free from potential omitted variable bias problems. 

In this project, I use logistic transformation to create the unobserved covariate correlated with the treatment and the outcome variables. Note that there could be various ways to generate the unobserved covariate, readers thus should pick a generating process consistent with your research design. 
