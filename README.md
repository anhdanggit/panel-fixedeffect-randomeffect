# Panel Data: Fixed Effect, Random Effect, Hausman Taylor IV

In this assignment, I analyse the impact of different factors in wage. 
The most interesting explanatory factor is education. 
The program is coded in Stata, but parts of the assignment is also available in R. 

#### Unobserved Individual Heterogeneity
The key problem is the endogeneity, as there are many unobserved individual factors that correlated with both wage and education. 
By the panel data, we can conduct the `within-transformation` to exclude the unobserved individual fixed effect. 

#### Hausman Test: Compare FE and RE
Random Effects model has more restricted assumption about the behavior of unobserved effect (assumed to be random). 
FE estimate is consistent, but this model fails to capture the effect of time-invariant factor (i.e. Education).
By the *test of overidentification*, we compare FE and RE model, if there is no systematic difference between FE and RE. RE model is preferred (allow us to test the time-invariant factors). 

#### Hausman-Taylor IV
When the RE model is not qualified, another approach enable us to estimate the coefficient of time-invariant factor, is using the IV constructing internally by Hausman-Taylor approach. 
