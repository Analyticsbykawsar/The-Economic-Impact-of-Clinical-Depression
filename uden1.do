
*ID **
egen id = group(land)

*panel data:
xtset Land År

generate lnDAYls_A=ln(DAYLs_A)
generate lnDAYls_K=ln(DAYLs_K)
generate lnDAYls_M=ln(DAYLs_M)

*dummyvariabel*
generate D_H1992 = 0
generate D_L1992 = 0
generate D_UM1992 = 0
generate D_LM1992 = 0


generate D_H2019 = 0
generate D_L2019 = 0
generate D_UM2019 = 0
generate D_LM2019 = 0


 replace D_H1992 = 1 if class_l=="H"
  replace D_L1992 = 1 if class_l=="L"
   replace D_UM1992 = 1 if class_l=="UM"
    replace D_LM1992 = 1 if class_l=="LM"

replace D_H2019 = 1 if class_2019=="H"
  replace D_L2019 = 1 if class_2019=="L"
   replace D_UM2019 = 1 if class_2019=="UM"
    replace D_LM2019 = 1 if class_2019=="LM"
	

*bar graf*
graph bar (mean) lnDAYls_A (mean) lnDAYls_K (mean) lnDAYls_M, by(class_l)
 
* gennemsnit af indkomstgrupper*
collapse (mean) lnBNP lnDAYls_A, by(class_l)
sort class_l
graph twoway (scatter  lnBNP lnDAYls_A, sort mlabel(class_l))
 

** DAYLS mod BNP 1992**
graph twoway (scatter  lnBNP lnDAYls_A if År==1992, sort mlabel(Land))

* 2 y-akser*
collapse (mean) lnBNP lnDAYls_A, by(År)
twoway line  lnBNP År, ytitle("Title y1") || line lnDAYls_A År, yaxis(2) ytitle("Title y2", axis(2))

 
line lnDAYls_A År if inlist(id, 32 ,47, 58, 60, 65, 66, 78, 79, 84, 86, 89, 125, 127, 129, 155, 162, 165, 173, 177, 182), msymbol(circle_hollow) by(Land)
line lnBNP År if inlist(id, 32 ,47, 58, 60, 65, 66, 78, 79, 84, 86, 89, 125, 127, 129, 155, 162, 165, 173, 177, 182), msymbol(circle_hollow) by(Land)

*overall scatterplot**
overall variation:
graph twoway (scatter  logBNP DAYLs_A, msize(small) msymbol(o)) (qfit logBNP DAYLs_A, clstyle(p3)  lwidth(medthick)), by(class_l)



** descriptive analyse**
xtsum lnBNP lnDAYls_A lnDAYls_K lnDAYls_M År Land D_H1992 D_L1992 D_UM1992 D_LM1992 D_H2019 D_L2019 D_UM2019 D_LM2019



*linear plot*
graph twoway (scatter lnBNP lnDAYls_A, msize(small) msymbol(o)) (qfit lnBNP lnDAYls_A, clstyle(p3) lwidth(medthick)), plotregion(style(none)) scale(1.2)


**AUTOKorrelation**
regress lnBNP DAYLs_A i.År
correlate lnBNP L.lnBNP
forvalues  j = 1/28 {
   quietly corr uhat L`j'.uhat
   display "Autocorrelation at lag `j' = "%6.3f r(rho)
    }
*normalty test Jarque-Bera normality test:  Chi(2) is below 0.05 which means that our data is not normal distibuted and we can reject our null-hypotese:
jb resid
*multicornarrity, if the vif value is less than 10 then we do not have multicornarrity in our data:
vif
*heter Is below 0,05 so there is heterkestdasy in our data.
imtest, white
estat hettest
rvfplot, yline(0)


***PANEL DATA REGRESSION Baseline******
regress lnBNP lnDAYls_A  i.År, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_A  i.År, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_A  i.År, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using R3.tex, label star mtitles se scalars(r2_a) lines 
eststo clear


*simple pooled ols ignoring the heteroskedasy F statisitk shows 17,63 and the probablity value is less than 5 % tells us that our model is fit for analyse. R-square is  0.0065 meaning that our independent variabel are explaining 0,65 % variance in out dependent variable, which is very low. All coefficient are all positiv relate with gdp_growth but with  gpi and loggdp_prcap being insignificant.


*** High class ****
regress lnBNP lnDAYls_A i.År if D_H1992, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_A  i.År if D_H1992, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_A  i.År if D_H1992, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using H1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear


*** Upper middel ****
regress lnBNP lnDAYls_A i.År if D_UM1992, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_A  i.År if D_UM1992, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_A  i.År if D_UM1992, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using H1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear

*** Lower middel ****
regress lnBNP lnDAYls_A i.År if D_LM1992, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_A  i.År if D_LM1992, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_A  i.År if D_LM1992, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using LM1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear


*** Low class ****
regress lnBNP lnDAYls_A i.År if D_l1992, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_A  i.År if D_l1992, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_A  i.År if D_l1992, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using H1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear





***PANEL DATA REGRESSION Baseline KØN******

*linear plot*
graph twoway (scatter lnBNP lnDAYls_A, msize(small) msymbol(o)) (qfit lnBNP lnDAYls_A, clstyle(p3) lwidth(medthick)), plotregion(style(none)) scale(1.2)


**AUTOKorrelation**
regress lnBNP lnDAYls_K lnDAYls_M i.År
correlate lnBNP L.lnBNP
forvalues  j = 1/28 {
   quietly corr uhat L`j'.uhat
   display "Autocorrelation at lag `j' = "%6.3f r(rho)
    }
Predict resid
*normalty test Jarque-Bera normality test:  Chi(2) is below 0.05 which means that our data is not normal distibuted and we can reject our null-hypotese:
jb resid
*multicornarrity, if the vif value is less than 10 then we do not have multicornarrity in our data:
vif
*heter Is below 0,05 so there is heterkestdasy in our data.
imtest, white
estat hettest
rvfplot, yline(0)


***PANEL DATA REGRESSION Baseline******
regress lnBNP lnDAYls_K lnDAYls_M i.År, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using BK2.tex, label star mtitles se scalars(r2_a) lines 
eststo clear
	
*simple pooled ols ignoring the heteroskedasy F statisitk shows 17,63 and the probablity value is less than 5 % tells us that our model is fit for analyse. R-square is  0.0065 meaning that our independent variabel are explaining 0,65 % variance in out dependent variable, which is very low. All coefficient are all positiv relate with gdp_growth but with  gpi and loggdp_prcap being insignificant.





*** High class ****
regress lnBNP lnDAYls_K lnDAYls_M i.År if D_H1992, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_H1992, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_H1992, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using HK1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear


*** Upper middel ****
regress lnBNP lnDAYls_K lnDAYls_M i.År if D_UM1992, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_UM1992, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_UM1992, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using UMK1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear

*** Lower middel ****
regress lnBNP lnDAYls_K lnDAYls_M i.År if D_LM1992, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_LM1992, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_LM1992, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using LMK1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear


*** Low class ****
regress lnBNP lnDAYls_K lnDAYls_M i.År if D_L1992, robust
eststo model, title ("OLS")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_L1992, fe  robust
eststo model2, title ("Fixed")
xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_L1992, re  robust
eststo model3, title ("Random")
esttab, mtitles se scalars(r2)
esttab using LK1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear



**comparing**

*if the value is more than 5 %, then the fixed model is better, so we can not reject the null-hypotese. so we can use fixed effect model for our analysis.
xtreg  logBNP DAYLs_A, re vce(cluster Land) theta
estimates store FE1
xtreg  logBNP DAYLs_A, fe  vce(cluster Land)
estimates store RE1
hausman FE1 RE1, sigmamore
eststo model7, title ("FE VS RE for model 1")
* is below or fail to fails to meet the asymptotic assumptions of the Hausman test
**In your case the thing is that you are testing the consistency of b1, conditioned on the fact that b0 is not just consistent but it is also more efficient (since it has less varience, that's why Var(b0)-Var(b1) turned out to be negative. Bottom line: go with fixed effects.** læs artiklen.
*Breusch-pagan LM-test Random vs ols model:
* the result is not signifikant since p value is not below 0.05 and we should therefor use the ols model when comparing to Random effekt.
quietly xtreg  logBNP DAYLs_A, re vce(cluster Land) theta
xttest0
* the result is signifikant p value is below 0.05 and we should therefor not use the ols model.

eststo clear
eststo:regress lnBNP lnDAYls_A  i.År, robust
eststo:xtreg  lnBNP lnDAYls_A  i.År, fe  robust
eststo:xtreg  lnBNP lnDAYls_A  i.År, re  robust

eststo:regress lnBNP lnDAYls_K lnDAYls_M i.År, robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År, fe  robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År, re  robust

esttab using N.tex, label star mtitles se scalars(r2_a) lines 
eststo clear

eststo:regress lnBNP lnDAYls_A  i.År if D_H1992==1, robust
eststo:xtreg  lnBNP lnDAYls_A  i.År if D_H1992==1, fe  robust
eststo:xtreg  lnBNP lnDAYls_A  i.År if D_H1992==1, re  robust

eststo:regress lnBNP lnDAYls_K lnDAYls_M i.År if D_H1992==1, robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_H1992==1, fe  robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_H1992==1, re  robust

esttab using N1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear

eststo:regress lnBNP lnDAYls_A  i.År if D_UM1992==1, robust
eststo:xtreg  lnBNP lnDAYls_A  i.År if D_UM1992==1, fe  robust
eststo:xtreg  lnBNP lnDAYls_A  i.År if D_UM1992==1, re  robust

eststo:regress lnBNP lnDAYls_K lnDAYls_M i.År if D_UM1992==1, robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_UM1992==1, fe  robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_UM1992==1, re  robust

esttab using N2.tex, label star mtitles se scalars(r2_a) lines 
eststo clear


eststo:regress lnBNP lnDAYls_A  i.År if D_LM1992==1, robust
eststo:xtreg  lnBNP lnDAYls_A  i.År if D_LM1992==1, fe  robust
eststo:xtreg  lnBNP lnDAYls_A  i.År if D_LM1992==1, re  robust

eststo:regress lnBNP lnDAYls_K lnDAYls_M i.År if D_LM1992==1, robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_LM1992==1, fe  robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_LM1992==1, re  robust

esttab using N3.tex, label star mtitles se scalars(r2_a) lines 
eststo clear

eststo:regress lnBNP lnDAYls_A  i.År if D_L1992==1, robust
eststo:xtreg  lnBNP lnDAYls_A  i.År if D_L1992==1, fe  robust
eststo:xtreg  lnBNP lnDAYls_A  i.År if D_L1992==1, re  robust

eststo:regress lnBNP lnDAYls_K lnDAYls_M i.År if D_L1992==1, robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_L1992==1, fe  robust
eststo:xtreg  lnBNP lnDAYls_K lnDAYls_M  i.År if D_L1992==1, re  robust

esttab using N4.tex, label star mtitles se scalars(r2_a) lines 
eststo clear



**GMM***

** GLOBAL**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År, gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan 
eststo:  xtabond2 lnBNP l.lnBNP lnDAYls_A i.År, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan 
eststo:  xtabond2 lnBNP l.lnBNP lnDAYls_A i.År, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan 

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År, gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using G.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear

** High**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_H1992==1, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_H1992==1, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_H1992==1, gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_H1992==1, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_H1992==1, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GH.tex,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear
** UM**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small   orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_UM1992==1, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_UM1992==1, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_UM1992==1, gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_UM1992==1, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_UM1992==1, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GUM.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear
** LM**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small   orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_LM1992==1, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if  D_LM1992==1, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_LM1992==1, gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_LM1992==1, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_LM1992==1, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GLM.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear

** L**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_L1992==1, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if  D_L1992==1, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_L1992==1, gmm(l.lnBNP lnDAYls_A , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_L1992==1, gmm(l.lnBNP lnDAYls_A , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_A i.År if D_L1992==1, gmm(l.lnBNP lnDAYls_A , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GL.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear


** Kvinder GLOBAL **
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År, gmm(l.lnBNP lnDAYls_K , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan 
eststo:  xtabond2 lnBNP l.lnBNP lnDAYls_K i.År, gmm(l.lnBNP lnDAYls_K , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan 
eststo:  xtabond2 lnBNP l.lnBNP lnDAYls_K i.År, gmm(l.lnBNP lnDAYls_K  , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan 

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År, gmm(l.lnBNP lnDAYls_K , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År, gmm(l.lnBNP lnDAYls_K , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År, gmm(l.lnBNP lnDAYls_K , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GK1.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear

** High**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_K , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_K , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_K , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_K  , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_K  , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_K , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GKH.tex,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear
** UM**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_K , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small   orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_K , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_K , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan


**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_K , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_K, lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_K , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GKUM.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear
** LM**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_K , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small   orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_K , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_K , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_K , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_K, lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_K, lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GKLM.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear

** L**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_K , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_K  , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_K  , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_K  , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_K  , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_K i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_K  , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GKL.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear


** MÆND GLOBAL **
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År, gmm(l.lnBNP lnDAYls_M , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan 
eststo:  xtabond2 lnBNP l.lnBNP lnDAYls_M i.År, gmm(l.lnBNP lnDAYls_M , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan 
eststo:  xtabond2 lnBNP l.lnBNP lnDAYls_M i.År, gmm(l.lnBNP lnDAYls_M  , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan 

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År, gmm(l.lnBNP lnDAYls_M, lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År, gmm(l.lnBNP lnDAYls_M , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År, gmm(l.lnBNP lnDAYls_M , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GM1.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear

** High**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_M , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_M, lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_M, lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_M , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_M , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_H1992==1 , gmm(l.lnBNP lnDAYls_M , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GMH.tex,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear
** UM**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small   orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan


**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_UM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GMUM.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear
** LM**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small   orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_M  , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_LM1992==1 , gmm(l.lnBNP lnDAYls_M , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GMLM.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear

** L**
**system**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_M , lag(1 3) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_M  , lag(3 7) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_M , lag(7 10) collapse) iv(i.År, equation(level)) cluster(id)  twostep robust small  orthogonal  nodiffsargan

**difference**
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_M  , lag(1 3) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_M  , lag(3 7) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq
eststo: xtabond2 lnBNP l.lnBNP lnDAYls_M i.År if D_L1992==1 , gmm(l.lnBNP lnDAYls_M  , lag(7 10) collapse) iv(i.År) cluster(id)  twostep robust small  orthogonal  nodiffsargan noleveleq

esttab using GML.tex ,label se starlevels( * 0.10 ** 0.05 *** 0.010)  stats(N j ar1p ar2p hansenp, labels("Observations" "No. of instruments" "AR1 (p-value)" "AR2 (p-value)" "Hansen-J (p-value)" "F Statistic"))
eststo clear



**** PANEL DATA with logBNP***
eststo:regress lnBNP l.lnBNP lnDAYls_A  i.År, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År, re  robust

eststo:regress lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År, re  robust

esttab using P1.tex, label star mtitles se scalars(r2_a) lines 
eststo clear
eststo:regress lnBNP l.lnBNP lnDAYls_A  i.År if D_H1992==1, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År if D_H1992==1, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År if D_H1992==1, re  robust

eststo:regress lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_H1992==1, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_H1992==1, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_H1992==1, re  robust

esttab using P3.tex, label star mtitles se scalars(r2_a) lines 
eststo clear

eststo:regress lnBNP l.lnBNP lnDAYls_A  i.År if D_UM1992==1, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År if D_UM1992==1, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År if D_UM1992==1, re  robust

eststo:regress lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_UM1992==1, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_UM1992==1, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_UM1992==1, re  robust

esttab using P4.tex, label star mtitles se scalars(r2_a) lines 
eststo clear


eststo:regress lnBNP l.lnBNP lnDAYls_A  i.År if D_LM1992==1, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År if D_LM1992==1, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År if D_LM1992==1, re  robust

eststo:regress lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_LM1992==1, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_LM1992==1, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_LM1992==1, re  robust

esttab using P5.tex, label star mtitles se scalars(r2_a) lines 
eststo clear

eststo:regress lnBNP l.lnBNP lnDAYls_A  i.År if D_L1992==1, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År if D_L1992==1, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_A  i.År if D_L1992==1, re  robust

eststo:regress lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_L1992==1, robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_L1992==1, fe  robust
eststo:xtreg  lnBNP l.lnBNP lnDAYls_K lnDAYls_M  i.År if D_L1992==1, re  robust

esttab using P6.tex, label star mtitles se scalars(r2_a) lines 
eststo clear
