global main     "C:\Users\uribe\OneDrive\Escritorio\Limpieza bases"

use "$main/Base_Final.dta", clear

*===============================
* Aplicando filtros y variables
*===============================

* Solo Individios mayores a 18 años 
keep if Edad>=18

* Se va ocupaciones de fuerzas armadas
drop if Skill_ocu==0 

* Log del ingreso mensual
gen lnwage = ln(ingreso+1)

drop ingreso zona

save "base_1111.dta"

*===============
* Estimaciones
*===============

global indiv "Edad i.Sexo i.Civil i.Educ i.Indigena i.Jefe"
global labor "i.Informal i.Skill_ocu c.lnwage"
global geogr "b7.zona_geo"

mlogit Sector $indiv $labor $geogr, baseoutcome(1)
mlogit Sector $indiv $labor $geogr, baseoutcome(2)
mlogit Sector $indiv $labor $geogr, baseoutcome(3)

estout using "resultados_mlogit.xls", cells(b(star fmt(3)) se(par fmt(3))) replace

*ssc install fitstat
eststo l1: qui mlogit Sector $indiv $labor $geogr, baseoutcome(3)
drop if _est_l1 == 0
estadd fitstat

* resumen estadistico
sum Sector Edad Sexo Civil Educ Indigena Jefe Informal Skill_ocu lnwage zona_geo

esttab l1, replace label title("Logit Multinomial") ///
        b(3) se(3) stats(N r2_p aic bic, /// 
        fmt(0 3 0 0) ///  
		labels("Observations" "Pseudo R2" "AIC" "BIC")) ///
		drop(0.Sexo 1.Civil 1.Educ 0.Indigena 0.Jefe 0.Informal 1.Skill_ocu 7.zona_geo) ///
		star(* 0.10 ** 0.05 *** 0.01) ///
        mtitle("Modelo 1") ///
		note("Standard errors in parentheses - ME: SE computed using Delta Method")

* Para exportar en html
esttab l1 using "resultados_logit.html", ///
    replace label title("Logit Multinomial") ///
    b(3) se(3) stats(N r2_p aic bic, fmt(0 3 0 0) ///
    labels("Observaciones" "Pseudo R2" "AIC" "BIC")) ///
    drop(0.Sexo 1.Civil 1.Educ 0.Indigena 0.Jefe 0.Informal 1.Skill_ocu 7.zona_geo) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitle("Modelo 1") ///
    note("Errores estándar entre paréntesis - Delta Method") ///
    html
	
* Relative risk ratio
qui mlogit Sector $indiv $labor $geogr, b(3) rrr
eststo rrr1

* Exportar tabla con RRR
esttab rrr1 using "resultados_rrr.html", ///
    replace label title("Logit Multinomial - Relative Risk Ratios") ///
    eform b(3) se(3) stats(N r2_p aic bic, fmt(0 3 0 0) ///
    labels("Observaciones" "Pseudo R2" "AIC" "BIC")) ///
    drop(0.Sexo 1.Civil 1.Educ 0.Indigena 0.Jefe 0.Informal 1.Skill_ocu 7.zona_geo) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitle("Modelo 1 - RRR") ///
    note("Errores estándar entre paréntesis - Delta Method") ///
    html


* Efectos Marginales 
qui mlogit Sector $indiv $labor $geogr , b(3)

margins, dydx(*) predict(outcome(1)) atmeans
eststo margins_m1
* Exportar tabla de efectos marginales
esttab margins_m1 using "resultados_margins1.html", ///
    replace label title("Efectos Marginales - Logit Multinomial") ///
    b(3) se(3) stats(N, fmt(0) labels("Observaciones")) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitle("Primario" "Secundario" "Terciario") ///
    note("Efectos marginales promedio - Errores estándar entre paréntesis (Delta Method)") ///
    html

	
margins, dydx(*) predict(outcome(2)) atmeans
eststo margins_m2
* Exportar tabla de efectos marginales
esttab margins_m2 using "resultados_margins2.html", ///
    replace label title("Efectos Marginales - Logit Multinomial") ///
    b(3) se(3) stats(N, fmt(0) labels("Observaciones")) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitle("Primario" "Secundario" "Terciario") ///
    note("Efectos marginales promedio - Errores estándar entre paréntesis (Delta Method)") ///
    html
	
margins, dydx(*) predict(outcome(3)) atmeans
eststo margins_m3
* Exportar tabla de efectos marginales
esttab margins_m3 using "resultados_margins3.html", ///
    replace label title("Efectos Marginales - Logit Multinomial") ///
    b(3) se(3) stats(N, fmt(0) labels("Observaciones")) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitle("Primario" "Secundario" "Terciario") ///
    note("Efectos marginales promedio - Errores estándar entre paréntesis (Delta Method)") ///
    html
	
	margins, dydx(*) predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) post
eststo margins_ame
esttab margins_ame using "resultados_margins_ame.csv", ///
    replace label title("Efectos Marginales Promedio - Logit Multinomial") ///
    b(3) se(3) stats(N, fmt(0) labels("Observaciones")) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Primario" "Secundario" "Terciario") ///
    note("Efectos marginales promedio - Errores estándar entre paréntesis (Delta Method)") ///
    csv

******************************
ssc install parmest

qui mlogit Sector $indiv $labor $geogr , b(3)

margins, dydx(*) predict(outcome(1)) post 
parmest, norestore list(parm estimate stderr) saving(ame1.dta), replace

margins, dydx(*) predict(outcome(2)) post 
parmest, norestore list(parm estimate stderr) saving(ame2.dta, replace)

margins, dydx(*) predict(outcome(3)) post
parmest, norestore list(parm estimate stderr) saving(ame3.dta, replace)


use ame1.dta, clear
gen sector = "Primario"
append using ame2.dta
replace sector = "Secundario" if missing(sector)
append using ame3.dta
replace sector = "Terciario" if missing(sector)

save efectos_marginales.dta, replace
export delimited using "efectos_marginales.csv", replace

*******************************


* Grafico Efectos Marginales
qui mlogit Sector $indiv $labor $geogr, vce(rob)
eststo margin1: margins, dydx(*) predict(outcome(1)) atmeans 
marginsplot, horizontal unique xline(0) recast(scatter) yscale(reverse) graphregion(color(white)) allx ///
			 title("Efectos Marginales Pr(Sector Primario)") ytitle("Variables") xtitle("") ///
			 ylabel(, nogrid) name(A, replace)
	
qui mlogit Sector $indiv $labor $geogr, vce(rob)
eststo margin1: margins, dydx(*) predict(outcome(2)) atmeans 
marginsplot, horizontal unique xline(0) recast(scatter) yscale(reverse) graphregion(color(white)) allx ///
			 title("Efectos Marginales Pr(Sector Secundario)") ytitle("Variables") xtitle("") ///
			 ylabel(, nogrid) name(B, replace)
	
qui mlogit Sector $indiv $labor $geogr, vce(rob)
eststo margin1: margins, dydx(*) predict(outcome(3)) atmeans 
marginsplot, horizontal unique xline(0) recast(scatter) yscale(reverse) graphregion(color(white)) allx ///
			 title("Efectos Marginales Pr(Sector Terciario)") ytitle("Variables") xtitle("") ///
			 ylabel(, nogrid) name(C, replace)
	
	
graph combine A B C, graphregion(color(white))
	
* TEST *

tab Sector
tab Sector, nolab

qui mlogit Sector $indiv $labor $geogr
estimate store todas

qui mlogit Sector $indiv $labor $geogr if Sector != 2
estimate store menos1

*H0: Independencia entre las categorías
hausman todas menos1, alleqs constant

qui mlogit Sector $indiv $labor $geogr

estat ic         // AIC y BIC
fitstat

* Vif
gen y_dummy = rnormal()
reg y_dummy $indiv $labor $geogr
vif

* LR test
qui mlogit Sector $indiv $labor $geogr, baseoutcome(3)
est store full
qui mlogit Sector $indiv $geogr, baseoutcome(3)
est store reduced
lrtest full reduced

* medidas de ajuste
fitstat
estat ic

* evaluacion de predicciones
qui mlogit Sector $indiv $labor $geogr, baseoutcome(3)

predict p1, outcome(1)
predict p2, outcome(2)
predict p3, outcome(3)

gen predcat = .
replace predcat = 1 if p1 >= p2 & p1 >= p3
replace predcat = 2 if p2 > p1 & p2 >= p3
replace predcat = 3 if p3 > p1 & p3 > p2

gen acierto = (predcat == Sector)
tab Sector predcat
sum acierto

export delimited "base_1.csv", replace



	
	
	
	









