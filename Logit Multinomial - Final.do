global main "C:\Users\uribe\OneDrive\Escritorio\Limpieza bases"

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

* Sector terciario como base
mlogit Sector $indiv $labor $geogr, baseoutcome(3)

*ssc install fitstat
eststo l1: qui mlogit Sector $indiv $labor $geogr, baseoutcome(3)
drop if _est_l1 == 0
estadd fitstat

* resumen estadistico
sum Sector Edad Sexo Civil Educ Indigena Jefe Informal Skill_ocu lnwage zona_geo

* Relative risk ratio
qui mlogit Sector $indiv $labor $geogr, b(3) rrr
eststo rrr1

* Efectos Marginales 
qui mlogit Sector $indiv $labor $geogr , b(3)

margins, dydx(*) predict(outcome(1)) atmeans
eststo margins_m1
margins, dydx(*) predict(outcome(2)) atmeans
eststo margins_m2
margins, dydx(*) predict(outcome(3)) atmeans
eststo margins_m3

margins, dydx(*) predict(outcome(1)) predict(outcome(2)) predict(outcome(3)) post
eststo margins_ame
******************************
* LR test
qui mlogit Sector $indiv $labor $geogr, baseoutcome(3)
est store full
qui mlogit Sector $indiv $geogr, baseoutcome(3)
est store reduced
lrtest full reduced

* Medidas de ajuste
qui mlogit Sector $indiv $labor $geogr, b(3)
estat ic         // AIC y BIC
fitstat

* Test IIA
tab Sector
tab Sector, nolab
qui mlogit Sector $indiv $labor $geogr
estimate store todas
qui mlogit Sector $indiv $labor $geogr if Sector != 2
estimate store menos1
*H0: Independencia entre las categorías
hausman todas menos1, alleqs constant

* Evaluación de predicciones
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

* VIF
gen y_dummy = rnormal()
reg y_dummy $indiv $labor $geogr
vif

	
	
	
	









