global bd "C:\Users\User\OneDrive - Universidad del Pacífico\1. Documentos\0. Bases de datos\03. ENDES\1. Data"
global temp "C:\Users\User\OneDrive - Universidad del Pacífico\1. Documentos\0. Bases de datos\03. ENDES\2. Temp"

use "$temp\base_endes_tmp.dta", clear
*Generando las variables usadas en los tabulados.*
recode hv201 (11=100) (12=100) (13/96=0), gen(aguaid)
label var aguaid "Agua intradomiciliaria [red publica dentro /*
*/de vivienda o dentro de edificio]"
recode s430c (0/8=0), gen(cui)
replace cui=100 if (s430d==1)
label var cui "Tiene cui/dni"
gen edadmes=v008-b3

*Indicador CRED.
gen cred2=0
replace cred2=1 if edadmes<2 & s466c>1
replace cred2=1 if (edadmes>=2 & edadmes<4) & s466c>=3
replace cred2=1 if (edadmes>=4 & edadmes<6) & s466c>=4
replace cred2=1 if (edadmes>=6 & edadmes<7) & s466c>=5
replace cred2=1 if (edadmes>=7 & edadmes<9) & s466c>=6
replace cred2=1 if (edadmes>=9 & edadmes<12) & s466c>=7
replace cred2=1 if (edadmes>=12 & edadmes<15) & s466c>=8
replace cred2=1 if (edadmes>=15 & edadmes<18) & s466c>=9
replace cred2=1 if (edadmes>=18 & edadmes<21) & s466c>=10
replace cred2=1 if (edadmes>=21 & edadmes<24) & s466c>=11
replace cred2=1 if (edadmes>=24 & edadmes<30) & s466c>=12
replace cred2=1 if (edadmes>=30 & edadmes<36) & s466c>=13
replace cred2=1 if (edadmes>=36 & edadmes<42) & s466c>=14
replace cred2=1 if (edadmes>=42 & edadmes<48) & s466c>=15
replace cred2=1 if (edadmes>=48 & edadmes<54) & s466c>=16
replace cred2=1 if (edadmes>=54 & edadmes<60) & s466c>=17
replace cred2=0 if s466c>20
replace cred2=0 if s466c==98
replace cred2=0 if s466c==.
replace cred2=0 if s466==0
replace cred2=0 if s466==8

label var cred2 "CRED de acuerdo al numero de controles que refiere madre [corregido como calcula INEI]"

gen cred2_1=.
replace cred2_1=100 if(cred2==1)
replace cred2_1=0 if(cred2==0)

**Creando el código de departamento**
tostring v024, gen(DEP)
replace DEP="0"+DEP if length(DEP)==1

**Creando el código de Provincia***
tostring sprovin, gen(PROVI)
replace PROVI="0"+PROVI if length(PROVI)==1

**creando variable Distrito****
tostring sdistri, gen(DISTRI)
replace DISTRI="0"+DISTRI if length(DISTRI)==1

**Creando el código de ubigeo.**
drop ubigeo
gen ubigeo=DEP+PROVI+DISTRI
label var ubigeo "UBIGEO"

*---------------------------------------------------------------- *.
* 17. Generacion del Indicador Vacunacion .
*---------------------------------------------------------------- *.
gen fNe1=1 if (s45nm1>=1 & s45nm1<=3)
replace fNe1=0 if fNe1==.
gen fNe2=1 if (s45nm2>=1 & s45nm2<=3)
replace fNe2=0 if fNe2==.
gen fNe3=1 if (s45nm3>=1 & s45nm3<=3) 
replace fNe3=0 if fNe3==.
gen fRo1=1 if (s45rt1>=1 & s45rt1<=3) 
replace fRo1=0 if fRo1==.
gen fRo2=1 if (s45rt2>=1 & s45rt2<=3) 
replace fRo2=0 if fRo2==.
label var fNe1 "Recibio Antineumococo 1"
label var fNe2 "Recibio Antineumococo 2"
label var fNe3 "Recibio Antineumococo 3"
label var fRo1 "Recibio Antirotavirus 1"
label var fRo2 "Recibio Antirotavirus 2"

*---------------------------------------------------------------- *.
* 17. Generacion del Indicador Vacunacion: .
*---------------------------------------------------------------- *.
*-------->>Antineumococo al 3er, 5to y 12vo mes<<-----------------*.
gen pNe= fNe1+fNe2+fNe3
label var pNe "Nro dosis antineumococo"
*----------------->>Antirotavirus al 2do, 4to mes<<---------------*.
gen pRo=fRo1+fRo2
label var pRo "Nro dosis antirotavirus"
gen sNe3=-1
*Con tarjeta.
replace sNe3=1 if (h1==1 & edadmes<=2)
replace sNe3=1 if (h1==1 & edadmes>2 & pNe>=1) 
replace sNe3=0 if (h1==1 & edadmes>2 & pNe<1) 
replace sNe3=1 if (h1==1 & edadmes>4 & pNe>=2) 
replace sNe3=3 if (h1==1 & edadmes>4 & pNe==1) 
replace sNe3=1 if (h1==1 & edadmes>12 & pNe>=3) 
replace sNe3=3 if (h1==1 & edadmes>12 & pNe<=2) 
replace sNe3=0 if (h1==1 & pNe==. & edadmes>2)
*Sin tarjeta.
replace sNe3=2 if ((h1==0 | h1==2 | h1==3) & edadmes<=2)
replace sNe3=2 if ((h1==0 | h1==2 | h1==3) & edadmes>2 & pNe>=1)
replace sNe3=0 if ((h1==0 | h1==2 | h1==3) & edadmes>2 & pNe <1)
replace sNe3=2 if ((h1==0 | h1==2 | h1==3) & edadmes>4 & pNe>=2)
replace sNe3=3 if ((h1==0 | h1==2 | h1==3) & edadmes>4 & pNe==1)
replace sNe3=2 if ((h1==0 | h1==2 | h1==3) & edadmes>12 & pNe>=3)
replace sNe3=3 if ((h1==0 | h1==2 | h1==3) & edadmes>12 & pNe<=2)
replace sNe3=0 if ((h1==0 | h1==2 | h1==3) & pNe==. & edadmes>2)
recode sNe3(-1=.)
label var sNe3 "Situacion de la vacunacion anti Neumococo"
label define sNe3 0 "Ninguna dosis" 1 "Completo [c/ tarjeta]" /*
*/2 "Completo [s/tarjeta]" 3 "Con alguna dosis"
label values sNe3 sNe3
gen sNe4=. 
replace sNe4=0 if (sNe3==0)
replace sNe4=100 if (sNe3==1)
replace sNe4=100 if (sNe3==2)
replace sNe4=0 if (sNe3==3)
label var sNe4 "Vacunacion completa para la edad: Neumococo"
label define sNe4 0 "Incompleta" 100 "Completa"
label values sNe4 sNe4 
gen sRo1=-1
*Con tarjeta.
replace sRo1=1 if (h1==1 & edadmes<=2)
replace sRo1=1 if (h1==1 & edadmes>2 & pRo>=1) 
replace sRo1=0 if (h1==1 & edadmes>2 & pRo<1)
replace sRo1=1 if (h1==1 & edadmes>4 & pRo>=2) 
replace sRo1=3 if (h1==1 & edadmes>4 & pRo==1) 
replace sRo1=0 if (h1==1 & pRo==. & edadmes>2)
*Sin tarjeta.
replace sRo1=2 if ((h1==0 | h1==2 | h1==3) & edadmes<=2)
replace sRo1=2 if ((h1==0 | h1==2 | h1==3) & edadmes>2 & pRo>=1)
replace sRo1=0 if ((h1==0 | h1==2 | h1==3) & edadmes>2 & pRo<1)
replace sRo1=2 if ((h1==0 | h1==2 | h1==3) & edadmes>4 & pRo>=2)
replace sRo1=3 if ((h1==0 | h1==2 | h1==3) & edadmes>4 & pRo==1)
replace sRo1=0 if ((h1==0 | h1==2 | h1==3) & pRo==. & edadmes>2)
recode sRo1(-1=.)
label var sRo1 "Situacion de la vacunacion anti Rotavirus"
label define sRo1 0 "Ninguna dosis" 1 "Completo [c/ tarjeta]" 2 "Completo [s/tarjeta]" 3 "Con alguna dosis"
label values sRo1 sRo1
gen sRo2=.
replace sRo2=0 if (sRo1==0)
replace sRo2=100 if (sRo1==1)
replace sRo2=100 if (sRo1==2)
replace sRo2=0 if (sRo1==3)
label var sRo2 "Vacunacion completa para la edad Rotavirus"
label define sRo2 0 "Incompleta" 100 "Completa"
label values sRo2 sRo2
gen edad = v008-b3
recode edad (6/35=1), gen(edad_6a35)
gen hierro6a35=.
replace hierro6a35=2 if (edad_6a35==1) 
replace hierro6a35=0 if (s465ea==. | s465eb==. | s465ec==. | s465ed==.) & (edad_6a35==1) 
replace hierro6a35=1 if (s465ea==1 | s465eb==1 | s465ec==1 | s465ed==1) & (edad_6a35==1)
recode hierro6a35 (0=.)
label var hierro6a35 "Niños de 6 a 35 tomaron suplemento de hierro"
label define hierro6a35 1 "si" 2 "no"
label values hierro6a35 hierro6a35
recode hierro6a35 (1=100) (2=0), gen(hierro6a35_1)
gen hierro6a35IndSin=hierro6a35_1
replace hierro6a35IndSin=100 if (edad<6)
replace hierro6a35IndSin=100 if (edad>35)
*===================================.
* INDICADOR SINTETICO.
*===================================.
*Seleccionar a los menores de 24 meses y los distritos12.
gen c=""
replace c="1" if (cred2_1==100)
replace c="0" if (cred2_1==0)
replace c="9" if (cred2_1==.)
gen R=""
replace R="1" if (sRo2==100)
replace R="0" if (sRo2==0)
replace R="9" if sRo2==.
gen N=""
replace N="1" if (sNe4==100)
replace N="0" if (sNe4==0)
replace N="9" if (sNe4==.)
gen H=""
replace H="1" if (hierro6a35IndSin==100)
replace H="0" if (hierro6a35IndSin==0)
replace H="9" if (hierro6a35IndSin==.)
gen cu=""
replace cu="1" if (cui==100)
replace cu="0" if (cui==0)
replace cu="9" if (cui==.)
gen y=c+R+N+H+cu
gen ind=0
replace ind=-1 if (strpos(y,"9")>0)
replace ind=1 if (y=="11111")
gen indFED=.
replace indFED=100 if (ind==1)
replace indFED=0 if (ind==0)
replace indFED=-1 if (ind==-1)
replace indFED=. if (indFED==-1)
gen indFED2=100-indFED

* GRAFICO
gen pesoP = v005/1000000
collapse (mean) paqueteIntegrado= indFED (count) n_count=indFED [pw=pesoP] , by(edad hv270)

replace paqueteIntegrado = paqueteIntegrado/100
* Cálculo del error estándar para porcentaje_anemia
gen se = sqrt(paqueteIntegrado * (1 - paqueteIntegrado) / n_count) 

* Intervalos de confianza
gen lower = paqueteIntegrado - 1.96 * se
gen upper = paqueteIntegrado + 1.96 * se


* Gráfico de prevalencia de anemia según edad y quintil de riqueza
twoway (scatter paqueteIntegrado edad if hv270 == 5, msymbol(O) mcolor("17 99 97")) ///
       (scatter paqueteIntegrado edad if hv270 == 1, msymbol(O) mcolor("200 70 60")) ///
       (lowess paqueteIntegrado edad if hv270 == 5, lcolor(black) lwidth(medium)) ///
       (lowess paqueteIntegrado edad if hv270 == 1, lcolor(red) lwidth(medium)) ///
       (rcap lower upper edad if hv270 == 5, lcolor("17 99 97")) ///
       (rcap lower upper edad if hv270 == 1, lcolor("200 70 60")), ///
       title("Cobertura del paquete integrado DIT. Perú 2019-2023") ///
       xtitle("Edad en meses") ///
       ytitle("95% CI Acceso al paquete integrado DIT") ///
       legend(order(1 "Quintil Superior" 2 "Quintil Inferior")) ///
       yline(0, lwidth(vvthin))	   