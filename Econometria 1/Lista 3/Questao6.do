// Nome: Bruno Tebaldi de Q. Barbosa
// Disciplina: Econometria 1
// 2018-04-03 - Lista 3 - Questao 6

cd "C:\Users\Tebaldi\Documents\GitHub\Econometria1\Lista 3"
clear

import delimited "C:\Users\Tebaldi\Documents\GitHub\Econometria1\Lista 3\temp_CSHomePrice.mat.csv"

order year circuit

collapse logpriceindex-numpanels3x_protestant, by(circuit year)

reg logpriceindex numpro_casecat_12

save basexe.dta, replace

import delimited "C:\Users\Tebaldi\Documents\GitHub\Econometria1\Lista 3\probs.txt", clear

rename scircuit circuit
rename syear year

merge 1:1 circuit year using "C:\Users\Tebaldi\Documents\GitHub\Econometria1\Lista 3\basexe.dta"

