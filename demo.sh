#! /bin/bash
clr="\e[0;30m\e[47m"
def="\e[0m"
echo -e "${clr}Demo model checker pour un sous ensemble de lustre ${def}"
echo ""
echo -e "${clr}Exemple 1 : flots de booléens alternant true/false ${def}"
more examples/positives/ex01.lus
read touche
src/lmoch examples/positives/ex01.lus check1 -v
read touche
echo -e "${clr}Exemple 2: if/then/else + node inlining ${def}"
more examples/positives/ex02.lus
read touche
src/lmoch examples/positives/ex02.lus  check2 -v
read touche
echo -e "${clr}Exemple 3: entiers ${def}"
more examples/positives/ex003.lus
read touche
src/lmoch examples/positives/ex003.lus check -v
read touche
echo -e "${clr} Exemple 4: plus complexe ${def}"
more examples/positives/dragon.lus 
read touche
src/lmoch examples/positives/dragon.lus check -v
read touche
echo -e "${clr} Exemple : boucle ${def} "
more examples/ex005.lus
read touche
src/lmoch examples/ex005.lus check -v
echo ""
echo "************"
echo -e "${clr} FIN ${def}"
