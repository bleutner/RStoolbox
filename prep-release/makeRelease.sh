#!/bin/bash
cd ${HOME}/eclipseWorkspace/RStoolbox || exit

## Check for uncommited changes
if [[ -n $(git status -s) ]] 
then
	echo "Uncommited changes. Clean-up first."
	git status
	exit 1
fi


## Begin build and check
git checkout master
datScr=$(git log -1 --format=%ct data-raw/)
datSys=$(git log -1 --format=%ct R/sysdata.rda)
if [ $(( satScr > datSys )) -eq 1 ]
then
    echo -e "\n**********************************************************"
    echo "Generate sysdata *****************************************"
    echo "**********************************************************"
    Rscript data-raw/generate_sysdata.R
    git commit -a -m "Automatic commit: Document & Update sysdata"
else
    echo 'R/sysdata.R is already up-to-date'
fi 

echo -e "\n**********************************************************"
echo "Document and install RStoolbox ***************************"
echo "**********************************************************"
Rscript -e "library(devtools); library(methods); document(); install()"


## Re-build example data
tmstr=$(git log -1 --format=%ct data/ inst/external/trainingPoints.rds inst/external/landsat/)
texmpl=$(git log example-data -1  --format=%ct)
if [ $(( tmstr > texmpl )) -eq 0 ]
then
    echo -e "\n**********************************************************"
    echo "Generate example data ************************************"
    echo "**********************************************************"
    git checkout example-data
    Rscript data-raw/generate_data.R
    git commit -a -m "Automatic commit: Update example data (landsat, rlogo, srtm, lsat)"

    ## Back to master
    git checkout master
    git checkout example-data data/rlogo.rda data/srtm.rda  data/lsat.rda inst/external/landsat inst/external/trainingPolygons.rds
    Rscript -e "library(devtools); library(methods); document()"
    git commit -a -m "Automatic commit: Pull example data from branch example-data"
else 
    echo "Example data already up-to-date."    
fi

## Website
echo -e "\n**********************************************************"
echo "Build website documentation ******************************"
echo "**********************************************************"
git checkout gh-pages
Rscript rstbx-docu/build_docu.R
git commit -a -m "Automatic commit: Update gh-pages package documentation"
git checkout master

echo -e "\n**********************************************************"
echo "R CMD check **********************************************"
echo "**********************************************************"
Rscript -e "library(devtools); library(methods);  check_win_release(); check_win_oldrelease(); check_win_devel(); check()" 
#Valgrind times out --> run locally
#Rscript -e "library(rhub); library(methods);  check(platform='debian-gcc-release', valgrind = TRUE)" &> ${HOME}/RHub_RStoolbox_check_with_valgrind.log
Rscript -e "library(rhub); library(methods);  check(platform=c('debian-gcc-release', 'debian-gcc-devel', 'linux-x86_64-rocker-gcc-san'))" 
## Check on MAC
Rscript -e "library(methods); library(rhub); check(platform='macos-elcapitan-release')"

Rscript -e "library(revdepcheck); revdep_check(num_workers = 4)"


cd ..
R CMD build RStoolbox 
R CMD check $(ls RStoolbox*tar.gz | tail -n1) -o /tmp --run-donttest --as-cran --use-valgrind 
rm RStoolbox_0*tar.gz


