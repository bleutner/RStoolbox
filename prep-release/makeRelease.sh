#!/bin/bash
cd ${HOME}/eclipseWorkspace/RStoolbox

## Check for uncommited changes
function enforce_up2date {
    git status | grep -q up-to-date
    status=$? 
    if [ $status -eq 1 ] 
    then
	   echo "Uncommited changes. Clean-up first."
	   git status
	   exit 1
    fi
}


## Begin build and check
git checkout master
enforce_up2date
echo "\n**********************************************************"
echo "Generate sysdata *****************************************"
echo "**********************************************************"
statDir=$(git diff --stat HEAD -- data-raw)
statSysDat=$(git diff --stat HEAD -- R/sysdata.rda)
if [[ ! -n $statDir ]] || [[ ! -n $statSysDat ]]
then
    Rscript data-raw/generate_sysdata.R
    git commit -a -m "Automatic commit: Document & Update sysdata"
else
    echo 'R/sysdata.R is up-to-date'
fi 

echo "\n**********************************************************"
echo "Document and install RStoolbox ***************************"
echo "**********************************************************"
Rscript -e "library(devtools); library(methods); document(); install()"


## Re-build example data
git checkout example-data
echo "\n**********************************************************"
echo "Generate example data ************************************"
echo "**********************************************************"
Rscript data-raw/generate_data.R
git commit -a -m "Automatic commit: Update example data (landsat, rlogo, srtm, lsat)"

## Back to master
git checkout master
git checkout example-data data/rlogo.rda data/srtm.rda  data/lsat.rda inst/external/landsat inst/external/trainingPolygons.rds
Rscript -e "library(devtools); library(methods); document()"
git commit -a -m "Automatic commit: Pull example data from branch example-data"

## Website
echo "\n**********************************************************"
echo "Build website documentation ******************************"
echo "**********************************************************"
git checkout gh-pages
Rscript rstbx-docu/build_docu.R
git commit -a -m "Automatic commit: Update gh-pages package documentation"
git checkout master

echo "\n**********************************************************"
echo "R CMD check **********************************************"
echo "**********************************************************"
#Rscript -e "library(devtools); library(methods);  check(); build_win(version = c('R-release', 'R-devel'))" 
#Valgrind times out --> run locally
#Rscript -e "library(rhub); library(methods);  check(platform='debian-gcc-release', valgrind = TRUE)" &> ${HOME}/RHub_RStoolbox_check_with_valgrind.log
Rscript -e "library(rhub); library(methods);  check(platform=c('debian-gcc-release', 'debian-gcc-devel', 'ubuntu-gcc-devel',  'windows-x86_64-oldrel', 'windows-x86_64-release', 'windows-x86_64-devel','linux-x86_64-rocker-gcc-san'))" &> ${HOME}/RHub_RStoolbox_checks.log

