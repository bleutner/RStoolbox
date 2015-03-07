---
layout: page
title: Remote Sensing Data Analysis in R
tagline: Supporting tagline
---
{% include JB/setup %}




## The Package

**RStoolbox** is a R package providing a wide range of tools for your every-day remote sensing processing needs. 
The available toolset covers many aspects from data import, pre-processing, data analysis, image classification and graphical display. 
RStoolbox builds upon the raster package, which makes it suitable for processing large data-sets even on smaller workstations. 
Moreover it most parts decent support for parallel processing is implemented. The package is published under GPL3.

## Getting the Code

View the most recent code on my [github](http://www.github.com/bleutner/RStoolbox) repository. 
Please be aware that during initial development function and argument names are likely to change without notice.
Only once the package is submitted to CRAN they will remain stable.

Code bundles (Note that the windows binary build can be slightly outdated).

 <ul class="tag_box" >
  <li><a href="http://www.github.com/bleutner/RStoolbox">RStoolbox on Github</a></li>
  <li><a href="https://github.com/bleutner/RStoolbox/tarball/master">Source tarball</a></li>
 <li><a href="miniCRAN/bin/windows/contrib/3.1/RStoolbox_0.0.0.9000.zip">Windows binary</a></li>
 </ul>

   
## Installing RStoolbox

To get the latest version, install RStoolbox directly from th [github repository](http://www.github.com/bleutner/RStoolbox). 
Apart from R itself, you'll also need to have r-base-dev (Linux) or [Rtools](http://cran.r-project.org/bin/windows/Rtools) (Windows) installed to compile the package.

    library(devtools)
    install_github("bleutner/RStoolbox")
    
If you are on Windows and don't have Rtools installed, you can install a pre-compiled Windows binary build like so:

	install.packages("RStoolbox", repos=c("http://bleutner.github.io/RStoolbox/miniCRAN", "http://cran.r-project.org"))     


## Feedback

I am greatfull for any feedback on RStoolbox. Please do not hesitat to report an issue [here](https://github.com/bleutner/RStoolbox/issues) if you discover bugs or have a feature idea.
If you find the package usefull I'd love to hear about it as well :-). Just drop me an email if you like: benjamin.leutner@uni-wuerzburg.de.



<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>



