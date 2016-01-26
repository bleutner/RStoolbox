---
layout: post
title: "Estimating Landsat 8 Esun values"
description: ""
category: 
tags: [r,RStoolbox]
---
{% include JB/setup %}

## Estimating Landsat 8 OLI Esun Values 

Extraterrestrial irradiation (Esun) has been a frequently required variable in several classic remote sensing image correction approaches. 
It is often needed to convert/normalize radiance to reflectance and also by several simple atmospheric correction approaches like dark object subtraction (DOS).

Nowadays, much more sophisiticated atmospherically corrected products, e.g. [Landsat CDR LEDAPS](http://landsat.usgs.gov/CDR_LSR.php) data
 and correction procedures, e.g. [6SV](http://6s.ltdri.org) are available.
Nevertheless, there is still a demand for classical DOS approaches, be it due to unavailability of corrected products or methodological comparisons. 

Esun depends on the wavelength and hence the spectral response characteristics of each band and is typically provided by the data supplier. 
In the case of Landsat 8 OLI, however, USGS and NASA decided [not to publish Esun values](http://landsat.usgs.gov/ESUN.php) because they are not required for conversion to reflectance any more 

In the following, I will show how the Esun values of Landsat8  OLI bands 
were derived for usage in [`RStoolbox::radCor()`](http://bleutner.github.io/RStoolbox/rstbx-docu/radCor.html).
I used the the extraterrestrial irradiance spectrum published by Thuillier et al. (2011) and calculated the band-specific Esun 
values based on the [Landsat 8 OLI spectral response curves](http://landsat.gsfc.nasa.gov/?p=5779).
There are several irradiance spectra which could be used. 
I opted for the Thuillier spectrum which is recommended by the Committee on Earth Observation Satellites (CEOS) 
as a [reference standard](https://eocalibration.wordpress.com/2006/12/15/ceos-recommended-solar-irradiance-spectrum-for-use-in-earth-observation-applications/).


First we get the solar reference spectrum:

 
 ```r
 library(xlsx)
 library(ggplot2)
 library(reshape2)
 
 # Download data
 download.file("http://media.libsyn.com/media/npl1/Solar_irradiance_Thuillier_2002.xls",
   destfile = "thuillierSolarSpectrum.xls")
 
 # Import solar spectrum
 sol <- read.xlsx("thuillierSolarSpectrum.xls", 3)
 
 ## Plot
 ggplot(sol, aes(x = nm, y = mW.m2.nm)) + geom_line() +
    ggtitle("Thuillier (2003) Solar Irradiance Spectrum") +
    xlab("Wavelength (nm)") 
 ```
 
 ![plot of chunk esun1]({{BASE_PATH}}/assets/knitFigs/esun1-1.png) 

Next we need the relative spectral response of each Landsat 8 OLI band from NASA:


```r
## Download OLI band response curves
download.file("http://landsat.gsfc.nasa.gov/wp-content/uploads/2013/06/Ball_BA_RSR.v1.1-1.xlsx", 
  destfile = "landsat8oli.xlsx")

# Import OLI band response
bands <- c("CoastalAerosol", "Blue", "Green", "Red", "NIR", "Cirrus", "SWIR1", "SWIR2","Pan")
resp <- lapply(bands, function(x){
            data.frame(read.xlsx("landsat8oli.xlsx", sheetName = x)[,1:2])
        })
names(resp) <- bands

## Plot
respDf <- melt(resp, c("Wavelength", "BA.RSR..watts."))
ggplot(respDf, aes( x = Wavelength, y = BA.RSR..watts., colour = L1)) +
    geom_line() + ggtitle("Landsat 8 OLI spectral response")+
    xlab("Wavelength (nm)")+
    ylab("Relative Spectral Response")
```

![plot of chunk esun2]({{BASE_PATH}}/assets/knitFigs/esun2-1.png) 


To do the Esun calculations in a precise manner we will linearly interpolate the solar irradiance spectrum to 0.01 nm increments: 


```r
## Linearly interpolate solar irradiance spectrum even finer
sora <- range(sol[,1])
xnew <- seq(sora[1],sora[2], 0.01)
ynew <- resamp  <- approx(x = sol[,1], y = sol[,2], xout = xnew)[["y"]]
sol  <- cbind(xnew,ynew)
```

In the last step, we resample the spectral response curves to the resolution of the solar spectrum.
Then, the responses are normalized so that each band integrates to one and used to calculate the weighted sum across the spectrum:  


```r
Esun <- sapply(resp, function(x){
            ## Resample band response to solar spectrum
            resamp  <- approx(x = x[,1], y = x[,2], xout = sol[,1])[["y"]]
            
            ## Convert to relative weights  
            weights <- resamp / sum(resamp, na.rm = TRUE)
            
            ## Calculated weighted sum
            sum(sol[,2] * weights, na.rm = TRUE)            
        })

## Let's see what we've got:
round(Esun,2) 
```

```
## CoastalAerosol           Blue          Green            Red            NIR 
##        1895.33        2004.57        1820.75        1549.49         951.76 
##         Cirrus          SWIR1          SWIR2            Pan 
##         366.97         247.55          85.46        1723.88
```
Having calculated these values, we can now again run dark object subtraction on Landsat 8 data (see [`RStoolbox::radCor()`](http://bleutner.github.io/RStoolbox/rstbx-docu/radCor.html)). 
For the other members of the Landsat family we will stick to the published Esun values. 
Of course, using this approach Esun could also be calculated for any other sensor.

The interpolation of the solar spectrum to higher resolution is not crucial, since the spectrum is already resolved rather highly.
Yet, it can't hurt to be precise. The presented resolution was chosen by testing various resolutions for convergence in Esun values. 
Increasing the resolution even further than 0.01 nm did not result in any significant changes to Esun.  
 
A note for completeness' sake: Others have found other ways to calculate Esun: 
For example [the GRASS implementation i.landsat.toar](https://grass.osgeo.org/grass71/manuals/i.landsat.toar.html) derives band-specific Esun values based on min/max radiance and the earth-sun distance. 
The values of both approaches are similar, yet not identical!

 
## References 
 
J. A. Barsi et al (2014) “The Spectral Response of the Landsat-8 Operational Land Imager,” 
Remote Sensing 6, 10232-10251; doi:10.3390/rs61010232
 
G. Thuillier et al. (2003) 
THE SOLAR SPECTRAL IRRADIANCE FROM 200 TO 2400 nm AS MEASURED BY THE SOLSPEC SPECTROMETER FROM THE ATLAS AND EURECA MISSIONS. 
Solar Physics 214(1): 1-22
