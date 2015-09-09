#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix spectralIndicesCpp(NumericMatrix x, CharacterVector indices,
		 const int redBand,  const int blueBand, const int greenBand, const int nirBand,
		 const int swir2Band, const int swir1Band,
		 double L,  double s,  double G,  double C1,  double C2,  double Levi) {

	int nind = indices.size();
	int nsamp = x.nrow();

/***
	for(int ro = 0; ro < nsamp; ++ro) {
		for(int co = 0; co < nind; ++co) {
			if(ISNAN(x(ro,co))) x(ro,co) = NA_REAL;
		}
	}
***/
	NumericMatrix out(nsamp, nind);
	NumericVector blue, green, red, nir, swir1, swir2;

	if(blueBand != NA_INTEGER)    blue = x(_,blueBand - 1);
	if(greenBand != NA_INTEGER)  green = x(_,greenBand - 1);
	if(redBand != NA_INTEGER)    red = x(_,redBand - 1);
	if(nirBand != NA_INTEGER)      nir = x(_,nirBand - 1);
	if(swir1Band != NA_INTEGER)  swir1 = x(_,swir1Band - 1);
	if(swir2Band != NA_INTEGER)  swir2 = x(_,swir2Band - 1);


	for(int j = 0; j < nind; ++j) {

		if(indices[j] == "DVI") {
			// Difference vegetation index
			out(_,j) = (s * nir - red);
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "CTVI") {
			// Corrected transformed vegetation index
			// Perry and Lautenschlager 1984
			NumericVector np = (nir-red)/(nir+red) + 0.5;
			out(_,j) = np / sqrt(abs(np));
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "EVI") {
			// Enhanced vegetation index
			// Huete et al 1990
			out(_,j) = G * ((nir - red) / (nir + C1 * red - C2 * blue + Levi));
			out(_,j) = ifelse( is_na(out(_,j)), NA_REAL, out(_,j));

		}
		else if(indices[j] == "GEMI") {
			out(_,j) = (((pow(nir, 2) - pow(red, 2)) * 2.0 + (nir * 1.5) + (red * 0.5) ) /
					(nir + red + 0.5)) * (1 - ((((pow(nir,2) - pow(red,2)) * 2 + (nir * 1.5) + (red * 0.5) ) /
							(nir + red + 0.5)) * 0.25)) - ((red - 0.125) / (1 - red));
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "LSWI") {
                        // Land surface water index
			out(_,j) = (nir-swir1) / (nir+swir1);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "MNDWI") {
                        // Modified Normalised Difference Water Index
			out(_,j) = (green-swir1) / (green+swir1);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "MSAVI") {
			// Modified soil adjusted vegetation index
			out(_,j) = nir + 0.5 - (0.5 * sqrt(pow(2.0 * nir + 1.0, 2) - 8.0 * (nir - (2.0 * red))));
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "MSAVI2") {
			// Modified soil adjusted vegetation index 2
			out(_,j) = (2.0 * nir + 1.0 - sqrt(pow(2.0 * nir + 1.0, 2) - 8.0 * (nir - red))) / 2.0;
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
	}
/***
		else if(indices[j] == "MSI") {
			// Moisture stress index
			out(_,j) =  swir2 / nir;
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));

	}
***/	
		else if(indices[j] == "NBRI"){
                        // Normalised Burn Ratio Index
			out(_,j) = (nir - swir2) / (nir + swir2);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0), NA_REAL, out(_,j));
		}
		else if(indices[j] == "NDVI") {
			//Normalized difference vegetation index
			out(_,j) = (nir - red) / (nir + red);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0), NA_REAL, out(_,j));
		}
		else if(indices[j] == "NDWI") {
			// Normalized difference water index
			out(_,j) = (green - nir)/(green + nir);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
	        else if(indices[j] == "NRVI") {
			// Normalized Ratio Vegetation Index
			// Baret and Guyot 1991
			NumericVector rvi = red / nir;
			out(_,j) = (rvi - 1.0)/(rvi + 1.0);
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "RVI") {
			// Ratio Vegetation Index
			// Richardson and Wiegand 1977
			out(_,j) = red / nir;
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
	
		else if(indices[j] == "SATVI"){
                        // Soil adjusted total vegetation index
			out(_,j) = ((swir1 - red) / (swir1 + red + L)) * (1.0 + L) - (swir2 / 2.0);
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
                }
		else if(indices[j] == "SAVI") {
			// Soil adjusted vegetation index
			// Huete1988
			out(_,j) = (nir - red) * (1.0 + L) / (nir + red + L);
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));

		}
		else if(indices[j] == "SLAVI") {
			out(_,j) = nir / (red + swir2);
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));

		}
		else if(indices[j] == "SR") {
			// Simple ratio index
			out(_,j) = nir / red;
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));

		}
		else if(indices[j] == "TVI") {
			// Transformed Vegetation Index
			// Deering 1975
			out(_,j) = sqrt((nir-red)/(nir+red) + 0.5);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > sqrt(1.5)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "TTVI") {
			// Thiams Transformed Vegetation Index
			// Thiam 1997
			out(_,j) = sqrt(abs((nir-red)/(nir+red) + 0.5));
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "WDVI") {
			out(_,j) = nir - s * red;
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}

	}

	return out;
}


