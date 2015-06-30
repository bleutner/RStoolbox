#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix spectralIndicesCpp(NumericMatrix& x, CharacterVector& indices,
		const int redBand, const int blueBand, const int greenBand,  const int nirBand, const int mirBand, const int swirBand,
		const double L, const double s, const double G, const double C1, const double C2, const double Levi) {

	NumericVector blue, green, red, nir, swir, mir;

	if(blueBand != NA_INTEGER)  blue = x(_,blueBand - 1);
	if(greenBand != NA_INTEGER)  green = x(_,greenBand - 1) ;
	if(redBand != NA_INTEGER)  red = x(_,redBand - 1);
	if(nirBand != NA_INTEGER)  nir = x(_,nirBand - 1);
	if(mirBand != NA_INTEGER)  mir = x(_,mirBand - 1);
	if(swirBand != NA_INTEGER)  swir = x(_,swirBand - 1);

	int nind = indices.size();
	int nsamp = x.nrow();
	NumericMatrix out(nsamp, nind);
	for(int j = 0; j < nind; ++j) {

		if(indices[j] == "DVI") {
			out(_,j) = (s * nir - red);
		}
		else if(indices[j] == "CTVI") {
			// Corrected transformed vegetation index
			// Perry and Lautenschlager 1984
			NumericVector np = (nir-red)/(nir+red) + 0.5;
			out(_,j) = np / sqrt(abs(np));
		}
		else if(indices[j] == "EVI") {
			// Enhanced vegetation index
			// Huete et al 1990
			out(_,j) = G * ((nir - red) / (nir + C1 * red - C2 * blue + Levi));
		}
		else if(indices[j] == "GEMI") {
			out(_,j) = (((pow(nir,2) - pow(red,2)) * 2 + (nir * 1.5) + (red * 0.5) ) /
					(nir + red + 0.5)) * (1 - ((((pow(nir,2) - pow(red,2)) * 2 + (nir * 1.5) + (red * 0.5) ) /
							(nir + red + 0.5)) * 0.25)) - ((red - 0.125) / (1 - red));
		}
		else if(indices[j] == "LSWI") {
			out(_,j) = (nir-swir) / (nir+swir);
			out(_,j) = ifelse(out(_,j) > 1 | out(_,j) < -1, NA_REAL, out(_,j));
		}
		else if(indices[j] == "MSAVI") {
			// Modified soil adjusted vegetation index
			out(_,j) = nir + 0.5 - (0.5 * sqrt(pow(2 * nir + 1, 2) - 8 * (nir - (2 * red))));
		}
		else if(indices[j] == "MSAVI2") {
			// Modified soil adjusted vegetation index 2
			out(_,j) = (2 * (nir + 1) - sqrt(pow(2 * nir + 1, 2) - 8 * (nir - red))) / 2;
		}
		else if(indices[j] == "MSI") {
			// Moisture stress index
			out(_,j) =  mir / nir;
		}
		else if(indices[j] == "NDVI") {
			//Normalized difference vegetation index
			out(_,j) = (nir - red) / (nir + red);
			out(_,j) = ifelse(out(_,j) > 1 | out(_,j) < -1, NA_REAL, out(_,j));
		}
		else if(indices[j] == "NDWI") {
			// Normalized difference water index
			out(_,j) = (green - nir)/(green + nir);
			out(_,j) = ifelse(out(_,j) > 1 | out(_,j) < -1, NA_REAL, out(_,j));
		}
		else if(indices[j] == "RVI") {
			// Ratio Vegetation Index
			// Richardson and Wiegand 1977
			out(_,j) = red / nir;
		}
		else if(indices[j] == "NRVI") {
			// Normalized Ratio Vegetation Index
			// Baret and Guyot 1991
			NumericVector rvi = red / nir;
			out(_,j) = (rvi - 1)/(rvi + 1);
		}
		else if(indices[j] == "SAVI") {
			// Soil adjusted vegetation index
			// Huete1988
			out(_,j) = (nir - red) * (1 + L) / (nir + red + L);
		}
		else if(indices[j] == "SLAVI") {
			out(_,j) = nir / (red + mir);
		}
		else if(indices[j] == "SR") {
			// Simple ratio
			out(_,j) = nir / red;
		}
		else if(indices[j] == "TVI") {
			// Transformed Vegetation Index
			// Deering 1975
			out(_,j) = sqrt((nir-red)/(nir+red) + 0.5);
		}
		else if(indices[j] == "TTVI") {
			// Thiam's Transformed Vegetation Index
			// Thiam 1997
			out(_,j) = sqrt(abs((nir-red)/(nir+red) + 0.5));
		}
		else if(indices[j] == "WDVI") {
			out(_,j) = nir - s * red;
		}

	}

	return out;
}



