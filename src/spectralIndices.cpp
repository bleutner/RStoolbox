#include<Rcpp.h>
#include <string>
#include <vector>

using namespace Rcpp;
using namespace std;

unordered_map<string, vector<double>> variables;

void setVariable(const string &name, const NumericVector &rcppVec) {
    variables[name] = vector<double>(rcppVec.begin(), rcppVec.end()); // Convert and assign
}

// Token types
enum TokenType { NUMBER, VARIABLE, OPERATOR, LPAREN, RPAREN, END };

struct Token {
    TokenType type;
    string value;
};

// Tokenizer
class Tokenizer {
    string expr;
    size_t pos;

public:
    Tokenizer(const string &expression) : expr(expression), pos(0) {}

    Token nextToken() {
        while (pos < expr.size() && isspace(expr[pos])) pos++;
        if (pos >= expr.size()) return {END, ""};

        if (isdigit(expr[pos]) || expr[pos] == '.') {
            size_t start = pos;
            while (pos < expr.size() && (isdigit(expr[pos]) || expr[pos] == '.')) pos++;
            return {NUMBER, expr.substr(start, pos - start)};
        }

        if (isalpha(expr[pos])) {
            size_t start = pos;
            while (pos < expr.size() && isalnum(expr[pos])) pos++;
            return {VARIABLE, expr.substr(start, pos - start)};
        }

        if (strchr("+-*/()", expr[pos])) {
            return {expr[pos] == '(' ? LPAREN : expr[pos] == ')' ? RPAREN : OPERATOR, string(1, expr[pos++])};
        }

        throw runtime_error("Unexpected character in expression");
    }
};

// Expression Evaluator
class Evaluator {
    Tokenizer tokenizer;
    Token currentToken;
    size_t vectorSize;

    void nextToken() { currentToken = tokenizer.nextToken(); }

    vector<double> expandScalar(double scalar) {
        return vector<double>(vectorSize, scalar);
    }

    vector<double> getValue() {
        if (currentToken.type == NUMBER) {
            double val = stod(currentToken.value);
            nextToken();
            return expandScalar(val);
        }
        if (currentToken.type == VARIABLE) {
            if (variables.find(currentToken.value) == variables.end())
                throw runtime_error("Unknown variable: " + currentToken.value);
            vector<double> val = variables[currentToken.value];
            nextToken();
            return val;
        }
        throw runtime_error("Expected number or variable");
    }

    vector<double> factor() {
        if (currentToken.type == LPAREN) {
            nextToken();
            vector<double> result = expression();
            if (currentToken.type != RPAREN)
                throw runtime_error("Mismatched parentheses");
            nextToken();
            return result;
        }
        return getValue();
    }

    vector<double> term() {
        vector<double> result = factor();
        while (currentToken.type == OPERATOR && (currentToken.value == "*" || currentToken.value == "/")) {
            string op = currentToken.value;
            nextToken();
            vector<double> right = factor();

            if (result.empty() || right.empty()) throw runtime_error("Vector size mismatch.");

            if (result.size() == 1) result = expandScalar(result[0]);
            if (right.size() == 1) right = expandScalar(right[0]);

            for (size_t i = 0; i < vectorSize; i++) {
                if (op == "*") result[i] *= right[i];
                else if (op == "/") {
                    if (right[i] == 0) throw runtime_error("Division by zero");
                    result[i] /= right[i];
                }
            }
        }
        return result;
    }

    vector<double> expression() {
        vector<double> result = term();
        while (currentToken.type == OPERATOR && (currentToken.value == "+" || currentToken.value == "-")) {
            string op = currentToken.value;
            nextToken();
            vector<double> right = term();

            if (result.empty() || right.empty()) throw runtime_error("Vector size mismatch.");

            if (result.size() == 1) result = expandScalar(result[0]);
            if (right.size() == 1) right = expandScalar(right[0]);

            for (size_t i = 0; i < vectorSize; i++) {
                if (op == "+") result[i] += right[i];
                else result[i] -= right[i];
            }
        }
        return result;
    }

public:
    Evaluator(const string &expr, size_t size) : tokenizer(expr), vectorSize(size) { nextToken(); }
    vector<double> eval() { return expression(); }
};

//[[Rcpp::export]]
NumericMatrix spectralIndicesCpp(NumericMatrix x, CharacterVector indices,
		const int redBand,  const int blueBand, const int greenBand, const int nirBand,
		const int redEdge1Band, const int redEdge2Band,  const int redEdge3Band,
		const int swir1Band, const int swir2Band, const int swir3Band,
		int maskLayer, const int maskValue,
		const double L,  const double s, const double G, const double C1,
		const double C2, double Levi, const double swir2ccc, const double swir2cdiff, const double sf,
		CharacterVector formulas) {

	const int nind = indices.size();
	const int nsamp = x.nrow();
	const int nc = x.ncol();

	NumericMatrix out(nsamp, nind);
	NumericVector blue, green, red, redEdge1, redEdge2, redEdge3, nir, swir1, swir2, swir3;

	// Apply mask layer
	if(!IntegerVector::is_na(maskLayer)){
		maskLayer-=1 ;
		std::vector<int> m;
		m.reserve(nsamp);
		if(IntegerVector::is_na(maskValue)){
			for(int i = 0; i < nsamp; i++) {
				if (ISNAN(x(i, maskLayer))) m.push_back(i);
			}
		} else {
			for(int i = 0; i < nsamp; i++) {
				if (x(i, maskLayer) == maskValue) m.push_back(i);
			}
		}

		for(int j = 0; j < nc; j++) {
			if (j == maskLayer) continue;
			for(int i = 0; i < (int)m.size(); i++) {
				x(m[i],j) = NA_REAL;
			}
		}
	}

	if(blueBand  != NA_INTEGER)     blue = x(_,blueBand - 1) / sf;
	if(greenBand != NA_INTEGER)    green = x(_,greenBand - 1) / sf;
	if(redBand   != NA_INTEGER)      red = x(_,redBand - 1) / sf;
	if(redEdge1Band != NA_INTEGER) redEdge1 = x(_,redEdge1Band - 1) / sf;
	if(redEdge2Band != NA_INTEGER) redEdge2 = x(_,redEdge2Band - 1) / sf;
	if(redEdge3Band != NA_INTEGER) redEdge3 = x(_,redEdge3Band - 1) / sf;
	if(nirBand   != NA_INTEGER)      nir = x(_,nirBand - 1) / sf;
	if(swir1Band != NA_INTEGER)    swir1 = x(_,swir1Band - 1) / sf;
	if(swir2Band != NA_INTEGER)    swir2 = x(_,swir2Band - 1) / sf;
	if(swir3Band != NA_INTEGER)    swir3 = x(_,swir3Band - 1) / sf;

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
		else if(indices[j] == "CLG") {
			// Green cholorphyl index
			// Wu et al 2012
			out(_,j) = (redEdge3)/(green)-1;
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "CLRE") {
			// RedEdge cholorphyl index
			// Clevers and Gitelson 2013
			out(_,j) = (redEdge3)/(redEdge1)-1;
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "EVI") {
			// Enhanced vegetation index
			// Huete et al 1990
			out(_,j) = G * ((nir - red) / (nir + C1 * red - C2 * blue + Levi));
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "EVI2") {
			// Two-band Enhanced vegetation index
			// Jiang et al 2008
			out(_,j) = G * ((nir - red) / (nir + 2.4 * red ));
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "GEMI") {
			out(_,j) = (((pow(nir, 2) - pow(red, 2)) * 2.0 + (nir * 1.5) + (red * 0.5) ) /
					(nir + red + 0.5)) * (1 - ((((pow(nir,2) - pow(red,2)) * 2 + (nir * 1.5) + (red * 0.5) ) /
							(nir + red + 0.5)) * 0.25)) - ((red - 0.125) / (1 - red));
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}	
		else if(indices[j] == "GNDVI") {
			// green Normalized diff vegetation index: -> more sensitive to cholorphyll than ndvi
			// Gitelson, A., and M. Merzlyak. "Remote Sensing of Chlorophyll Concentration in Higher Plant Leaves." Advances in Space Research 22 (1998): 689-692
			out(_,j) = (nir - green)/( nir + green);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "KNDVI") {
			// kernel Normalized difference vegetation index
			// Camps Valls (2021) A unified vegetation index forquantifying theterrestrial biosphere
			// This implementation assumes sigma = mean(nir,red), which is the setting suggested by the authors
			// Other values for sigma are not currently implemented.
			out(_,j) = tanh(pow((nir - red) / (nir + red), 2));
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < 0), NA_REAL, out(_,j));
		}
		else if(indices[j] == "MNDWI") {
			// Modified Normalised Difference Water Index
			out(_,j) = (green-swir2) / (green+swir2);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "MTCI") {
			// Meris Terrestrial Chlorophyll Index
			// Clevers and Gitelson 2013, Dash and Curran 2004
			out(_,j) = (redEdge2-redEdge1) / (redEdge1-red);
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "MCARI") {
			// Modified Chlorophyll Absorption ratio index
			// Daughtery et al. 2000
			out(_,j) = (redEdge1 - red - redEdge1 + green) * (redEdge1 / red);
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
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
		else if(indices[j] == "NDVIC") {
			//Normalized difference vegetation index
			out(_,j) = (nir - red) / (nir + red) * (1 - (swir2 - swir2ccc)/swir2cdiff);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0), NA_REAL, out(_,j));
		}
		else if(indices[j] == "NBRI"){
			// Normalised Burn Ratio Index
			out(_,j) = (nir - swir3) / (nir + swir3);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0), NA_REAL, out(_,j));
		}
		else if(indices[j] == "NDVI") {
			//Normalized difference vegetation index
			out(_,j) = (nir - red) / (nir + red);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0), NA_REAL, out(_,j));
		}
		else if(indices[j] == "NDWI") {
			// Normalized difference water index
			// McFeeters 1996
			out(_,j) = (green - nir)/(green + nir);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "NDWI2") {
			// Normalized difference water index (also known as Normalized Difference Moisture Index (NDBI)
			// Gao 1996, Chen 2005
			// a.k.a. LSWI
			out(_,j) = (nir - swir2)/(nir + swir2);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "NDREI1") {
			// Normalized difference red edge index
			// Gitelson and Merzlyak 1994
			out(_,j) = (redEdge2 - redEdge1)/(redEdge2 + redEdge1);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "NDREI2") {
			// Normalized difference red edge index 2
			// Barnes et al 2000
			out(_,j) = (redEdge3 - redEdge1)/(redEdge3 + redEdge1);
			out(_,j) = ifelse(is_na(out(_,j)) | (out(_,j) > 1.0) | (out(_,j) < -1.0) , NA_REAL, out(_,j));
		}
		else if(indices[j] == "NRVI") {
			// Normalized Ratio Vegetation Index
			// Baret and Guyot 1991
			NumericVector rvi = red / nir;
			out(_,j) = (rvi - 1.0)/(rvi + 1.0);
			out(_,j) = ifelse(is_na(out(_,j)), NA_REAL, out(_,j));
		}
		else if(indices[j] == "REIP") {
			// Ratio Vegetation Index
			// Richardson and Wiegand 1977
			out(_,j) = 0.705+0.35*((red+redEdge3)/(2-redEdge1))/(redEdge2-redEdge1) ;
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
			out(_,j) = ((swir2 - red) / (swir2 + red + L)) * (1.0 + L) - (swir3 / 2.0);
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
		else {
		    std::string formula = Rcpp::as<std::string>(formulas[j]);
		    //std::string formula = "red * red";

		    size_t vec_size = 0;
            bool red_present = formula.find("red") != std::string::npos;
            bool blue_present = formula.find("blue") != std::string::npos;
            bool green_present = formula.find("green") != std::string::npos;
            bool redEdge1_present = formula.find("redEdge1") != std::string::npos;
            bool redEdge2_present = formula.find("redEdge2") != std::string::npos;
            bool redEdge3_present = formula.find("redEdge3") != std::string::npos;
            bool nir_present = formula.find("nir") != std::string::npos;
            bool swir1_present = formula.find("swir1") != std::string::npos;
            bool swir2_present = formula.find("swir2") != std::string::npos;
            bool swir3_present = formula.find("swir3") != std::string::npos;

		    if (red_present) vec_size = red.size();
            else if (blue_present) vec_size = blue.size();
            else if (green_present) vec_size = green.size();
            else if (redEdge1_present) vec_size = redEdge1.size();
            else if (redEdge2_present) vec_size = redEdge2.size();
            else if (redEdge3_present) vec_size = redEdge3.size();
            else if (nir_present) vec_size = nir.size();
            else if (swir1_present) vec_size = swir1.size();
            else if (swir2_present) vec_size = swir2.size();
            else if (swir3_present) vec_size = swir3.size();
            else {
                Rcpp::stop("No valid variable found in the formula.");
            }

            std::vector<double> vars;

            std::vector<std::string> valid_var_names = {
                "blue", "green", "red", "redEdge1", "redEdge2", "redEdge3", "nir", "swir1", "swir2", "swir3"
            };

		    std::vector<double> result(vec_size);

            setVariable("blue", blue);
            setVariable("green", green);
            setVariable("red", red);
            setVariable("redEdge1", redEdge1);
            setVariable("redEdge2", redEdge2);
            setVariable("redEdge3", redEdge3);
            setVariable("nir", nir);
            setVariable("swir1", swir1);
            setVariable("swir2", swir2);
            setVariable("swir3", swir3);

            Evaluator evaluator(formula.c_str(), vec_size);
            vector<double> true_result = evaluator.eval();

            Rcpp::NumericVector r_result(true_result.begin(), true_result.end());
            out(_,j) = r_result;

		}

	}

	return out;
}


