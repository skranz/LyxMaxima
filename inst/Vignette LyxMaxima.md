RMaxima
===============================================================

**Date: 2013-08-19**

**Author: Sebastian Kranz**

An R based GUI to use Maxima as computer algebra system for Lyx. Only tested for windows. Maxima must be installed seperately and be available in PATH.
             
## Installation under Windows

Sorry, so far the package is only tested under windows. You first must install Maxima separately. You should also add the /bin directory of Maxima to the Windows PATH variable.

You can install the package from GitHub in the usual fashion.

```r
library(devtools)
install_github(repo = "restorepoint", username = "skranz")
install_github(repo = "stringtools", username = "skranz")
install_github(repo = "RMaxima", username = "skranz")
install_github(repo = "LyxMaxima", username = "skranz")

```


## Starting the GUI


```r
library(LyxMaxima)

# Should open a cmd window (Maxima pipe) and a tcltk window (GUI)
start.LyxMaxima(init = TRUE, LYXCAS.PATH = "C:/libraries/LyxMaxima/LyxMaxima/")

# Test a simplification of latex code
lyx.go(txt = "5+\\frac{x^2+x^2}{1+\\frac{2}{x*5}}*2")

```


## Variable representations

 Variables can have 3 main representations:
```
 1. Latex original : y, u_{1}(x_{L}), \alpha_{1}, \alpha_{\beta}^{2}(\gamma,x,\delta)
 2. Latex canonical: y, u_{1L}      , \alpha_{1}, \alpha_{\beta2\gamma x\delta}
 3. Maxima         : y, u_1L        , alpha_1   , alpha_beta2gammaxdelta
```
 The user has to specify a canonical form for every variable that appears and is not
 in canonical form in the section #DEFINE
 We can assign several Latex original versions to one Latex canonical. Example: 
```
u_{1}^{L} and u_{1}(x_{L}) can both have form u_{1L}
```
 But every Latex original must have a unique canonical. The program creates a one-to-one mapping between Latex canonical and Maxima 

## Internal structure

### Types of code
 
 - ly:  Lyx code, a latex expression
 - ma:  maxima code as you would type it in Maxima
 - mao: maxima output code. Maxima code that will write results into get.mx()$outfile, which can be parsed afterwards
 - matex: Maxima code mixed with latex expressions. Latex expressions are within $ $ tags. The maxima code cannot use $, but one can use § as a substitute.
 
### Workflow of the conversion

 1. User copy lyx code into the clipboard and presses a GUI button. The user may copy direct lyx code or latex source, which still contains $ $).
 2. The functions lyx.xxx will be called by the button handlers.
 3. The functions ly.mao.xxx transform the lyx code into mao code. Latex output will be wrapped in a Maxima call to tex().
 4. Call to send.to.maxima to run the mao code in Maxima. Maxima generates an outfile (location: get.mx()$outfile). The outfile contains the Maxima output from the expressions that shall be evaluated plus some tags that help parsing.
 5. read.maxima.out parses the outfile 
 6. convert.maxima.output converts the math output wrapped in tex() into proper latex.
 7. The result is copied back to the clipboard and can be pasted by the user into R 

We use many functions from RMaxima and stringtools. See the description there for details
