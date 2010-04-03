\name{genChild}
\alias{parseArrayLine}
\alias{genChild}
\alias{getArray}
\alias{symb2oa}
\alias{oa2symb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Internal utility functions to support automatic creation of child arrays
  from entries of the data frame oacat
}
\description{
The functions are used internally for creating the child arrays listed in
data frame \code{oacat} from the parent arrays that come with \pkg{DoE.base}
(or using full factorials).
}
\usage{
parseArrayLine(array.line)
genChild(array.list)
getArray(nbRuns, descr)
symb2oa(nbRuns, descr)
oa2symb(name)
}

\arguments{
  \item{array.line}{ a row from data frame \code{\link{oacat}}}
  \item{array.list}{ the output from function \code{parseArrayLine}}
  \item{nbRuns}{ the number of runs of the array to be received}
  \item{descr}{ a character string containing the description of the array
      to be retrieved, of the form \cr
      \code{nl1~fl1;nl2~fl2; ...}, where \code{nl} stands for the number of levels
      and \code{fl} for their respective frequency; the string may (but need not)
      contain a trailing semicolon}
  \item{name}{ name of an array according to the naming conventions in
       \code{\link{oacat}}}
}
\details{
  Function \code{parseArrayLine} transforms information from a row of \code{oacat}
  into a list format digestible by function \code{genChild}.

  Function \code{genChild} creates a child array from the appropriate information
  provided by function \code{parseArrayLine}.

  Function \code{getArray} retrieves a stored orthogonal array based on the list
  information it receives.
  
  Functions \code{symb2oa} and \code{oa2symb} can be used for
  switching between design names from data frame \code{\link{oacat}}
  and list type information used by functions internally. Note that the result
  from \code{oa2symb} is not sufficient to get back to the \code{oa} representation,
  but needs the number of runs in addition.
}

\value{
  \code{parseArrayLine} returns a list with design and lineage description in symbolic form.
  
  \code{genChild} and \code{getArray} return an array matrix of class \code{\link{oa}}.
  
  \code{symb2oa} and \code{oa2symb} return a character string.
}
\author{ Boyko Amarov and Ulrike Groemping }
\seealso{ See also \code{\link{oacat}}, \code{\link{oa}}
    }
\keyword{ design }
\keyword{ array }
