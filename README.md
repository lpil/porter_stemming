porter_stemming
=====

Porter Stemming Algorithm Implementation in Erlang  

Alden Dima (alden.dima@nist.gov)
National Institute of Standards and Technology,
Gaithersburg, MD
September, 2007

This software was developed at the National Institute of Standards
and Technology by employees of the Federal Government in the course
of their official duties. Pursuant to title 17 Section 105 of the
United States Code this software is not subject to copyright
protection and is in the public domain. This software is an
experimental system. NIST assumes no responsibility whatsoever for
its use by other parties, and makes no guarantees, expressed or
implied, about its quality, reliability, or any other characteristic.
We would appreciate acknowledgement if the software is used.

This software can be redistributed and/or modified freely provided
that any derivative works bear some notice that they are derived
from it, and any modified versions bear some notice that they have
been modified.


The basic philosophy behind this implementation is to map the
structure of Martin Porter's original description of his algorithm
(http://tartarus.org/~martin/PorterStemmer) as closely as possible
into Erlang. I made liberal use of Erlang's pattern matching
facility. In order for this to work, there is one quirk - the
word to be matched must be reversed before it is stemmed: "hopping"
becomes "gnippoh". This is necessary because Erlang's pattern
matching won't work with Stem ++ "ing" but instead requires
"gni" ++ Mets (where Mets is the backwards stem).  Despite this
quirk, the flipping the words allowed me to greatly simplify the
rest of the coding, almost to the point of being a tedious translation
of the textual description of the algorithm.

Thanks to Paul Black for his helpful comments.
