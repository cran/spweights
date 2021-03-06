/* Copyright 2000 by Roger S. Bivand. 
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
**/

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Applic.h>
#define ROFFSET 1

SEXP listw2sn(SEXP nbs, SEXP wts)
{
	int i, ii, j=0, n, pc=0;
	SEXP ans;
	double *card;

	n = LENGTH(nbs);
	card = (double *) R_alloc(n, sizeof(double));
	for (i=0; i < n; i++) {
		card[i] = LENGTH(VECTOR_ELT(nbs, i));
		j += card[i];
	}
	PROTECT(ans = NEW_LIST(3)); pc++;
	SET_VECTOR_ELT(ans, 0, NEW_INTEGER(j));
	SET_VECTOR_ELT(ans, 1, NEW_INTEGER(j));
	SET_VECTOR_ELT(ans, 2, NEW_NUMERIC(j));

	for (i=0, ii=0; i < n; i++) {
	    for (j=0; j < card[i]; j++) {
		INTEGER_POINTER(VECTOR_ELT(ans, 0))[ii] = i+ROFFSET;
	        INTEGER_POINTER(VECTOR_ELT(ans, 1))[ii] = 
		    INTEGER_POINTER(VECTOR_ELT(nbs, i))[j]; 
		NUMERIC_POINTER(VECTOR_ELT(ans, 2))[ii] = 
		    NUMERIC_POINTER(VECTOR_ELT(wts, i))[j]; 
		ii++;
	    }
	}
	UNPROTECT(pc); 
	return(ans);
}


