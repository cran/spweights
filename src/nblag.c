/* Copyright 2001 by Roger S. Bivand. 
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


SEXP nblagC(SEXP n, SEXP maxlag, SEXP card, SEXP nbs, SEXP sptimes)
{
	int i, j, n1=INTEGER_POINTER(n)[0], maxlag1=INTEGER_POINTER(maxlag)[0];
	int sptimes1=INTEGER_POINTER(sptimes)[0];
	int size, p_new=0, p_active=0, p_already=0, k, l, pc=0;
	int *new, *active, *already, *work;
	SEXP lags;
	
	PROTECT(lags = NEW_LIST(maxlag1)); pc++;
	for (i=0; i < maxlag1; i++)
		SET_VECTOR_ELT(lags, i, NEW_LIST(n1));
	for (i=0; i < n1; i++) {
	    if (INTEGER_POINTER(card)[i] == 1 &&
		INTEGER_POINTER(VECTOR_ELT(nbs, i))[0] ==0) {
		SET_VECTOR_ELT(VECTOR_ELT(lags, 0), i, NEW_INTEGER(1));
		INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(lags, 0), i))[0] = 0;
	    } else {
		SET_VECTOR_ELT(VECTOR_ELT(lags, 0), i,
		    NEW_INTEGER(INTEGER_POINTER(card)[i]));
		for (j=0; j < INTEGER_POINTER(card)[i]; j++)
		    INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(lags, 0), i))[j] =
			INTEGER_POINTER(VECTOR_ELT(nbs, i))[j];
		}
	}
	for (i=0, size=0; i < n1; i++) size += INTEGER_POINTER(card)[i];
	
	new = (int *) R_alloc((long) sptimes1*size, sizeof(int));
	active = (int *) R_alloc((long) sptimes1*size, sizeof(int));
	already = (int *) R_alloc((long) sptimes1*size, sizeof(int));
	work = (int *) R_alloc((long) sptimes1*size, sizeof(int));

	for (i=0; i < n1; i++) {
	    already[p_already++] = i+ROFFSET;
	    if(p_already >= size) 
		error("Number of neighbours exceeds array size, increase sptimes\n");
	    if(p_new+INTEGER_POINTER(card)[i] >= size) 
		error("Number of neighbours exceeds array size, increase sptimes\n");
	    for (j=0; j < INTEGER_POINTER(card)[i]; j++)
		new[p_new++] =
		    INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(lags, 0), i))[j];
	    for (l=1; l < maxlag1; l++) {
		if(INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(lags, l-1), i))[0] != 0) {
	    	    if(p_already+p_new >= size) 
			error("Number of neighbours exceeds array size, increase sptimes\n");
		    for (j=0; j < p_new; j++)
			already[p_already++] = new[j];
		    for (j=0, p_active=0; j < p_new; j++)
			active[p_active++] = new[j];
		    p_new = 0;
		    for (j=0; j < INTEGER_POINTER(card)[i]; j++) {
	    		if(p_new+INTEGER_POINTER(card)[active[j]-ROFFSET] 
			    >= size) 
				error("Number of neighbours exceeds array size, increase sptimes\n");
			for (k=0; k < INTEGER_POINTER(card)[active[j]-ROFFSET];
			    k++)  new[p_new++] = 
				INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(lags, 0),
				active[j]-ROFFSET))[k];
		    }
		    R_isort(new, p_new);
		    R_isort(already, p_already);
		    for (j=0, p_active=0; j < p_new; j++) {
			if(j == 0) active[p_active++] = new[j];
			else if(new[j] != active[p_active-1])
			    active[p_active++] = new[j];
		    }
		    for (j=0; j < p_active; j++) work[j] = 0;
		    
		    for (j=0, k=0; j < p_already; j++) {
			while(active[k] > already[j]) {
			    j++;
			}
			while(active[k] < already[j]) {
			    k++;
			}
			if (active[k] == already[j]) {
			    work[k] = 1;
			    k++;
			}
		    }
		    for (j=0, p_new=0; j < p_active; j++) 
			if(work[j] == 0) new[p_new++] = active[j];
		    if (p_new > 0) {
		    	SET_VECTOR_ELT(VECTOR_ELT(lags, l), i,
			    NEW_INTEGER(p_new));
		    	for (j=0; j < p_new; j++)
			    INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(lags, l), i))[j] = new[j];
		    } else {
		    	SET_VECTOR_ELT(VECTOR_ELT(lags, l), i, NEW_INTEGER(1));
			INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(lags, l), i))[0] = 0;
		    }
		} else {
		     SET_VECTOR_ELT(VECTOR_ELT(lags, l), i, NEW_INTEGER(1));
		     INTEGER_POINTER(VECTOR_ELT(VECTOR_ELT(lags, l), i))[0] = 0;
		}
	    }
	    p_new = 0;
	    p_active = 0;
	    p_already = 0;
	}
	UNPROTECT(pc);
	return(lags);
}


