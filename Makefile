
PATSHOMEQ="$(PATSHOME)"
PATSCONTRIBQ="$(PATSCONTRIB)"

RMF=rm -f

PATSCC=$(PATSHOMEQ)/bin/patscc

PATSFLAGS=-DATS_MEMALLOC_LIBC
CFLAGS=-O2 -pthread
LFLAGS=

all:: httpsrv
httpsrv: httpsrv.dats ; $(PATSCC) $(PATSFLAGS) $(CFLAGS) -o $@ $< $(LFLAGS) 
cleanall:: ; $(RMF) httpsrv

cleanall:: ; $(RMF) *_dats.c

clean:: cleanall
