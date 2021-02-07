
PATSHOMEQ="$(PATSHOME)"
PATSCONTRIBQ="$(PATSCONTRIB)"

RMF=rm -f

PATSCC=$(PATSHOMEQ)/bin/patscc

PATSFLAGS=-DATS_MEMALLOC_LIBC
CFLAGS=-O2
LFLAGS=

all:: http0
http0: http0.dats ; $(PATSCC) $(PATSFLAGS) $(CFLAGS) -o $@ $< $(LFLAGS) 
cleanall:: ; $(RMF) http0

cleanall:: ; $(RMF) *_dats.c

clean:: cleanall
