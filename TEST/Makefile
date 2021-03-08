
PATSHOMEQ="$(PATSHOME)"
PATSCONTRIBQ="$(PATSCONTRIB)"

RMF=rm -f

PATSCC=$(PATSHOMEQ)/bin/patscc

PATSFLAGS=-DATS_MEMALLOC_LIBC
CFLAGS=-O2
LFLAGS=

all:: test01
test01: test01.dats ; $(PATSCC) $(PATSFLAGS) $(CFLAGS) -o $@ $< $(LFLAGS) 
cleanall:: ; $(RMF) test01

all:: test02
test02: test02.dats ; $(PATSCC) $(PATSFLAGS) $(CFLAGS) -o $@ $< $(LFLAGS) 
cleanall:: ; $(RMF) test02

all:: test03
test03: test03.dats ; $(PATSCC) $(PATSFLAGS) $(CFLAGS) -o $@ $< $(LFLAGS) 
cleanall:: ; $(RMF) test03

all:: test04
test04: test04.dats ; $(PATSCC) $(PATSFLAGS) $(CFLAGS) -o $@ $< $(LFLAGS) 
cleanall:: ; $(RMF) test04

cleanall:: ; $(RMF) *_dats.c

clean:: cleanall
