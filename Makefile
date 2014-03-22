#######################################################################
#                                                                     #
#                            OCamlSpotter                             #
#                                                                     #
#                             Jun FURUSE                              #
#                                                                     #
#   Copyright 2008-2012 Jun Furuse. All rights reserved.              #
#   This file is distributed under the terms of the GNU Library       #
#   General Public License, with the special exception on linking     #
#   described in file LICENSE.                                        #
#                                                                     #
#######################################################################


# Various commands and dir
##########################
CAMLRUN= ocamlrun
OCAMLC   = ocamlc -annot -bin-annot -w A-4-9-27 -warn-error A-4-9-10-27-32-33-34-39
OCAMLOPT = ocamlopt -annot -bin-annot -w A-4-9 -warn-error A-4-9-32-33-34
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC= ocamlyacc
OCAMLLIB = $(LIBDIR)
OCAMLBIN = $(BINDIR)

# Compilation
#############
OCAMLSRCDIR=..
INCLUDES_DEP=-I +compiler-libs

# Requires unix!
COMPFLAGS= $(INCLUDES_DEP) -I +unix

MODULES= lexer parse compile main

OBJS=		$(addsuffix .cmo, $(MODULES))

XOBJS=		$(addsuffix .cmx, $(MODULES))

kyon: $(OBJS)
	ocamlc -o $@ -I +compiler-libs ocamlcommon.cma ocamlbytecomp.cma $(OBJS)

clean::
	rm -f kyon

beforedepend:: main.ml

clean::

# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

beforedepend::

depend: beforedepend
	ocamldep $(INCLUDES) *.mli *.ml > .depend

clean::
	rm -f *.cm* *.annot

include .depend
