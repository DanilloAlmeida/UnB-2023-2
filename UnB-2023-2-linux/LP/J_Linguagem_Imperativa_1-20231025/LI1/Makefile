## File generated by the BNF Converter (bnfc 2.9.4.1).

# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : TestLI

# Rules for building the parser.

AbsLI.hs LexLI.x ParLI.y PrintLI.hs TestLI.hs : LI1.cf
	bnfc --haskell LI1.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

TestLI : AbsLI.hs LexLI.hs ParLI.hs PrintLI.hs TestLI.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsLI.hs AbsLI.hs.bak ComposOp.hs ComposOp.hs.bak DocLI.txt DocLI.txt.bak ErrM.hs ErrM.hs.bak LayoutLI.hs LayoutLI.hs.bak LexLI.x LexLI.x.bak ParLI.y ParLI.y.bak PrintLI.hs PrintLI.hs.bak SkelLI.hs SkelLI.hs.bak TestLI.hs TestLI.hs.bak XMLLI.hs XMLLI.hs.bak ASTLI.agda ASTLI.agda.bak ParserLI.agda ParserLI.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak LI1.dtd LI1.dtd.bak TestLI LexLI.hs ParLI.hs ParLI.info ParDataLI.hs Makefile


# EOF
