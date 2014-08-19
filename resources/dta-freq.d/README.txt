This archive contains frequency counts from the Deutsches Textarchiv ("DTA",
http://www.deutschestextarchiv.de), and the digitalised Polytechnischen Journal
("Dingler", http://dingler.culture.hu-berlin.de).  Each included file contains
frequency records for surface wordforms in one of three subcorpora:

  dta-full	: DTA + DTAE + Dingler
  dta-core	: DTA
  dta-cora-1850+: DTA (1850 and later)

All included files contain one surface wordform record per line, and each line
contains 3 TAB-separated fields:

  FREQ "\t" UTEXT "\t" WTEXT "\n"

where FREQ is the raw frequency of the surface form UTEXT, and WTEXT is the
transliteration of UTEXT into that subset of ISO-8859-1 (Latin-1) used in
contemporary German orthography as determined by unicruft
(http://odo.dwds.de/~moocow/software/unicruft/), except where otherwise
dictated by context-specific heuristics in the original documents
(e.g. the Greek letter pi "π" may appear as itself rather than its
unicruft transliteration "pi" when it appears in a mathematical expression
in the original document(s)).
