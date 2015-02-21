module ItLexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = Masc  | Fem  | Neutr | MascOrFem 
          | Sg    | Pl 
          | Fst   | Snd  | Thrd 
          | Nom   | AccOrDat 
          | Pers  | Refl | Wh 
          | Past  | Pres | Fut | Perf | Infl | Imperf
          | A | Da | Di | In | Su | Con | Per
          | Acc | Dat
          deriving (Eq,Show,Ord)

-- \224 = a with grave accent
-- \232 = e with grave accent
-- \236 = i with grave accent
-- \242 = o with grave accent

lexicon :: String -> [Cat]

-- PROPER NOUNS
lexicon "giovanni" = [Cat "giovanni" "NP" [Thrd,Masc,Sg]  []]
lexicon "beppe"    = [Cat "beppe"    "NP" [Thrd,Masc,Sg]  []]
lexicon "maria"    = [Cat "maria"    "NP" [Thrd,Fem,Sg]   []]
lexicon "bianca"   = [Cat "bianca"   "NP" [Thrd,Fem,Sg]   []]

-- SUBJECT PRONOUNS
lexicon "io"   = [Cat "io"   "NP" [Pers,Fst,Sg,Nom] 		[]]
lexicon "tu"   = [Cat "tu"   "NP" [Pers,Snd,Sg,Nom] 		[]]
lexicon "lui"  = [Cat "lui"  "NP" [Pers,Thrd,Sg,Nom,Masc] 	[]]
lexicon "lei"  = [Cat "lei"  "NP" [Pers,Thrd,Sg,Nom,Fem] 	[]]
lexicon "noi"  = [Cat "noi"  "NP" [Pers,Fst,Pl,Nom] 		[]]
lexicon "voi"  = [Cat "voi"  "NP" [Pers,Snd,Pl,Nom] 		[]]
lexicon "loro" = [Cat "loro" "NP" [Pers,Thrd,Pl,Nom] 		[]]
lexicon "pro"  = [Cat "pro"  "NP" [Pers,Nom]				[]]

-- DIRECT OBJECT CLITICS
lexicon "mi" = [Cat "mi" "CL" [Pers,Fst,Sg,Acc] 		   []] 
lexicon "ti" = [Cat "ti" "CL" [Pers,Snd,Sg,Acc]  	  	   []]
lexicon "lo" = [Cat "lo" "CL" [Pers,Thrd,Sg,Acc,Masc] 	   [],
				Cat "lo" "DET" [Sg,Masc] 	  []]
lexicon "la" = [Cat "la" "CL" [Pers,Thrd,Sg,Acc,Fem]  	   [],
				Cat "la" "DET" [Sg,Fem]  	  []]
lexicon "l'" = [Cat "l'" "CL" [Pers,Thrd,Sg,Acc,MascOrFem] [],
				Cat "l'" "DET" [Sg,MascOrFem] []]
lexicon "ci" = [Cat "ci" "CL" [Pers,Fst,Pl,Acc]  	  	   []]
lexicon "vi" = [Cat "vi" "CL" [Pers,Snd,Pl,Acc]  	  	   []]
lexicon "li" = [Cat "li" "CL" [Pers,Thrd,Pl,Acc,Masc] 	   []]
lexicon "le" = [Cat "le" "CL" [Pres,Thrd,Pl,Acc,Fem]	   [],
				Cat "le"  "DET" [Pl,Fem] 	  []]

-- DETERMINERS
-- definite article
lexicon "il"  = [Cat "il"  "DET" [Sg,Masc] []]
--lexicon "lo"  = [Cat "il"  "DET" [Sg,Masc] []]
--lexicon "la"  = [Cat "la"  "DET" [Sg,Fem]  []]
--lexicon "l'" =  [Cat "l'"  "DET" [Sg,MascOrFem] []]
lexicon "i"	  = [Cat "i"   "DET" [Pl,Masc] []]
lexicon "gli" = [Cat "gli" "DET" [Pl,Masc] []]
lexicon "gl'" = [Cat "gl'" "DET" [Pl,Masc] []]
--lexicon "le"  = [Cat "le"  "DET" [Pl,Fem]  []]

-- indefinite article
lexicon "un"  = [Cat "un"  "DET" [Sg,Masc] []]
lexicon "uno" = [Cat "uno" "DET" [Sg,Masc] []]
lexicon "una" = [Cat "una" "DET" [Sg,Fem]  []]
lexicon "un'" = [Cat "un'" "DET" [Sg,Fem]  []]

-- every
lexicon "ogni" = [Cat "ogni" "DET" [Sg] []]

-- some
lexicon "alcuni" = [Cat "alcuni" "DET" [Pl,Masc] []]
lexicon "alcune" = [Cat "alcune" "DET" [Pl,Fem]  []]

-- many
lexicon "molti" = [Cat "molti" "DET" [Pl,Masc] []]
lexicon "molte" = [Cat "molte" "DET" [Pl,Fem]  []]

-- this, these
lexicon "questo" = [Cat "questo" "DET" [Sg,Masc] 	  []]
lexicon "questa" = [Cat "questa" "DET" [Sg,Fem]  	  []]
lexicon "quest'" = [Cat "quest'" "DET" [Sg,MascOrFem] []]
lexicon "questi" = [Cat "questi" "DET" [Pl,Masc] 	  []]
lexicon "queste" = [Cat "queste" "DET" [Pl,Fem]  	  []]

-- that, those
lexicon "quel"   = [Cat "quel"   "DET" [Sg,Masc] 	  []]
lexicon "quello" = [Cat "quello" "DET" [Sg,Masc] 	  []]
lexicon "quella" = [Cat "quella" "DET" [Sg,Fem]  	  []]
lexicon "quell'" = [Cat "quell'" "DET" [Sg,MascOrFem] []]
lexicon "quei"   = [Cat "quei"   "DET" [Pl,Masc] 	  []]
lexicon "quegli" = [Cat "quegli" "DET" [Pl,Masc] 	  []]
lexicon "quegl'" = [Cat "quegl'" "DET" [Pl,Masc] 	  []]
lexicon "quelle" = [Cat "quelle" "DET" [Pl,Fem]  	  []]

-- COMMON NOUNS
-- cosa: thing
lexicon "cosa" = [Cat "cosa" "CN" [Sg,Fem,Thrd] []]
lexicon "cose" = [Cat "cose" "CN" [Pl,Fem,Thrd] []]

-- persona: person
lexicon "persona" = [Cat "persona" "CN" [Sg,Fem,Thrd] []]
lexicon "persone" = [Cat "persone" "CN" [Pl,Fem,Thrd] []]

-- ragazzo: boy
lexicon "ragazzo" = [Cat "ragazzo" "CN" [Sg,Masc,Thrd] []]
lexicon "ragazzi" = [Cat "ragazzi" "CN" [Pl,Masc,Thrd] []]

-- ragazza: girl
lexicon "ragazza" = [Cat "ragazza" "CN" [Sg,Fem,Thrd] []]
lexicon "ragazze" = [Cat "ragazze" "CN" [Pl,Fem,Thrd] []]

-- uomo: man
lexicon "uomo"   = [Cat "uomo"   "CN" [Sg,Masc,Thrd] []]
lexicon "uomini" = [Cat "uomini" "CN" [Pl,Masc,Thrd] []]

-- donna: woman
lexicon "donna" = [Cat "donna" "CN" [Sg,Fem,Thrd] []]
lexicon "donne" = [Cat "donne" "CN" [Pl,Fem,Thrd] []]

-- principessa: princess
lexicon "principessa" = [Cat "principessa" "CN" [Sg,Fem,Thrd] []]
lexicon "principesse" = [Cat "principesse" "CN" [Pl,Fem,Thrd] []]

-- gnomo: gnome, dwarf
lexicon "gnomo" = [Cat "gnomo" "CN" [Sg,Masc,Thrd] []]
lexicon "gnoma" = [Cat "gnoma" "CN" [Sg,Fem,Thrd]  []]
lexicon "gnomi" = [Cat "gnomi" "CN" [Pl,Masc,Thrd] []]
lexicon "gnome" = [Cat "gnome" "CN" [Pl,Fem,Thrd]  []]

-- gigante: giant
lexicon "gigante" 	 = [Cat "gigante" 	 "CN" [Sg,Masc,Thrd] []]
lexicon "gigantessa" = [Cat "gigantessa" "CN" [Sg,Fem,Thrd]  []]
lexicon "giganti" 	 = [Cat "giganti" 	 "CN" [Pl,Masc,Thrd] []]
lexicon "gigantesse" = [Cat "gigantesse" "CN" [Pl,Fem,Thrd]  []]

-- mago: wizard
lexicon "mago" =  [Cat "mago" "CN"  [Sg,Masc,Thrd] []]
lexicon "maga" =  [Cat "maga" "CN"  [Sg,Fem,Thrd]  []]
lexicon "maghi" = [Cat "maghi" "CN" [Pl,Masc,Thrd] []]
lexicon "maghe" = [Cat "maghe" "CN" [Pl,Fem,Thrd]  []]

-- spada: sword
lexicon "spada" = [Cat "spada" "CN" [Sg,Fem,Thrd] []]
lexicon "spade" = [Cat "spade" "CN" [Pl,Fem,Thrd] []]

-- stiletto: dagger
lexicon "stiletto" = [Cat "stiletto" "CN" [Sg,Masc,Thrd] []]
lexicon "stiletti" = [Cat "stiletti" "SN" [Pl,Masc,Thrd] []]

-- giornale: newspaper
lexicon "giornale" = [Cat "giornale" "CN" [Sg,Masc,Thrd] []]
lexicon "giornali" = [Cat "giornali" "CN" [Pl,Masc,Thrd] []]

-- campagna: countryside
lexicon "campagna" = [Cat "campagna" "CN" [Sg,Fem,Thrd] []]
lexicon "campagne" = [Cat "campagne" "CN" [Pl,Fem,Thrd] []]

-- PREPOSITIONS
-- a: to, at
lexicon "a"   = [Cat "a"   "PREP" [A]   []]

-- da: from, by
lexicon "da"  = [Cat "da"  "PREP" [Da]  []]

-- di: of
lexicon "di"  = [Cat "di"  "PREP" [Di]  []]

-- in: in
lexicon "in"  = [Cat "in"  "PREP" [In]  []]

-- su: on
lexicon "su"  = [Cat "su"  "PREP" [Su]  []]

-- con: with
lexicon "con" = [Cat "con" "PREP" [Con] []]

-- per: for
lexicon "per" = [Cat "per" "PREP" [Per] []]


-- e: and
lexicon "e" = [Cat "e" "CONJ" [] []]
lexicon "." = [Cat "." "CONJ" [] []]


-- AUXILIARIES

-- essere: to be, auxiliary verb
-- present
lexicon "sono"  = [Cat "sono"  "AUX" [Pres,Sg,Fst] [],
	Cat "sono"  "AUX" [Pres,Pl,Thrd] []]
lexicon "sei"   = [Cat "sei"   "AUX" [Pres,Sg,Snd] []]
lexicon "\232"  = [Cat "\232"  "AUX" [Pres,Sg,Thrd] []]
lexicon "siamo" = [Cat "siamo" "AUX" [Pres,Pl,Fst]  []]
lexicon "siete" = [Cat "siete" "AUX" [Pres,Pl,Snd]  []]
--lexicon "sono"  = [Cat "sono"  "AUX" [Pres,Pl,Thrd] []]
-- imperfect (used in past perfect)
lexicon "ero"     = [Cat "ero"     "AUX" [Past,Sg,Fst]  []]
lexicon "eri"     = [Cat "eri"     "AUX" [Past,Sg,Snd]  []]
lexicon "era"     = [Cat "era"     "AUX" [Past,Sg,Thrd] []]
lexicon "eravamo" = [Cat "eravamo" "AUX" [Past,Pl,Fst]  []]
lexicon "eravate" = [Cat "eravate" "AUX" [Past,Pl,Snd]  []]
lexicon "erano"   = [Cat "erano"   "AUX" [Past,Pl,Thrd] []]

-- avere: to have, auxiliary verb
-- present
lexicon "ho" 	  = [Cat "ho" 	   "AUX" [Pres,Sg,Fst]  []]
lexicon "hai" 	  = [Cat "hai" 	   "AUX" [Pres,Sg,Snd]  []]
lexicon "ha" 	  = [Cat "ha"      "AUX" [Pres,Sg,Thrd] []]
lexicon "abbiamo" = [Cat "abbiamo" "AUX" [Pres,Pl,Fst]  []]
lexicon "avete"   = [Cat "avete"   "AUX" [Pres,Pl,Snd]  []]
lexicon "hanno"   = [Cat "hanno"   "AUX" [Pres,Pl,Thrd] []]
-- imperfect (used in past perfect)
lexicon "avevo"   = [Cat "avevo"   "AUX" [Past,Sg,Fst]  []]
lexicon "avevi"   = [Cat "avevi"   "AUX" [Past,Sg,Snd]  []]
lexicon "aveva"   = [Cat "aveva"   "AUX" [Past,Sg,Thrd] []]
lexicon "avevamo" = [Cat "avevamo" "AUX" [Past,Pl,Fst]  []]
lexicon "avevate" = [Cat "avevate" "AUX" [Past,Pl,Snd]  []]
lexicon "avevano" = [Cat "avevano" "AUX" [Past,Pl,Thrd] []]


-- VERBS

-- parlare (con): to talk (with)
-- present
lexicon "parlo"    = 
	[Cat "parlo" 	"VP" [Pres,Sg,Fst]  [],
	 Cat "parlo" 	"VP" [Pres,Sg,Fst]  [Cat "_" "PP" [Con][]]]
lexicon "parli"    = 
	[Cat "parli" 	"VP" [Pres,Sg,Snd]  [],
	 Cat "parli" 	"VP" [Pres,Sg,Snd]  [Cat "_" "PP" [Con][]]]
lexicon "parla"    = 
	[Cat "parla" 	"VP" [Pres,Sg,Thrd] [],
	 Cat "parla" 	"VP" [Pres,Sg,Thrd] [Cat "_" "PP" [Con][]]]
lexicon "parliamo" = 
	[Cat "parliamo" "VP" [Pres,Pl,Fst]  [],
	 Cat "parliamo" "VP" [Pres,Pl,Fst]  [Cat "_" "PP" [Con][]]]
lexicon "parlate"  = 
	[Cat "parlate"  "VP" [Pres,Pl,Snd]  [],
	 Cat "parlate"  "VP" [Pres,Pl,Snd]  [Cat "_" "PP" [Con][]]]
lexicon "parlano"  = 
	[Cat "parlano"  "VP" [Pres,Pl,Thrd] [],
	 Cat "parlano"  "VP" [Pres,Pl,Thrd] [Cat "_" "PP" [Con][]]]

-- imperfect
lexicon "parlavo"    = 
	[Cat "parlavo" 	  "VP" [Imperf,Past,Sg,Fst]  [],
	 Cat "parlavo" 	  "VP" [Imperf,Past,Sg,Fst]  [Cat "_" "PP" [Con][]]]
lexicon "parlavi"    = 
	[Cat "parlavi" 	  "VP" [Imperf,Past,Sg,Snd]  [],
	 Cat "parlavi" 	  "VP" [Imperf,Past,Sg,Snd]  [Cat "_" "PP" [Con][]]]
lexicon "parlava"    = 
	[Cat "parlava" 	  "VP" [Imperf,Past,Sg,Thrd] [],
	 Cat "parlava" 	  "VP" [Imperf,Past,Sg,Thrd] [Cat "_" "PP" [Con][]]]
lexicon "parlavamo"  = 
	[Cat "parlavamo"  "VP" [Imperf,Past,Pl,Fst]  [],
	 Cat "parlavamo"  "VP" [Imperf,Past,Pl,Fst]  [Cat "_" "PP" [Con][]]]
lexicon "parlavate"  = 
	[Cat "parlavate"  "VP" [Imperf,Past,Pl,Snd]  [],
	 Cat "parlavate"  "VP" [Imperf,Past,Pl,Snd]  [Cat "_" "PP" [Con][]]]
lexicon "parlavano"  = 
	[Cat "parlavano"  "VP" [Imperf,Past,Pl,Thrd] [],
	 Cat "parlavano"  "VP" [Imperf,Past,Pl,Thrd] [Cat "_" "PP" [Con][]]]

-- perfect
lexicon "parlato" = [Cat "parlato" "VP" [Perf] [],
					 Cat "parlato" "VP" [Perf] [Cat "_" "PP" [Con][]]]

-- preterit
lexicon "parlai"    = 
	[Cat "parlai"    "VP" [Past,Sg,Fst]  [],
	 Cat "parlai" 	 "VP" [Past,Sg,Fst]  [Cat "_" "PP" [Con][]]]
lexicon "parlasti"  = 
	[Cat "parlasti"  "VP" [Past,Sg,Snd]  [],
	 Cat "parlasti"  "VP" [Past,Sg,Snd]  [Cat "_" "PP" [Con][]]]
lexicon "parl\242"  = 
	[Cat "parl\242"  "VP" [Past,Sg,Thrd] [],
	 Cat "parl\242"  "VP" [Past,Sg,Thrd] [Cat "_" "PP" [Con][]]]
lexicon "parlammo"  = 
	[Cat "parlammo"  "VP" [Past,Pl,Fst]  [],
	 Cat "parlammo"  "VP" [Past,Pl,Fst]  [Cat "_" "PP" [Con][]]]
lexicon "parlaste"  = 
	[Cat "parlaste"  "VP" [Past,Pl,Snd]  [],
	 Cat "parlaste"  "VP" [Past,Pl,Snd]  [Cat "_" "PP" [Con][]]]
lexicon "parlarono"  = 
	[Cat "parlarono" "VP" [Past,Pl,Thrd] [],
	 Cat "parlarono" "VP" [Past,Pl,Thrd] [Cat "_" "PP" [Con][]]]

-- future
lexicon "parler\242" = 
	[Cat "parler\242" "VP" [Fut,Sg,Fst]  [],
	 Cat "parler\242" "VP" [Fut,Sg,Fst]  [Cat "_" "PP" [Con][]]]
lexicon "parlerai"   = 
	[Cat "parlerai"   "VP" [Fut,Sg,Snd]  [],
	 Cat "parlerai"   "VP" [Fut,Sg,Snd]  [Cat "_" "PP" [Con][]]]
lexicon "parler\224" = 
	[Cat "parler\224" "VP" [Fut,Sg,Thrd] [],
	 Cat "parler\224" "VP" [Fut,Sg,Thrd] [Cat "_" "PP" [Con][]]]
lexicon "parleremo"  = 
	[Cat "parleremo"  "VP" [Fut,Pl,Fst]  [],
	 Cat "parleremo"  "VP" [Fut,Pl,Fst]  [Cat "_" "PP" [Con][]]]
lexicon "parlerete"  = 
	[Cat "parlerete"  "VP" [Fut,Pl,Snd]  [],
	 Cat "parlerete"  "VP" [Fut,Pl,Snd]  [Cat "_" "PP" [Con][]]]
lexicon "parleranno" = 
	[Cat "parleranno" "VP" [Fut,Pl,Thrd] [],
	 Cat "parleranno" "VP" [Fut,Pl,Thrd] [Cat "_" "PP" [Con][]]]

-- dare: to give
-- present
lexicon "do" 	= [Cat "do"    "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [Acc] [],
											 	    Cat "_" "PP" [A]   []],
				   Cat "do"    "VP" [Pres,Sg,Fst]  [Cat "_" "PP" [A]   []]]
lexicon "dai"   = [Cat "dai"   "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [Acc] [],
											 	    Cat "_" "PP" [A]   []],
				   Cat "dai"   "VP" [Pres,Sg,Snd]  [Cat "_" "PP" [A]   []]]
lexicon "d\224" = [Cat "d\224" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [Acc] [],
											 	    Cat "_" "PP" [A]   []],
				   Cat "d\224" "VP" [Pres,Sg,Thrd] [Cat "_" "PP" [A]   []]]
lexicon "diamo" = [Cat "diamo" "VP" [Pres,Pl,Fst]  [Cat "_" "NP" [Acc] [],
											 	    Cat "_" "PP" [A]   []],
				   Cat "diamo" "VP" [Pres,Pl,Fst]  [Cat "_" "PP" [A]   []]]
lexicon "date"  = [Cat "date"  "VP" [Pres,Pl,Snd]  [Cat "_" "NP" [Acc] [],
											 	    Cat "_" "PP" [A]   []],
				   Cat "date"  "VP" [Pres,Pl,Snd]  [Cat "_" "PP" [A]   []],
				   Cat "date" "VP" [Perf] [Cat "_" "NP" [Acc] [],
									  Cat "_" "PP" [A]   []],
				   Cat "date" "VP" [Perf] [Cat "_" "PP" [A]   []]]
lexicon "danno" = [Cat "danno" "VP" [Pres,Pl,Thrd] [Cat "_" "NP" [Acc] [],
											 	    Cat "_" "PP" [A]   []],
				   Cat "danno" "VP" [Pres,Pl,Thrd] [Cat "_" "PP" [A]   []]]

-- imperfect
lexicon "davo" 	 = [Cat "davo"   "VP" [Imperf,Past,Sg,Fst]  [Cat "_" "NP" [Acc] [],
											 	      		 Cat "_" "PP" [A]   []],
				    Cat "davo"   "VP" [Imperf,Past,Sg,Fst]  [Cat "_" "PP" [A]   []]]
lexicon "davi"   = [Cat "davi"   "VP" [Imperf,Past,Sg,Snd]  [Cat "_" "NP" [Acc] [],
											 	     		 Cat "_" "PP" [A]   []],
				    Cat "davi"   "VP" [Imperf,Past,Sg,Snd]  [Cat "_" "PP" [A]   []]]
lexicon "dava"   = [Cat "dava"   "VP" [Imperf,Past,Sg,Thrd] [Cat "_" "NP" [Acc] [],
											 	     		 Cat "_" "PP" [A]   []],
				    Cat "dava" 	 "VP" [Imperf,Past,Sg,Thrd] [Cat "_" "PP" [A]   []]]
lexicon "davamo" = [Cat "davamo" "VP" [Imperf,Past,Pl,Fst]  [Cat "_" "NP" [Acc] [],
											 	   		     Cat "_" "PP" [A]   []],
				    Cat "davamo" "VP" [Imperf,Past,Pl,Fst]  [Cat "_" "PP" [A]   []]]
lexicon "davate" = [Cat "davate" "VP" [Imperf,Past,Pl,Snd]  [Cat "_" "NP" [Acc] [],
											 	   		     Cat "_" "PP" [A]   []],
				    Cat "davate" "VP" [Imperf,Past,Pl,Snd]  [Cat "_" "PP" [A]   []]]
lexicon "davano" = [Cat "davano" "VP" [Imperf,Past,Pl,Thrd] [Cat "_" "NP" [Acc] [],
											 	     		 Cat "_" "PP" [A]   []],
				    Cat "davano" "VP" [Imperf,Past,Pl,Thrd] [Cat "_" "PP" [A]   []]]


-- perfect
lexicon "dato" = [Cat "dato" "VP" [Perf] [Cat "_" "NP" [Acc] [],
										  Cat "_" "PP" [A]   []],
				  Cat "dato" "VP" [Perf] [Cat "_" "PP" [A]   []]]
lexicon "data" = [Cat "data" "VP" [Perf] [Cat "_" "NP" [Acc] [],
										  Cat "_" "PP" [A]   []],
				  Cat "data" "VP" [Perf] [Cat "_" "PP" [A]   []]]
lexicon "dati" = [Cat "dati" "VP" [Perf] [Cat "_" "NP" [Acc] [],
										  Cat "_" "PP" [A]   []],
				  Cat "dati" "VP" [Perf] [Cat "_" "PP" [A]   []]]
--lexicon "date" = [Cat "date" "VP" [Perf] [Cat "_" "NP" [Acc] [],
--									  Cat "_" "PP" [A]   []],
--				  Cat "date" "VP" [Perf] [Cat "_" "PP" [A]   []]]

-- preterit
lexicon "diedi"   = [Cat "diedi"   "VP" [Past,Sg,Fst]  [Cat "_" "NP" [Acc] [],
											 	      	Cat "_" "PP" [A]   []],
				     Cat "diedi"   "VP" [Past,Sg,Fst]  [Cat "_" "PP" [A]   []]]
lexicon "desti"   = [Cat "desti"   "VP" [Past,Sg,Snd]  [Cat "_" "NP" [Acc] [],
											 	     	Cat "_" "PP" [A]   []],
				     Cat "desti"   "VP" [Past,Sg,Snd]  [Cat "_" "PP" [A]   []]]
lexicon "diede"   = [Cat "diede"   "VP" [Past,Sg,Thrd] [Cat "_" "NP" [Acc] [],
											 	     	Cat "_" "PP" [A]   []],
				     Cat "diede"   "VP" [Past,Sg,Thrd] [Cat "_" "PP" [A]   []]]
lexicon "demmo"   = [Cat "demmo"   "VP" [Past,Pl,Fst]  [Cat "_" "NP" [Acc] [],
											 	   		Cat "_" "PP" [A]   []],
				     Cat "demmo"   "VP" [Past,Pl,Fst]  [Cat "_" "PP" [A]   []]]
lexicon "deste"   = [Cat "deste"   "VP" [Past,Pl,Snd]  [Cat "_" "NP" [Acc] [],
											 	   		Cat "_" "PP" [A]   []],
				     Cat "deste"   "VP" [Past,Pl,Snd]  [Cat "_" "PP" [A]   []]]
lexicon "diedero" = [Cat "diedero" "VP" [Past,Pl,Thrd] [Cat "_" "NP" [Acc] [],
											 	     	Cat "_" "PP" [A]   []],
				     Cat "diedero" "VP" [Past,Pl,Thrd] [Cat "_" "PP" [A]   []]]

-- future
lexicon "dar\242" = [Cat "dar\242" "VP" [Fut,Sg,Fst]  [Cat "_" "NP" [Acc] [],
											 	       Cat "_" "PP" [A]   []],
				    Cat "dar\242"  "VP" [Fut,Sg,Fst]  [Cat "_" "PP" [A]   []]]
lexicon "darai"   = [Cat "darai"   "VP" [Fut,Sg,Snd]  [Cat "_" "NP" [Acc] [],
											 	       Cat "_" "PP" [A]   []],
				     Cat "darai"   "VP" [Fut,Sg,Snd]  [Cat "_" "PP" [A]   []]]
lexicon "dar\224" = [Cat "dar\224" "VP" [Fut,Sg,Thrd] [Cat "_" "NP" [Acc] [],
											 	       Cat "_" "PP" [A]   []],
				     Cat "dar\224" "VP" [Fut,Sg,Thrd] [Cat "_" "PP" [A]   []]]
lexicon "daremo"  = [Cat "daremo"  "VP" [Fut,Pl,Fst]  [Cat "_" "NP" [Acc] [],
											 	   	   Cat "_" "PP" [A]   []],
				     Cat "daremo"  "VP" [Fut,Pl,Fst]  [Cat "_" "PP" [A]   []]]
lexicon "darete"  = [Cat "darete"  "VP" [Fut,Pl,Snd]  [Cat "_" "NP" [Acc] [],
											 	   	   Cat "_" "PP" [A]   []],
				     Cat "darete"  "VP" [Fut,Pl,Snd]  [Cat "_" "PP" [A]   []]]
lexicon "daranno" = [Cat "daranno" "VP" [Fut,Pl,Thrd] [Cat "_" "NP" [Acc] [],
											 	       Cat "_" "PP" [A]   []],
				     Cat "daranno" "VP" [Fut,Pl,Thrd] [Cat "_" "PP" [A]   []]]

-- leggere: to read
-- present
lexicon "leggo" =    [Cat "leggo"    "VP" [Pres,Sg,Fst]   [],
				   	  Cat "leggo"    "VP" [Pres,Sg,Fst]   [Cat "_" "NP" [Acc] []]]
lexicon "leggi" = 	 [Cat "leggi"    "VP" [Pres,Sg,Snd]   [],
				      Cat "leggi"    "VP" [Pres,Sg,Snd]   [Cat "_" "NP" [Acc] []]]
lexicon "legge" =    [Cat "legge"    "VP" [Pres,Sg,Thrd]  [],
				   	  Cat "legge"    "VP" [Pres,Sg,Thrd]  [Cat "_" "NP" [Acc] []]]
lexicon "leggiamo" = [Cat "leggiamo" "VP" [Pres,Pl,Fst]   [],
				   	  Cat "leggiamo" "VP" [Pres,Pl,Fst]   [Cat "_" "NP" [Acc] []]]
lexicon "leggete"  = [Cat "leggete"  "VP" [Pres,Pl,Snd]   [],
				   	  Cat "leggete"  "VP" [Pres,Pl,Snd]   [Cat "_" "NP" [Acc] []]]
lexicon "leggono"  = [Cat "leggono"  "VP" [Pres,Pl,Thrd]  [],
				   	  Cat "leggono"  "VP" [Pres,Pl,Thrd]  [Cat "_" "NP" [Acc] []]]

-- imperfect
lexicon "leggevo"   = [Cat "leggevo"   "VP" [Imperf,Past,Sg,Fst]  [],
				   	   Cat "leggevo"   "VP" [Imperf,Past,Sg,Fst]  [Cat "_" "NP" [Acc] []]]
lexicon "leggevi"   = [Cat "leggevi"   "VP" [Imperf,Past,Sg,Snd]  [],
				       Cat "leggevi"   "VP" [Imperf,Past,Sg,Snd]  [Cat "_" "NP" [Acc] []]]
lexicon "leggeva"   = [Cat "leggeva"   "VP" [Imperf,Past,Sg,Thrd] [],
				   	   Cat "leggeva"   "VP" [Imperf,Past,Sg,Thrd] [Cat "_" "NP" [Acc] []]]
lexicon "leggevamo" = [Cat "leggevamo" "VP" [Imperf,Past,Pl,Fst]  [],
				   	   Cat "leggevamo" "VP" [Imperf,Past,Pl,Fst]  [Cat "_" "NP" [Acc] []]]
lexicon "leggevate" = [Cat "leggevate" "VP" [Imperf,Past,Pl,Snd]  [],
				   	   Cat "leggevate" "VP" [Imperf,Past,Pl,Snd]  [Cat "_" "NP" [Acc] []]]
lexicon "leggevano" = [Cat "leggevano" "VP" [Imperf,Past,Pl,Thrd] [],
				   	   Cat "leggevano" "VP" [Imperf,Past,Pl,Thrd] [Cat "_" "NP" [Acc] []]]

-- perfect
lexicon "letto" = [Cat "letto" "VP" [Perf] [],
				   Cat "letto" "VP" [Perf] [Cat "_" "NP" [Acc] []]]
lexicon "letta" = [Cat "letta" "VP" [Perf] [],
				   Cat "letta" "VP" [Perf] [Cat "_" "NP" [Acc] []]]
lexicon "letti" = [Cat "letti" "VP" [Perf] [],
				   Cat "letti" "VP" [Perf] [Cat "_" "NP" [Acc] []]]
lexicon "lette" = [Cat "lette" "VP" [Perf] [],
				   Cat "lette" "VP" [Perf] [Cat "_" "NP" [Acc] []]]

-- preterit
lexicon "lessi"    = [Cat "lessi"    "VP" [Past,Sg,Fst]   [],
				   	  Cat "lessi"    "VP" [Past,Sg,Fst]   [Cat "_" "NP" [Acc] []]]
lexicon "leggesti" = [Cat "leggesti" "VP" [Past,Sg,Snd]   [],
				      Cat "leggesti" "VP" [Past,Sg,Snd]   [Cat "_" "NP" [Acc] []]]
lexicon "lesse"    = [Cat "lesse"    "VP" [Past,Sg,Thrd]  [],
				   	  Cat "lesse"    "VP" [Past,Sg,Thrd]  [Cat "_" "NP" [Acc] []]]
lexicon "leggemmo" = [Cat "leggemmo" "VP" [Past,Pl,Fst]   [],
				   	  Cat "leggemmo" "VP" [Past,Pl,Fst]   [Cat "_" "NP" [Acc] []]]
lexicon "leggeste" = [Cat "leggeste" "VP" [Past,Pl,Snd]   [],
				   	  Cat "leggeste" "VP" [Past,Pl,Snd]   [Cat "_" "NP" [Acc] []]]
lexicon "lessero"  = [Cat "lessero"  "VP" [Past,Pl,Thrd]  [],
				   	  Cat "lessero"  "VP" [Past,Pl,Thrd]  [Cat "_" "NP" [Acc] []]]

-- future
lexicon "legger\242" = [Cat "legger\242" "VP" [Fut,Sg,Fst]   [],
				   	    Cat "legger\242" "VP" [Fut,Sg,Fst]   [Cat "_" "NP" [Acc] []]]
lexicon "leggerai"   = [Cat "leggerai"   "VP" [Fut,Sg,Snd]   [],
				        Cat "leggerai"   "VP" [Fut,Sg,Snd]   [Cat "_" "NP" [Acc] []]]
lexicon "legger\224" = [Cat "legger\224" "VP" [Fut,Sg,Thrd]  [],
				   	    Cat "legger\224" "VP" [Fut,Sg,Thrd]  [Cat "_" "NP" [Acc] []]]
lexicon "leggeremo"  = [Cat "leggeremo"  "VP" [Fut,Pl,Fst]   [],
				   	    Cat "leggeremo"  "VP" [Fut,Pl,Fst]   [Cat "_" "NP" [Acc] []]]
lexicon "leggerete"  = [Cat "leggerete"  "VP" [Fut,Pl,Snd]   [],
				   	    Cat "leggerete"  "VP" [Fut,Pl,Snd]   [Cat "_" "NP" [Acc] []]]
lexicon "leggeranno" = [Cat "leggeranno" "VP" [Fut,Pl,Thrd]  [],
				   	    Cat "leggeranno" "VP" [Fut,Pl,Thrd]  [Cat "_" "NP" [Acc] []]]


-- partire: to leave
-- present
lexicon "parto" =    [Cat "parto"    "VP" [Pres,Sg,Fst]   [],
				   	  Cat "parto"    "VP" [Pres,Sg,Fst]   [Cat "_" "PP" [Per] []]]
lexicon "parti" = 	 [Cat "parti"    "VP" [Pres,Sg,Snd]   [],
				      Cat "parti"    "VP" [Pres,Sg,Snd]   [Cat "_" "PP" [Per] []]]
lexicon "parte" =    [Cat "parte"    "VP" [Pres,Sg,Thrd]  [],
				   	  Cat "parte"    "VP" [Pres,Sg,Thrd]  [Cat "_" "PP" [Per] []]]
lexicon "partiamo" = [Cat "partiamo" "VP" [Pres,Pl,Fst]   [],
				   	  Cat "partiamo" "VP" [Pres,Pl,Fst]   [Cat "_" "PP" [Per] []]]
lexicon "partite"  = [Cat "partite"  "VP" [Pres,Pl,Snd]   [],
				   	  Cat "partite"  "VP" [Pres,Pl,Snd]   [Cat "_" "PP" [Per] []],
				   	  Cat "partite" "VP" [Perf] [],
				      Cat "partite" "VP" [Perf] [Cat "_" "PP" [Per] []]]
lexicon "partono"  = [Cat "partono"  "VP" [Pres,Pl,Thrd]  [],
				   	  Cat "partono"  "VP" [Pres,Pl,Thrd]  [Cat "_" "PP" [Per] []]]

-- imperfect
lexicon "partivo"   = [Cat "partivo"   "VP" [Imperf,Past,Sg,Fst]  [],
				   	   Cat "partivo"   "VP" [Imperf,Past,Sg,Fst]  [Cat "_" "PP" [Per] []]]
lexicon "partivi"   = [Cat "partivi"   "VP" [Imperf,Past,Sg,Snd]  [],
				       Cat "partivi"   "VP" [Imperf,Past,Sg,Snd]  [Cat "_" "PP" [Per] []]]
lexicon "partiva"   = [Cat "partiva"   "VP" [Imperf,Past,Sg,Thrd] [],
				   	   Cat "partiva"   "VP" [Imperf,Past,Sg,Thrd] [Cat "_" "PP" [Per] []]]
lexicon "partivamo" = [Cat "partivamo" "VP" [Imperf,Past,Pl,Fst]  [],
				   	   Cat "partivamo" "VP" [Imperf,Past,Pl,Fst]  [Cat "_" "PP" [Per] []]]
lexicon "partivate" = [Cat "partivate" "VP" [Imperf,Past,Pl,Snd]  [],
				   	   Cat "partivate" "VP" [Imperf,Past,Pl,Snd]  [Cat "_" "PP" [Per] []]]
lexicon "partivano" = [Cat "partivano" "VP" [Imperf,Past,Pl,Thrd] [],
				   	   Cat "partivano" "VP" [Imperf,Past,Pl,Thrd] [Cat "_" "PP" [Per] []]]

-- perfect
lexicon "partito" = [Cat "partito" "VP" [Perf] [],
				     Cat "partito" "VP" [Perf] [Cat "_" "PP" [Per] []]]
lexicon "partita" = [Cat "partita" "VP" [Perf] [],
				     Cat "partita" "VP" [Perf] [Cat "_" "PP" [Per] []]]
lexicon "partiti" = [Cat "partiti" "VP" [Perf] [],
				     Cat "partiti" "VP" [Perf] [Cat "_" "PP" [Per] []]]
--lexicon "partite" = [Cat "partite" "VP" [Perf] [],
--				     Cat "partite" "VP" [Perf] [Cat "_" "PP" [Per] []]]

-- preterit
lexicon "partii"    = [Cat "partii"    "VP" [Past,Sg,Fst]  [],
				   	   Cat "partii"    "VP" [Past,Sg,Fst]  [Cat "_" "PP" [Per] []]]
lexicon "partisti"  = [Cat "partisti"  "VP" [Past,Sg,Snd]  [],
				       Cat "partisti"  "VP" [Past,Sg,Snd]  [Cat "_" "PP" [Per] []]]
lexicon "part\236"  = [Cat "part\236"  "VP" [Past,Sg,Thrd] [],
				   	   Cat "part\236"  "VP" [Past,Sg,Thrd] [Cat "_" "PP" [Per] []]]
lexicon "partimmo"  = [Cat "partimmo"  "VP" [Past,Pl,Fst]  [],
				   	   Cat "partimmo"  "VP" [Past,Pl,Fst]  [Cat "_" "PP" [Per] []]]
lexicon "partiste"  = [Cat "partiste"  "VP" [Past,Pl,Snd]  [],
				   	   Cat "partiste"  "VP" [Past,Pl,Snd]  [Cat "_" "PP" [Per] []]]
lexicon "partirono" = [Cat "partirono" "VP" [Past,Pl,Thrd] [],
				   	   Cat "partirono" "VP" [Past,Pl,Thrd] [Cat "_" "PP" [Per] []]]

-- future
lexicon "partir\242" = [Cat "partir\242" "VP" [Fut,Sg,Fst]   [],
				   	    Cat "partir\242" "VP" [Fut,Sg,Fst]   [Cat "_" "PP" [Per] []]]
lexicon "partirai"   = [Cat "partirai"   "VP" [Fut,Sg,Snd]   [],
				        Cat "partirai"   "VP" [Fut,Sg,Snd]   [Cat "_" "PP" [Per] []]]
lexicon "partir\224" = [Cat "partir\224" "VP" [Fut,Sg,Thrd]  [],
				   	    Cat "partir\224" "VP" [Fut,Sg,Thrd]  [Cat "_" "PP" [Per] []]]
lexicon "partiremo"  = [Cat "partiremo"  "VP" [Fut,Pl,Fst]   [],
				   	    Cat "partiremo"  "VP" [Fut,Pl,Fst]   [Cat "_" "PP" [Per] []]]
lexicon "partirete"  = [Cat "partirete"  "VP" [Fut,Pl,Snd]   [],
				   	    Cat "partirete"  "VP" [Fut,Pl,Snd]   [Cat "_" "PP" [Per] []]]
lexicon "partiranno" = [Cat "partiranno" "VP" [Fut,Pl,Thrd]  [],
				   	    Cat "partiranno" "VP" [Fut,Pl,Thrd]  [Cat "_" "PP" [Per] []]]

lexicon _ = []