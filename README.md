Chris Ward, 2014

Code taken from van Eijk and Unger, *Computational Semantics with Functional Programming*.
Modified to parse Italian sentences. 

Italian Parser
---------------

**What's in the lexicon:**
- proper nouns
- subject pronouns
- direct object clitics
- determiners
    - definite article
    - indefinite article
    - ogni, alcuni, molti (every, some, many)
    - questo, quello (this, that)
- common nouns
- prepositions
    - a, da, di, in, su, con, per
- auxiliaries
    - essere
    - avere
- verbs
    - parlare
    - dare
    - leggere
    - partire

**Parser features:**
- tenses: present, imperfect, present perfect, past perfect, preterit, future
- verb constructions: intransitive, transitive, ditransitive
- verbs with optional PP arguments
- null subject (pro drop); parsed as "pro"
- postverbal direct objects and preverbal direct object clitics
- morphological contractions, such as "l'uomo" and "alla ragazza"
- tense and aspect spread out across auxiliaries and verbs
- agreement between 3rd person direct object clitics and past participles

**Modifications and additions made to P.hs:**
- added aspect feature and function (:245) and added a condition for checking the aspect feature in combine (:270)
- modified scan function to split l'+vowel/h contraction (:291)
- wrote insertPro function to check if a sentence begins with a VP, and if so, insert "pro" as the first element in the sentence. Modified lexer to make the check. (pro is in the lexicon as a pronoun). (:301)
- rewrote preproc to handle preposition+article contractions (:311)
- wrote parseCl to parse clitics (:452)
- modified vpRule to handle clitics (:460)
- wrote clVpRule to parse VPs with direct object clitics (:489)
- modified auxVpRule to properly parse auxiliary constructions (:495)
- test sentence suite (:509)

**How morphological contractions are handled:**
- when an article and a noun or an article and a preposition are contracted into one word, they are split into the article (either l' or gl') and the noun, or the article and the root form of the preposition, and parsed separately. 

**How auxVpRule works:**
- parses the auxiliary verb with parseAux
- parses the remainder of parseAux with vpRule
- the feature set is the result of unifying the feature sets of the auxiliary and verb
- checks that Perf feature can be assigned to the verb

**How clVpRule works:**
- parses the clitic with parseCl
- parses the remainder of parseCl with vpRule
- the feature set is the feature set of the vp parsed with vpRule, since we don't want the clitic's features to impact those of the verb
- checks that Acc feat can be assigned to the clitic

**Test Sentences:**
Note: while running the program in ghci,`testSents` will give you the list of test sentences, `prsTestSents` will give you a massive list of all of the sentences parsed, and `prsTestSent n` will give you the nth sentence in the test suite parsed.  

*Beppe parla col mago.*
Beppe speaks with the wizard.

*Quest'uomo parlava con quella donna.*
This man spoke with that woman.

*Ho letto il giornale.*
I read the newspaper.

*L'ho letto.*
I read it.

*Ogni ragazzo leggeva il giornale.*
Every boy read the newspaper. 

*L'uomo aveva dato una spada agli gnomi.*
The man gave a sword to the dwarves.

*Li hai dati alle donne.*
You gave them to the women.
*Note*: when a 3rd person direct object clitic is used with the perfect, the participle must agree with the clitic, hence dati instead of data.

*Molte persone partiranno per la campagna.*
Many people will leave for the countryside.

*Alcuni giganti parlarono.*
Some giants talked.

*Sono partito.*
I left. 

*Parlerò.*
I will speak. 




