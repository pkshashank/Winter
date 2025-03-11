concrete MathEng of Math = open SyntaxEng, ParadigmsEng, SymbolicEng in {
  lincat
    Sentence = S ;
    NounPhrase = NP ;
    VerbPhrase = VP ;
    Adjective = A ;
    Conjunction = Conj ;
    CommonNoun = CN ;
    Determiner = Det ;
  
  lin
        SmkNPVP np vp = mkS (mkCl np vp) ;
        VPmkAdj adj = mkVP adj ;

        NPconj conj np1 np2 = mkNP conj np1 np2 ;
        NPmkDetCN det cn = mkNP det cn ;

    -- Lexicon
        NPmkInt = symb ;

        Prime = mkA "prime" ;
        Coprime = mkA "coprime" ;
        Countable = mkA "countable" ;

        And = and_Conj ;
        Or = or_Conj ;

        Integer = mkCN (mkN "integer") ;
        Real_Number = mkCN (mkN "real number" ("reals" | "real numbers")) ;

        The_Pl = thePl_Det ;
        The_Sg = theSg_Det ;
  

}
