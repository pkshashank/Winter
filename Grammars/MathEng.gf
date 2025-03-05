concrete MathEng of Math = open SyntaxEng, ParadigmsEng, SymbolicEng in {
  lincat
    Sentence = S ;
    NounPhrase = NP ;
    VerbPhrase = VP ;
    Adjective = A ;
    Conjunction = Conj ;
  
  lin
        SmkNPVP np vp = mkS (mkCl np vp) ;
        VPmkAdj adj = mkVP adj ;
        NPconj conj np1 np2 = mkNP conj np1 np2 ;
  

    -- Lexicon
        NPmkInt = symb ;
        Prime = mkA "prime" ;
        Coprime = mkA "coprime" ;
        And = and_Conj ;
        Or = or_Conj ;


  

}
