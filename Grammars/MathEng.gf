concrete MathEng of Math = open SyntaxEng, ParadigmsEng in {
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
        Two = mkNP (mkPN ("two" | "2")) ;
        Three = mkNP (mkPN ("three" | "3")) ;
        Prime = mkA "prime" ;
        Coprime = mkA "coprime" ;
        And = and_Conj ;
        Or = or_Conj ;


  

}
