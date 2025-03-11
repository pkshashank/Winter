abstract Math = {
    flags startcat = Sentence ;
    cat 
        Sentence ;
        NounPhrase ;
        VerbPhrase ;
        Adjective ;
        Conjunction ;
        CommonNoun ;
        Determiner ;
   
    fun
        SmkNPVP : NounPhrase -> VerbPhrase -> Sentence ;
        VPmkAdj : Adjective -> VerbPhrase ;

        NPconj : Conjunction -> NounPhrase -> NounPhrase -> NounPhrase ;
        NPmkDetCN : Determiner -> CommonNoun -> NounPhrase ;

    -- Lexical entries
        NPmkInt : Int -> NounPhrase ;

        Prime, Coprime, Countable : Adjective ;


        And, Or : Conjunction ;

        Integer, Real_Number : CommonNoun ;


        The_Pl, The_Sg : Determiner ;
  

}
