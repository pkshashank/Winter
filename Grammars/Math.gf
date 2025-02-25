abstract Math = {
    flags startcat = Sentence ;
    cat 
        Sentence ;
        NounPhrase ;
        VerbPhrase ;
        Adjective ;
        Conjunction ;
   
    fun
        SmkNPVP : NounPhrase -> VerbPhrase -> Sentence ;
        VPmkAdj : Adjective -> VerbPhrase ;
        NPconj : Conjunction -> NounPhrase -> NounPhrase -> NounPhrase ;

    -- Lexical entries
        Two : NounPhrase ;
        Three : NounPhrase ;
        Prime : Adjective ;
        Coprime : Adjective ;
        And, Or : Conjunction ;

}
