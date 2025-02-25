abstract Blocks = Math ** {
    flags startcat = Block ;
    cat
        Block ;
        Assumption ;
        Claim ;
    fun
        BlmkCl : Claim -> Block ;
        BlmkAssBl :  Assumption -> Block -> Block ;
        BlmkClBl : Claim -> Block -> Block ;

    -- making Claims and Assumptions
        mkClaim : Sentence -> Claim ;
        mkAssumption : Sentence -> Assumption ;
}