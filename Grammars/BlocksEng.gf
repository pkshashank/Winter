concrete BlocksEng of Blocks = MathEng ** open SyntaxEng, ParadigmsEng in {
    lincat
        Block = Text ;
        Assumption = Imp ;
        Claim = Sentence ;
    
    lin
        BlmkCl s = mkText s ;
        BlmkAssBl a bl = mkText (mkText (mkUtt a) fullStopPunct) bl ;
        BlmkClBl cl bl = mkText (mkText cl) bl ;

    -- making Claims and Assumptions
        mkClaim s = s ;
        mkAssumption s = mkImp (mkVP (mkVS (mkV "Assume")) s) ;
}
