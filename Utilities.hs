module Utilities where

import LogicalTheory
import LogicalLexicon

(<||>) :: Either a b -> Either a b -> Either a b
Left _ <||> r = r
l     <||> _ = l