module Language.SimpleC.Printer where

import Language.SimpleC.AST

instance Show Program where
    show (Program (decls,defs)) = 
        let decls' = foldr (\d r -> show d ++ "\n" ++ r) "" decls 
            defs'  = foldr (\d r -> show d ++ "\n" ++ r) "" defs
        in decls' ++ defs'

instance Show Definition where
    show (FunctionDef _ fn params stat) = 
      fn ++ "(" ++ show params ++ "){\n" ++ show stat ++ "\n}"

instance Show a => Show (AnnStatement a) where
    show (ExprStat _ e) = show e ++ ";"
    show (Local  _ var Nothing) = show var ++ ";" 
    show (Local  _ var (Just v)) = show var ++ " := " ++ show v ++ ";"
    show (Sequence s1 s2) = show s1 ++ "\n" ++ show s2
    show (IfThen _ c t) = "if(" ++ show c ++ "){\n" ++ show t ++ "\n}"
    show (If _ c t e) =  "if(" ++ show c ++ "){\n" ++ show t ++ "\n}else{\n" ++ show e ++ "\n}"
    show (While _ c s) = "while(" ++ show c ++ "){\n" ++ show s ++ "\n}"
    show (For _ c1 c2 c3 s) = "for( " ++ show c1 ++ "; " ++ show c2 ++ ";" ++ show c3 ++ "){\n" ++ show s ++ "\n}"
    show (Return _ e) = "return " ++ show e ++ ";"
--    show (CallS _ fn args) = fn ++ "(" ++ show args ++ ");"
    show (Label _ var stat) = show var ++ ": " ++ show stat
    show (Goto _ var) = "goto " ++ show var++";"
    show (Skip) = "skip;"
    
