module Language.SimpleC.Printer where

import Language.SimpleC.AST

instance Show Program where
    show (Program (decls,defs)) = 
        let decls' = foldr (\d r -> show d ++ "\n" ++ r) "" decls 
            defs'  = foldr (\d r -> show d ++ "\n" ++ r) "" defs
        in decls' ++ defs'

instance Show Definition where
    show (FunctionDef _ fn params stat) = 
      fn ++ "(" ++ show params ++ "){\n" ++ foldr (\s r -> show s ++ "\n" ++ r) [] stat ++ "\n}"

instance Show a => Show (AnnStatement a) where
    show (ExprStat pc e) = show pc ++ "@"++show e ++ ";"
    show (Local  pc var Nothing) = show pc ++ "@"++show var ++ ";\n" 
    show (Local  pc var (Just v)) = show pc ++ "@"++show var ++ " := " ++ show v ++ ";\n"
--    show (Sequence s1 s2) = show s1 ++ "\n" ++ show s2
    show (IfThen pc c t) = show pc ++ "@if(" ++ show c ++ "){\n" ++ foldr (\s r -> show s ++ "\n" ++ r) [] t ++ "}"
    show (If pc c t e) =  show pc ++ "@if(" ++ show c ++ "){\n" ++ foldr (\s r -> show s ++ "\n" ++ r) [] t ++ "}else{\n" ++ foldr (\s r -> show s ++ "\n" ++ r) [] e ++ "}"
    show (While pc c s) = show pc ++ "@while(" ++ show c ++ "){\n" ++ foldr (\s r -> show s ++ "\n" ++ r) [] s ++ "\n}"
    show (For pc c1 c2 c3 s) = show pc ++ "@for( " ++ show c1 ++ "; " ++ show c2 ++ ";" ++ show c3 ++ "){\n" ++ foldr (\s r -> show s ++ "\n" ++ r) [] s ++ "\n}"
    show (Return pc e) = show pc ++ "@return " ++ show e ++ ";"
--    show (CallS _ fn args) = fn ++ "(" ++ show args ++ ");"
    show (Label pc var stat) = show pc++"@"++show var ++ ": " ++ foldr (\s r -> show s ++ "\n" ++ r) [] stat
    show (Goto pc var) = show pc ++ "@goto " ++ show var++";"
    show (Skip) = "skip;"
    
