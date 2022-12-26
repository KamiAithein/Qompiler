module Quack.Evaluate where

import Quack.Common

import qualified Data.HashMap.Strict as H

-- 
simplify :: ParseTree -> ParseTree
simplify parseTree@ParseTree{pAst=ASTIdentity} = parseTree
simplify parseTree@ParseTree{pAst=pAst@(ASTLabel s), pContext=ctx} =
    case H.lookup s ctx of
        Just body -> parseTree{pAst=body}
        Nothing   -> parseTree

simplify parseTree@ParseTree{pAst=ASTApp apper appee, pContext=ctx} =
    let [ apper'@ParseTree{pAst=apper'pAst} , appee'@ParseTree{pAst=appee'pAst}] = 
            map (simplify . (parseTreeFrom ctx)) [apper, appee]
        result = apply (apper'pAst) (appee'pAst)
    in parseTreeFrom ctx $ result

simplify parseTree@ParseTree{pAst=ASTFunction param Nothing} = parseTree
simplify parseTree@ParseTree{pAst=ASTFunction param (Just body), pContext=ctx} =
    parseTree{pAst=ASTFunction param $ (Just . pAst . simplify . parseTreeFrom ctx) body}

-- {-
-- (\a.b)c
-- -}
apply :: AST -> AST -> AST
apply  ASTIdentity other = other
apply (ASTFunction param Nothing) other = ASTIdentity
apply (ASTFunction param (Just body)) other = subInWith param body other
            --subInWith x y z = y[x/z]
    where   subInWith :: Param -> AST -> AST -> AST

            subInWith p body@(ASTLabel (s:_)) with
                    | p == s = with
                    | otherwise = body 
    
            subInWith p (ASTApp apper appee) with = 
                let subber = (\body -> subInWith p body with)
                in ASTApp (subber apper) (subber appee)
    
            subInWith p body@(ASTFunction p2 Nothing) with = body
            subInWith p body@(ASTFunction p2 (Just body2)) with =
                ASTFunction p2 $ Just $ subInWith p body2 with
    
            subInWith p (ASTIdentity) with = ASTIdentity
-- apply (ASTFunction param Nothing) appee = ASTIdentity
-- apply (ASTFunction param (Just body)) appee = subWithIn param appee body
--     where   subWithIn :: Param -> AST -> AST -> AST
--             --(\\a.Identity)_
--             subWithIn param with (ASTIdentity) = 
--                 ASTIdentity
--             subWithIn param with body@(ASTLabel (l:_)) -- params only 1 char 
--                 --(\\a.a)b
--                 | param == l = with
--                 | otherwise  = body
            
--             subWithIn param with (ASTApp apper appee) =
--                 let [apper', appee'] = map (subWithIn param with) [apper, appee]  
--                 in apply $ ASTApp apper' appee'
            
--             subWithIn param with body@(ASTFunction param2 Nothing) = 
--                 body
            
--             subWithIn param with (ASTFunction param2 (Just body2)) =
--                 ASTFunction param2 $ Just $ subWithIn param with body2 -- this ignores binding of variables
apply apper appee = ASTApp apper appee -- cannot simplify