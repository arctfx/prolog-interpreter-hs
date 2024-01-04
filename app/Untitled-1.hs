
data Status = Success | Failed | Unknown

type Database = [AST]
-- read db = [x_1,...,x_k-1], rest of db = [x_k,...,x_n]
-- curr = x_k
data Position a = Position [a] [a]

next :: Position a -> Position a
next Position [] (x : xs) = Position [x] xs
next Position (x : xs) [] = Position [] (x : xs)
next Position db (x : xs) = Position (db ++ x) xs  
next p = p -- Position [] []

getAt :: Position a -> Maybe a
getAt Position [] [] = Nothing 
getAt Position _ (x : xs) = Just x
getAt Position (x : xs) [] = Nothing -- position at the end of the list

end :: Position a -> Bool
end Position (x : xs) [] = True
end Position _ _ = False

-- sets the position at the begining of db [a]
begin :: Position a -> Position a
begin Position [] [] = Position [] []
begin Position db [] = Position [] db
begin Position sub rest = Position [] (sub ++ db)

data Frame = Frame Position [Pterm] Pterm -- position, resolvent, goal
type Stack = [Frame]

-- ALG
-- wrapper
-- sStatus, sStack, sFrame, sGoal, sCompare
data SVars = SVars Status Stack Frame Pterm Pterm

sStatus = Unknows
sStack = [Frame (Position [] db) [sG] sG] -- set position at the beginning of the database

-- while sStack is not empty
loop sStack where

loop ::  
loop [] = ...
loop (x : xs) =
    -- pop a sFrame from the stack; sFrame = x
    let x = (Frame pos resolvent goal) in
        loopResolvent resolvent
        where
            -- loopResolvent ::  
            loopResolvent (t : ts) =
                -- sGoal = t
                -- pop sGoal from the resolvent = ts
                loopPos pos t ts
                where
                    -- loopPos ::
                    loopPos sGoal sStack (Frame pos resolvent goal) =
                        if end pos == False then
                            -- while body
                            -- sCompare = getAt pos
                            -- rename vars in sCompare
                            -- advance sFrame pos (4 line from here)
                            -- sStatus = Unknown
                            case plUnify sGoal (getAt pos) of
                                Just u ->
                                    if not $ end $ next pos then
                                        newStack = Frame (next pos) : sStack
                                        foo newStack
                                    else 
                                        foo sStack
                                    where
                                        foo stack =
                                            newCompare = plUnifierApplyToTerm u $ getAt pos
                                            newResolvent = newCompare ++ [plUnifierApplyToTerm u term | term <- resolvent]
                                            newGoal = plUnifierApplyToTerm u goal -- sFrame.goal
                                            newPos = begin pos
                                            sStatus = Success
                                            -- return 
                                -- Nothing -> loopRes ...
                            loopPos $ next pos
                        else -- return
                if end newPos && not (sStatus == Success) then
                    -- break
                else -- loop
        if newResolvent == [] && sStatus == Success then
            return goal
        else loop xs
Nothing