exitIf action1 f action2 = do
    (b, v) <- action1
    if b then return ()
        else action2 (f v)



exitIf2 action1 f p action2 = do
    v <- action1
    if p v then return ()
        else action2 (f v) >>
            exitIf2  action1 f p action2
--                         
-- exitIf1 action1 f action2 = do
    -- (b, v) <- action1
    -- if b then return ()
        -- else action2 (f v)
            
        
getInt :: IO Int        
getInt = 
    getLine >>= return.read