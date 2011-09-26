{-# LANGUAGE Arrows #-}

hangman :: StdGen -> Circuit String [String]
hangman rng = proc userInput -> do
    word <- getWord rng -< ()
    let letter = listToMaybe userInput
    guessed <- updateGuess -< (word, letter)
    hung <- updateHung -< (word, letter)
    let hangman = "HangMan: ["++replicate hung '#'++replicate (5-hung) ' '++"]"
    returnA -<
        if word == guessed
            then                   [guessed, "You won!"]
            else
                if hung >= 5 then  [guessed, hangman, "You died!"]
                             else  [guessed, hangman]
  where
    updateGuess :: Circuit (String, Maybe Char) String
    updateGuess = accum' (repeat '_') $ \(word, letter) guess ->
        case letter of
            Just l  -> map (\(w, g) -> if w == l then w else g) (zip word guess)
            Nothing -> take (length word) guess
    updateHung :: Circuit (String, Maybe Char) Int
    updateHung = proc (word, letter) -> do
        total -< case letter of
            Just l  -> if l `elem` word then 0 else 1
            Nothing -> 0

main = do
    rng <- getStdGen
    interact (unlines .                     -- Concatenate lines out output
        ("Welcome to Arrow Hangman":) .     -- Prepend a greeting to the output
        concat . runCircuit (hangman rng) . -- Process the input lazily
        ("":) .                             -- Act as if the user pressed ENTER once at the start
        lines)                              -- Split input into lines
