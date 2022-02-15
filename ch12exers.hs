module Ch12 where

toA :: String -> String
toA x = if (x == "the" || x == "The") then "a" else x

replaceThe :: [Char] -> [Char]
replaceThe x = init (concat(map (++" ") ((map toA (words x)))))

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel sentence = countTheBeforeVowel' sentence 0
 where countTheBeforeVowel' sentence counter
        | (length (words sentence)) <= 2 = if ((elem (head ((words sentence) !! 1)) "aeiou") && ((head (words sentence)) == "the") || ((head (words sentence)) == "The") || ((head (words sentence)) == "THE")) then (counter + 1) else (counter)
        | ((head (words sentence)) == "the") || ((head (words sentence)) == "The") || ((head (words sentence)) == "THE") = if (elem (head ((words sentence) !! 1)) "aeiou") then countTheBeforeVowel' (init (concat (map (++" ") (tail $ tail (words sentence))))) (counter + 1) else countTheBeforeVowel' (init (concat (map (++" ") (tail $ tail (words sentence))))) counter
        | otherwise = counter


-- recursive function takes first word of a list (x:xs), if it's "the" checks the next word of the list, heads that word (takes first element), and if that is aeiou (can be done with || "a" || "e" or elem), then add one to the counter argument (this will be a where function inside the original)


vowelCount :: String -> Integer
vowelCount xs = toInteger (length [v | v <- xs, elem v "aeiou"])

consonantCount :: String -> Integer
consonantCount xs = toInteger (length [c | c <- xs, (not (elem c "aeiou"))])

isWord word = if (vowelCount word) > (consonantCount word) then False else True