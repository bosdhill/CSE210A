doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

---list comprehension
--- [x*2 | x <- [1..10]]

---list comprehension with predicate 
--- [x*2 | x <- [1..10], x*2 >= 12]

---list comprehension
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 

--- oof	
insults nouns adjectives = [ y ++ " " ++ x | x <- nouns, y <- adjectives]  
