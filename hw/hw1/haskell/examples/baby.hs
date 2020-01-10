doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2
mapList x y = if not null y
                then
                    (x head y) :: (mapList x tail y)
                else
                    []