load ../../harness

@test "dbb6648ff26c" {
  check 'Yk:=     4 -    1   ; 
y   :=     -3     *  1   ' '⇒ skip; y := (-3*1), {Yk → 3}
⇒ y := (-3*1), {Yk → 3}
⇒ skip, {Yk → 3, y → -3}'
}
