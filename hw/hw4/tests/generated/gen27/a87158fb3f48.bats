load ../../harness

@test "a87158fb3f48" {
  check 'skip  ;

if (C*   -1   <z *     0   ∨  true)      then 

  z   :=    -3+    z      else 



x     :=  1     +     3  ' '⇒ if (((C*-1)<(z*0))∨true) then { z := (-3+z) } else { x := (1+3) }, {}
⇒ z := (-3+z), {}
⇒ skip, {z → -3}'
}
