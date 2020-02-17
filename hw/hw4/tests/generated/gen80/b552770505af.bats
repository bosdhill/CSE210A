load ../../harness

@test "b552770505af" {
  check 'v  :=  BS    -  z   ; 
y:=   x*   -1 ' '⇒ skip; y := (x*-1), {v → 0}
⇒ y := (x*-1), {v → 0}
⇒ skip, {v → 0, y → 0}'
}
