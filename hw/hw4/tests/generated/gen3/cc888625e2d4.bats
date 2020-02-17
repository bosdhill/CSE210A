load ../../harness

@test "cc888625e2d4" {
  check 'if (J   + 2<     z -    1)     then {I     :=  x    *     y   ;
x   :=    0}      else 
MG   :=   2  ' '⇒ MG := 2, {}
⇒ skip, {MG → 2}'
}
