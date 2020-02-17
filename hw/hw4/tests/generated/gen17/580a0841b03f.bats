load ../../harness

@test "580a0841b03f" {
  check 'if (false   ∨   1   *   2    <    y   -x)    then x   :=-4   -    x else  
Vz  :=y *  z   ' '⇒ Vz := (y*z), {}
⇒ skip, {Vz → 0}'
}
