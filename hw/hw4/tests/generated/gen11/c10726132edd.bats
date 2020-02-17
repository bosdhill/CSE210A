load ../../harness

@test "c10726132edd" {
  check 'if (-4   * y =   -3    - -1   ∧    false)    then  
j     :=  x +4      else   
 z:=y *  LE  ' '⇒ z := (y*LE), {}
⇒ skip, {z → 0}'
}
