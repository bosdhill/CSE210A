load ../../harness

@test "236a26e261a4" {
  check 'if (false∨     false)  then 
   l     :=    z   +y    else   z   :=     x    *     z  ' '⇒ z := (x*z), {}
⇒ skip, {z → 0}'
}
