load ../../harness

@test "093651c8ab32" {
  check 'if (x    * -4 =  3  * 3  ∨1    +    z  =y *  y) then     z    :=y    +x  else     x    :=   qR -     hc     ' '⇒ x := (qR-hc), {}
⇒ skip, {x → 0}'
}
