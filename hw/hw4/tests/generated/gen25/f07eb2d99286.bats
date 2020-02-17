load ../../harness

@test "f07eb2d99286" {
  check 'if (2    *     al    <y    +  0∧     true)     then  z   :=     y     +     y   else   y    :=   z ' '⇒ y := z, {}
⇒ skip, {y → 0}'
}
