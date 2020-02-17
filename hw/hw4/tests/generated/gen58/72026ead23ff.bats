load ../../harness

@test "72026ead23ff" {
  check 'c  :=     z *  z   ; a    :=  z -    3   ' '⇒ skip; a := (z-3), {c → 0}
⇒ a := (z-3), {c → 0}
⇒ skip, {a → -3, c → 0}'
}
