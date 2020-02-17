load ../../harness

@test "735ad9deb4f4" {
  check 'z   := y   *-4    ; 
 x    :=    y  *    -3     ' '⇒ skip; x := (y*-3), {z → 0}
⇒ x := (y*-3), {z → 0}
⇒ skip, {x → 0, z → 0}'
}
