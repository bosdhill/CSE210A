load ../../harness

@test "9430d32d4bbe" {
  check 'BZ   := x -    -4    ;y:=z    *   -4  ' '⇒ skip; y := (z*-4), {BZ → 4}
⇒ y := (z*-4), {BZ → 4}
⇒ skip, {BZ → 4, y → 0}'
}
