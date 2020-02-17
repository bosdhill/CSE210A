load ../../harness

@test "101a8e92020a" {
  check 'if (¬(l+     WT     <    -2   + z))    then  x    :=    yt   *    -4  else skip' '⇒ x := (yt*-4), {}
⇒ skip, {x → 0}'
}
