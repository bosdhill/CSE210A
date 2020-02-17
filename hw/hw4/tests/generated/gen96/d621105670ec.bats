load ../../harness

@test "d621105670ec" {
  check 'if (¬false) then skip     else   y  :=  161925106361765248759434910652773038786     *f5    ' '⇒ skip, {}'
}
