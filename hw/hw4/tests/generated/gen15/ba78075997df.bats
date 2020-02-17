load ../../harness

@test "ba78075997df" {
  check 'if (true ∧     -3- eC   <   2 *  z)      then    z:=  1 else skip   ' '⇒ z := 1, {}
⇒ skip, {z → 1}'
}
