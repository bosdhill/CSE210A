load ../../harness

@test "d4dcd1ddc59f" {
  check 'if (2  *     fl   < 2  +     -2     ∨    true)      then z    :=  2     - kw   else  
 y   :=y +  2  ' '⇒ z := (2-kw), {}
⇒ skip, {z → 2}'
}
