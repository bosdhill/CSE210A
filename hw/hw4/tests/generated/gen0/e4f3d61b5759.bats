load ../../harness

@test "e4f3d61b5759" {
  check 'if (2   + x     <     3  -     y     ∨   y  =     -3 +    x)   then  
y  := -3 +    1   else skip' '⇒ y := (-3+1), {}
⇒ skip, {y → -2}'
}
