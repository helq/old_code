### e-mail convoluted

## === first try ===
# bGludXhlcm83ODlAZ21haWwuY29tCg==
# Z        n     V ja      wo    =

sed '{
s/ZnVj/bGludXhlcm83ODlAZ2/
s/aw/1haWwuY2/
s/o=/9tCg==/
}' <<<'ZnVjawo=' | base64 -d

sed '{
s/Z[^789]V\(j\)/bGludXhlcm83ODlAZ2/
s/\(a\)\(w\)/1h\1W\2uY2/
s/o\([^WU9VCg==]*=\)/9tCg\1\1/
}' <<<'ZnVjawo=' | base64 -d

sed "$(printf '{\ns/Z[^789]V\(j\)/bGludXh%sDlAZ2/\ns/\(a\)\(w\)/1h\\1W\\2uY2/\ns/o\([^WU9VCg==]*=\)/9tCg\\1\\1/\n}' lcm83O)" <<<'ZnVjawo=' | base64 -d


# === second try ===
b=@;a=g'mail'\.;z=ero;b(){ echo $5$z$2$3$4$b$1om;};b $a'c' {7..9} linux


# === third try ===

# aGVsc UBsa      XZlLmRlCg==
# │   │ │_ │____  │ ________│
# │   │  │      │ │ │
# dHJ5I  GhhcmRlc go=

sed '{
s/dHJ5I/aGVsc/
s/GhhcmR/UBsa/
s/lcgo=/XZlLmRlCg==/
}' <<<'dHJ5IGhhcmRlcgo=' | base64 -d

sed '{
s/d[^axo5]*5\(I\)/aG\x56\x73c/
s/Gh*cm\([^WT3Ou2YuKM67eC54IHgpKM67eC5mKHggeCkpCg==]\)/UBsa/
s/\(.\)c\(\x21*g\)o\(\x3d\)/XZ\1LmR\1C\2\3\3/
}' <<<'dHJ5IGhhcmRlcgo=' | base64 -d

#   │       \342\224\202
#   ├       \342\224\234
#   ─       \342\224\200
#   ┌       \342\224\214
#   └       \342\224\224
#   ╖       \342\225\226
#   ┘       \342\224\230
#   ┐       \342\224\220
#   ┼       \342\224\274
#   ╨       \342\225\250
#   ╷       \342\225\267
#   ╵       \342\225\265
#   ┤       \342\224\244
#   ╴       \342\224\264


echo && sed "$(tr -d ' \342\202\234\200\214\224\226\230\220\274\250\225\267\265\244\264' <<<'{ 
s/d[^ax       │     │     ┌──┐  │o            │    '\
'\x35]*5\  (  ├─┐┌─┐│┌─┐  │┌╖│  │╷╷  ╷┌─┐   ┌─┤┌─┐ I\)/aG\x56\x73c/
 s/Gh *cm\([^ │ │├─┘││ │  │└╨┘  ││└┐┌┘├─┘   │ │├─┘ WT3Ou2YuKM67eC54IHgpKM67eC5mKHggeCkpCg==]\)/UBsa/
s/\(.\)c\(g\) ╵ ╵└─╴╵└─┤  └─┘   ╵╵ └┘ └─╴ o └─┘└─╴   \(\x3d\)/XZ\1LmR\1C\2\3\3/
}                      ┼   ')" <<<'dHJ5IGhhcmRlcgo=' | base64 -d
