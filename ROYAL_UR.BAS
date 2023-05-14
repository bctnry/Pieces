5010 ' Royal Game of Ur (in GW-BASIC.)
5020 ' (c) Sebastian Higgins 2023.
5025 ' 2023.5.14

5030 ' A - Player 1 available tile count
5040 ' B - Player 1 finished tile count
5050 ' C - Player 2 available tile count
5060 ' D - Player 2 finished tile count
5070 ' E - Dice
5080 ' F - Board for player 1
5090 ' G - Board for player 2
5100 ' H - Move available verdict
5110 ' J - Moves
5120 ' `INPUT "", Y` is used as GETKEY here.

10 DEFINT A-H, J-K, Z
11 A = 7
12 C = 7
13 H = 0
14 CLS
15 OPTION BASE 1
20 DIM F(15), G(15), J(14)
30 RANDOMIZE TIMER
31 DEF FNW (X) = X >= 5 AND X <= 12
32 DEF FNS (X) = X = 14 OR X = 4 OR X = 8

40 WHILE B < 7 AND D < 7
45   H = 0
47   CLS
48   GOSUB 1500
49   PRINT "======== PLAYER 1 ========"
50   GOSUB 2000
65   GOSUB 1000
70   PRINT "Dice verdict for player 1: "; E
80   IF NOT (E = 0) THEN GOTO 120
110  PRINT "Player 1 rolled a 0; cannot move."
111  INPUT "", Y
112  GOTO 275
120  IF NOT (A > 0 AND F(E) = 0) GOTO 130
127    H = 1
128    J(E) = 2
130  FOR I = 1 TO 14
135    IF NOT(F(I) = 1) GOTO 210
140    IF NOT(I + E = 15) GOTO 165
146    H = 1
147    J(I) = 1
155    GOTO 210
165    IF I + E > 15 GOTO 210
170    IF F(I + E) = 1 GOTO 210
175    IF G(I + E) AND FNS(I + E) AND FNW(I + E) GOTO 210
185    H = 1
186    J(I) = 1
210  NEXT I
215  IF H = 1 THEN GOTO 220
216  PRINT "No available choice..."
217  INPUT "", Y
218  GOTO 275
220  PRINT "Available choices:"
221  GOSUB 1550
224  INPUT "Your choice: ", Z
225  IF J(Z) = 1 OR J(Z) = 2 GOTO 230
226  PRINT "This choice is not available."
227  GOTO 220
230  IF Z <= 4 AND F(Z) = 0 THEN GOTO 265
235  IF NOT (Z + E = 15) THEN GOTO 240
236  B = B + 1
240  F(Z) = 0
250  F(Z + E) = 1
260  IF NOT (FNW(Z + E) AND G(Z + E) = 1) GOTO 263
261  G(Z + E) = 0
262  C = C + 1
263  IF FNS(Z + E) THEN GOTO 268 ELSE GOTO 275
265  F(Z) = 1
266  A = A - 1
267  IF NOT FNS(Z) GOTO 275
268  PRINT "Player 1 landed on a special tile!"
269  INPUT "", Y
270  GOTO 45

275  H = 0
276  GOSUB 1500
277  CLS
278  PRINT "======== PLAYER 2 ========"
280  GOSUB 2000
300  GOSUB 1000
310  PRINT "Dice verdict for player 2: "; E
320  IF NOT (E = 0) THEN GOTO 350
330  PRINT "Player 2 rolled a 0; cannot move."
335  INPUT "", Y
340  GOTO 561
350  IF NOT (C > 0 AND G(E) = 0) GOTO 360
356    J(E) = 2
357    H = 1
360  FOR I = 1 TO 14
370    IF NOT(G(I) = 1) GOTO 470
380    IF NOT(I + E = 15) GOTO 415
395    H = 1
396    J(I) = 1
400    GOTO 470
415    IF I + E > 15 GOTO 470
420    IF G(I + E) = 1 GOTO 470
430    IF F(I + E) AND FNS(I + E) AND FNW(I + E) GOTO 470
445    H = 1
446    J(I) = 1
470  NEXT I
475  IF H = 1 THEN GOTO 480
476  PRINT "No available choice..."
477  INPUT "", Y
478  GOTO 561
480  PRINT "Available choices:"
481  GOSUB 1550
485  INPUT "Your choice: ", Z
486  IF J(Z) = 1 OR J(Z) = 2 THEN GOTO 490
487  PRINT "This choice is not available."
488  GOTO 480
490  IF Z <= 4 AND G(Z) = 0 THEN GOTO 530
495  IF NOT (Z + E = 15) GOTO 500
496  D = D + 1
500  G(Z) = 0
510  G(Z + E) = 1
520  IF NOT (FNW(Z + E) AND F(Z + E) = 1) GOTO 525
521  F(Z + E) = 0
522  A = A + 1
525  IF FNS(Z + E) THEN GOTO 540 ELSE GOTO 561
530  G(Z) = 1
535  C = C - 1
536  IF NOT FNS(Z) GOTO 561
540  PRINT "Player 2 landed on special tile!"
541  INPUT "", Y
542  GOTO 275
561 WEND
     
600 IF B = 7 THEN PRINT "Player 1 wins" ELSE PRINT "Player 2 wins"
610 END

5100 ' SUB: Roll dice.
1000 E = INT(RND * (4 + 1))
1001 RETURN

5200 ' SUB: Clear J
1500 FOR K = 1 TO 14
1520 J(K) = 0
1530 NEXT K
1540 RETURN

5300 ' SUB: Print choices
1550 FOR K = 1 TO 14
1555 IF J(K) = 1 THEN PRINT USING "#= "; K;
1560 IF J(K) = 2 THEN PRINT USING "+# "; K;
1570 NEXT K
1580 PRINT
1590 RETURN

5400 ' SUB: Print board.
2000 PRINT "Player 1: "; A; " "; B
2005 PRINT " 1 2 3 4 5 6 7 8 9 A B C D E "
2010 PRINT "+-+-+-+-+               +-+-+"
2020 PRINT "|";
2021 FOR I = 1 TO 4
2030   IF F(I) = 1 THEN GOTO 2031 ELSE GOTO 2035
2031   PRINT "A|";
2032   GOTO 2050
2035   IF FNS(I) THEN PRINT "*|";  ELSE PRINT " |";
2050 NEXT I
2051 PRINT "               |";
2052 FOR I = 13 TO 14
2053   IF F(I) = 1 THEN GOTO 2054 ELSE GOTO 2056
2054   PRINT "A|";
2055   GOTO 2057
2056   IF FNS(I) THEN PRINT "*|";  ELSE PRINT " |";
2057 NEXT I
2058 PRINT
2059 PRINT "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
2060 PRINT "        |";
2063 FOR I = 5 TO 12
2080   IF F(I) = 1 OR G(I) = 1 THEN GOTO 2090 ELSE GOTO 2167
2090   IF F(I) = 1 THEN PRINT "A|";  ELSE PRINT "B|";
2166   GOTO 2168
2167   IF FNS(I) THEN PRINT "*|";  ELSE PRINT " |";
2168 NEXT I
2169 PRINT
2170 PRINT "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
2180 PRINT "|";
2190 FOR I = 1 TO 4
2200   IF G(I) = 1 THEN GOTO 2210 ELSE GOTO 2230
2210   PRINT "B|";
2220   GOTO 2240
2230   IF FNS(I) THEN PRINT "*|";  ELSE PRINT " |";
2240 NEXT I
2250 PRINT "               |";
2260 FOR I = 13 TO 14
2270   IF G(I) = 1 THEN GOTO 2280 ELSE GOTO 2300
2280   PRINT "B|";
2290   GOTO 2310
2300   IF FNS(I) THEN PRINT "*|";  ELSE PRINT " |";
2310 NEXT I
2315 PRINT
2320 PRINT "+-+-+-+-+               +-+-+"
2330 PRINT "Player 2: "; C; D
2340 RETURN
