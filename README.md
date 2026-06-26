remember that we have the 1° 5 lines of a .sco like this

```text
i1 0 0 1

; advance statement ;
a 0 0 0

```

which are ignored from 'harvest (function that grab values from p-fields)
<br><br><br>
you can set the default layout at `customize-group`, as well the path for the file that stores information of p-fields! `C-c I` edit this file and `C-q` quits and save it - if you `C-c i` with your cursor at a p-field, it will message the text set. 1° line is the instrument ID, 2° line start, 3° duration etc., each paragraph is a new definition of an instrument
<br><br><br>
`csound-recalculate-starts` (set to CTRL+SHIFT+TAB) will either grab a selection or it will grab the line the cursor is as the anchor for calculations, till the end of the paragraph! it accounts for custom and native macros but will only adjust numeric values

```text
i5 0 15 (cursor here)
i5 1 5
i5 + 5
i5 1 5
```
will be re-calculated to
```text
i5 0  15
i5 15 5
i5 +  5
i5 25 5
```
<br><br>
we have 2 custom macros, `++N` `+-N`, which acts like an ordinary `+` and a sum/subtraction of a value (N)! better than `^+N` as it requires one to account the duration of the last note if we want to overlap it by any value. in case you need to check the file output, the mode parses it into `/tmp` and runs Csound from there
<br><br><br>
you can cyle between p-fields using SHIFT+TAB or TAB
<br><br><br>
`csound-cycle-column` accepts 4 arguments, the column number (starts with 0) to cycle, the direction, if it'll cycle on decimals (useful and maybe only useful, if your using cpsxpch as your note abstraction) and a 0 or 1 set if the cycle is tied to [L]
<br><br>
when you open Csound or csound-mode, it will harvest (you can execute this function by yourself (`harvest-all-columns-to-cycle-list`)) values from instruments, each column is a dedicated list and each paragraph will have a dedicated list too. you set the cycle logic by setting states via `csound-toggle-cycle-scope`, [L] local, [G] global, [A] all. if your cycle-column didn't defined the LOCAL-ONLY argument or your using `csound-cycle-current-column` (cycles through the column cursor is)
```text
i5 0 5 10
i5 0 5 11

i5 0 5 10
i5 0 5 12

i3 0 5 17

[G] list for i5 (10, 11, 12)
[L] list for i5 (10, 11) or (10, 12)
[A] list is (10, 11, 12, 17)
```
<br><br>
`play-from-point` will ask for a numeric value and it will set `a` on the beginning of the .sco (keep in mind we have `; advance statement ;` as a referential line) to a line that has a comment with `pN`, e.g. of (`play-from-point` `2`)
```text
i11  27.01   .73   .55   6.26
i11  27.74   1.46  .55   6.22  2.9
i11  29.2    1.46  .55   6.18  3
i11  ++2.92  .73   .53   6.30  2.8   ;p1
i11  34.31   1.46  .55   6.28  3.2
i11  35.77   .73   .53   6.30  3.5
i11  36.5    1.46  .54   6.28  3.3
i11  37.96   1.46  .517  6.30  3.8   ;p2
i11  39.42   .73   .48   7.00  3.9
i11  40.15   .73   .547  6.30  3.8
i11  40.88   .73   .57   6.28  3.8
i11  41.61   .73   .512  6.26  3.1
```
will start playing the score from 37.96, doesn't matter where your cursor is
<br><br><br>
`csound-stop-and-track` will check when the play was stopped and it will move the cursor to the closest line with a `str` (p2) statement in the current paragraph! useful for tracking where you are at the score
<br><br><br>
`csound-smart-duplicate` will create a new line with the `p2` value by the previous line `p2 + p3`, `csound-custom-duplicate` asks for a number to sum from the previous line's `p2`
<br><br><br>
guess that's it. check my ENGRAM layout (thanks Arno https://github.com/binarybottle/engram) shortcuts for anything else (guess the rest is self explanatory)... there are `csound-vega-XXXXXX` functions which will require: https://altair-viz.github.io/ and they will render some chart visualization of the current score! it's a WIP, you can check an output at: https://happort.org/temp
