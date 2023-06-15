# sen2r v. 1.5.5


## Test environments
* [local installation] Windows 11, 64 bit, R 4.3.0: 
  There were no ERRORs, WARNINGs nor NOTEs.
* [local installation] Ubuntu 22.04.2, 64 bit, R 4.3.0: 
  There were no ERRORs, WARNINGs nor NOTEs.
* [macbuilder] macOS 13.3.1 (Mac mini Apple M1), 64 bit, R 4.3.0
  (`https://mac.r-project.org/macbuilder/results/1686841396-983eb296818f2468/`):
  There were no ERRORs, WARNINGs nor NOTEs.
* [devtools] `check_win_devel()`
  (`https://win-builder.r-project.org/Fw686Vbi0d3z/`): 
  There were no ERRORs, WARNINGs nor NOTEs.
* [devtools] `check_win_release()`
  (`https://win-builder.r-project.org/dSW8wwg6b57y/`): 
  There were no ERRORs, WARNINGs nor NOTEs.
* [devtools] `check_win_oldrelease()`
  (`https://win-builder.r-project.org/T21XO5cyA9yj/`): 
  There were no ERRORs, WARNINGs nor NOTEs.

Additional issues (`M1mac`) were present:
```
* checking tests ...
  Running ‘spelling.R’
  Running ‘testthat.R’/Users/ripley/R/R-devel/bin/BATCH: line 60: 50463 Segmentation fault: 11  ${R_HOME}/bin/R -f ${in} ${opts} ${R_BATCH_OPTIONS} > ${out} 2>&1

 ERROR
Running the tests in ‘tests/testthat.R’ failed.
```
This submission should have managed this issue (I cannot be sure about that
because I was not able to reproduce this issue on any MacOS machines, see e.g.
`https://mac.r-project.org/macbuilder/results/1686841396-983eb296818f2468/`
and previous 
`https://mac.r-project.org/macbuilder/results/1686565664-5f7f28746adca176/`,
which are not showing any errors.)
