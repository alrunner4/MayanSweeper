rm -f Minesweeper Minesweeper.prof
doMakeProfile && Minesweeper +RTS -p && open -a xcode Minesweeper.prof
