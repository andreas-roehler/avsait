#!/bin/sh

# Author: Andreas Röhler

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# run-tests.sh -[OPTION]
# selecting single test-files

# This script tests functions from avsait.el

# Code:

if [ $1 == en ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif [ $1 == e25 ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e26 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e27 ];then
    #  export EMACS="$HOME/emacs-20220306/src/emacs -Q"
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e28 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e29 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e30 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
else
    EMACS=emacs
fi

echo "
before shift \$EMACS: $EMACS"
shift
echo "after shift \$EMACS: $EMACS"
echo "\$*: $*"
echo "\$_: $_"
PDIR=$PWD
echo "\$PWD: $PWD"
# IFLOCAL set in .bashrc, thus unset remotly
IFLOCAL=${IFLOCAL:=1}
echo "\$IFLOCAL: $IFLOCAL"
TESTDIR=$PWD/test
export TESTDIR

# EMACS_TEST_VERBOSE

SETUP=avsait-setup-ert-tests.el
echo "\$SETUP: $SETUP"

AVSAIT=$PWD/avsait.el

TEST1=$TESTDIR/avsait-ert-pretty-print-tests.el

h1() { 
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq avsait-debug-p nil)" \
--eval "(setq avsait-verbose-p nil)" \
--eval "(setq avsait-install-dir \"$PWD\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
--eval "(add-to-list 'load-path \"$PWD/\")" \
-load $SETUP \
-load $AVSAIT \
-l $TEST1 \
-f ert-run-tests-batch-and-exit
}

entfernt() {
$EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $AVSAIT \
-l $TEST1 \
--eval "(setq avsait-debug-p nil)" \
-f ert-run-tests-batch-and-exit
}

hier() {
    date; $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq avsait-debug-p nil)" \
--eval "(setq python-indent-guess-indent-offset nil)" \
--eval "(setq python-indend-offset 4)" \
--eval "(setq python-mode-v5-behavior-p nil)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $SETUP \
-load $AVSAIT \
-l $TEST1 \
-f ert-run-tests-batch-and-exit
}

if [ $IFLOCAL -eq 0 ]; then

    # sleep 1

    while getopts 123456789abcdefghijklmnopqrstuvx option
    do
        case $option in
	    1) echo "h1: Lade \$TEST1: \"$TEST1\"";h1;;
	    # 2) echo "h2: Lade \$TEST2: \"$TEST2\"";h2;;
	    # 3) echo "h3: Lade \$TEST3: \"$TEST3\"";h3;;
	    # 4) echo "h4: Lade \$TEST4: \"$TEST4\"";h4;;
	    # 5) echo "h5: Lade \$TEST5: \"$TEST5\"";h5;;
	    # 6) echo "h6: Lade \$TEST6: \"$TEST6\"";h6;;
	    # 7) echo "h7: Lade \$TEST7: \"$TEST7\"";h7;;
	    # 8) echo "h8: Lade \$TEST8: \"$TEST8\"";h8;;
	    # 9) echo "h9: Lade \$TEST9: \"$TEST9\"";h9;;
	    # a) echo "h10: Lade \$TEST10: \"$TEST10\"";h10;;
	    # b) echo "h11: Lade \$TEST11: \"$TEST11\"";h11;;
	    # c) echo "h12: Lade \$TEST12: \"$TEST12\"";h12;;
	    # d) echo "h13: Lade \$TEST13: \"$TEST13\"";h13;;
	    # e) echo "h14: Lade \$TEST14: \"$TEST14\"";h14;;
	    # f) echo "h15: Lade \$TEST15: \"$TEST15\"";h15;;
	    # g) echo "h16: Lade \$TEST16: \"$TEST16\"";h16;;
            # h) echo "h17: Running python-tests.el";h17;;
	    # i) echo "h18: Lade \$TEST18: \"$TEST18\"";h18;;
	    # j) echo "h19: Lade \$TEST19: \"$TEST19\"";h19;;
	    # k) echo "h20: Lade \$TEST20: \"$TEST20\"";h20;;
	    # l) echo "h21: Lade neu: \"neu\"";neu;;
	    # m) echo "h22: Lade \$TEST22: \"$TEST22\"";h22;;
	    n) echo "Lade Testumgebung ‘hier’";hier

	esac
	echo "\$*: $*"

    done

    else
    echo "entfernt"
    echo "\$IFLOCAL: $IFLOCAL"
    echo "Lade testumgebung \"ENTFERNT\""
    entfernt
fi
