Usage
====

Start app with:

    ./run

Then enter name and expense, one per line. End input with a blank line.

Example:

    ###
    ./run
    alice 9
    bob 7

    ###

Run with input from stdin:

    echo -e 'alice 9\nbob 7\n'  | ./run 2>/dev/null

Help messages are printed on stderr, and can thus be redirected in batch mode.
