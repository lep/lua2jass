function lli {
    runhaskell compile.hs "$1" | \
        python interpreter.py /dev/stdin 2>/dev/null | \
        sed 's/None/nil/g' | \
        sed 's/False/false/g' | \
        sed 's/True/true/g'
}

orig=$(lua "$1")
mine=$(lli "$1")

[ "$orig" = "$mine" ] || exit 1;
