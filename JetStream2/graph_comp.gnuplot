set terminal pdf enhanced size 7,1.5 font 'Linux Libertine, 8'

set datafile separator ","

set style line 1 lt 1 lc rgb "#6db6ff"
set style line 2 lt 1 lc rgb "#490092"

# Box setup
set style fill solid
set style histogram clustered
set style data histograms

# Remove top / right borders
set border 3 
set ytics nomirror
set xtics nomirror

set ylabel "Score"

# Legend
set key left top

# Xtics and labels
set xtic scale 0 #hide
set xtics rotate by 30 right
set bmargin 6

set lmargin 8
set rmargin 1

set output 'jetstream2.pdf'
plot '<(sed -n "1,33p" ./jetstream2.csv)' using 2:xtic(1) ti col lc rgb "#6db6ff", '' u 3:xtic(1) ti col lc rgb "#490092" #, '' u 13 ti col

plot '<(sed -n "1p;34,66p" ./jetstream2.csv)' using 2:xtic(1) ti col lc rgb "#6db6ff", '' u 3:xtic(1) ti col lc rgb "#490092" #, '' u 13 ti col

