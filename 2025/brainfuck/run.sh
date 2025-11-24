file=$1
day=${file%-[AB]}
make $file.out
./$file.out < ../input/$day.in
