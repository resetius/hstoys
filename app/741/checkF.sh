rm -f fifo
mkfifo fifo
# ./Ftest --single 3 '2 1 3' < fifo | ./F > fifo
#./Ftest --verbose --single 5 '148095 148093 148091 148094 148092' < fifo | ./F > fifo
#./Ftest --single 4 '4 2 1 3' < fifo | ./F > fifo
#./Ftest --single 5 '5 2 4 1 3' < fifo | ./F > fifo
#./Ftest --multi 1000 < fifo | ./F > fifo
./Ftest --multi 100 9999 < fifo | ./F > fifo
