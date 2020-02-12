#!/bin/bash

cd /data/un-comtrade

codes=(4 8 12 16 20 24 660 10 28 32 51 533 36 40 31 44 48 50 52 112 56 84 204 60 64 68 535 70 72 74 76 86 96 100 854 108 116 120 124 132 136 140 148 152 156 162 166 170 174 178 180 184 188 191 192 531 196 203 384 208 262 212 214 218 818 222 226 232 233 231 238 234 242 246 250 254 258 260 266 270 268 276 288 292 300 304 308 312 316 320 831 324 624 328 332 334 336 340 344 348 352 356 360 364 368 372 833 376 380 388 392 832 400 398 404 296 408 410 414 417 418 428 422 426 430 434 438 440 442 446 807 450 454 458 462 466 470 584 474 478 480 175 484 583 498 492 496 499 500 504 508 104 516 520 524 528 540 554 558 562 566 570 574 580 578 512 586 585 275 591 598 600 604 608 612 616 620 630 634 642 643 646 638 652 654 659 662 663 666 670 882 674 678 682 686 688 690 694 702 534 703 705 90 706 710 239 728 724 144 729 740 744 748 752 756 760 158 762 834 764 626 768 772 776 780 788 792 795 796 798 800 804 784 826 840 581 858 860 548 862 704 92 850 876 732 887 894 716)

pid=$(pgrep -f download)
rm -f $(ls -lth | grep 116 | awk '{print $9}')

if [[ -n "${pid}" ]]; then
    exit 1
fi

i=0
while [[ i -lt 247 ]]; do
    sub=(${codes[@]:$i:5})
    partner=$(printf "%%2C%s" "${sub[@]}")
    partner=${partner:3}
    for y in {2000..2019}; do
        echo "$partner,$y"
        month=("${y}01%2C${y}02%2C${y}03%2C${y}04" "${y}05%2C${y}06%2C${y}07%2C${y}08" "${y}09%2C${y}10%2C${y}11%2C${y}12")
        k=0
        for m in ${month[@]}; do
            filename="${y}-${i}-${k}.csv"
            if [[ ! -f "${filename}" ]]; then
                line=$(curl -X GET "https://comtrade.un.org/api/get?max=50000&type=C&freq=M&px=HS&ps=${m}&r=all&p=${partner}&rg=1%2C2&cc=271111%2C271121%2C270900%2C2709&uitoken=9d654581976af10d33fa464e5298539e&fmt=csv")
                flag_data=$(echo $line | grep "No data")
                flag_complex=$(echo $line | grep "complex")
                flag_rate=$(echo $line | grep "LIMIT")
                if [[ -n "${flag_data}" || -n "${flag_complex}" ]]; then
                    continue
                fi
                echo ${line} > ${filename}
            fi
            k=`expr $k + 1`
            sleep 1s
        done
    done
    i=`expr $i + 5`
done