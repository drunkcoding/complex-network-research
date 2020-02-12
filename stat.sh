touch stat.csv
echo "time,driver nodes,distinguished,redundant,ordinary,critical edges" > stat.csv
for file in $(find /root/graphml -type f -name '*.graphml')
do
    d=$(echo ${file} | cut -d'/' -f4|cut -d'.' -f1)
    echo ${file} ${d}
    /root/netctrl/build/./src/ui/netctrl -M statistics /root/graphml/ > tmp
    l=$(tail -1 tmp | sed -En "s/ /,/g")
    echo "${d},${l}" >> stat.csv
done