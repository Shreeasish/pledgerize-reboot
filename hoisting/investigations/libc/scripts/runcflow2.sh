while read p; do
  c='callgraph-symbol'
  echo $p
  cflow -m $p -i _s  --level begin='' --level '0=    |' --level '1=    |' --level end0='' --level end1='' *.c > 'callgraphs/'$p$c
done <customSymbols.list

# for d in $(find ./ -name '*.c')
# do
#   # if [ -d "$d" ]; then
#   b=`echo ${d:2:-2} | tr / -`-
  # c='callgraph-entrypoint'
#   e=`basename ${d:0:-2}`
#   echo $e
#   cflow -m $e -i _s  --level begin='' --level '0=    |' --level '1=    |' --level end0='' --level end1='' $d > 'cgs/'$b$c
#   # fi
# done