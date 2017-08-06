#! /bin/sh

usage() {
    cat <<EOF
Usage: $0 INSTANCE_NUMBER PORT PUNTER_0 [PUNETER_1]
EOF
}

n=$1

if [ x"$n" = x ]; then
    usage
    exit 1
fi

port=$2

if [ x"$port" = x ]; then
    usage
    exit 1
fi

p0=$3
if [ x"$p0" = x ]; then
    usage
    exit 1
fi

p1=$4
if [ x"$p1" = x ]; then
    p1=greedy
fi


n2=$(expr "$n" '-' 2)

for i in $(seq 1 $n2) ; do
    log=log/${port}/any-junk/${i}.out
    mkdir -p $(dirname $log)
    ./run-lamduct.sh $port any > $log 2>&1  &
done

log=log/${port}/${p1}.out
mkdir -p $(dirname $log)

sleep 0.5

./run-lamduct.sh $port ${p1} > $log 2>&1 &


log=log/${port}/${p0}.out
mkdir -p $(dirname $log)

sleep 0.5

./run-lamduct.sh $port ${p0} > $log 2>&1 &

tail -f $log
