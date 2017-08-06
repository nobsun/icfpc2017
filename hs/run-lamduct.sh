#! /bin/sh

usage() {
    cat <<EOF
Usage: $0 PORT PUNTER
EOF
}

port=$1
punter=$2

if [ x"$port" = x ]; then
    usage
    exit 1
fi

if [ x"$punter" = x ]; then
    usage
    exit 1
fi

echo port="$port"
echo punter="$punter"

mkdir -p run

if [ ! -x ./run/TestPunterOffline ]; then
    for exe in \
        ./dist/build/TestPunterOffline/TestPunterOffline \
        ./.stack-work/install/x86_64-linux/lts-8.23/8.0.2/bin/TestPunterOffline \
        ; do
        if [ -x $exe ]; then
            cp -a $exe ./run/TestPunterOffline.$$
            mv ./run/TestPunterOffline.$$ ./run/TestPunterOffline
            break
        fi
    done
fi

gen_punter() {
    if [ ! -x ./run/${punter}.sh ]; then
        cat <<EOF > ./run/${punter}.sh
#! /bin/sh

./TestPunterOffline $punter
EOF
        chmod a+x ./run/${punter}.sh
    fi
}

set -x

log=log/${port}/${punter}.cli

rm -f $log
mkdir -p $(dirname $log)

gen_punter

lamduct \
    --client-instance-logfile $log \
    --game-port $port \
    --log-level 3 \
    ./run/${punter}.sh
