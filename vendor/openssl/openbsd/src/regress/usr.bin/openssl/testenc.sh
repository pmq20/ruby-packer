#!/bin/sh
#	$OpenBSD: testenc.sh,v 1.1 2014/08/26 17:50:07 jsing Exp $

testsrc=$2/openssl.cnf
test=$1/p
cmd=/usr/bin/openssl

cd $1

cat $testsrc >$test;

echo cat
$cmd enc < $test > $test.cipher
$cmd enc < $test.cipher >$test.clear
cmp $test $test.clear
if [ $? != 0 ]
then
	exit 1
else
	/bin/rm $test.cipher $test.clear
fi
echo base64
$cmd enc -a -e < $test > $test.cipher
$cmd enc -a -d < $test.cipher >$test.clear
cmp $test $test.clear
if [ $? != 0 ]
then
	exit 1
else
	/bin/rm $test.cipher $test.clear
fi

/bin/rm -f $test
exit 0

# These tests are now done by the makefile.

for i in rc4 \
	des-cfb des-ede-cfb des-ede3-cfb \
	des-ofb des-ede-ofb des-ede3-ofb \
	des-ecb des-ede des-ede3 desx \
	des-cbc des-ede-cbc des-ede3-cbc \
	rc2-ecb rc2-cfb rc2-ofb rc2-cbc \
	bf-ecb bf-cfb bf-ofb bf-cbc rc4 \
	cast5-ecb cast5-cfb cast5-ofb cast5-cbc
do
	echo $i
	$cmd $i -bufsize 113 -e -k test < $test > $test.$i.cipher
	$cmd $i -bufsize 157 -d -k test < $test.$i.cipher >$test.$i.clear
	cmp $test $test.$i.clear
	if [ $? != 0 ]
	then
		exit 1
	else
		/bin/rm $test.$i.cipher $test.$i.clear
	fi

	echo $i base64
	$cmd $i -bufsize 113 -a -e -k test < $test > $test.$i.cipher
	$cmd $i -bufsize 157 -a -d -k test < $test.$i.cipher >$test.$i.clear
	cmp $test $test.$i.clear
	if [ $? != 0 ]
	then
		exit 1
	else
		/bin/rm $test.$i.cipher $test.$i.clear
	fi
done
rm -f $test
