#!/usr/bin/env perl

$flavour = shift;
$output = shift;
open STDOUT,">$output";

if ($flavour =~ /64/) {
	$LEVEL		="2.0W";
	$SIZE_T		=8;
	$ST		="std";
} else {
	$LEVEL		="1.1";
	$SIZE_T		=4;
	$ST		="stw";
}

$rp="%r2";
$sp="%r30";
$rv="%r28";

$code=<<___;
	.LEVEL	$LEVEL
#if 0
	.SPACE	\$TEXT\$
	.SUBSPA	\$CODE\$,QUAD=0,ALIGN=8,ACCESS=0x2C,CODE_ONLY
#else
	.text
#endif

	.EXPORT	OPENSSL_cpuid_setup,ENTRY
	.ALIGN	8
OPENSSL_cpuid_setup
	.PROC
	.CALLINFO	NO_CALLS
	.ENTRY
	bv	($rp)
	.EXIT
	nop
	.PROCEND

	.EXPORT	OPENSSL_wipe_cpu,ENTRY
	.ALIGN	8
OPENSSL_wipe_cpu
	.PROC
	.CALLINFO	NO_CALLS
	.ENTRY
	xor		%r0,%r0,%r1
	fcpy,dbl	%fr0,%fr4
	xor		%r0,%r0,%r19
	fcpy,dbl	%fr0,%fr5
	xor		%r0,%r0,%r20
	fcpy,dbl	%fr0,%fr6
	xor		%r0,%r0,%r21
	fcpy,dbl	%fr0,%fr7
	xor		%r0,%r0,%r22
	fcpy,dbl	%fr0,%fr8
	xor		%r0,%r0,%r23
	fcpy,dbl	%fr0,%fr9
	xor		%r0,%r0,%r24
	fcpy,dbl	%fr0,%fr10
	xor		%r0,%r0,%r25
	fcpy,dbl	%fr0,%fr11
	xor		%r0,%r0,%r26
	fcpy,dbl	%fr0,%fr22
	xor		%r0,%r0,%r29
	fcpy,dbl	%fr0,%fr23
	xor		%r0,%r0,%r31
	fcpy,dbl	%fr0,%fr24
	fcpy,dbl	%fr0,%fr25
	fcpy,dbl	%fr0,%fr26
	fcpy,dbl	%fr0,%fr27
	fcpy,dbl	%fr0,%fr28
	fcpy,dbl	%fr0,%fr29
	fcpy,dbl	%fr0,%fr30
	fcpy,dbl	%fr0,%fr31
	bv		($rp)
	.EXIT
	ldo		0($sp),$rv
	.PROCEND
___
$code =~ s/cmpib,\*/comib,/gm	if ($SIZE_T==4);
$code =~ s/,\*/,/gm		if ($SIZE_T==4);
$code =~ s/\bbv\b/bve/gm	if ($SIZE_T==8);
print $code;
close STDOUT;

