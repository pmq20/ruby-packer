#!/usr/bin/env perl

$0 =~ m/(.*[\/\\])[^\/\\]+$/; $dir=$1;
push(@INC, "${dir}perlasm", "perlasm");
require "x86asm.pl";

&asm_init($ARGV[0],"x86cpuid");

for (@ARGV) { $sse2=1 if (/-DOPENSSL_IA32_SSE2/); }

&function_begin("OPENSSL_ia32_cpuid");
	&xor	("edx","edx");
	&pushf	();
	&pop	("eax");
	&mov	("ecx","eax");
	&xor	("eax",1<<21);
	&push	("eax");
	&popf	();
	&pushf	();
	&pop	("eax");
	&xor	("ecx","eax");
	&xor	("eax","eax");
	&bt	("ecx",21);
	&jnc	(&label("nocpuid"));
	&cpuid	();
	&mov	("edi","eax");		# max value for standard query level

	&xor	("eax","eax");
	&cmp	("ebx",0x756e6547);	# "Genu"
	&setne	(&LB("eax"));
	&mov	("ebp","eax");
	&cmp	("edx",0x49656e69);	# "ineI"
	&setne	(&LB("eax"));
	&or	("ebp","eax");
	&cmp	("ecx",0x6c65746e);	# "ntel"
	&setne	(&LB("eax"));
	&or	("ebp","eax");		# 0 indicates Intel CPU
	&jz	(&label("intel"));

	&cmp	("ebx",0x68747541);	# "Auth"
	&setne	(&LB("eax"));
	&mov	("esi","eax");
	&cmp	("edx",0x69746E65);	# "enti"
	&setne	(&LB("eax"));
	&or	("esi","eax");
	&cmp	("ecx",0x444D4163);	# "cAMD"
	&setne	(&LB("eax"));
	&or	("esi","eax");		# 0 indicates AMD CPU
	&jnz	(&label("intel"));

	# AMD specific
	&mov	("eax",0x80000000);
	&cpuid	();
	&cmp	("eax",0x80000001);
	&jb	(&label("intel"));
	&mov	("esi","eax");
	&mov	("eax",0x80000001);
	&cpuid	();
	&and	("ecx","\$IA32CAP_MASK1_AMD_XOP");	# isolate AMD XOP bit
	&or	("ecx",1);		# make sure ecx is not zero
	&mov	("ebp","ecx");

	&cmp	("esi",0x80000008);
	&jb	(&label("intel"));

	&mov	("eax",0x80000008);
	&cpuid	();
	&movz	("esi",&LB("ecx"));	# number of cores - 1
	&inc	("esi");		# number of cores

	&mov	("eax",1);
	&xor	("ecx","ecx");
	&cpuid	();
	&bt	("edx","\$IA32CAP_BIT0_HT");
	&jnc	(&label("generic"));
	&shr	("ebx",16);
	&and	("ebx",0xff);
	&cmp	("ebx","esi");
	&ja	(&label("generic"));
	&xor	("edx","\$IA32CAP_MASK0_HT");	# clear hyper-threading bit
	&jmp	(&label("generic"));
	
&set_label("intel");
	&cmp	("edi",4);
	&mov	("edi",-1);
	&jb	(&label("nocacheinfo"));

	&mov	("eax",4);
	&mov	("ecx",0);		# query L1D
	&cpuid	();
	&mov	("edi","eax");
	&shr	("edi",14);
	&and	("edi",0xfff);		# number of cores -1 per L1D

&set_label("nocacheinfo");
	&mov	("eax",1);
	&xor	("ecx","ecx");
	&cpuid	();
	# force reserved bits to 0.
	&and	("edx","\$~(IA32CAP_MASK0_INTELP4 | IA32CAP_MASK0_INTEL)");
	&cmp	("ebp",0);
	&jne	(&label("notintel"));
	# set reserved bit#30 on Intel CPUs
	&or	("edx","\$IA32CAP_MASK0_INTEL");
	&and	(&HB("eax"),15);	# family ID
	&cmp	(&HB("eax"),15);	# P4?
	&jne	(&label("notintel"));
	# set reserved bit#20 to engage RC4_CHAR
	&or	("edx","\$IA32CAP_MASK0_INTELP4");
&set_label("notintel");
	&bt	("edx","\$IA32CAP_BIT0_HT");	# test hyper-threading bit
	&jnc	(&label("generic"));
	&xor	("edx","\$IA32CAP_MASK0_HT");
	&cmp	("edi",0);
	&je	(&label("generic"));

	&or	("edx","\$IA32CAP_MASK0_HT");
	&shr	("ebx",16);
	&cmp	(&LB("ebx"),1);		# see if cache is shared
	&ja	(&label("generic"));
	&xor	("edx","\$IA32CAP_MASK0_HT"); # clear hyper-threading bit if not

&set_label("generic");
	&and	("ebp","\$IA32CAP_MASK1_AMD_XOP");	# isolate AMD XOP flag
	# force reserved bits to 0.
	&and	("ecx","\$~IA32CAP_MASK1_AMD_XOP");
	&mov	("esi","edx");
	&or	("ebp","ecx");		# merge AMD XOP flag

	&bt	("ecx","\$IA32CAP_BIT1_OSXSAVE");	# check OSXSAVE bit
	&jnc	(&label("clear_avx"));
	&xor	("ecx","ecx");
	&data_byte(0x0f,0x01,0xd0);	# xgetbv
	&and	("eax",6);
	&cmp	("eax",6);
	&je	(&label("done"));
	&cmp	("eax",2);
	&je	(&label("clear_avx"));
&set_label("clear_xmm");
	# clear AESNI and PCLMULQDQ bits.
	&and	("ebp","\$~(IA32CAP_MASK1_AESNI | IA32CAP_MASK1_PCLMUL)");
	# clear FXSR.
	&and	("esi","\$~IA32CAP_MASK0_FXSR");
&set_label("clear_avx");
	# clear AVX, FMA3 and AMD XOP bits.
	&and	("ebp","\$~(IA32CAP_MASK1_AVX | IA32CAP_MASK1_FMA3 | IA32CAP_MASK1_AMD_XOP)");
&set_label("done");
	&mov	("eax","esi");
	&mov	("edx","ebp");
&set_label("nocpuid");
&function_end("OPENSSL_ia32_cpuid");

&external_label("OPENSSL_ia32cap_P");

&function_begin_B("OPENSSL_wipe_cpu","");
	&xor	("eax","eax");
	&xor	("edx","edx");
	&picmeup("ecx","OPENSSL_ia32cap_P");
	&mov	("ecx",&DWP(0,"ecx"));
	&bt	(&DWP(0,"ecx"),"\$IA32CAP_BIT0_FPU");
	&jnc	(&label("no_x87"));
	if ($sse2) {
		# Check SSE2 and FXSR bits.
		&and	("ecx", "\$(IA32CAP_MASK0_FXSR | IA32CAP_MASK0_SSE2)");
		&cmp	("ecx", "\$(IA32CAP_MASK0_FXSR | IA32CAP_MASK0_SSE2)");
		&jne	(&label("no_sse2"));
		&pxor	("xmm0","xmm0");
		&pxor	("xmm1","xmm1");
		&pxor	("xmm2","xmm2");
		&pxor	("xmm3","xmm3");
		&pxor	("xmm4","xmm4");
		&pxor	("xmm5","xmm5");
		&pxor	("xmm6","xmm6");
		&pxor	("xmm7","xmm7");
	&set_label("no_sse2");
	}
	# just a bunch of fldz to zap the fp/mm bank followed by finit...
	&data_word(0xeed9eed9,0xeed9eed9,0xeed9eed9,0xeed9eed9,0x90e3db9b);
&set_label("no_x87");
	&lea	("eax",&DWP(4,"esp"));
	&ret	();
&function_end_B("OPENSSL_wipe_cpu");

&function_begin_B("OPENSSL_atomic_add");
	&mov	("edx",&DWP(4,"esp"));	# fetch the pointer, 1st arg
	&mov	("ecx",&DWP(8,"esp"));	# fetch the increment, 2nd arg
	&push	("ebx");
	&nop	();
	&mov	("eax",&DWP(0,"edx"));
&set_label("spin");
	&lea	("ebx",&DWP(0,"eax","ecx"));
	&nop	();
	&data_word(0x1ab10ff0);	# lock;	cmpxchg	%ebx,(%edx)	# %eax is envolved and is always reloaded
	&jne	(&label("spin"));
	&mov	("eax","ebx");	# OpenSSL expects the new value
	&pop	("ebx");
	&ret	();
&function_end_B("OPENSSL_atomic_add");

&initseg("OPENSSL_cpuid_setup");

&asm_finish();
