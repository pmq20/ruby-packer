--- openbsd/src/lib/libssl/src/crypto/modes/modes_lcl.h	Sat Dec  6 17:15:50 2014
+++ crypto/modes/modes_lcl.h	Sun Jul 17 17:45:27 2016
@@ -43,14 +43,16 @@
 			asm ("bswapl %0"		\
 			: "+r"(ret));	ret;		})
 # elif (defined(__arm__) || defined(__arm)) && !defined(__STRICT_ALIGNMENT)
-#  define BSWAP8(x) ({	u32 lo=(u64)(x)>>32,hi=(x);	\
+#  if (__ARM_ARCH >= 6)
+#   define BSWAP8(x) ({	u32 lo=(u64)(x)>>32,hi=(x);	\
 			asm ("rev %0,%0; rev %1,%1"	\
 			: "+r"(hi),"+r"(lo));		\
 			(u64)hi<<32|lo;			})
-#  define BSWAP4(x) ({	u32 ret;			\
+#   define BSWAP4(x) ({	u32 ret;			\
 			asm ("rev %0,%1"		\
 			: "=r"(ret) : "r"((u32)(x)));	\
 			ret;				})
+#  endif
 # endif
 #endif
 #endif
