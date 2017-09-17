include_directories(
	.
	../include
	../include/compat
	../crypto/modes
	../crypto/asn1
	../ssl
	../apps/openssl
	../apps/openssl/compat
)

add_definitions(-D_PATH_SSL_CA_FILE=\"${CMAKE_CURRENT_SOURCE_DIR}/../apps/openssl/cert.pem\")

foreach(lib IN LISTS OPENSSL_LIBS)
	if(${lib} STREQUAL "tls-shared")
		set(TESTS_LIBS ${TESTS_LIBS} tls)
	elseif(${lib} STREQUAL "ssl-shared")
		set(TESTS_LIBS ${TESTS_LIBS} ssl)
	elseif(${lib} STREQUAL "crypto-shared")
		set(TESTS_LIBS ${TESTS_LIBS} crypto)
	else()
		set(TESTS_LIBS ${TESTS_LIBS} ${lib})
	endif()
endforeach()

# aeadtest
add_executable(aeadtest aeadtest.c)
target_link_libraries(aeadtest ${TESTS_LIBS})
add_test(aeadtest aeadtest ${CMAKE_CURRENT_SOURCE_DIR}/aeadtests.txt)

# aes_wrap
add_executable(aes_wrap aes_wrap.c)
target_link_libraries(aes_wrap ${TESTS_LIBS})
add_test(aes_wrap aes_wrap)

# arc4randomforktest
# Windows/mingw does not have fork, but Cygwin does.
if(NOT CMAKE_HOST_WIN32 AND NOT CMAKE_SYSTEM_NAME MATCHES "MINGW")
	add_executable(arc4randomforktest arc4randomforktest.c)
	target_link_libraries(arc4randomforktest ${TESTS_LIBS})
	add_test(arc4randomforktest ${CMAKE_CURRENT_SOURCE_DIR}/arc4randomforktest.sh)
endif()

# asn1test
add_executable(asn1test asn1test.c)
target_link_libraries(asn1test ${TESTS_LIBS})
add_test(asn1test asn1test)

# asn1time
add_executable(asn1time asn1time.c)
target_link_libraries(asn1time ${TESTS_LIBS})
add_test(asn1time asn1time)

# base64test
add_executable(base64test base64test.c)
target_link_libraries(base64test ${TESTS_LIBS})
add_test(base64test base64test)

# bftest
add_executable(bftest bftest.c)
target_link_libraries(bftest ${TESTS_LIBS})
add_test(bftest bftest)

# biotest
# the BIO tests rely on resolver results that are OS and environment-specific
if(ENABLE_EXTRATESTS)
	add_executable(biotest biotest.c)
	target_link_libraries(biotest ${TESTS_LIBS})
	add_test(biotest biotest)
endif()

# bntest
add_executable(bntest bntest.c)
set_source_files_properties(bntest.c PROPERTIES COMPILE_FLAGS -ULIBRESSL_INTERNAL)
target_link_libraries(bntest ${TESTS_LIBS})
add_test(bntest bntest)

# bytestringtest
add_executable(bytestringtest bytestringtest.c)
target_link_libraries(bytestringtest ${TESTS_LIBS})
add_test(bytestringtest bytestringtest)

# casttest
add_executable(casttest casttest.c)
target_link_libraries(casttest ${TESTS_LIBS})
add_test(casttest casttest)

# chachatest
add_executable(chachatest chachatest.c)
target_link_libraries(chachatest ${TESTS_LIBS})
add_test(chachatest chachatest)

# cipher_list
add_executable(cipher_list cipher_list.c)
target_link_libraries(cipher_list ${TESTS_LIBS})
add_test(cipher_list cipher_list)

# cipherstest
add_executable(cipherstest cipherstest.c)
target_link_libraries(cipherstest ${TESTS_LIBS})
add_test(cipherstest cipherstest)

# clienttest
add_executable(clienttest clienttest.c)
target_link_libraries(clienttest ${TESTS_LIBS})
add_test(clienttest clienttest)

# cts128test
add_executable(cts128test cts128test.c)
target_link_libraries(cts128test ${TESTS_LIBS})
add_test(cts128test cts128test)

# destest
add_executable(destest destest.c)
target_link_libraries(destest ${TESTS_LIBS})
add_test(destest destest)

# dhtest
add_executable(dhtest dhtest.c)
target_link_libraries(dhtest ${TESTS_LIBS})
add_test(dhtest dhtest)

# dsatest
add_executable(dsatest dsatest.c)
target_link_libraries(dsatest ${TESTS_LIBS})
add_test(dsatest dsatest)

# ecdhtest
add_executable(ecdhtest ecdhtest.c)
target_link_libraries(ecdhtest ${TESTS_LIBS})
add_test(ecdhtest ecdhtest)

# ecdsatest
add_executable(ecdsatest ecdsatest.c)
target_link_libraries(ecdsatest ${TESTS_LIBS})
add_test(ecdsatest ecdsatest)

# ectest
add_executable(ectest ectest.c)
target_link_libraries(ectest ${TESTS_LIBS})
add_test(ectest ectest)

# enginetest
add_executable(enginetest enginetest.c)
target_link_libraries(enginetest ${TESTS_LIBS})
add_test(enginetest enginetest)

# evptest
add_executable(evptest evptest.c)
target_link_libraries(evptest ${TESTS_LIBS})
add_test(evptest evptest ${CMAKE_CURRENT_SOURCE_DIR}/evptests.txt)

# explicit_bzero
# explicit_bzero relies on SA_ONSTACK, which is unavailable on Windows
if(NOT CMAKE_HOST_WIN32)
	if(HAVE_MEMMEM)
		add_executable(explicit_bzero explicit_bzero.c)
	else()
		add_executable(explicit_bzero explicit_bzero.c compat/memmem.c)
	endif()
	target_link_libraries(explicit_bzero ${TESTS_LIBS})
	add_test(explicit_bzero explicit_bzero)
endif()

# exptest
add_executable(exptest exptest.c)
set_source_files_properties(exptest.c PROPERTIES COMPILE_FLAGS -ULIBRESSL_INTERNAL)
target_link_libraries(exptest ${TESTS_LIBS})
add_test(exptest exptest)

# freenull
add_executable(freenull freenull.c)
target_link_libraries(freenull ${TESTS_LIBS})
add_test(freenull freenull)

# gcm128test
add_executable(gcm128test gcm128test.c)
target_link_libraries(gcm128test ${TESTS_LIBS})
add_test(gcm128test gcm128test)

# gost2814789t
add_executable(gost2814789t gost2814789t.c)
target_link_libraries(gost2814789t ${TESTS_LIBS})
add_test(gost2814789t gost2814789t)

# hkdf_test
add_executable(hkdf_test hkdf_test.c)
target_link_libraries(hkdf_test ${TESTS_LIBS})
add_test(hkdf_test hkdf_test)

# hmactest
add_executable(hmactest hmactest.c)
target_link_libraries(hmactest ${TESTS_LIBS})
add_test(hmactest hmactest)

# ideatest
add_executable(ideatest ideatest.c)
target_link_libraries(ideatest ${TESTS_LIBS})
add_test(ideatest ideatest)

# igetest
add_executable(igetest igetest.c)
target_link_libraries(igetest ${TESTS_LIBS})
add_test(igetest igetest)

# md4test
add_executable(md4test md4test.c)
target_link_libraries(md4test ${TESTS_LIBS})
add_test(md4test md4test)

# md5test
add_executable(md5test md5test.c)
target_link_libraries(md5test ${TESTS_LIBS})
add_test(md5test md5test)

# mont
add_executable(mont mont.c)
target_link_libraries(mont ${TESTS_LIBS})
add_test(mont mont)

# ocsp_test
if(ENABLE_EXTRATESTS)
	add_executable(ocsp_test ocsp_test.c)
	target_link_libraries(ocsp_test ${TESTS_LIBS})
	if(NOT MSVC)
		add_test(ocsptest ${CMAKE_CURRENT_SOURCE_DIR}/ocsptest.sh)
	else()
		add_test(ocsptest ${CMAKE_CURRENT_SOURCE_DIR}/ocsptest.bat)
	endif()
endif()

# optionstest
add_executable(optionstest optionstest.c)
target_link_libraries(optionstest ${TESTS_LIBS})
add_test(optionstest optionstest)

# pbkdf2
add_executable(pbkdf2 pbkdf2.c)
target_link_libraries(pbkdf2 ${TESTS_LIBS})
add_test(pbkdf2 pbkdf2)

# pidwraptest
# pidwraptest relies on an OS-specific way to give out pids and is generally
# awkward on systems with slow fork
if(ENABLE_EXTRATESTS AND NOT MSVC)
	add_executable(pidwraptest pidwraptest.c)
	target_link_libraries(pidwraptest ${TESTS_LIBS})
	add_test(pidwraptest ${CMAKE_CURRENT_SOURCE_DIR}/pidwraptest.sh)
endif()

# pkcs7test
add_executable(pkcs7test pkcs7test.c)
target_link_libraries(pkcs7test ${TESTS_LIBS})
add_test(pkcs7test pkcs7test)

# poly1305test
add_executable(poly1305test poly1305test.c)
target_link_libraries(poly1305test ${TESTS_LIBS})
add_test(poly1305test poly1305test)

# pq_test
add_executable(pq_test pq_test.c)
target_link_libraries(pq_test ${TESTS_LIBS})
if(NOT MSVC)
	add_test(pq_test ${CMAKE_CURRENT_SOURCE_DIR}/pq_test.sh)
else()
	add_test(pq_test ${CMAKE_CURRENT_SOURCE_DIR}/pq_test.bat)
endif()
set_tests_properties(pq_test PROPERTIES ENVIRONMENT "srcdir=${CMAKE_CURRENT_SOURCE_DIR}")

# randtest
add_executable(randtest randtest.c)
target_link_libraries(randtest ${TESTS_LIBS})
add_test(randtest randtest)

# rc2test
add_executable(rc2test rc2test.c)
target_link_libraries(rc2test ${TESTS_LIBS})
add_test(rc2test rc2test)

# rc4test
add_executable(rc4test rc4test.c)
target_link_libraries(rc4test ${TESTS_LIBS})
add_test(rc4test rc4test)

# rfc5280time
add_executable(rfc5280time rfc5280time.c)
target_link_libraries(rfc5280time ${TESTS_LIBS})
if(SMALL_TIME_T)
	add_test(rfc5280time ${CMAKE_CURRENT_SOURCE_DIR}/rfc5280time_small.test)
else()
	add_test(rfc5280time rfc5280time)
endif()

# rmdtest
add_executable(rmdtest rmdtest.c)
target_link_libraries(rmdtest ${TESTS_LIBS})
add_test(rmdtest rmdtest)

# rsa_test
add_executable(rsa_test rsa_test.c)
target_link_libraries(rsa_test ${TESTS_LIBS})
add_test(rsa_test rsa_test)

# servertest
add_executable(servertest servertest.c)
target_link_libraries(servertest ${TESTS_LIBS})
if(NOT MSVC)
	add_test(servertest ${CMAKE_CURRENT_SOURCE_DIR}/servertest.sh)
else()
	add_test(servertest ${CMAKE_CURRENT_SOURCE_DIR}/servertest.bat)
endif()
set_tests_properties(servertest PROPERTIES ENVIRONMENT "srcdir=${CMAKE_CURRENT_SOURCE_DIR}")

# sha1test
add_executable(sha1test sha1test.c)
target_link_libraries(sha1test ${TESTS_LIBS})
add_test(sha1test sha1test)

# sha256test
add_executable(sha256test sha256test.c)
target_link_libraries(sha256test ${TESTS_LIBS})
add_test(sha256test sha256test)

# sha512test
add_executable(sha512test sha512test.c)
target_link_libraries(sha512test ${TESTS_LIBS})
add_test(sha512test sha512test)

# ssl_versions
add_executable(ssl_versions ssl_versions.c)
target_link_libraries(ssl_versions ${TESTS_LIBS})
add_test(ssl_versions ssl_versions)

# ssltest
add_executable(ssltest ssltest.c)
target_link_libraries(ssltest ${TESTS_LIBS})
if(NOT MSVC)
	add_test(ssltest ${CMAKE_CURRENT_SOURCE_DIR}/ssltest.sh)
else()
	add_test(ssltest ${CMAKE_CURRENT_SOURCE_DIR}/ssltest.bat)
endif()
set_tests_properties(ssltest PROPERTIES ENVIRONMENT "srcdir=${CMAKE_CURRENT_SOURCE_DIR}")

# testdsa
if(NOT MSVC)
	add_test(testdsa ${CMAKE_CURRENT_SOURCE_DIR}/testdsa.sh)
else()
	add_test(testdsa ${CMAKE_CURRENT_SOURCE_DIR}/testdsa.bat)
endif()
set_tests_properties(testdsa PROPERTIES ENVIRONMENT "srcdir=${CMAKE_CURRENT_SOURCE_DIR}")

# testenc
if(NOT MSVC)
	add_test(testenc ${CMAKE_CURRENT_SOURCE_DIR}/testenc.sh)
else()
	add_test(testenc ${CMAKE_CURRENT_SOURCE_DIR}/testenc.bat)
endif()
set_tests_properties(testenc PROPERTIES ENVIRONMENT "srcdir=${CMAKE_CURRENT_SOURCE_DIR}")

# testrsa
if(NOT MSVC)
	add_test(testrsa ${CMAKE_CURRENT_SOURCE_DIR}/testrsa.sh)
else()
	add_test(testrsa ${CMAKE_CURRENT_SOURCE_DIR}/testrsa.bat)
endif()
set_tests_properties(testrsa PROPERTIES ENVIRONMENT "srcdir=${CMAKE_CURRENT_SOURCE_DIR}")

# timingsafe
add_executable(timingsafe timingsafe.c)
target_link_libraries(timingsafe ${TESTS_LIBS})
add_test(timingsafe timingsafe)

# tlsexttest
add_executable(tlsexttest tlsexttest.c)
target_link_libraries(tlsexttest ${TESTS_LIBS})
add_test(tlsexttest tlsexttest)

# tlstest
set(TLSTEST_SRC tlstest.c)
check_function_exists(pipe2 HAVE_PIPE2)
if(HAVE_PIPE2)
	add_definitions(-DHAVE_PIPE2)
else()
	set(TLSTEST_SRC ${TLSTEST_SRC} compat/pipe2.c)
endif()

add_executable(tlstest ${TLSTEST_SRC})
target_link_libraries(tlstest ${TESTS_LIBS})
if(NOT MSVC)
	add_test(tlstest ${CMAKE_CURRENT_SOURCE_DIR}/tlstest.sh)
else()
	add_test(tlstest ${CMAKE_CURRENT_SOURCE_DIR}/tlstest.bat)
endif()
set_tests_properties(tlstest PROPERTIES ENVIRONMENT "srcdir=${CMAKE_CURRENT_SOURCE_DIR}")

# tls_ext_alpn
add_executable(tls_ext_alpn tls_ext_alpn.c)
target_link_libraries(tls_ext_alpn ${TESTS_LIBS})
add_test(tls_ext_alpn tls_ext_alpn)

# tls_prf
add_executable(tls_prf tls_prf.c)
target_link_libraries(tls_prf ${TESTS_LIBS})
add_test(tls_prf tls_prf)

# utf8test
add_executable(utf8test utf8test.c)
target_link_libraries(utf8test ${TESTS_LIBS})
add_test(utf8test utf8test)

# verifytest
add_executable(verifytest verifytest.c)
target_link_libraries(verifytest tls ${TESTS_LIBS})
add_test(verifytest verifytest)

# x25519test
add_executable(x25519test x25519test.c)
target_link_libraries(x25519test ${TESTS_LIBS})
add_test(x25519test x25519test)

if(ENABLE_VSTEST AND USE_SHARED)
	add_custom_command(TARGET x25519test POST_BUILD
		COMMAND "${CMAKE_COMMAND}" -E copy
		"$<TARGET_FILE:tls-shared>"
		"$<TARGET_FILE:ssl-shared>"
		"$<TARGET_FILE:crypto-shared>"
		"${CMAKE_CURRENT_BINARY_DIR}"
		COMMENT "Copying DLLs for regression tests")
endif()

