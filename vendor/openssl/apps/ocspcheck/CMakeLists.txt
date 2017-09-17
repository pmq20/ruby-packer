if(NOT MSVC)

include_directories(
	.
	./compat
	../../include
	../../include/compat
)

set(
	OCSPCHECK_SRC
	http.c
	ocspcheck.c
)

check_function_exists(inet_ntop HAVE_INET_NTOP)
if(HAVE_INET_NTOP)
        add_definitions(-DHAVE_INET_NTOP)
else()
        set(OCSPCHECK_SRC ${OCSPCHECK_SRC} compat/inet_ntop.c)
endif()

check_function_exists(inet_ntop HAVE_MEMMEM)
if(HAVE_MEMMEM)
        add_definitions(-DHAVE_MEMMEM)
else()
        set(OCSPCHECK_SRC ${OCSPCHECK_SRC} compat/memmem.c)
endif()

if(NOT "${OPENSSLDIR}" STREQUAL "")
	add_definitions(-DDEFAULT_CA_FILE=\"${OPENSSLDIR}/cert.pem\")
else()
	add_definitions(-DDEFAULT_CA_FILE=\"${CMAKE_INSTALL_PREFIX}/etc/ssl/cert.pem\")
endif()

add_executable(ocspcheck ${OCSPCHECK_SRC})
target_link_libraries(ocspcheck tls ${OPENSSL_LIBS})

if(ENABLE_LIBRESSL_INSTALL)
	install(TARGETS ocspcheck DESTINATION ${CMAKE_INSTALL_BINDIR})
	install(FILES ocspcheck.8 DESTINATION ${CMAKE_INSTALL_MANDIR}/man8)

endif(ENABLE_LIBRESSL_INSTALL)

endif()
