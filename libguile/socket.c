/* Copyright 1996-1998,2000-2007,2009,2011-2015,2018,2021,2022,2025
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */




#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <errno.h>
#include <verify.h>

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#else /* ! HAVE_WINSOCK2_H */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#ifdef HAVE_UNIX_DOMAIN_SOCKETS
#include <sys/un.h>
#endif
#endif /* ! HAVE_WINSOCK2_H */
#include <netdb.h>
#include <arpa/inet.h>

#include "scm.h"
#if SCM_ENABLE_MINI_GMP
#include "mini-gmp.h"
#else
#include <gmp.h>
#endif

#include "arrays.h"
#include "async.h"
#include "bytevectors.h"
#include "dynwind.h"
#include "feature.h"
#include "fports.h"
#include "gsubr.h"
#include "list.h"
#include "modules.h"
#include "numbers.h"
#include "pairs.h"
#include "srfi-13.h"
#include "strings.h"
#include "symbols.h"
#include "syscalls.h"
#include "vectors.h"

#include "socket.h"


#if defined (HAVE_UNIX_DOMAIN_SOCKETS) && !defined (SUN_LEN)
#define SUN_LEN(ptr) (offsetof (struct sockaddr_un, sun_path) \
		      + strlen ((ptr)->sun_path))
#endif

/* The largest possible socket address.  Wrapping it in a union guarantees
   that the compiler will make it suitably aligned.  */
typedef union
{
  struct sockaddr     sockaddr;
  struct sockaddr_in  sockaddr_in;

#ifdef HAVE_UNIX_DOMAIN_SOCKETS
  struct sockaddr_un  sockaddr_un;
#endif
#ifdef HAVE_IPV6
  struct sockaddr_in6 sockaddr_in6;
#endif
} scm_t_max_sockaddr;


/* Maximum size of a socket address.  */
#define MAX_ADDR_SIZE   (sizeof (scm_t_max_sockaddr))




#ifdef HAVE_INET_NETOF
SCM_DEFINE (scm_inet_netof, "inet-netof", 1, 0, 0, 
            (SCM address),
	    "Return the network number part of the given IPv4\n"
	    "Internet address.  E.g.,\n\n"
	    "@lisp\n"
	    "(inet-netof 2130706433) @result{} 127\n"
	    "@end lisp")
#define FUNC_NAME s_scm_inet_netof
{
  struct in_addr addr;
  addr.s_addr = htonl (SCM_NUM2ULONG (1, address));
  return scm_from_ulong (inet_netof (addr));
}
#undef FUNC_NAME
#endif

#ifdef HAVE_INET_LNAOF
SCM_DEFINE (scm_lnaof, "inet-lnaof", 1, 0, 0, 
            (SCM address),
	    "Return the local-address-with-network part of the given\n"
	    "IPv4 Internet address, using the obsolete class A/B/C system.\n"
	    "E.g.,\n\n"
	    "@lisp\n"
	    "(inet-lnaof 2130706433) @result{} 1\n"
	    "@end lisp")
#define FUNC_NAME s_scm_lnaof
{
  struct in_addr addr;
  addr.s_addr = htonl (SCM_NUM2ULONG (1, address));
  return scm_from_ulong (inet_lnaof (addr));
}
#undef FUNC_NAME
#endif

#ifdef HAVE_INET_MAKEADDR
SCM_DEFINE (scm_inet_makeaddr, "inet-makeaddr", 2, 0, 0,
            (SCM net, SCM lna),
	    "Make an IPv4 Internet address by combining the network number\n"
	    "@var{net} with the local-address-within-network number\n"
	    "@var{lna}.  E.g.,\n\n"
	    "@lisp\n"
	    "(inet-makeaddr 127 1) @result{} 2130706433\n"
	    "@end lisp")
#define FUNC_NAME s_scm_inet_makeaddr
{
  struct in_addr addr;
  unsigned long netnum;
  unsigned long lnanum;

  netnum = SCM_NUM2ULONG (1, net);
  lnanum = SCM_NUM2ULONG (2, lna);
  addr = inet_makeaddr (netnum, lnanum);
  return scm_from_ulong (ntohl (addr.s_addr));
}
#undef FUNC_NAME
#endif

#ifdef HAVE_IPV6

/* flip a 128 bit IPv6 address between host and network order.  */
#ifdef WORDS_BIGENDIAN
#define FLIP_NET_HOST_128(addr)
#else
#define FLIP_NET_HOST_128(addr)\
{\
  int i;\
  \
  for (i = 0; i < 8; i++)\
    {\
      uint8_t c = (addr)[i];\
      \
      (addr)[i] = (addr)[15 - i];\
      (addr)[15 - i] = c;\
    }\
}
#endif

#ifdef WORDS_BIGENDIAN
#define FLIPCPY_NET_HOST_128(dest, src) memcpy (dest, src, 16)
#else
#define FLIPCPY_NET_HOST_128(dest, src) \
{ \
  const uint8_t *tmp_srcp = (src) + 15; \
  uint8_t *tmp_destp = (dest); \
  \
  do { \
    *tmp_destp++ = *tmp_srcp--; \
  } while (tmp_srcp != (src)); \
}
#endif


#if (SIZEOF_UINTPTR_T * SCM_CHAR_BIT) > 128
#error "Assumption that uintptr_t <= 128 bits has been violated."
#endif

#if (SIZEOF_UNSIGNED_LONG * SCM_CHAR_BIT) > 128
#error "Assumption that unsigned long <= 128 bits has been violated."
#endif

#if (SIZEOF_UNSIGNED_LONG_LONG * SCM_CHAR_BIT) > 128
#error "Assumption that unsigned long long <= 128 bits has been violated."
#endif

/* convert a 128 bit IPv6 address in network order to a host ordered
   SCM integer.  */
static SCM
scm_from_ipv6 (const uint8_t *src)
{
  mpz_t z;
  mpz_init (z);
  mpz_import (z,
              1,  /* chunk */
              1,  /* big-endian chunk ordering */
              16, /* chunks are 16 bytes long */
              1,  /* big-endian byte ordering */
              0,  /* "nails" -- leading unused bits per chunk */
              src);
  SCM ret = scm_from_mpz (z);
  mpz_clear (z);
  return ret;
}

/* convert a host ordered SCM integer to a 128 bit IPv6 address in
   network order.  */
static void
scm_to_ipv6 (uint8_t dst[16], SCM src)
{
  if (SCM_I_INUMP (src))
    {
      scm_t_signed_bits n = SCM_I_INUM (src);
      if (n < 0)
	scm_out_of_range (NULL, src);
#ifdef WORDS_BIGENDIAN
      memset (dst, 0, 16 - sizeof (scm_t_signed_bits));
      memcpy (dst + (16 - sizeof (scm_t_signed_bits)),
              &n,
              sizeof (scm_t_signed_bits));
#else
      memset (dst + sizeof (scm_t_signed_bits),
              0,
              16 - sizeof (scm_t_signed_bits));
      /* FIXME: this pair of ops is kinda wasteful -- should rewrite as
         a single loop perhaps, similar to the handling of bignums. */
      memcpy (dst, &n, sizeof (scm_t_signed_bits));
      FLIP_NET_HOST_128 (dst);
#endif
    }
  else if (SCM_BIGP (src))
    {
      size_t count;
      mpz_t z;
      
      mpz_init (z);
      scm_to_mpz (src, z);

      if (mpz_sgn (z) < 0 || mpz_sizeinbase (z, 2) > 128)
        {
          mpz_clear (z);
          scm_out_of_range (NULL, src);
        }
      
      memset (dst, 0, 16);
      mpz_export (dst,
                  &count,
                  1, /* big-endian chunk ordering */
                  16, /* chunks are 16 bytes long */
                  1, /* big-endian byte ordering */
                  0, /* "nails" -- leading unused bits per chunk */
                  z);
      mpz_clear (z);
    }
  else
    scm_wrong_type_arg_msg ("scm_to_ipv6", 0, src, "integer");
}

#endif  /* HAVE_IPV6 */



SCM_DEFINE (scm_inet_ntop, "inet-ntop", 2, 0, 0,
            (SCM family, SCM address),
	    "Convert a network address into a printable string.\n"
	    "Note that unlike the C version of this function,\n"
	    "the input is an integer with normal host byte ordering.\n"
	    "@var{family} can be @code{AF_INET} or @code{AF_INET6}.  E.g.,\n\n"
	    "@lisp\n"
	    "(inet-ntop AF_INET 2130706433) @result{} \"127.0.0.1\"\n"
	    "(inet-ntop AF_INET6 (- (expt 2 128) 1))\n"
	    "  @result{} \"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff\"\n"
	    "@end lisp")
#define FUNC_NAME s_scm_inet_ntop
{
  int af;
#ifdef INET6_ADDRSTRLEN
  char dst[INET6_ADDRSTRLEN];
#else
  char dst[46];
#endif
  const char *result;

  af = scm_to_int (family);
  SCM_ASSERT_RANGE (1, family,
		    af == AF_INET
#ifdef HAVE_IPV6
		    || af == AF_INET6
#endif
		    );
  if (af == AF_INET)
    {
      uint32_t addr4;

      addr4 = htonl (SCM_NUM2ULONG (2, address));
      result = inet_ntop (af, &addr4, dst, sizeof (dst));
    }
#ifdef HAVE_IPV6
  else if (af == AF_INET6)
    {
      char addr6[16];

      scm_to_ipv6 ((uint8_t *) addr6, address);
      result = inet_ntop (af, &addr6, dst, sizeof (dst));
    }
#endif
  else
    SCM_MISC_ERROR ("unsupported address family", family);

  if (result == NULL)
    SCM_SYSERROR;

  return scm_from_locale_string (dst);
}
#undef FUNC_NAME

SCM_DEFINE (scm_inet_pton, "inet-pton", 2, 0, 0,
            (SCM family, SCM address),
	    "Convert a string containing a printable network address to\n"
	    "an integer address.  Note that unlike the C version of this\n"
	    "function,\n"
	    "the result is an integer with normal host byte ordering.\n"
	    "@var{family} can be @code{AF_INET} or @code{AF_INET6}.  E.g.,\n\n"
	    "@lisp\n"
	    "(inet-pton AF_INET \"127.0.0.1\") @result{} 2130706433\n"
	    "(inet-pton AF_INET6 \"::1\") @result{} 1\n"
	    "@end lisp")
#define FUNC_NAME s_scm_inet_pton
{
  int af;
  char *src;
  uint32_t dst[4];
  int rv, eno;

  af = scm_to_int (family);
  SCM_ASSERT_RANGE (1, family,
		    af == AF_INET
#ifdef HAVE_IPV6
		    || af == AF_INET6
#endif
		    );

  src = scm_to_locale_string (address);
  rv = inet_pton (af, src, dst);
  eno = errno;
  free (src);
  errno = eno;

  if (rv == -1)
    SCM_SYSERROR;
  else if (rv == 0)
    SCM_MISC_ERROR ("Bad address", SCM_EOL);
  if (af == AF_INET)
    return scm_from_ulong (ntohl (*dst));
#ifdef HAVE_IPV6
  else if (af == AF_INET6)
    return scm_from_ipv6 ((uint8_t *) dst);
#endif
  else
    SCM_MISC_ERROR ("unsupported address family", family);
}
#undef FUNC_NAME


SCM_SYMBOL (sym_socket, "socket");

static SCM
scm_socket_fd_to_port (int fd)
{
  return scm_i_fdes_to_port (fd, scm_mode_bits ("r+0"), sym_socket,
                             SCM_FPORT_OPTION_NOT_SEEKABLE);
}

SCM_DEFINE (scm_socket, "socket", 3, 0, 0,
            (SCM family, SCM style, SCM proto),
	    "Return a new socket port of the type specified by @var{family},\n"
	    "@var{style} and @var{proto}.  All three parameters are\n"
	    "integers.  Supported values for @var{family} are\n"
	    "@code{AF_UNIX}, @code{AF_INET} and @code{AF_INET6}.\n"
	    "Typical values for @var{style} are @code{SOCK_STREAM},\n"
	    "@code{SOCK_DGRAM} and @code{SOCK_RAW}.\n\n"
	    "@var{proto} can be obtained from a protocol name using\n"
	    "@code{getprotobyname}.  A value of zero specifies the default\n"
	    "protocol, which is usually right.\n\n"
	    "A single socket port cannot be used for communication until it\n"
	    "has been connected to another socket.")
#define FUNC_NAME s_scm_socket
{
  int fd;

  fd = socket (scm_to_int (family),
	       scm_to_int (style),
	       scm_to_int (proto));
  if (fd == -1)
    SCM_SYSERROR;
  return scm_socket_fd_to_port (fd);
}
#undef FUNC_NAME

#ifdef HAVE_SOCKETPAIR
SCM_DEFINE (scm_socketpair, "socketpair", 3, 0, 0,
            (SCM family, SCM style, SCM proto),
	    "Return a pair of connected (but unnamed) socket ports of the\n"
	    "type specified by @var{family}, @var{style} and @var{proto}.\n"
	    "Many systems support only socket pairs of the @code{AF_UNIX}\n"
	    "family.  Zero is likely to be the only meaningful value for\n"
	    "@var{proto}.")
#define FUNC_NAME s_scm_socketpair
{
  int fam;
  int fd[2];

  fam = scm_to_int (family);

  if (socketpair (fam, scm_to_int (style), scm_to_int (proto), fd) == -1)
    SCM_SYSERROR;

  return scm_cons (scm_socket_fd_to_port (fd[0]),
                   scm_socket_fd_to_port (fd[1]));
}
#undef FUNC_NAME
#endif

/* Possible results for `getsockopt ()'.  Wrapping it into a union guarantees
   suitable alignment.  */
typedef union
{
#ifdef HAVE_STRUCT_LINGER
  struct linger linger;
#endif
  size_t size;
  int    integer;
  struct timeval timeval;
} scm_t_getsockopt_result;

SCM_DEFINE (scm_getsockopt, "getsockopt", 3, 0, 0,
            (SCM sock, SCM level, SCM optname),
	    "Return an option value from socket port @var{sock}.\n"
	    "\n"
	    "@var{level} is an integer specifying a protocol layer, either\n"
	    "@code{SOL_SOCKET} for socket level options, or a protocol\n"
	    "number from the @code{IPPROTO} constants or @code{getprotoent}\n"
	    "(@pxref{Network Databases}).\n"
	    "\n"
	    "@defvar SOL_SOCKET\n"
	    "@defvarx IPPROTO_IP\n"
	    "@defvarx IPPROTO_TCP\n"
	    "@defvarx IPPROTO_UDP\n"
	    "@end defvar\n"
	    "\n"
	    "@var{optname} is an integer specifying an option within the\n"
	    "protocol layer.\n"
	    "\n"
	    "For @code{SOL_SOCKET} level the following @var{optname}s are\n"
	    "defined (when provided by the system).  For their meaning see\n"
	    "@ref{Socket-Level Options,,, libc, The GNU C Library Reference\n"
	    "Manual}, or @command{man 7 socket}.\n"
	    "\n"
	    "@defvar SO_DEBUG\n"
	    "@defvarx SO_REUSEADDR\n"
	    "@defvarx SO_STYLE\n"
	    "@defvarx SO_TYPE\n"
	    "@defvarx SO_ERROR\n"
	    "@defvarx SO_DONTROUTE\n"
	    "@defvarx SO_BROADCAST\n"
	    "@defvarx SO_SNDBUF\n"
	    "@defvarx SO_RCVBUF\n"
	    "@defvarx SO_KEEPALIVE\n"
	    "@defvarx SO_OOBINLINE\n"
	    "@defvarx SO_NO_CHECK\n"
	    "@defvarx SO_PRIORITY\n"
	    "@defvarx SO_REUSEPORT\n"
	    "The value returned is an integer.\n"
	    "@end defvar\n"
	    "\n"
	    "@defvar SO_LINGER\n"
	    "The value returned is a pair of integers\n"
	    "@code{(@var{enable} . @var{timeout})}.  On old systems without\n"
	    "timeout support (ie.@: without @code{struct linger}), only\n"
	    "@var{enable} has an effect but the value in Guile is always a\n"
	    "pair.\n"
	    "@end defvar"
            "\n"
	    "@defvar SO_RCVTIMEO\n"
	    "@defvarx SO_SNDTIMEO\n"
	    "@var{value} is a pair of integers @code{(@var{SECONDS}\n"
	    ". @var{MICROSECONDS})}.\n"
	    "@end defvar")
#define FUNC_NAME s_scm_getsockopt
{
  int fd;
  /* size of optval is the largest supported option.  */
  scm_t_getsockopt_result optval;
  socklen_t optlen = sizeof (optval);
  int ilevel;
  int ioptname;

  memset (&optval, 0, optlen);
  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT (1, sock);
  ilevel = scm_to_int (level);
  ioptname = scm_to_int (optname);

  fd = SCM_FPORT_FDES (sock);
  if (getsockopt (fd, ilevel, ioptname, (void *) &optval, &optlen) == -1)
    SCM_SYSERROR;

#if defined(SO_RCVTIMEO) && defined(SO_SNDTIMEO)
  if (ioptname == SO_RCVTIMEO || ioptname == SO_SNDTIMEO)
    {
      struct timeval *opt_time = (struct timeval *) &optval;

      return scm_cons (scm_from_long (opt_time->tv_sec),
                       scm_from_long (opt_time->tv_usec));
    }
#endif

  if (ilevel == SOL_SOCKET)
    {
#ifdef SO_LINGER
      if (ioptname == SO_LINGER)
	{
#ifdef HAVE_STRUCT_LINGER
	  struct linger *ling = (struct linger *) &optval;

	  return scm_cons (scm_from_long (ling->l_onoff),
			   scm_from_long (ling->l_linger));
#else
	  return scm_cons (scm_from_long (*(int *) &optval),
			   scm_from_int (0));
#endif
	}
#endif
    }
  return scm_from_int (*(int *) &optval);
}
#undef FUNC_NAME

SCM_DEFINE (scm_setsockopt, "setsockopt", 4, 0, 0,
            (SCM sock, SCM level, SCM optname, SCM value),
	    "Set an option on socket port @var{sock}.  The return value is\n"
	    "unspecified.\n"
	    "\n"
	    "@var{level} is an integer specifying a protocol layer, either\n"
	    "@code{SOL_SOCKET} for socket level options, or a protocol\n"
	    "number from the @code{IPPROTO} constants or @code{getprotoent}\n"
	    "(@pxref{Network Databases}).\n"
	    "\n"
	    "@defvar SOL_SOCKET\n"
	    "@defvarx IPPROTO_IP\n"
	    "@defvarx IPPROTO_TCP\n"
	    "@defvarx IPPROTO_UDP\n"
	    "@end defvar\n"
	    "\n"
	    "@var{optname} is an integer specifying an option within the\n"
	    "protocol layer.\n"
	    "\n"
	    "For @code{SOL_SOCKET} level the following @var{optname}s are\n"
	    "defined (when provided by the system).  For their meaning see\n"
	    "@ref{Socket-Level Options,,, libc, The GNU C Library Reference\n"
	    "Manual}, or @command{man 7 socket}.\n"
	    "\n"
	    "@defvar SO_DEBUG\n"
	    "@defvarx SO_REUSEADDR\n"
	    "@defvarx SO_STYLE\n"
	    "@defvarx SO_TYPE\n"
	    "@defvarx SO_ERROR\n"
	    "@defvarx SO_DONTROUTE\n"
	    "@defvarx SO_BROADCAST\n"
	    "@defvarx SO_SNDBUF\n"
	    "@defvarx SO_RCVBUF\n"
	    "@defvarx SO_KEEPALIVE\n"
	    "@defvarx SO_OOBINLINE\n"
	    "@defvarx SO_NO_CHECK\n"
	    "@defvarx SO_PRIORITY\n"
	    "@defvarx SO_REUSEPORT\n"
	    "@var{value} is an integer.\n"
	    "@end defvar\n"
	    "\n"
	    "@defvar SO_LINGER\n"
	    "@var{value} is a pair of integers @code{(@var{ENABLE}\n"
	    ". @var{TIMEOUT})}.  On old systems without timeout support\n"
	    "(ie.@: without @code{struct linger}), only @var{ENABLE} has an\n"
	    "effect but the value in Guile is always a pair.\n"
	    "@end defvar\n"
            "\n"
	    "@defvar SO_RCVTIMEO\n"
	    "@defvarx SO_SNDTIMEO\n"
	    "@var{value} is a pair of integers @code{(@var{SECONDS}\n"
	    ". @var{MICROSECONDS})}.\n"
	    "@end defvar\n"
	    "\n"
	    "@c  Note that we refer only to ``man ip'' here.  On GNU/Linux it's\n"
	    "@c  ``man 7 ip'' but on NetBSD it's ``man 4 ip''.\n"
	    "@c \n"
	    "For IP level (@code{IPPROTO_IP}) the following @var{optname}s\n"
	    "are defined (when provided by the system).  See @command{man\n"
	    "ip} for what they mean.\n"
	    "\n"
	    "@defvar IP_MULTICAST_IF\n"
            "This sets the source interface used by multicast traffic.\n"
	    "@end defvar\n"
	    "\n"
	    "@defvar IP_MULTICAST_TTL\n"
            "This sets the default TTL for multicast traffic. This defaults \n"
            "to 1 and should be increased to allow traffic to pass beyond the\n"
            "local network.\n"
	    "@end defvar\n"
	    "\n"
	    "@defvar IP_ADD_MEMBERSHIP\n"
	    "@defvarx IP_DROP_MEMBERSHIP\n"
	    "These can be used only with @code{setsockopt}, not\n"
	    "@code{getsockopt}.  @var{value} is a pair\n"
	    "@code{(@var{MULTIADDR} . @var{INTERFACEADDR})} of IPv4\n"
	    "addresses (@pxref{Network Address Conversion}).\n"
	    "@var{MULTIADDR} is a multicast address to be added to or\n"
	    "dropped from the interface @var{INTERFACEADDR}.\n"
	    "@var{INTERFACEADDR} can be @code{INADDR_ANY} to have the system\n"
	    "select the interface.  @var{INTERFACEADDR} can also be an\n"
	    "interface index number, on systems supporting that.\n"
	    "@end defvar")
#define FUNC_NAME s_scm_setsockopt
{
  int fd;

  int opt_int;
#ifdef HAVE_STRUCT_LINGER
  struct linger opt_linger;
#endif

#ifdef HAVE_STRUCT_IP_MREQ
  struct ip_mreq opt_mreq;
#endif

  struct timeval opt_time;

  const void *optval = NULL;
  socklen_t optlen = 0;

  int ilevel, ioptname;

  sock = SCM_COERCE_OUTPORT (sock);

  SCM_VALIDATE_OPFPORT (1, sock);
  ilevel = scm_to_int (level);
  ioptname = scm_to_int (optname);

  fd = SCM_FPORT_FDES (sock);
  
  if (ilevel == SOL_SOCKET)
    {
#ifdef SO_LINGER
      if (ioptname == SO_LINGER)
	{
#ifdef HAVE_STRUCT_LINGER
	  SCM_ASSERT (scm_is_pair (value), value, SCM_ARG4, FUNC_NAME);
	  opt_linger.l_onoff = scm_to_int (SCM_CAR (value));
	  opt_linger.l_linger = scm_to_int (SCM_CDR (value));
	  optlen = sizeof (struct linger);
	  optval = &opt_linger;
#else
	  SCM_ASSERT (scm_is_pair (value), value, SCM_ARG4, FUNC_NAME);
	  opt_int = scm_to_int (SCM_CAR (value));
	  /* timeout is ignored, but may as well validate it.  */
	  scm_to_int (SCM_CDR (value));
	  optlen = sizeof (int);
	  optval = &opt_int;
#endif
	}
#endif
    }

#ifdef HAVE_STRUCT_IP_MREQ
  if (ilevel == IPPROTO_IP &&
      (ioptname == IP_ADD_MEMBERSHIP || ioptname == IP_DROP_MEMBERSHIP))
    {
      /* Fourth argument must be a pair of addresses. */
      SCM_ASSERT (scm_is_pair (value), value, SCM_ARG4, FUNC_NAME);
      opt_mreq.imr_multiaddr.s_addr = htonl (scm_to_ulong (SCM_CAR (value)));
      opt_mreq.imr_interface.s_addr = htonl (scm_to_ulong (SCM_CDR (value)));
      optlen = sizeof (opt_mreq);
      optval = &opt_mreq;
    }
#endif

#if defined(SO_RCVTIMEO) && defined(SO_SNDTIMEO)
  if (ioptname == SO_RCVTIMEO || ioptname == SO_SNDTIMEO)
    {
      SCM_ASSERT_TYPE (scm_is_pair (value), value, SCM_ARG4, FUNC_NAME,
                       "value");
      opt_time.tv_sec = scm_to_ulong (SCM_CAR (value));
      opt_time.tv_usec = scm_to_ulong (SCM_CDR (value));

      optlen = sizeof (opt_time);
      optval = &opt_time;
    }
#endif

  if (optval == NULL)
    {
      /* Most options take an int.  */
      opt_int = scm_to_int (value);
      optlen = sizeof (int);
      optval = &opt_int;
    }

  if (setsockopt (fd, ilevel, ioptname, optval, optlen) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Our documentation hard-codes this mapping, so make sure it holds.  */
verify (SHUT_RD == 0);
verify (SHUT_WR == 1);
verify (SHUT_RDWR == 2);

SCM_DEFINE (scm_shutdown, "shutdown", 2, 0, 0,
          (SCM sock, SCM how),
	    "Sockets can be closed simply by using @code{close-port}. The\n"
	    "@code{shutdown} procedure allows reception or transmission on a\n"
	    "connection to be shut down individually, according to the parameter\n"
	    "@var{how}:\n\n"
	    "@table @asis\n"
	    "@item 0\n"
	    "Stop receiving data for this socket.  If further data arrives,  reject it.\n"
	    "@item 1\n"
	    "Stop trying to transmit data from this socket.  Discard any\n"
	    "data waiting to be sent.  Stop looking for acknowledgement of\n"
	    "data already sent; don't retransmit it if it is lost.\n"
	    "@item 2\n"
	    "Stop both reception and transmission.\n"
	    "@end table\n\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_shutdown
{
  int fd;
  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT (1, sock);
  fd = SCM_FPORT_FDES (sock);
  if (shutdown (fd, scm_to_signed_integer (how, 0, 2)) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* convert fam/address/args into a sockaddr of the appropriate type.
   args is modified by removing the arguments actually used.
   which_arg and proc are used when reporting errors:
   which_arg is the position of address in the original argument list.
   proc is the name of the original procedure.
   size returns the size of the structure allocated.  */

#define MAX(A, B) ((A) >= (B) ? (A) : (B))

static struct sockaddr *
scm_fill_sockaddr (int fam, SCM address, SCM *args, int which_arg,
		   const char *proc, size_t *size)
#define FUNC_NAME proc
{
  switch (fam)
    {
    case AF_INET:
      {
	struct sockaddr_in *soka;
	unsigned long addr;
	int port;

	SCM_VALIDATE_ULONG_COPY (which_arg, address, addr);
	SCM_VALIDATE_CONS (which_arg + 1, *args);
	port = scm_to_int (SCM_CAR (*args));
	*args = SCM_CDR (*args);
	soka = (struct sockaddr_in *) scm_malloc (sizeof (struct sockaddr_in));
        memset (soka, '\0', sizeof (struct sockaddr_in));

#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
	soka->sin_len = sizeof (struct sockaddr_in);
#endif
	soka->sin_family = AF_INET;
	soka->sin_addr.s_addr = htonl (addr);
	soka->sin_port = htons (port);
	*size = sizeof (struct sockaddr_in);
	return (struct sockaddr *) soka;
      }
#ifdef HAVE_IPV6
    case AF_INET6:
      {
	/* see RFC2553.  */
	int port;
	struct sockaddr_in6 *soka;
	unsigned long flowinfo = 0;
	unsigned long scope_id = 0;

	SCM_VALIDATE_CONS (which_arg + 1, *args);
	port = scm_to_int (SCM_CAR (*args));
	*args = SCM_CDR (*args);
	if (scm_is_pair (*args))
	  {
	    SCM_VALIDATE_ULONG_COPY (which_arg + 2, SCM_CAR (*args), flowinfo);
	    *args = SCM_CDR (*args);
	    if (scm_is_pair (*args))
	      {
		SCM_VALIDATE_ULONG_COPY (which_arg + 3, SCM_CAR (*args),
					 scope_id);
		*args = SCM_CDR (*args);
	      }
	  }
	soka = (struct sockaddr_in6 *) scm_malloc (sizeof (struct sockaddr_in6));

#ifdef HAVE_STRUCT_SOCKADDR_IN6_SIN6_LEN
	soka->sin6_len = sizeof (struct sockaddr_in6);
#endif
	soka->sin6_family = AF_INET6;
	scm_to_ipv6 (soka->sin6_addr.s6_addr, address);
	soka->sin6_port = htons (port);
	soka->sin6_flowinfo = flowinfo;
#ifdef HAVE_SIN6_SCOPE_ID
	soka->sin6_scope_id = scope_id;
#endif
	*size = sizeof (struct sockaddr_in6);
	return (struct sockaddr *) soka;
      }
#endif
#ifdef HAVE_UNIX_DOMAIN_SOCKETS
    case AF_UNIX:
      {
	struct sockaddr_un *soka;
	int addr_size;
	char *c_address;
	size_t c_address_size;

	scm_dynwind_begin (0);

	c_address = scm_to_locale_stringn (address, &c_address_size);
	scm_dynwind_free (c_address);

	/* the static buffer size in sockaddr_un seems to be arbitrary
	   and not necessarily a hard limit.  e.g., the glibc manual
	   suggests it may be possible to declare it size 0.  let's
	   ignore it.  if the O/S doesn't like the size it will cause
	   connect/bind etc., to fail.  sun_path is always the last
	   member of the structure.  */
	addr_size = sizeof (struct sockaddr_un)
	  + MAX (0, c_address_size + 1 - (sizeof soka->sun_path));
	soka = (struct sockaddr_un *) scm_malloc (addr_size);
	memset (soka, 0, addr_size);
	soka->sun_family = AF_UNIX;
        /* we accept 0-bytes here (used for abstract sockets in Linux);
           therefore do not use strlen() or SUN_LEN!  */
	memcpy (soka->sun_path, c_address, c_address_size);
	*size = offsetof (struct sockaddr_un, sun_path) + c_address_size;

	scm_dynwind_end ();
	return (struct sockaddr *) soka;
      }
#endif
    default:
      scm_out_of_range (proc, scm_from_int (fam));
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_connect, "connect", 2, 1, 1,
            (SCM sock, SCM fam_or_sockaddr, SCM address, SCM args),
	    "Initiate a connection from a socket using a specified address\n"
	    "family to the address\n"
	    "specified by @var{address} and possibly @var{args}.\n"
	    "The format required for @var{address}\n"
	    "and @var{args} depends on the family of the socket.\n\n"
	    "For a socket of family @code{AF_UNIX},\n"
	    "only @var{address} is specified and must be a string with the\n"
	    "filename where the socket is to be created.\n\n"
	    "For a socket of family @code{AF_INET},\n"
	    "@var{address} must be an integer IPv4 host address and\n"
	    "@var{args} must be a single integer port number.\n\n"
	    "For a socket of family @code{AF_INET6},\n"
	    "@var{address} must be an integer IPv6 host address and\n"
	    "@var{args} may be up to three integers:\n"
	    "port [flowinfo] [scope_id],\n"
	    "where flowinfo and scope_id default to zero.\n\n"
	    "Alternatively, the second argument can be a socket address object "
	    "as returned by @code{make-socket-address}, in which case the "
	    "no additional arguments should be passed.\n\n"
	    "Return true, unless the socket was configured to be non-blocking\n"
            "and the operation has not finished yet.\n")
#define FUNC_NAME s_scm_connect
{
  int fd;
  struct sockaddr *soka;
  size_t size;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT (1, sock);
  fd = SCM_FPORT_FDES (sock);

  if (scm_is_eq (address, SCM_UNDEFINED))
    /* No third argument was passed to FAM_OR_SOCKADDR must actually be a
       `socket address' object.  */
    soka = scm_to_sockaddr (fam_or_sockaddr, &size);
  else
    soka = scm_fill_sockaddr (scm_to_int (fam_or_sockaddr), address,
			      &args, 3, FUNC_NAME, &size);

  if (connect (fd, soka, size) == -1)
    {
      int save_errno = errno;

      free (soka);
      errno = save_errno;
      if (errno == EINPROGRESS || errno == EAGAIN)
        return SCM_BOOL_F;
      SCM_SYSERROR;
    }
  free (soka);
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bind, "bind", 2, 1, 1,
            (SCM sock, SCM fam_or_sockaddr, SCM address, SCM args),
	    "Assign an address to the socket port @var{sock}.\n"
	    "Generally this only needs to be done for server sockets,\n"
	    "so they know where to look for incoming connections.  A socket\n"
	    "without an address will be assigned one automatically when it\n"
	    "starts communicating.\n\n"
	    "The format of @var{address} and @var{args} depends\n"
	    "on the family of the socket.\n\n"
	    "For a socket of family @code{AF_UNIX}, only @var{address}\n"
	    "is specified and must be a string with the filename where\n"
	    "the socket is to be created.\n\n"
	    "For a socket of family @code{AF_INET}, @var{address}\n"
	    "must be an integer IPv4 address and @var{args}\n"
	    "must be a single integer port number.\n\n"
	    "The values of the following variables can also be used for\n"
	    "@var{address}:\n\n"
	    "@defvar INADDR_ANY\n"
	    "Allow connections from any address.\n"
	    "@end defvar\n\n"
	    "@defvar INADDR_LOOPBACK\n"
	    "The address of the local host using the loopback device.\n"
	    "@end defvar\n\n"
	    "@defvar INADDR_BROADCAST\n"
	    "The broadcast address on the local network.\n"
	    "@end defvar\n\n"
	    "@defvar INADDR_NONE\n"
	    "No address.\n"
	    "@end defvar\n\n"
	    "For a socket of family @code{AF_INET6}, @var{address}\n"
	    "must be an integer IPv6 address and @var{args}\n"
	    "may be up to three integers:\n"
	    "port [flowinfo] [scope_id],\n"
	    "where flowinfo and scope_id default to zero.\n\n"
	    "Alternatively, the second argument can be a socket address object "
	    "as returned by @code{make-socket-address}, in which case the "
	    "no additional arguments should be passed.\n\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_bind
{
  struct sockaddr *soka;
  size_t size;
  int fd;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT (1, sock);
  fd = SCM_FPORT_FDES (sock);

  if (scm_is_eq (address, SCM_UNDEFINED))
    /* No third argument was passed to FAM_OR_SOCKADDR must actually be a
       `socket address' object.  */
    soka = scm_to_sockaddr (fam_or_sockaddr, &size);
  else
    soka = scm_fill_sockaddr (scm_to_int (fam_or_sockaddr), address,
			      &args, 3, FUNC_NAME, &size);


  if (bind (fd, soka, size) == -1)
  {
    int save_errno = errno;

    free (soka);
    errno = save_errno;
    SCM_SYSERROR;
  }
  free (soka);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_listen, "listen", 2, 0, 0,
            (SCM sock, SCM backlog),
	    "Enable @var{sock} to accept connection\n"
	    "requests.  @var{backlog} is an integer specifying\n"
	    "the maximum length of the queue for pending connections.\n"
	    "If the queue fills, new clients will fail to connect until\n"
	    "the server calls @code{accept} to accept a connection from\n"
	    "the queue.\n\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_listen
{
  int fd;
  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT (1, sock);
  fd = SCM_FPORT_FDES (sock);
  if (listen (fd, scm_to_int (backlog)) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Put the components of a sockaddr into a new SCM vector.  */
static SCM_C_INLINE_KEYWORD SCM
_scm_from_sockaddr (const scm_t_max_sockaddr *address, unsigned addr_size,
		    const char *proc)
{
  SCM result = SCM_EOL;
  short int fam = ((struct sockaddr *) address)->sa_family;

  switch (fam)
    {
    case AF_INET:
      {
	const struct sockaddr_in *nad = (struct sockaddr_in *) address;

	result = scm_c_make_vector (3, SCM_UNSPECIFIED);

	SCM_SIMPLE_VECTOR_SET(result, 0,
			      scm_from_short (fam));
	SCM_SIMPLE_VECTOR_SET(result, 1,
			      scm_from_ulong (ntohl (nad->sin_addr.s_addr)));
	SCM_SIMPLE_VECTOR_SET(result, 2,
			      scm_from_ushort (ntohs (nad->sin_port)));
      }
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      {
	const struct sockaddr_in6 *nad = (struct sockaddr_in6 *) address;

	result = scm_c_make_vector (5, SCM_UNSPECIFIED);
	SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_short (fam));
	SCM_SIMPLE_VECTOR_SET(result, 1, scm_from_ipv6 (nad->sin6_addr.s6_addr));
	SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_ushort (ntohs (nad->sin6_port)));
	SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_uint32 (nad->sin6_flowinfo));
#ifdef HAVE_SIN6_SCOPE_ID
	SCM_SIMPLE_VECTOR_SET(result, 4, scm_from_ulong (nad->sin6_scope_id));
#else
	SCM_SIMPLE_VECTOR_SET(result, 4, SCM_INUM0);
#endif
      }
      break;
#endif
#ifdef HAVE_UNIX_DOMAIN_SOCKETS
    case AF_UNIX:
      {
	const struct sockaddr_un *nad = (struct sockaddr_un *) address;

	result = scm_c_make_vector (2, SCM_UNSPECIFIED);

	SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_short (fam));
	/* When addr_size is not enough to cover sun_path, do not try
	   to access it. */
	if (addr_size <= offsetof (struct sockaddr_un, sun_path))
	  SCM_SIMPLE_VECTOR_SET(result, 1, SCM_BOOL_F);
	else
          {
            size_t path_size = addr_size - offsetof (struct sockaddr_un, sun_path);
            SCM_SIMPLE_VECTOR_SET (result, 1,
                                   scm_from_locale_stringn (nad->sun_path,
                                                            path_size));
          }
      }
      break;
#endif
    default:
      result = SCM_UNSPECIFIED;
      scm_misc_error (proc, "unrecognised address family: ~A",
		      scm_list_1 (scm_from_int (fam)));

    }
  return result;
}

/* The publicly-visible function.  Return a Scheme object representing
   ADDRESS, an address of ADDR_SIZE bytes.  */
SCM
scm_from_sockaddr (const struct sockaddr *address, unsigned addr_size)
{
  return (_scm_from_sockaddr ((scm_t_max_sockaddr *) address,
			      addr_size, "scm_from_sockaddr"));
}

/* Convert ADDRESS, an address object returned by either
   `scm_from_sockaddr ()' or `scm_make_socket_address ()', into its C
   representation.  On success, a non-NULL pointer is returned and
   ADDRESS_SIZE is updated to the actual size (in bytes) of the returned
   address.  The result must eventually be freed using `free ()'.  */
struct sockaddr *
scm_to_sockaddr (SCM address, size_t *address_size)
#define FUNC_NAME "scm_to_sockaddr"
{
  short int family;
  struct sockaddr *c_address = NULL;

  SCM_VALIDATE_VECTOR (1, address);

  *address_size = 0;
  family = scm_to_short (SCM_SIMPLE_VECTOR_REF (address, 0));

  switch (family)
    {
    case AF_INET:
      {
	if (SCM_SIMPLE_VECTOR_LENGTH (address) != 3)
	  scm_misc_error (FUNC_NAME,
			  "invalid inet address representation: ~A",
			  scm_list_1 (address));
	else
	  {
	    struct sockaddr_in c_inet;

            memset (&c_inet, '\0', sizeof (struct sockaddr_in));

#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
            c_inet.sin_len = sizeof (struct sockaddr_in);
#endif

	    c_inet.sin_addr.s_addr =
	      htonl (scm_to_ulong (SCM_SIMPLE_VECTOR_REF (address, 1)));
	    c_inet.sin_port =
	      htons (scm_to_ushort (SCM_SIMPLE_VECTOR_REF (address, 2)));
	    c_inet.sin_family = AF_INET;

	    *address_size = sizeof (c_inet);
	    c_address = scm_malloc (sizeof (c_inet));
	    memcpy (c_address, &c_inet, sizeof (c_inet));
	  }

	break;
      }

#ifdef HAVE_IPV6
    case AF_INET6:
      {
	if (SCM_SIMPLE_VECTOR_LENGTH (address) != 5)
	  scm_misc_error (FUNC_NAME, "invalid inet6 address representation: ~A",
			  scm_list_1 (address));
	else
	  {
	    struct sockaddr_in6 c_inet6;

	    scm_to_ipv6 (c_inet6.sin6_addr.s6_addr,
			 SCM_SIMPLE_VECTOR_REF (address, 1));
	    c_inet6.sin6_port =
	      htons (scm_to_ushort (SCM_SIMPLE_VECTOR_REF (address, 2)));
	    c_inet6.sin6_flowinfo =
	      scm_to_uint32 (SCM_SIMPLE_VECTOR_REF (address, 3));
#ifdef HAVE_SIN6_SCOPE_ID
	    c_inet6.sin6_scope_id =
	      scm_to_ulong (SCM_SIMPLE_VECTOR_REF (address, 4));
#endif

	    c_inet6.sin6_family = AF_INET6;

	    *address_size = sizeof (c_inet6);
	    c_address = scm_malloc (sizeof (c_inet6));
	    memcpy (c_address, &c_inet6, sizeof (c_inet6));
	  }

	break;
      }
#endif

#ifdef HAVE_UNIX_DOMAIN_SOCKETS
    case AF_UNIX:
      {
	if (SCM_SIMPLE_VECTOR_LENGTH (address) != 2)
	  scm_misc_error (FUNC_NAME, "invalid unix address representation: ~A",
			  scm_list_1 (address));
	else
	  {
	    SCM path;
	    size_t path_len = 0;

	    path = SCM_SIMPLE_VECTOR_REF (address, 1);
	    if (!scm_is_string (path) && !scm_is_false (path))
	      scm_misc_error (FUNC_NAME, "invalid unix address "
			      "path: ~A", scm_list_1 (path));
	    else
	      {
		struct sockaddr_un c_unix;

		if (scm_is_false (path))
		  path_len = 0;
		else
		  path_len = scm_c_string_length (path);

#ifdef UNIX_PATH_MAX
		if (path_len >= UNIX_PATH_MAX)
#else
/* We can hope that this limit will eventually vanish, at least on GNU.
   However, currently, while glibc doesn't define `UNIX_PATH_MAX', it
   documents it has being limited to 108 bytes.  */
		if (path_len >= sizeof (c_unix.sun_path))
#endif
		  scm_misc_error (FUNC_NAME, "unix address path "
				  "too long: ~A", scm_list_1 (path));
		else
		  {
		    if (path_len)
		      {
			scm_to_locale_stringbuf (path, c_unix.sun_path,
#ifdef UNIX_PATH_MAX
						 UNIX_PATH_MAX);
#else
			                         sizeof (c_unix.sun_path));
#endif
			c_unix.sun_path[path_len] = '\0';

			/* Sanity check.  */
			if (strlen (c_unix.sun_path) != path_len)
			  scm_misc_error (FUNC_NAME, "unix address path "
					  "contains nul characters: ~A",
					  scm_list_1 (path));
		      }
		    else
		      c_unix.sun_path[0] = '\0';

		    c_unix.sun_family = AF_UNIX;

		    *address_size = SUN_LEN (&c_unix);
		    c_address = scm_malloc (sizeof (c_unix));
		    memcpy (c_address, &c_unix, sizeof (c_unix));
		  }
	      }
	  }

	break;
      }
#endif

    default:
      scm_misc_error (FUNC_NAME, "unrecognised address family: ~A",
		      scm_list_1 (scm_from_ushort (family)));
    }

  return c_address;
}
#undef FUNC_NAME


/* Return a newly-allocated `sockaddr' structure that reflects ADDRESS, being
   an address of family FAMILY, with the family-specific parameters ARGS (see
   the description of `connect' for details).  The returned structure may be
   freed using `free ()'.  */
struct sockaddr *
scm_c_make_socket_address (SCM family, SCM address, SCM args,
			   size_t *address_size)
{
  struct sockaddr *soka;

  soka = scm_fill_sockaddr (scm_to_ushort (family), address, &args, 1,
			    "scm_c_make_socket_address", address_size);

  return soka;
}

SCM_DEFINE (scm_make_socket_address, "make-socket-address", 2, 0, 1,
	    (SCM family, SCM address, SCM args),
	    "Return a Scheme address object that reflects @var{address}, "
	    "being an address of family @var{family}, with the "
	    "family-specific parameters @var{args} (see the description of "
	    "@code{connect} for details).")
#define FUNC_NAME s_scm_make_socket_address
{
  SCM result = SCM_BOOL_F;
  struct sockaddr *c_address;
  size_t c_address_size;

  c_address = scm_c_make_socket_address (family, address, args,
					 &c_address_size);
  if (c_address != NULL)
    {
      result = scm_from_sockaddr (c_address, c_address_size);
      free (c_address);
    }

  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_accept4, "accept", 1, 1, 0, 
            (SCM sock, SCM flags),
	    "Accept a connection on a bound, listening socket.  If there\n"
	    "are no pending connections in the queue, there are two\n"
            "possibilities: if the socket has been configured as\n"
            "non-blocking, return @code{#f} directly.  Otherwise wait\n"
            "until a connection is available.  When a connection comes,\n"
	    "the return value is a pair in which the @emph{car} is a new\n"
            "socket port for the connection and the @emph{cdr} is an\n"
            "object with address information about the client which\n"
            "initiated the connection.\n\n"
	    "@var{sock} does not become part of the\n"
	    "connection and will continue to accept new requests.")
#define FUNC_NAME s_scm_accept4
{
  int fd;
  int newfd;
  int c_flags;
  SCM address;
  SCM newsock;
  socklen_t addr_size = MAX_ADDR_SIZE;
  scm_t_max_sockaddr addr;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT (1, sock);
  c_flags = SCM_UNBNDP (flags) ? 0 : scm_to_int (flags);

  fd = SCM_FPORT_FDES (sock);
  SCM_SYSCALL (newfd = accept4 (fd, (struct sockaddr *) &addr, &addr_size,
                                c_flags));
  if (newfd == -1)
    {
      if (errno == EAGAIN || errno == EWOULDBLOCK)
        return SCM_BOOL_F;
      SCM_SYSERROR;
    }
  newsock = scm_socket_fd_to_port (newfd);
  address = _scm_from_sockaddr (&addr, addr_size, FUNC_NAME);

  return scm_cons (newsock, address);
}
#undef FUNC_NAME

SCM
scm_accept (SCM sock)
{
  return scm_accept4 (sock, SCM_UNDEFINED);
}

SCM_DEFINE (scm_getsockname, "getsockname", 1, 0, 0, 
            (SCM sock),
	    "Return the address of @var{sock}, in the same form as the\n"
	    "object returned by @code{accept}.  On many systems the address\n"
	    "of a socket in the @code{AF_FILE} namespace cannot be read.")
#define FUNC_NAME s_scm_getsockname
{
  int fd;
  socklen_t addr_size = MAX_ADDR_SIZE;
  scm_t_max_sockaddr addr;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT (1, sock);
  fd = SCM_FPORT_FDES (sock);
  if (getsockname (fd, (struct sockaddr *) &addr, &addr_size) == -1)
    SCM_SYSERROR;

  return _scm_from_sockaddr (&addr, addr_size, FUNC_NAME);
}
#undef FUNC_NAME

SCM_DEFINE (scm_getpeername, "getpeername", 1, 0, 0, 
            (SCM sock),
	    "Return the address that @var{sock}\n"
	    "is connected to, in the same form as the object returned by\n"
	    "@code{accept}.  On many systems the address of a socket in the\n"
	    "@code{AF_FILE} namespace cannot be read.")
#define FUNC_NAME s_scm_getpeername
{
  int fd;
  socklen_t addr_size = MAX_ADDR_SIZE;
  scm_t_max_sockaddr addr;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT (1, sock);
  fd = SCM_FPORT_FDES (sock);
  if (getpeername (fd, (struct sockaddr *) &addr, &addr_size) == -1)
    SCM_SYSERROR;

  return _scm_from_sockaddr (&addr, addr_size, FUNC_NAME);
}
#undef FUNC_NAME

SCM_DEFINE (scm_recv, "recv!", 2, 1, 0,
            (SCM sock, SCM buf, SCM flags),
	    "Receive data from a socket port.\n"
	    "@var{sock} must already\n"
	    "be bound to the address from which data is to be received.\n"
	    "@var{buf} is a bytevector into which\n"
	    "the data will be written.  The size of @var{buf} limits\n"
	    "the amount of\n"
	    "data which can be received: in the case of packet\n"
	    "protocols, if a packet larger than this limit is encountered\n"
	    "then some data\n"
	    "will be irrevocably lost.\n\n"
	    "The optional @var{flags} argument is a value or\n"
	    "bitwise OR of MSG_OOB, MSG_PEEK, MSG_DONTROUTE etc.\n\n"
	    "The value returned is the number of bytes read from the\n"
	    "socket.\n\n"
	    "Note that the data is read directly from the socket file\n"
	    "descriptor:\n"
	    "any unread buffered port data is ignored.")
#define FUNC_NAME s_scm_recv
{
  int rv, fd, flg;

  SCM_VALIDATE_OPFPORT (1, sock);

  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    flg = scm_to_int (flags);
  fd = SCM_FPORT_FDES (sock);

  SCM_VALIDATE_BYTEVECTOR (1, buf);

  SCM_SYSCALL (rv = recv (fd,
                          SCM_BYTEVECTOR_CONTENTS (buf),
                          SCM_BYTEVECTOR_LENGTH (buf),
                          flg));

  if (SCM_UNLIKELY (rv == -1))
    SCM_SYSERROR;

  scm_remember_upto_here (buf);
  return scm_from_int (rv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_send, "send", 2, 1, 0,
            (SCM sock, SCM message, SCM flags),
	    "Transmit bytevector @var{message} on socket port @var{sock}.\n"
	    "@var{sock} must already be bound to a destination address.  The\n"
	    "value returned is the number of bytes transmitted --\n"
	    "it's possible for\n"
	    "this to be less than the length of @var{message}\n"
	    "if the socket is\n"
	    "set to be non-blocking.  The optional @var{flags} argument\n"
	    "is a value or\n"
	    "bitwise OR of MSG_OOB, MSG_PEEK, MSG_DONTROUTE etc.\n\n"
	    "Note that the data is written directly to the socket\n"
	    "file descriptor:\n"
	    "any unflushed buffered port data is ignored.\n\n"
	    "This operation is defined only for strings containing codepoints\n"
	    "zero to 255.")
#define FUNC_NAME s_scm_send
{
  int rv, fd, flg;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_OPFPORT (1, sock);

  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    flg = scm_to_int (flags);

  fd = SCM_FPORT_FDES (sock);

  SCM_VALIDATE_BYTEVECTOR (1, message);

  SCM_SYSCALL (rv = send (fd,
                          SCM_BYTEVECTOR_CONTENTS (message),
                          SCM_BYTEVECTOR_LENGTH (message),
                          flg));

  if (rv == -1)
    SCM_SYSERROR;

  scm_remember_upto_here_1 (message);
  return scm_from_int (rv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_recvfrom, "recvfrom!", 2, 3, 0,
            (SCM sock, SCM buf, SCM flags, SCM start, SCM end),
	    "Receive data from socket port @var{sock} (which must be already\n"
	    "bound), returning the originating address as well as the data.\n"
	    "This is usually for use on datagram sockets, but can be used on\n"
	    "stream-oriented sockets too.\n"
	    "\n"
	    "The data received is stored in bytevector @var{buf}, using\n"
	    "either the whole bytevector or just the region between the optional\n"
	    "@var{start} and @var{end} positions.  The size of @var{buf}\n"
	    "limits the amount of data that can be received.  For datagram\n"
	    "protocols, if a packet larger than this is received then excess\n"
	    "bytes are irrevocably lost.\n"
	    "\n"
	    "The return value is a pair.  The @code{car} is the number of\n"
	    "bytes read.  The @code{cdr} is a socket address object which is\n"
	    "where the data came from, or @code{#f} if the origin is\n"
	    "unknown.\n"
	    "\n"
	    "The optional @var{flags} argument is a or bitwise OR\n"
	    "(@code{logior}) of @code{MSG_OOB}, @code{MSG_PEEK},\n"
	    "@code{MSG_DONTROUTE} etc.\n"
	    "\n"
	    "Data is read directly from the socket file descriptor, any\n"
	    "buffered port data is ignored.\n"
	    "\n"
	    "On a GNU/Linux system @code{recvfrom!} is not multi-threading,\n"
	    "all threads stop while a @code{recvfrom!} call is in progress.\n"
	    "An application may need to use @code{select}, @code{O_NONBLOCK}\n"
	    "or @code{MSG_DONTWAIT} to avoid this.")
#define FUNC_NAME s_scm_recvfrom
{
  int rv, fd, flg;
  SCM address;
  size_t offset, cend;
  socklen_t addr_size = MAX_ADDR_SIZE;
  scm_t_max_sockaddr addr;

  SCM_VALIDATE_OPFPORT (1, sock);
  fd = SCM_FPORT_FDES (sock);

  if (SCM_UNBNDP (flags))
    flg = 0;
  else
    SCM_VALIDATE_ULONG_COPY (3, flags, flg);

  ((struct sockaddr *) &addr)->sa_family = AF_UNSPEC;

  SCM_VALIDATE_BYTEVECTOR (1, buf);

  if (SCM_UNBNDP (end))
    cend = SCM_BYTEVECTOR_LENGTH (buf);
  else
    {
      cend = scm_to_size_t (end);
      if (SCM_UNLIKELY (cend > SCM_BYTEVECTOR_LENGTH (buf)))
        scm_out_of_range (FUNC_NAME, end);
    }

  if (SCM_UNBNDP (start))
    offset = 0;
  else
    {
      offset = scm_to_size_t (start);
      if (SCM_UNLIKELY (cend < offset))
        scm_out_of_range (FUNC_NAME, start);
    }

  SCM_SYSCALL (rv = recvfrom (fd,
                              SCM_BYTEVECTOR_CONTENTS (buf) + offset,
                              cend - offset, flg,
                              (struct sockaddr *) &addr, &addr_size));

  if (rv == -1)
    SCM_SYSERROR;

  /* `recvfrom' does not necessarily return an address.  Usually nothing
     is returned for stream sockets.  */
  if (((struct sockaddr *) &addr)->sa_family != AF_UNSPEC)
    address = _scm_from_sockaddr (&addr, addr_size, FUNC_NAME);
  else
    address = SCM_BOOL_F;

  scm_remember_upto_here_1 (buf);

  return scm_cons (scm_from_int (rv), address);
}
#undef FUNC_NAME

SCM_DEFINE (scm_sendto, "sendto", 3, 1, 1,
            (SCM sock, SCM message, SCM fam_or_sockaddr, SCM address, SCM args_and_flags),
	    "Transmit bytevector @var{message} on socket port\n"
	    "@var{sock}.  The\n"
	    "destination address is specified using the @var{fam_or_sockaddr},\n"
	    "@var{address} and\n"
	    "@var{args_and_flags} arguments, or just a socket address object "
	    "returned by @code{make-socket-address}, in a similar way to the\n"
	    "@code{connect} procedure.  @var{args_and_flags} contains\n"
	    "the usual connection arguments optionally followed by\n"
	    "a flags argument, which is a value or\n"
	    "bitwise OR of MSG_OOB, MSG_PEEK, MSG_DONTROUTE etc.\n\n"
	    "The value returned is the number of bytes transmitted --\n"
	    "it's possible for\n"
	    "this to be less than the length of @var{message} if the\n"
	    "socket is\n"
	    "set to be non-blocking.\n"
	    "Note that the data is written directly to the socket\n"
	    "file descriptor:\n"
	    "any unflushed buffered port data is ignored.\n"
	    "This operation is defined only for strings containing codepoints\n"
	    "zero to 255.")
#define FUNC_NAME s_scm_sendto
{
  int rv, fd, flg;
  struct sockaddr *soka;
  size_t size;

  sock = SCM_COERCE_OUTPORT (sock);
  SCM_VALIDATE_FPORT (1, sock);
  fd = SCM_FPORT_FDES (sock);

  if (!scm_is_number (fam_or_sockaddr))
    {
      /* FAM_OR_SOCKADDR must actually be a `socket address' object.  This
	 means that the following arguments, i.e. ADDRESS and those listed in
	 ARGS_AND_FLAGS, are the `MSG_' flags.  */
      soka = scm_to_sockaddr (fam_or_sockaddr, &size);
      if (!scm_is_eq (address, SCM_UNDEFINED))
	args_and_flags = scm_cons (address, args_and_flags);
    }
  else
    soka = scm_fill_sockaddr (scm_to_int (fam_or_sockaddr), address,
			      &args_and_flags, 3, FUNC_NAME, &size);

  if (scm_is_null (args_and_flags))
    flg = 0;
  else
    {
      SCM_VALIDATE_CONS (5, args_and_flags);
      flg = SCM_NUM2ULONG (5, SCM_CAR (args_and_flags));
    }

  SCM_VALIDATE_BYTEVECTOR (1, message);

  SCM_SYSCALL (rv = sendto (fd,
                            SCM_BYTEVECTOR_CONTENTS (message),
                            SCM_BYTEVECTOR_LENGTH (message),
                            flg, soka, size));

  if (rv == -1)
    {
      int save_errno = errno;
      free (soka);
      errno = save_errno;
      SCM_SYSERROR;
    }
  free (soka);

  scm_remember_upto_here_1 (message);
  return scm_from_int (rv);
}
#undef FUNC_NAME



void
scm_init_socket ()
{
  /* protocol families.  */
#ifdef AF_UNSPEC
  scm_c_define ("AF_UNSPEC", scm_from_int (AF_UNSPEC));
#endif
#if defined HAVE_UNIX_DOMAIN_SOCKETS && defined AF_UNIX
  scm_c_define ("AF_UNIX", scm_from_int (AF_UNIX));
#endif
#ifdef AF_INET
  scm_c_define ("AF_INET", scm_from_int (AF_INET));
#endif
#ifdef AF_INET6
  scm_c_define ("AF_INET6", scm_from_int (AF_INET6));
#endif

#ifdef PF_UNSPEC
  scm_c_define ("PF_UNSPEC", scm_from_int (PF_UNSPEC));
#endif
#ifdef PF_UNIX
  scm_c_define ("PF_UNIX", scm_from_int (PF_UNIX));
#endif
#ifdef PF_INET
  scm_c_define ("PF_INET", scm_from_int (PF_INET));
#endif
#ifdef PF_INET6
  scm_c_define ("PF_INET6", scm_from_int (PF_INET6));
#endif

  /* standard addresses.  */
#ifdef INADDR_ANY
  scm_c_define ("INADDR_ANY", scm_from_ulong (INADDR_ANY));
  scm_c_define ("IN6ADDR_ANY", scm_from_ulong (0));
#endif
#ifdef INADDR_BROADCAST
  scm_c_define ("INADDR_BROADCAST", scm_from_ulong (INADDR_BROADCAST));
#endif
#ifdef INADDR_NONE
  scm_c_define ("INADDR_NONE", scm_from_ulong (INADDR_NONE));
#endif
#ifdef INADDR_LOOPBACK
  scm_c_define ("INADDR_LOOPBACK", scm_from_ulong (INADDR_LOOPBACK));
  scm_c_define ("IN6ADDR_LOOPBACK", scm_from_ulong (1));
#endif

  /* socket types.

     SOCK_PACKET is deliberately omitted, the GNU/Linux socket(2) and
     packet(7) advise that it's obsolete and strongly deprecated.  */

#ifdef SOCK_STREAM
  scm_c_define ("SOCK_STREAM", scm_from_int (SOCK_STREAM));
#endif
#ifdef SOCK_DGRAM
  scm_c_define ("SOCK_DGRAM", scm_from_int (SOCK_DGRAM));
#endif
#ifdef SOCK_SEQPACKET
  scm_c_define ("SOCK_SEQPACKET", scm_from_int (SOCK_SEQPACKET));
#endif
#ifdef SOCK_RAW
  scm_c_define ("SOCK_RAW", scm_from_int (SOCK_RAW));
#endif
#ifdef SOCK_RDM
  scm_c_define ("SOCK_RDM", scm_from_int (SOCK_RDM));
#endif

  /* accept4 flags.  */
#ifdef SOCK_CLOEXEC
  scm_c_define ("SOCK_CLOEXEC", scm_from_int (SOCK_CLOEXEC));
#endif
#ifdef SOCK_NONBLOCK
  scm_c_define ("SOCK_NONBLOCK", scm_from_int (SOCK_NONBLOCK));
#endif

  /* setsockopt level.

     SOL_IP, SOL_TCP and SOL_UDP are defined on gnu/linux, but not on for
     instance NetBSD.  We define IPPROTOs because that's what the posix spec
     shows in its example at

     http://www.opengroup.org/onlinepubs/007904975/functions/getsockopt.html
  */
#ifdef SOL_SOCKET
  scm_c_define ("SOL_SOCKET", scm_from_int (SOL_SOCKET));
#endif
#ifdef IPPROTO_IP
  scm_c_define ("IPPROTO_IP", scm_from_int (IPPROTO_IP));
#endif
#ifdef IPPROTO_IPV6
  scm_c_define ("IPPROTO_IPV6", scm_from_int (IPPROTO_IPV6));
#endif
#ifdef IPPROTO_TCP
  scm_c_define ("IPPROTO_TCP", scm_from_int (IPPROTO_TCP));
#endif
#ifdef IPPROTO_UDP
  scm_c_define ("IPPROTO_UDP", scm_from_int (IPPROTO_UDP));
#endif

  /* setsockopt names.  */
#ifdef SO_DEBUG
  scm_c_define ("SO_DEBUG", scm_from_int (SO_DEBUG));
#endif
#ifdef SO_REUSEADDR
  scm_c_define ("SO_REUSEADDR", scm_from_int (SO_REUSEADDR));
#endif
#ifdef SO_STYLE
  scm_c_define ("SO_STYLE", scm_from_int (SO_STYLE));
#endif
#ifdef SO_TYPE
  scm_c_define ("SO_TYPE", scm_from_int (SO_TYPE));
#endif
#ifdef SO_ERROR
  scm_c_define ("SO_ERROR", scm_from_int (SO_ERROR));
#endif
#ifdef SO_DONTROUTE
  scm_c_define ("SO_DONTROUTE", scm_from_int (SO_DONTROUTE));
#endif
#ifdef SO_BROADCAST
  scm_c_define ("SO_BROADCAST", scm_from_int (SO_BROADCAST));
#endif
#ifdef SO_SNDBUF
  scm_c_define ("SO_SNDBUF", scm_from_int (SO_SNDBUF));
#endif
#ifdef SO_RCVBUF
  scm_c_define ("SO_RCVBUF", scm_from_int (SO_RCVBUF));
#endif
#ifdef SO_KEEPALIVE
  scm_c_define ("SO_KEEPALIVE", scm_from_int (SO_KEEPALIVE));
#endif
#ifdef SO_OOBINLINE
  scm_c_define ("SO_OOBINLINE", scm_from_int (SO_OOBINLINE));
#endif
#ifdef SO_NO_CHECK
  scm_c_define ("SO_NO_CHECK", scm_from_int (SO_NO_CHECK));
#endif
#ifdef SO_PRIORITY
  scm_c_define ("SO_PRIORITY", scm_from_int (SO_PRIORITY));
#endif
#ifdef SO_LINGER
  scm_c_define ("SO_LINGER", scm_from_int (SO_LINGER));
#endif
#ifdef SO_REUSEPORT				  /* new in Linux 3.9 */
  scm_c_define ("SO_REUSEPORT", scm_from_int (SO_REUSEPORT));
#endif
#ifdef SO_RCVTIMEO
  scm_c_define ("SO_RCVTIMEO", scm_from_int (SO_RCVTIMEO));
#endif
#ifdef SO_SNDTIMEO
  scm_c_define ("SO_SNDTIMEO", scm_from_int (SO_SNDTIMEO));
#endif

  /* recv/send options.  */
#ifdef MSG_DONTWAIT
  scm_c_define ("MSG_DONTWAIT", scm_from_int (MSG_DONTWAIT));
#endif
#ifdef MSG_OOB
  scm_c_define ("MSG_OOB", scm_from_int (MSG_OOB));
#endif
#ifdef MSG_PEEK
  scm_c_define ("MSG_PEEK", scm_from_int (MSG_PEEK));
#endif
#ifdef MSG_DONTROUTE
  scm_c_define ("MSG_DONTROUTE", scm_from_int (MSG_DONTROUTE));
#endif

  /* TCP options.  */
#ifdef TCP_NODELAY
  scm_c_define ("TCP_NODELAY", scm_from_int (TCP_NODELAY));
#endif
#ifdef TCP_CORK
  scm_c_define ("TCP_CORK", scm_from_int (TCP_CORK));
#endif

#ifdef IP_ADD_MEMBERSHIP
  scm_c_define ("IP_ADD_MEMBERSHIP", scm_from_int (IP_ADD_MEMBERSHIP));
  scm_c_define ("IP_DROP_MEMBERSHIP", scm_from_int (IP_DROP_MEMBERSHIP));
#endif

#ifdef IP_MULTICAST_TTL 
  scm_c_define ("IP_MULTICAST_TTL", scm_from_int ( IP_MULTICAST_TTL));
#endif

#ifdef IP_MULTICAST_IF 
  scm_c_define ("IP_MULTICAST_IF", scm_from_int ( IP_MULTICAST_IF));
#endif

#ifdef IPV6_V6ONLY
  scm_c_define ("IPV6_V6ONLY", scm_from_int (IPV6_V6ONLY));
#endif

  scm_add_feature ("socket");

#include "socket.x"
}

