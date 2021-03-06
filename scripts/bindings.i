%module bindings

%feature("intern_function", "lispify");

%insert("lisphead") %{
(in-package :nettle)
%}

%include "stdint.i"
typedef unsigned int size_t;

/* ignore these because CFFI reports the wrong sizes */
%ignore "aes_ctx";
%ignore "gcm_ctx";
%ignore "gcm_block";
%ignore "gcm_key";
%ignore "gcm_aes_ctx";
%ignore "yarrow256_ctx";

%include "/c/usr/local/nettle/include/nettle/aes.h"
%include "/c/usr/local/nettle/include/nettle/arcfour.h"
%include "/c/usr/local/nettle/include/nettle/arctwo.h"
%include "/c/usr/local/nettle/include/nettle/asn1.h"
%include "/c/usr/local/nettle/include/nettle/base16.h"
%include "/c/usr/local/nettle/include/nettle/base64.h"
%include "/c/usr/local/nettle/include/nettle/bignum.h"
%include "/c/usr/local/nettle/include/nettle/blowfish.h"
%include "/c/usr/local/nettle/include/nettle/buffer.h"
%include "/c/usr/local/nettle/include/nettle/camellia.h"
%include "/c/usr/local/nettle/include/nettle/cast128.h"
%include "/c/usr/local/nettle/include/nettle/cbc.h"
%include "/c/usr/local/nettle/include/nettle/ctr.h"
%include "/c/usr/local/nettle/include/nettle/des.h"
%include "/c/usr/local/nettle/include/nettle/dsa.h"
%include "/c/usr/local/nettle/include/nettle/ecc.h"
%include "/c/usr/local/nettle/include/nettle/ecc-curve.h"
%include "/c/usr/local/nettle/include/nettle/ecdsa.h"
%include "/c/usr/local/nettle/include/nettle/gcm.h"
%include "/c/usr/local/nettle/include/nettle/gosthash94.h"
%include "/c/usr/local/nettle/include/nettle/hmac.h"
%include "/c/usr/local/nettle/include/nettle/knuth-lfib.h"
%include "/c/usr/local/nettle/include/nettle/macros.h"
%include "/c/usr/local/nettle/include/nettle/md2.h"
%include "/c/usr/local/nettle/include/nettle/md4.h"
%include "/c/usr/local/nettle/include/nettle/md5.h"
%include "/c/usr/local/nettle/include/nettle/memxor.h"
%include "/c/usr/local/nettle/include/nettle/nettle-meta.h"
%include "/c/usr/local/nettle/include/nettle/nettle-stdint.h"
%include "/c/usr/local/nettle/include/nettle/nettle-types.h"
%include "/c/usr/local/nettle/include/nettle/pbkdf2.h"
%include "/c/usr/local/nettle/include/nettle/pgp.h"
%include "/c/usr/local/nettle/include/nettle/pkcs1.h"
%include "/c/usr/local/nettle/include/nettle/realloc.h"
%include "/c/usr/local/nettle/include/nettle/ripemd160.h"
%include "/c/usr/local/nettle/include/nettle/rsa.h"
%include "/c/usr/local/nettle/include/nettle/salsa20.h"
%include "/c/usr/local/nettle/include/nettle/serpent.h"
%include "/c/usr/local/nettle/include/nettle/sexp.h"
%include "/c/usr/local/nettle/include/nettle/sha.h"
%include "/c/usr/local/nettle/include/nettle/sha1.h"
%include "/c/usr/local/nettle/include/nettle/sha2.h"
%include "/c/usr/local/nettle/include/nettle/sha3.h"
%include "/c/usr/local/nettle/include/nettle/twofish.h"
%include "/c/usr/local/nettle/include/nettle/umac.h"
%include "/c/usr/local/nettle/include/nettle/yarrow.h"

