#include <stdio.h>
#include <nettle/aes.h>
#include <nettle/gcm.h>
#include <nettle/yarrow.h>
#include <nettle/hmac.h>
#include <nettle/md5.h>
#include <nettle/sha2.h>
/*#include <nettle/ecc.h>*/
/*#include <nettle/ecc-curve.h>*/
/*#include <nettle/ecdsa.h>*/
#include <nettle/rsa.h>
#include <nettle/buffer.h>

int main()
{
	printf("aes_ctx: %d\n", sizeof(struct aes_ctx));
	printf("gcm_ctx: %d\n", sizeof(struct gcm_ctx));
	printf("gcm_key: %d\n", sizeof(struct gcm_key));
	printf("gcm_aes_ctx: %d\n", sizeof(struct gcm_aes_ctx));

	printf("yarrow256_ctx: %d\n", sizeof(struct yarrow256_ctx));

	printf("md5_ctx: %d\n", sizeof(struct md5_ctx));
	printf("sha1_ctx: %d\n", sizeof(struct sha1_ctx));
	printf("sha256_ctx: %d\n", sizeof(struct sha256_ctx));
	printf("sha512_ctx: %d\n", sizeof(struct sha512_ctx));

	printf("hmac_md5_ctx: %d\n", sizeof(struct hmac_md5_ctx));
	printf("hmac_sha1_ctx: %d\n", sizeof(struct hmac_sha1_ctx));
	printf("hmac_sha256_ctx: %d\n", sizeof(struct hmac_sha256_ctx));
	printf("hmac_sha512_ctx: %d\n", sizeof(struct hmac_sha512_ctx));

	/*printf("ecc_point: %d\n", sizeof(struct ecc_point));*/
	/*printf("ecc_scalar: %d\n", sizeof(struct ecc_scalar));*/

	printf("rsa_public_key: %d\n", sizeof(struct rsa_public_key));
	printf("rsa_private_key: %d\n", sizeof(struct rsa_private_key));

	printf("nettle_buffer: %d\n", sizeof(struct nettle_buffer));
}

