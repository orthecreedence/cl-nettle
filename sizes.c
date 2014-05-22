#include <stdio.h>
#include <nettle/aes.h>
#include <nettle/gcm.h>
#include <nettle/yarrow.h>

int main()
{
	printf("aes_ctx: %d\n", sizeof(struct aes_ctx));
	printf("gcm_ctx: %d\n", sizeof(struct gcm_ctx));
	printf("gcm_key: %d\n", sizeof(struct gcm_key));
	printf("gcm_aes_ctx: %d\n", sizeof(struct gcm_aes_ctx));
    printf("yarrow256_ctx: %d\n", sizeof(struct yarrow256_ctx));
}

