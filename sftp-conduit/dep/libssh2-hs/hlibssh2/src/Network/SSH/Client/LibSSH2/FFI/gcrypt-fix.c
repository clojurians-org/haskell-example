#include <gcrypt.h>
#include <pthread.h>
#include <error.h>
#include <errno.h>
#include "gcrypt-fix.h"

GCRY_THREAD_OPTION_PTHREAD_IMPL;

void gcrypt_fix() {
   gcry_control (GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread);
}
