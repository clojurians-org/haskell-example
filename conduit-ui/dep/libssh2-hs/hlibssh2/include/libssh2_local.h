
#include <sys/stat.h>

// This definition is used to determine sizeof(struct stat),
// because c2hs lacks support for such case.
typedef struct stat stat_t;

