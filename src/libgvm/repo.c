#include "geomsvn.h"

int geomsvn_init_repo(void *pool, const char *repo_path) {
	APR_SUBPOOL_SETUP(pool);
	svn_repos_t *repos;
	int ret = 0;
	const char *repo_full_path = svn_path_canonicalize(repo_path, pool);
	GSVN_ERR(svn_repos_create(&repos, repo_full_path, NULL, NULL, NULL, NULL, subpool));
	svn_destory_pool(subpool);
	return ret;
}
