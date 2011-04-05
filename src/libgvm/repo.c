#include "gvm.h"
#include "gvm_svn.h"

int gvm_init_repo(struct gvm_info *repo_info, const char *repo_path) {
	struct geomsvn_info *internal = NULL;
	if (!repo_info || !repo_path) return -2;
	internal = (struct geomsvn_info *)repo_info->internal;
	if (!internal) gvm_info_init(repo_info);
	repo_info->repo_full_path = svn_path_canonicalize(repo_path, internal->pool);
	if (svn_repos_find_root_path(repo_info->repo_full_path, internal->pool)) {
		return -1;
	} else {
		svn_repos_create(&(internal->repos), repo_info->repo_full_path, NULL, NULL, NULL, NULL, internal->pool);
	}
}

int gvm_open_repo(struct gvm_info *repo_info, const char *repo_path) {
	struct geomsvn_info *internal = NULL;
	if (!repo_info || !repo_path) return -2;
	internal = (struct geomsvn_info *)repo_info->internal;
	if (!internal) gvm_info_init(repo_info);
	repo_info->repo_full_path = svn_path_canonicalize(repo_path, internal->pool);
	if (svn_repos_find_root_path(repo_info->repo_full_path, internal->pool)) {
		svn_repos_open(&(internal->repos), repo_info->repo_full_path, internal->pool);
	} else {
		return -1;
	}
}

