int content_get(struct vcs_db *db, int rid, Blob *pBlob);
int content_is_private(struct vcs_db *db, int rid);
void content_make_public(struct vcs_db *db, int rid);
int content_deltify(struct vcs_db *db, int rid, int srcid, int force);
int content_put(struct vcs_db *db, Blob *pBlob);
