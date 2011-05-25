/*                    B R L C A D D B . C X X
 * BRL-CAD
 *
 * Copyright (c) 2011 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
/** @file BrlcadDb.cxx
 *
 */

#include "BrlcadDb.h"
#include "StringUtils.h"

#include "db.h"
#include "raytrace.h"

int BrlcadDb::FS_PATH_NOT_VALID = -1;
int BrlcadDb::G_PATH_NOT_VALID = -2;
int BrlcadDb::CORRUPT_OBJ_DATA = -3;

BrlcadDb*
BrlcadDb::makeDb(const std::string path) {
  struct db_i *dbip;

 /* Test by attempting to open */
  if ((dbip = db_open(path.c_str(), "r")) == DBI_NULL) {
      return NULL;
  }
  db_close(dbip);

  return new BrlcadDb(path);
}

BrlcadDb::BrlcadDb(const std::string path) :
  path(path), dbip(DBI_NULL)
{
}

BrlcadDb::~BrlcadDb()
{
  this->close();
}

const bool
BrlcadDb::open(const bool readOnly) {
  this->dbip = db_open(this->path.c_str(), (readOnly ? "r" : ""));
  if (this->dbip == DBI_NULL)
      return false;

  if (db_dirbuild(this->dbip)) {
      this->close();
      return false;
  }
  db_update_nref(dbip, &rt_uniresource);
  return true;
}

void
BrlcadDb::close() {
  if (this->dbip != DBI_NULL){
    db_close(dbip);
    this->dbip = DBI_NULL;
  }
}

const bool
BrlcadDb::isValidPath(const std::string path) {
  if (this->open() == false)
    return false;
  bool retVal = this->_isValidPath(path);
  this->close();
  return retVal;
}

const bool
BrlcadDb::_isValidPath(const std::string path) {
  /* Assume that 'path' is formatted to be a BRLCAD DB path */
  std::list<std::string> pathStack;
  StringUtils::splitPath(path, &pathStack);
  std::list<std::string>::const_iterator it = pathStack.begin();

  std::string pathSoFar = "";
  std::string pathStep = "";
  struct directory *dp;
  struct db_full_path dfp;
  int dbStep = 0;
  int exists = 0;

  for (; it != pathStack.end(); ++it)
    {
      pathStep = (std::string) *it;
      pathSoFar += pathStep;

      db_full_path_init(&dfp);
      exists = db_string_to_path(&dfp, this->dbip, pathSoFar.c_str());
      db_free_full_path(&dfp);

      if (exists != 0)
          return false;

      pathSoFar += PATH_DELIM;
      ++dbStep;
    }

  return true;
}

const int
BrlcadDb::list(const std::string path, std::list<std::string>* list)
{
  if (this->open() == false) return G_PATH_NOT_VALID;

  const int retVal = this->_list(path, list);
  this->close();
  return retVal;
}

const int
BrlcadDb::_list(const std::string path, std::list<std::string>* items)
{
  struct directory *dp;
  struct db_full_path dfp;
  int dbStep = 0;
  int exists = 0;
  int itemsAdded = 0;

  /* If we are getting TOPS of the file... */
  if ((path.length() == 0 || path == "/")) {
    for (int i = 0; i < RT_DBNHASH; i++)
      for (dp = dbip->dbi_Head[i]; dp != RT_DIR_NULL; dp = dp->d_forw)
        if (dp->d_nref == 0 && !(dp->d_flags & RT_DIR_HIDDEN) && (dp->d_addr != RT_DIR_PHONY_ADDR)) {
          items->push_back(std::string(dp->d_namep));
          ++itemsAdded;
        }
    return itemsAdded;
  }
  /* If not TOPS, then look it up! */

  /* Check to see if we have a valid path, or just a objectName */
  std::string objName = StringUtils::getLastStepOfPath(path);

  dp = db_lookup(this->dbip, objName.c_str(), 0);
  if (dp == RT_DIR_NULL)  return G_PATH_NOT_VALID;

  struct rt_db_internal in;
  struct rt_comb_internal *comb;

  if (rt_db_get_internal5(&in, dp, dbip, NULL, &rt_uniresource) < 0)
    return CORRUPT_OBJ_DATA;

  //check     in.idb_major_type and in.idb_minor_type here
  if (in.idb_major_type != DB5_MAJORTYPE_BRLCAD || in.idb_type != ID_COMBINATION)
    return 0;

  comb = (struct rt_comb_internal *) in.idb_ptr;

  size_t i;
  size_t node_count;
  struct rt_tree_array *rt_tree_array;
  union tree *ntp;

  RT_CK_RESOURCE(&rt_uniresource);

  /* TODO No tree?  Error or just return no children? */
  if (!comb->tree) {
      rt_db_free_internal(&in);
      return 0;
  }
  RT_CK_TREE(comb->tree);

  node_count = db_tree_nleaves(comb->tree);
  if (node_count == 0) {
      rt_db_free_internal(&in);
      return 0;
  }

  ntp = db_dup_subtree(comb->tree, &rt_uniresource);
  RT_CK_TREE(ntp);

  /* Convert to "v4 / GIFT style", so that the flatten makes sense. */
  if (db_ck_v4gift_tree(ntp) < 0)
          db_non_union_push(ntp, &rt_uniresource);
  RT_CK_TREE(ntp);

  node_count = db_tree_nleaves(ntp);
  rt_tree_array = (struct rt_tree_array *) bu_calloc(node_count,
                  sizeof(struct rt_tree_array), "rt_tree_array");

  /*
   * free=0 means that the tree won't have any leaf nodes freed.
   */
  (void) db_flatten_tree(rt_tree_array, ntp, OP_UNION, 0, &rt_uniresource);

  union tree *itp = NULL;
  for (i = 0; i < node_count; i++)
    {
      itp = rt_tree_array[i].tl_tree;

      RT_CK_TREE(itp);
      BU_ASSERT_LONG(itp->tr_op, ==, OP_DB_LEAF);
      BU_ASSERT_PTR(itp->tr_l.tl_name, !=, NULL);

      items->push_back(std::string(itp->tr_l.tl_name));
    }

  if (rt_tree_array)
          bu_free((genptr_t) rt_tree_array, "rt_tree_array");
  db_free_tree(ntp, &rt_uniresource);

  rt_db_free_internal(&in);

  return node_count;
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
