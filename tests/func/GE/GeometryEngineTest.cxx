/*         G E O M E T R Y E N G I N E T E S T . C X X
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
/** @file GeometryEngineTest.cxx
 *
 *
 */

#include <bu.h>
#include <raytrace.h>
#include <list>
#include "FileDataSource.h"

struct node_data {
  char* path;
  struct bu_external extList;
};

/* Start Function */
int
start_fn_call(struct db_tree_state *, const struct db_full_path * pathp,
    const struct rt_comb_internal *, genptr_t)
{

  char *name = db_path_to_string(pathp);
  std::cout << "start: '" << name << "'" << std::endl;

  return 1;
}

/* Leaf Function */
union tree *
leaf_fn_call(struct db_tree_state *tsp, const struct db_full_path *pathp,
    struct rt_db_internal *ip, genptr_t)
{
  int i = 0;
  char *name = db_path_to_string(pathp);
  std::cout << "leaf: '"; // << name << "'" << std::endl;

  for (i = 0; i < pathp->fp_len; ++i)
    if (pathp->fp_names[i])
      std::cout << "/" << pathp->fp_names[i]->d_namep;
    else
      std::cout << "/" << "**NULL**";

  std::cout << "\tMAX i = " << i << std::endl;

  bu_external* ext = (bu_external*)bu_calloc(sizeof(bu_external),1,"bu_external calloc");
  int rVal = db_get_external(ext, pathp->fp_names[i], tsp->ts_dbip);



  return (union tree *)NULL;
}

int
main(int argc, char* argv[])
{
  if (argc < 2)
    {
      std::cout << "Usage " << argv[0] << " BRLCAD-Databse." << std::endl;
      return 1;
    }

  const char* gName = argv[1];
  std::cout << "Using: '" << gName << "' ." << std::endl;

  std::string testName(gName);
  int ret = FileDataSource::walkPath(testName);

  std::cout << "\nDone, got: " << ret << " steps\n" << std::endl;


  return 0;


  std::list<node_data> dataList;
  struct db_i* dbip = DBI_NULL;

  struct db_tree_state tree_state; /* includes tol & model */

  tree_state = rt_initial_tree_state; /* struct copy */

  /* Open DB */
  if ((dbip = db_open(gName, "r")) == DBI_NULL)
    {
      perror(gName);
      bu_exit(1, "Unable to open geometry file (%s)\n", gName);
    }
  if (db_dirbuild(dbip))
    {
      db_close(dbip);
      bu_exit(1, "ERROR: db_dirbuild failed\n");
    }

  db_update_nref(dbip, &rt_uniresource);


  /* Walk down from top Objects */
  struct directory* dp;
  struct db_full_path dfp;
  char* myArgs[1];

  db_full_path_init(&dfp);
  for (int i = 0; i < RT_DBNHASH; i++)    {
      for (dp = dbip->dbi_Head[i]; dp != RT_DIR_NULL; dp = dp->d_forw)        {
          if (dp->d_nref == 0 && !(dp->d_flags & RT_DIR_HIDDEN) && (dp->d_addr
              != RT_DIR_PHONY_ADDR))            {
              db_string_to_path(&dfp, dbip, dp->d_namep);

              myArgs[0] = dp->d_namep;

              (void) db_walk_tree(
                  dbip,         /* db_i */
                  1,            /* argc */
                  (const char **)(myArgs), /* argv */
                  1,            /* ncpu */
                  &tree_state,  /* state */
                  start_fn_call,/* start func */
                  NULL,         /* end func */
                  leaf_fn_call, /* leaf func */
                  (genptr_t)&dataList);  /* client_data */
           }
        }
    }
  db_free_full_path(&dfp);

  db_close(dbip);

  return 0;
}

// Local Variables:
// tab-width: 8
// mode: C++
// c-basic-offset: 4
// indent-tabs-mode: t
// c-file-style: "stroustrup"
// End:
// ex: shiftwidth=4 tabstop=8
