#include "common.h"
#include <time.h>
#include <sys/stat.h>
#include "gvm.h"

int
main(int argc, const char *argv[])
{
  struct gvm_info *repo_info;

  time_t tb, t0, t1;
  int tdiff;
  tb = time(NULL);
  t0 = time(NULL);
  t1 = time(NULL);

  if (argc < 2) {
	  printf("Please supply .g file for test\n");
	  exit(0);
  }

  BU_GETSTRUCT(repo_info, gvm_info);
  gvm_info_init(repo_info);

  char *repo_path = "./GS_repository";

  gvm_init_repo(repo_info, repo_path);
  gvm_open_repo(repo_info, repo_path);


    /* time for initial setup */
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf("initial setup: %d sec\n", tdiff);
  t0 = time(NULL);

  gvm_import_g_file(repo_info, argv[1]);

  /* time for initial import */
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf(".g import : %d sec\n", tdiff);
  t0 = time(NULL);


  if (argc == 3) {
	  gvm_commit_g_file(repo_info, argv[2]);

	  /* time for change application */
	  t1 = time(NULL);
	  tdiff = (int)difftime(t1,t0);
	  printf("apply changes: %d sec\n", tdiff);
	  t0 = time(NULL);
  }

  gvm_export_g_file(repo_info, argv[1], "test.g", LATEST_VERSION);

  /* time for reassembly */
  t1 = time(NULL);
  tdiff = (int)difftime(t1,t0);
  printf("reassemble .g file: %d sec\n", tdiff);
  t0 = time(NULL);

  /* total time */
  tdiff = (int)difftime(t0,tb);
  printf("total delta: %d sec\n", tdiff);
}
