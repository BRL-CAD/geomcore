#ifndef __APPLICATION_H__
#define __APPLICATION_H__

// system headers
#include <string>

// interface headers
#include "Date.h"
#include "Time.h"


namespace Utility {

  /** Application is responsible for defining and handling command-line options
   * and, more importantly, for actually doing something.
   */
  class Application
  {
  protected:
    Date _runDate;
    Time _runTime;

  public:
    Application(int argc=0, char *argv[]=0);
    ~Application();

    /** main routine for an application, should only return when the
     * application is completed.
     */
    bool run();

    /** date when the application was compiled */
    Date  buildDate() const;
    /** time of day when the application was compiled */
    Time buildTime() const;

    /** date when the application run-loop was started */
    Date runDate() const;
    /** time when the application run-loop was started */
    Time runTime() const;

    /** status message of how long app has been running */
    std::string uptime() const;

  };

}


#endif  /* __APPLICATION_H__ */

// Local Variables: ***
// mode: C++ ***
// tab-width: 8 ***
// c-basic-offset: 2 ***
// indent-tabs-mode: t ***
// End: ***
// ex: shiftwidth=2 tabstop=8
