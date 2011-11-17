SET(glog_srcs
	logging.cc
	raw_logging.cc
	vlog_is_on.cc
	utilities.cc
	demangle.cc
	symbolize.cc
	signalhandler.cc
	)

add_library(glog ${glog_srcs})
