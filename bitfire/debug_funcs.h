#ifndef DEBUG_FUNCS_H
#define DEBUG_FUNCS_H

//void debug_print_hex(unsigned char, int, char*, int);

#ifdef _MSC_VER
	#define debug_message(format,...);  { if(ENABLE_DEBUG) { fprintf(stdout,"DEBUG: %s: %s(): ",__FILE__,__FUNCTION__); fprintf(stdout,format,__VA_ARGS__); } }
	#define info_message(format,...);   { if(ENABLE_INFO) { fprintf(stdout,"INFO: "); fprintf(stdout,format,__VA_ARGS__); } }
	#define fail_message(format,...);   { if(ENABLE_FAIL) { fprintf(stdout,"FAIL: %s: ",__FILE__); fprintf(stdout,format,__VA_ARGS__); } }
	#define fatal_message(format,...);  { if(ENABLE_FATAL) { fprintf(stderr,"ERROR: "); fprintf(stderr,format,__VA_ARGS__); exit(2); } }
#else
	#define debug_message(format,args...);  { if(ENABLE_DEBUG) { fprintf(stdout,"DEBUG: %s: %s(): ",__FILE__,__FUNCTION__); fprintf(stdout,format,##args); } }
	#define info_message(format,args...);   { if(ENABLE_INFO) { fprintf(stdout,"INFO: "); fprintf(stdout,format,##args); } }
	#define fail_message(format,args...);   { if(ENABLE_FAIL) { fprintf(stdout,"FAIL: %s: ",__FILE__); fprintf(stdout,format,##args); } }
	#define fatal_message(format,args...);  { if(ENABLE_FATAL) { fprintf(stderr,"ERROR: "); fprintf(stderr,format,##args); exit(2); } }

#endif

#endif /* DEBUG_FUNCS_H */
