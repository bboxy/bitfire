/*
 * (c) Copyright 2021 by Tobias Bindhammer. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * The name of its author may not be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

int debug_level;

//log levels
#define ENABLE_DEBUG      0
#define ENABLE_BENCHMARK  1
#define ENABLE_INFO       1
#define ENABLE_FAIL       1
#define ENABLE_FATAL      1

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
