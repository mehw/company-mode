/* print_commented_functions.c - print header with commented functions */

/* Copyright (C) 2015 mehw

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/* Compile with librt:
   $ gcc -Wall -lrt -o print_commented_functions print_commented_functions.c
*/

/* Usage:
   $ ./print_commented_functions <number_of_repetitions>
*/

#include <stdio.h>
#include <time.h>

const char *func_prefix = "foo";
const char *func_type = "int";
const char *func_args = "(int a, float b)";
const char *func_body = "{return 0;}";
const char *comment = "/** This is a quite long, useless, and annoying comment in Doxygen style: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. */";

int main(int argc, char *argv[]) {
  int i, n;
  struct timespec tnow;

  sscanf(argv[1],"%d",&n);

  printf("// number of repetitions: %d\n", n);
  printf("#ifndef MASSIVE_COMMENTED_HEADER_H\n");
  printf("#define MASSIVE_COMMENTED_HEADER_H\n");

  for(i = 0; i < n; i++) {
    clock_gettime(CLOCK_REALTIME, &tnow);
    printf("%s\n", comment);
    printf("%s %s%lu%lu%s%s\n", func_type, func_prefix, tnow.tv_sec, tnow.tv_nsec, func_args, func_body);
  }

  printf("#endif\n");

  return 0;
}
