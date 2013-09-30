/*
   Copyright 2009, 2010, 2011 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#include <iostream>

#include <google/protobuf/compiler/importer.h>

#include "proto2piqi.h"


using namespace std;
using namespace google::protobuf::compiler;


void usage()
{
    cerr << "usage: proto2piqi <*.proto>+" << endl;
}


class ErrorCollector : public MultiFileErrorCollector
{
    virtual void AddError(
            const string & filename, int line, int column,
            const string & message)
    {
        cerr << filename << ":" << line << ":" << column
             << ": " << message << endl;
    }
};


int main(int argc, char *argv[])
{
    //TODO: handle -I command-line option
    if (argc < 2)
    {
        usage();
        return 1;
    }
    argv++; // skip program name
    argc--;

    DiskSourceTree sourceTree;
    sourceTree.MapPath("", ""); // XXX

    MultiFileErrorCollector *errorCollector = new ErrorCollector;
    Importer importer(&sourceTree, errorCollector);

    int i;
    for (i = 0; i < argc; i++)
    {
        const google::protobuf::FileDescriptor *fd;
        fd = importer.Import(argv[i]);
        if (!fd)
        {
            cerr << "errors occured while importing " << argv[i]
                 << endl;
            return 2;
        }

        piqi::dump_file_descriptor(fd);
    }

	return 0;
}
