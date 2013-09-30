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

#ifndef PROTO2PIQI_H__
#define PROTO2PIQI_H__


#include <google/protobuf/descriptor.h>


namespace piqi
{


extern
void dump_descriptor(const google::protobuf::Descriptor *d);

extern
void dump_file_descriptor(const google::protobuf::FileDescriptor *fd);


} // namespace piqi


#endif
