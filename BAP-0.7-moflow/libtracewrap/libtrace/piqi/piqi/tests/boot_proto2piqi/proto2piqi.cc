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

#include <assert.h>

#include <iostream>
#include <sstream>
#include <string>
#include <set>

#include <google/protobuf/descriptor.h>

#include <proto2piqi.h>


using namespace std;
using namespace google::protobuf;


namespace piqi
{

// set for seen record and enum descriptors
typedef set<const void *> seen_set;

// current file descriptor 
FileDescriptor *current_fd;


// check if the element has been seen
// if the element has not been seen update the set of seen elements by inserting
// the element into it
static
bool check_update_seen(const void *p, seen_set &seen)
{
    bool is_seen = (seen.find(p) != seen.end()) ? true : false;
    if (!is_seen)
        seen.insert(p);
    return is_seen;
}


static
string make_record_name(const Descriptor *r)
{
    const Descriptor *parent = r->containing_type();
    if (parent)
        return make_record_name(parent) + "-" + r->name();
    else 
        return r->name();
}


static
string make_enum_name(const EnumDescriptor *e)
{
    const Descriptor *parent = e->containing_type();
    if (parent)
        return make_record_name(parent) + "-" + e->name();
    else
        return e->name();
}


static
string gen_default_value(const FieldDescriptor *f)
{
    if (f->has_default_value())
    {
        // the field has explicitly defined default value
        ostringstream t;
        switch (f->type())
        {
            case FieldDescriptor::TYPE_DOUBLE:
                t << f->default_value_double(); break;
            case FieldDescriptor::TYPE_FLOAT:
                t << f->default_value_float(); break;
            case FieldDescriptor::TYPE_INT64:
            case FieldDescriptor::TYPE_SINT64:
            case FieldDescriptor::TYPE_SFIXED64:
                t << f->default_value_int64(); break;
            case FieldDescriptor::TYPE_UINT64:
            case FieldDescriptor::TYPE_FIXED64:
                t << f->default_value_uint64(); break;
            case FieldDescriptor::TYPE_INT32:
            case FieldDescriptor::TYPE_SINT32:
            case FieldDescriptor::TYPE_SFIXED32:
                t << f->default_value_int32(); break;
            case FieldDescriptor::TYPE_UINT32:
            case FieldDescriptor::TYPE_FIXED32:
                t << f->default_value_uint32(); break;
            case FieldDescriptor::TYPE_BOOL:
                t << (f->default_value_bool()? "true" : "false");
                break;
            case FieldDescriptor::TYPE_STRING:
            case FieldDescriptor::TYPE_BYTES:
                // XXX, TODO: escape
                t << '"' << f->default_value_string() << '"';
                break;
            case FieldDescriptor::TYPE_ENUM:
                {
                    const EnumValueDescriptor *v = f->default_value_enum();
                    string res = ".default." + v->name();
                    return res;
                }
            default:
                assert(!"unknown field type");
        }
        return ".default " + t.str();
    }
    else // XXX: assign implicit default value depending on fields type
    {
        /*
        string res;
        switch (f->type())
        {
            case FieldDescriptor::TYPE_DOUBLE:
            case FieldDescriptor::TYPE_FLOAT:
            case FieldDescriptor::TYPE_INT64:
            case FieldDescriptor::TYPE_SINT64:
            case FieldDescriptor::TYPE_SFIXED64:
            case FieldDescriptor::TYPE_UINT64:
            case FieldDescriptor::TYPE_FIXED64:
            case FieldDescriptor::TYPE_INT32:
            case FieldDescriptor::TYPE_SINT32:
            case FieldDescriptor::TYPE_SFIXED32:
            case FieldDescriptor::TYPE_UINT32:
            case FieldDescriptor::TYPE_FIXED32:
                res = "0"; break;
            case FieldDescriptor::TYPE_BOOL:
                res = "false"; break;
            case FieldDescriptor::TYPE_STRING:
            case FieldDescriptor::TYPE_BYTES: // XXX
                res = "\"\""; break;
            case FieldDescriptor::TYPE_ENUM:
                {
                    const EnumDescriptor *e = f->enum_type();
                    const EnumValueDescriptor *v = e->value(0);
                    res += ".default." + v->name();
                    return res;
                }
            default:
                return res;
        }
        return ".default " + res;
        */
        return "";
    }
}


static
void dump_field(const FieldDescriptor *f, string &accu)
{
    const string &name = f->name();

    string type; // typename
    switch (f->type())
    {
        case FieldDescriptor::TYPE_DOUBLE:
            type = "float64"; break;
        case FieldDescriptor::TYPE_FLOAT:
            type = "float32"; break;
        case FieldDescriptor::TYPE_INT64:
            type = "protobuf-int64"; break;
        case FieldDescriptor::TYPE_UINT64:
            type = "uint64"; break;
        case FieldDescriptor::TYPE_FIXED64:
            type = "uint64-fixed"; break;
        case FieldDescriptor::TYPE_SFIXED64:
            type = "int64-fixed"; break;
        case FieldDescriptor::TYPE_SINT64:
            type = "int64"; break;
        case FieldDescriptor::TYPE_INT32:
            type = "protobuf-int32"; break;
        case FieldDescriptor::TYPE_UINT32:
            type = "uint32"; break;
        case FieldDescriptor::TYPE_FIXED32:
            type = "unt32-fixed"; break;
        case FieldDescriptor::TYPE_SFIXED32:
            type = "int32-fixed"; break;
        case FieldDescriptor::TYPE_SINT32:
            type = "int32"; break;
        case FieldDescriptor::TYPE_BOOL:
            type = "bool"; break;
        case FieldDescriptor::TYPE_STRING:
            type = "string"; break;
        case FieldDescriptor::TYPE_BYTES:
            type = "binary"; break;
        case FieldDescriptor::TYPE_MESSAGE:
            {
                const Descriptor *r = f->message_type();
                type = make_record_name(r);
                break;
            }
        case FieldDescriptor::TYPE_ENUM:
            {
                const EnumDescriptor *e = f->enum_type();
                type = make_enum_name(e);
                break;
            }
        case FieldDescriptor::TYPE_GROUP:
            assert(!"unsupported type: group");
        default:
            assert(!"unknown field type");
    }

    string label;
    switch (f->label())
    {
        case FieldDescriptor::LABEL_REQUIRED:
            label = "";
            break;
        case FieldDescriptor::LABEL_OPTIONAL:
            label = " .optional " + gen_default_value(f);
            break;
        case FieldDescriptor::LABEL_REPEATED:
            label = " .repeated";
            break;
        default:
            assert(!"unknown field label");
    }

    // obtain string representation of the tag (i.e. user-assigned field number)
    // C++ style
    ostringstream t; t << f->number();
    string code = t.str();

    accu +=
        " .field [" /* field elements */
            " .name " + name +
            " .type " + type +
            label +
            " .code " + code +
        "]"; /* fields elements */
}


// forward declaration
static
void dump_field_type(const FieldDescriptor *f, string &accu, seen_set &seen);


static
void dump_record(const Descriptor *r, string &accu, seen_set &seen)
{
    if (r->file() != current_fd) return; // skip imported definition
    if (check_update_seen(r, seen)) return; // already processed

    int i;
    const string name = make_record_name(r);

    // generate the record definition
    accu +=
        ".record [" /* record elements */
            " .name " + name;

    for (i = 0; i < r->field_count(); i++)
        dump_field(r->field(i), accu);

    accu += "]\n"; /* record elements */

    // generate nested definitions
    for (i = 0; i < r->nested_type_count(); i++)
        dump_record(r->nested_type(i), accu, seen);

    // dump field type declarations for previously unseen types
    // XXX: dump field types before record type to optimize further processing?
    for (i = 0; i < r->field_count(); i++)
        dump_field_type(r->field(i), accu, seen);
}


static
void dump_enum_value(const EnumValueDescriptor *v, string &accu)
{
    const string &name = v->name();

    // obtain string representation of enum constant value
    // C++ style
    ostringstream t; t << v->number();
    string value = t.str();

    accu +=
        " .option [" /* enum constant value */
            " .name " + name +
            " .code " + value +
        "]"; /* enum constant value */
}


static
void dump_enum(const EnumDescriptor *e, string &accu, seen_set &seen)
{
    if (e->file() != current_fd) return; // skip imported definition
    if (check_update_seen(e, seen)) return; // already processed

    string name = make_enum_name(e);

    accu +=
        ".enum [" /* enum elements */
            " .name " + name;

    int i;
    for (i = 0; i < e->value_count(); i++)
        dump_enum_value(e->value(i), accu);

    accu += "]\n"; /* enum elements */
}


static
void dump_field_type(const FieldDescriptor *f, string &accu, seen_set &seen)
{
    switch (f->type())
    {
        case FieldDescriptor::TYPE_MESSAGE:
            dump_record(f->message_type(), accu, seen);
            break;
        case FieldDescriptor::TYPE_ENUM:
            dump_enum(f->enum_type(), accu, seen);
            break;
        default: ;
    }
}


void dump_descriptor(const Descriptor *d)
{
    string accu; // result accumulator
    seen_set seen; // set for seen record and enum descriptors

    dump_record(d, accu, seen);
    cout << accu;
}


// globally seen descriptors, never mind deallocation
static seen_set seen;


void dump_file_descriptor(const FileDescriptor *fd)
{
    int i;
    current_fd = (FileDescriptor *)fd;

    // dump enums
    for (i = 0; i < fd->enum_type_count(); i++)
    {
        string accu; // result accumulator
        dump_enum(fd->enum_type(i), accu, seen);
        cout << accu;
    }

    // dump messages
    for (i = 0; i < fd->message_type_count(); i++)
    {
        string accu; // result accumulator
        dump_record(fd->message_type(i), accu, seen);
        cout << accu;
    }
}


} // namespace piqi
