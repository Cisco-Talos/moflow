#!/usr/bin/env python
import sys
from datetime import datetime

import idc
import idautils
import idaapi
from idaapi import PluginForm
from PySide import QtCore, QtGui
from PySide.QtCore import Qt

idaflow_path = 'c:/moflow/triage/idaflow'
sys.path.append(idaflow_path)

from TraceReader.tracereader import TraceReader

from MoflowGui.idaflow_gui import Ui_Form
#del(sys.modules['Ui_Form'])
del(sys.modules['MoflowGui.idaflow_gui'])
from MoflowGui.idaflow_gui import Ui_Form

#defines
CALL_FAR = 16
CALL_NEAR = 17
JMP_FAR = 18
JMP_NEAR = 19

func_coverage_color  =  0xCCCCCC # light grey
block_coverage_color =  0xB2F0B2 # light green
func_call_color      =  0xA0D8A0 # mild green
#taint_color          =  0x33A0FF # orange
#selected_line_color  =  0x33C0FF # orange-yellow
taint_color          =  0x33C0FF # orange
selected_line_color  =  0x33A0FF # orange-yellow


tainted_addrs = list()
function_addrs = list()

covered_addrs = set()
covered_blocks = set()
covered_functions = set()


class TaintInfoForm(PluginForm):
    def refreshTaintTable(self, taint_frames):
        self.taintTable.setRowCount(len(taint_frames))
        row = 0
        for tainted in taint_frames:
            location_str = "0x%08x" % (tainted.address)
            if idaapi.get_func(tainted.address):
                func_name = idc.GetFunctionName(tainted.address)
                func_addr = idc.LocByName(func_name)
                #table_item = QtGui.QTableWidgetItem("0x%08x:%s+0x%08x" % (tainted.address, func_name, tainted.address - func_addr))
                table_item = QtGui.QTableWidgetItem("0x%08x" % (tainted.address))

                table_item.setFont(QtGui.QFont("Consolas", 10))
                self.taintTable.setItem(row, 0, table_item)
                """
                else:
                    table_item = QtGui.QTableWidgetItem("0x%08x" % (tainted.address))
                    table_item.setFont(QtGui.QFont("Consolas", 10))
                    self.taintTable.setItem(row, 0, table_item)
                """
                disasm_str = idc.GetDisasm(tainted.address)
                if disasm_str != '':
                    #print disasm_str
                    table_item = QtGui.QTableWidgetItem(disasm_str)
                    table_item.setFont(QtGui.QFont("Consolas", 10))
                    self.taintTable.setItem(row, 1, table_item)

                taint_info_str = ''
                for el in tainted.operand_list.elem:
                    taint_str  = ''
                    if el.taint_info.no_taint == False:
                        taint_str = "tainted"
                    op_usage = ''
                    if el.operand_usage.read:
                        op_usage = "read"
                    elif el.operand_usage.written:
                        op_usage += "write"
                    elif el.operand_usage.index:
                        op_usage += "idx"
                    elif el.operand_usage.base:
                        op_usage += "base"
                    if op_usage != '':
                        if el.operand_info_specific.mem_operand.IsInitialized():
                            el_value = ''
                            for byte in el.value:
                                el_value += "%02x " % (ord(byte))
                            #print "    memory: 0x%08x[0:%d] %s %s val: %s" % (el.operand_info_specific.mem_operand.address, el.bit_length, op_usage, taint_str, el_value)
                            taint_info_str += "mem[0x%08x] = 0x%s %s %s\n" % (el.operand_info_specific.mem_operand.address, el_value, taint_str, op_usage)
                        elif el.operand_info_specific.reg_operand.IsInitialized():
                            el_value = ''
                            for byte in el.value:
                                el_value += "%02x" % (ord(byte))
                            #print "  register: %s[0:%d] %s %s val: %s" % (el.operand_info_specific.reg_operand.name, el.bit_length, op_usage, taint_str, el_value[::-1])
                            taint_info_str += "%s = 0x%s %s %s\n" % (el.operand_info_specific.reg_operand.name, el_value[::-1], taint_str, op_usage)

                    table_item = QtGui.QTableWidgetItem(taint_info_str[:len(taint_info_str) - 1])
                    table_item.setFont(QtGui.QFont("Consolas", 10))
                    self.taintTable.setItem(row, 2, table_item)

                    #print ida_comment
                    #idc.MakeRptCmt(tainted.address, str(ida_comment))

                row = row + 1


        self.taintTable.setRowCount(row)
        self.taintTable.currentRow = row - 1
        self.taintTable.resizeColumnsToContents()
        self.taintTable.resizeRowsToContents()



    prev_selected_taint = 0
    def taintTableSelectionChanged(self):
        if len(self.taintTable.selectedItems()) > 0:
            #for item in self.tree.selectedItems():
            #item = self.taintTable.getItem(self.taintTable.selectedRow(), 0)
            #item = self.taintTable.selectedItems()[len(self.taintTable.selectedItems()) - 1]
            item = self.taintTable.selectedItems()[0]
            addr_str = item.text()
            #print "taint table selected %s" % (addr[2:10])

            idc.SetColor(self.prev_selected_taint, idc.CIC_ITEM, taint_color)
            addr = int(addr_str[2:10], 16)
            idc.Jump(addr)
            idc.SetColor(addr, idc.CIC_ITEM, selected_line_color)
            self.prev_selected_taint = addr
            self.taintTable.setFocus()


    def OnCreate(self, form):
        # Get parent widget
        self.parent = self.FormToPySideWidget(form)
        self.PopulateForm()

    def PopulateForm(self):

        #taint table
        self.taintTableLabel = QtGui.QLabel("Taint Information")
        self.taintTable = QtGui.QTableWidget()
        self.taintTable.setColumnCount(3)
        self.taintTable.setHorizontalHeaderLabels(("Location", "Instruction", "Information"))
        self.taintTable.setEditTriggers(QtGui.QTableWidget.NoEditTriggers)
        self.taintTable.setMinimumHeight(300)
        self.taintTable.setSelectionBehavior(QtGui.QTableWidget.SelectRows)


        # Create layout
        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.taintTableLabel)
        layout.addWidget(self.taintTable)
        self.parent.setLayout(layout)

        self.taintTable.connect(self.taintTable, QtCore.SIGNAL('itemSelectionChanged()'), self.taintTableSelectionChanged)

    def OnClose(self, form):
        """
        Called when the plugin form is closed
        """
        pass

    def Show(self):
        return PluginForm.Show(self, "Taint Information", options=PluginForm.FORM_PERSIST)






class TaintSliceInfoForm(PluginForm):

    def refreshTaintSliceTable(self, slice_addrs, slice_annotations):
        self.taintTable.setRowCount(len(slice_addrs))
        row = 0
        annotation = ''
        for tainted_addr in slice_addrs:
            if slice_annotations.has_key(tainted_addr):
                annotation = slice_annotations[tainted_addr]

            if idaapi.get_func(tainted_addr):
                func_name = idc.GetFunctionName(tainted_addr)
                func_addr = idc.LocByName(func_name)
                #table_item = QtGui.QTableWidgetItem("0x%08x:%s+0x%08x" % (tainted.address, func_name, tainted.address - func_addr))
                table_item = QtGui.QTableWidgetItem("0x%08x" % (tainted_addr))

                table_item.setFont(QtGui.QFont("Consolas", 10))
                self.taintTable.setItem(row, 0, table_item)
                """
                else:
                    table_item = QtGui.QTableWidgetItem("0x%08x" % (tainted.address))
                    table_item.setFont(QtGui.QFont("Consolas", 10))
                    self.taintTable.setItem(row, 0, table_item)
                """
                disasm_str = idc.GetDisasm(tainted_addr)
                if disasm_str != '':
                    #print disasm_str
                    table_item = QtGui.QTableWidgetItem(disasm_str)
                    table_item.setFont(QtGui.QFont("Consolas", 10))
                    self.taintTable.setItem(row, 1, table_item)


                    table_item = QtGui.QTableWidgetItem(annotation[1:len(annotation) - 1])
                    table_item.setFont(QtGui.QFont("Consolas", 10))
                    self.taintTable.setItem(row, 2, table_item)

                    #print ida_comment
                    #idc.MakeRptCmt(tainted.address, str(ida_comment))

                row = row + 1


        self.taintTable.setRowCount(row)
        self.taintTable.currentRow = row - 1
        self.taintTable.resizeColumnsToContents()
        self.taintTable.resizeRowsToContents()



    prev_selected_taint = 0
    def taintTableSelectionChanged(self):
        if len(self.taintTable.selectedItems()) > 0:
            #for item in self.tree.selectedItems():
            #item = self.taintTable.selectedItems()[len(self.taintTable.selectedItems()) - 1]
            item = self.taintTable.selectedItems()[0]
            #item = self.taintTable.item(self.taintTable.selectedRow(), 0)
            addr_str = item.text()
            #print "taint table selected %s" % (addr[2:10])

            idc.SetColor(self.prev_selected_taint, idc.CIC_ITEM, taint_color)
            addr = int(addr_str[2:10], 16)
            idc.Jump(addr)
            idc.SetColor(addr, idc.CIC_ITEM, selected_line_color)
            self.prev_selected_taint = addr
            self.taintTable.setFocus()


    def OnCreate(self, form):
        # Get parent widget
        self.parent = self.FormToPySideWidget(form)
        self.PopulateForm()

    def PopulateForm(self):

        #taint table
        self.taintTableLabel = QtGui.QLabel("Taint Information")
        self.taintTable = QtGui.QTableWidget()
        self.taintTable.setColumnCount(3)
        self.taintTable.setHorizontalHeaderLabels(("Location", "Instruction", "Information"))
        self.taintTable.setEditTriggers(QtGui.QTableWidget.NoEditTriggers)
        self.taintTable.setMinimumHeight(300)
        self.taintTable.setSelectionBehavior(QtGui.QTableWidget.SelectRows)

        # Create layout
        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.taintTableLabel)
        layout.addWidget(self.taintTable)
        self.parent.setLayout(layout)

        self.taintTable.connect(self.taintTable, QtCore.SIGNAL('itemSelectionChanged()'), self.taintTableSelectionChanged)

    def OnClose(self, form):
        """
        Called when the plugin form is closed
        """
        pass

    def Show(self):
        return PluginForm.Show(self, "Slice Information", options=PluginForm.FORM_PERSIST)



class PathFinderGraph(idaapi.GraphViewer):
    def __init__(self, title, addrs):
        idaapi.GraphViewer.__init__(self, title, addrs)
        self.ids = {}
        self.nodes = {}
        self.history = []
        self.includes = []
        self.excludes = []
        self.edge_nodes = []
        self.delete_on_click = False
        self.include_on_click = False

        self.graph_addrs = addrs

    def OnRefresh(self):
        self.Clear()
        prev = 'START'
        prev_node = self.AddNode(prev)
        node = None
        node_hash = {}
        #print "Layout %d nodes\n" % (len(function_addrs))
        offset = 0
        if len(self.graph_addrs) > 200:
            print("    Truncating graph layout to 200 most recent references")
            offset = len(self.graph_addrs) - 200

        node_count = 0
        for addr in self.graph_addrs[offset:]:
            if addr != prev:
            #print "%x -> %x" % (prev, fname)
                if node_hash.has_key(addr):
                    node = node_hash[addr]
                else:
                    if idaapi.get_func(addr):
                        func_name = idc.GetFunctionName(addr)
                        node = self.AddNode(func_name)
                        node_hash[addr] = node
                        node_count += 1
                self.AddEdge(prev_node, node)
                prev = addr
                prev_node = node
        end_node = self.AddNode('END')
        self.AddEdge(prev_node, end_node)
        return True

        """
        self.ids = {}
        self.nodes = {}

        for path in self.results:
            nogo = False

            for include in self.includes:
                if include not in path:
                    nogo = True

            for exclude in self.excludes:
                if exclude in path:
                    nogo = True
                    break

            if not nogo:
                prev_func = None

                for func in path:
                    if not self.ids.has_key(func):
                        self.ids[func] = self.AddNode(func)
                        self.nodes[self.ids[func]] = func
                    if prev_func is not None:
                        self.AddEdge(prev_func, self.ids[func])
                    prev_func = self.ids[func]

                try:
                    self.edge_nodes.append(path[-2])
                except:
                    pass
        """

    def OnGetText(self, node_id):
        node = str(self[node_id])
        if node in self.edge_nodes:
            return (node, 0xff00f0)
        return node

    def OnCommand(self, cmd_id):
        if self.cmd_undo == cmd_id:
            self._undo()
        elif self.cmd_include == cmd_id:
            self.include_on_click = True
        elif self.cmd_delete == cmd_id:
            self.delete_on_click = True
        elif self.cmd_reset == cmd_id:
            self._reset()

    def OnDblClick(self, node_id):
        idc.Jump(idc.LocByName(self.nodes[node_id]))

    def OnClick(self, node_id):
        if self.delete_on_click:
            self.delete_on_click = False
            self.excludes.append(self.nodes[node_id])
            self.history.append('exclude')
        elif self.include_on_click:
            self.include_on_click = False
            self.includes.append(self.nodes[node_id])
            self.history.append('include')
        self.Refresh()

    def Show(self):
        if not idaapi.GraphViewer.Show(self):
            return False
        else:
            self.cmd_undo = self.AddCommand("Undo", "U")
            self.cmd_reset = self.AddCommand("Reset graph", "R")
            self.cmd_delete = self.AddCommand("Exclude node", "X")
            self.cmd_include = self.AddCommand("Include node", "I")
            return True

    def _undo(self):
        self.delete_on_click = False
        self.include_on_click = False

        if self.history:
            last_action = self.history.pop(-1)
        else:
            last_action = None

        if last_action == 'include' and self.includes:
            self.includes.pop(-1)
        elif last_action == 'exclude' and self.excludes:
            self.excludes.pop(-1)

        self.Refresh()

    def _reset(self):
        self.history = []
        self.includes = []
        self.excludes = []
        self.delete_on_click = False
        self.include_on_click = False
        self.Refresh()




class MoflowMainForm(PluginForm):
    def imports_names_cb(self, ea, name, ord):
        self.items.append((ea, '' if not name else name, ord))
        # True -> Continue enumeration
        return True


    def BuildImports(self):
        tree = {}
        nimps = idaapi.get_import_module_qty()

        for i in xrange(0, nimps):
            name = idaapi.get_import_module_name(i)
            if not name:
                continue
            # Create a list for imported names
            self.items = []

            # Enum imported entries in this module
            idaapi.enum_import_names(i, self.imports_names_cb)

            if name not in tree:
                tree[name] = []
            tree[name].extend(self.items)

        return tree


    def BuildExports(self):
        return list(idautils.Entries())


    def PopulateTree(self):
        # Clear previous items
        self.tree.clear()

        # Build imports
        root = QtGui.QTreeWidgetItem(self.tree)
        root.setText(0, "Imports")

        for dll_name, imp_entries in self.BuildImports().items():
            imp_dll = QtGui.QTreeWidgetItem(root)
            imp_dll.setText(0, dll_name)

            for imp_ea, imp_name, imp_ord in imp_entries:
                item = QtGui.QTreeWidgetItem(imp_dll)
                item.setText(0, "%s [0x%08x]" %(imp_name, imp_ea))


        # Build exports
        root = QtGui.QTreeWidgetItem(self.tree)
        root.setText(0, "Exports")

        for exp_i, exp_ord, exp_ea, exp_name in self.BuildExports():
            item = QtGui.QTreeWidgetItem(root)
            item.setText(0, "%s [#%d] [0x%08x]" % (exp_name, exp_ord, exp_ea))


    def openTaintInfo(self):
        self.moflowOutput.append("Rendering taint information table..")
        start_time = datetime.now()

        if self.taintInfo == None:
            self.taintInfo = TaintInfoForm()

        self.taintInfo.Show()
        if hasattr(self, 'reader'):
            self.taintInfo.refreshTaintTable(self.reader.taint_frames)

        elapsed_time = datetime.now() - start_time
        self.moflowOutput.append("Taint information table rendered in %s\n" % str(elapsed_time))


    taintInfo = None
    taintGraph = None
    def openTaintGraph(self):
        self.moflowOutput.append("Creating taint graph from %d data references..\n" % (len(function_addrs) - 1))

        if self.taintGraph == None:
            self.taintGraph = PathFinderGraph("Tainted Graph", function_addrs)
        else:
            self.taintGraph.OnRefresh()
        self.taintGraph.Show()

        #self.graphViewer = GraphViewerForm()
        #self.graphViewer.Show("GraphViewer")

    taintSliceGraph = None
    def openTaintSliceGraph(self):
        addrs = self.slice_functions
        self.moflowOutput.append("Creating slice graph from %d data references..\n" % (len(addrs) - 1))
        if self.taintSliceGraph == None:
            self.taintSliceGraph = PathFinderGraph("Slice Graph", addrs)
        else:
            self.taintSliceGraph.OnRefresh()
        self.taintSliceGraph.Show()


    slice_addrs = []
    slice_comments = {}
    slice_functions = []

    def openTaintSlice(self):
        bil_filename = idc.AskFile(0, "*.bil", "Open BAP IL")
        if bil_filename:
            f = open(bil_filename, "r")
            comment = ""
            addr = 0
            prev_addr = 0
            taint_addr_string = ''
            record_call = False
            record_branch = False
            callgraph = dict()
            cfg = dict()

            for line in f:
                if line[:4] == 'addr':
                    addr = int(line[5:15].split(" ")[0], 16)

                    # write prev comment at last addr
                    if comment != "":
                        idc.MakeRptCmt(prev_addr, comment)
                        #print "%08x : %s" % (prev_addr, comment)
                        self.slice_comments[prev_addr] = comment

                    # handle comment
                    comment = "\n"
                    ctx_loc = line.find('@context')
                    if ctx_loc > 0:
                        comment += "  " + line[ctx_loc:]

                    # handle taint
                    if idaapi.get_func(addr):
                        func_name = idc.GetFunctionName(addr)
                        func_addr = idc.LocByName(func_name)
                        taint_addr_string += "TAINT: %X %s+%X\n" % (addr, func_name, addr - func_addr)
                        tainted_addrs.append(addr)
                        covered_addrs.add(addr)

                    #handle control flow
                    if record_call == True:
                        if prev_addr not in callgraph:
                            callgraph[prev_addr] = set()
                        callgraph[prev_addr].add(addr)
                        record_call = False

                    if record_branch == True:
                        if prev_addr not in cfg:
                            cfg[prev_addr] = set()
                        cfg[prev_addr].add(addr)
                        record_branch = False

                    if line.find('asm "call') or line.find('asm "ret'):
                        record_call = True

                    if line.find('asm "jmp    *'):
                        record_branch = True

                    prev_addr = addr

                elif line[:7] == 'special':
                    self.moflowOutput.append("    BIL Info: %s" % line.rstrip())
                else:
                    comment += line

            if comment != "":
                idc.MakeRptCmt(addr, comment)

            #print taint_addr_string
        #for addr in covered_addrs:
        #   print "%X %s" % (addr, idc.GetFunctionName(addr))

        #print str(len(covered_addrs)) + " covered blocks"
        self.moflowOutput.append("    Loaded %d tainted instructions from slice\n" % len(tainted_addrs))

        #for src, xrefs in callgraph.items():
          #  if idaapi.get_func(src):
         #       for dst in xrefs:
        #            print "CALL %X -> %X" % (src, dst)
                    #idc.AddCodeXref(src, dst, CALL_FAR)

        #for src, xrefs in cfg.items():
        #    if idaapi.get_func(src):
        #        for dst in xrefs:
        #            print "JMP %X -> %X" % (src, dst)
        #            #idc.AddCodeXref(src, dst, JMP_FAR)

        # get covered functions

        slice_functions = list()
        slice_coverage = set()
        slice_addrs = tainted_addrs

        prev_addr = 0
        prev_func = 0
        for addr in slice_addrs:
            if addr == prev_addr:
                continue

            func = idaapi.get_func(addr)
            if func and func.startEA != prev_func:
                slice_functions.append(func.startEA)
                slice_coverage.add(func.startEA)
                #print "%x" % (func.startEA)
                prev_func = func.startEA

            prev_addr = addr

        self.slice_functions = slice_functions
        self.slice_addrs = tainted_addrs

        """
        # clear function coloring
        for addr in slice_coverage:
            func = idaapi.get_func(addr)
            for head in idautils.Heads(func.startEA, func.endEA):
                idc.SetColor(head, idc.CIC_ITEM, 0xFFFFFF)

        # color functions
        for addr in slice_coverage:
            func = idaapi.get_func(addr)
            #print "FUNC: %X %s" % (func.startEA, idc.GetFunctionName(func.startEA))
            for head in idautils.Heads(func.startEA, func.endEA):
                # coverage
                idc.SetColor(head, idc.CIC_ITEM, func_coverage_color)

        # get covered blocks
        for addr in covered_addrs:
            func = idaapi.get_func(addr)
            if func:
                for block in idaapi.FlowChart(func):
                    if addr >= block.startEA and addr <= block.endEA:
                        covered_blocks.add(block)
                        break;

        # color blocks
        for block in covered_blocks:
            #print "BLOCK: %X" % block.startEA
            for head in idautils.Heads(block.startEA, block.endEA):
                idc.SetColor(head, idc.CIC_ITEM, block_coverage_color)
                # calls
                for xref in idautils.XrefsFrom(head):
                    if xref.type == CALL_NEAR or xref.type == CALL_FAR:
                        idc.SetColor(head, idc.CIC_ITEM, func_call_color)
        # taint
        for addr in tainted_addrs:
            idc.SetColor(addr, idc.CIC_ITEM, taint_color)
            #if idaapi.get_func(addr):
                #print "TAINT: %X %s" % (addr, idc.GetFunctionName(addr))
        """
        #self.openTaintSliceGraph()
        self.slicePath.setText(bil_filename)


    taintSliceInfo = None
    def openTaintSliceInformation(self):
        self.moflowOutput.append("Loading Taint Slice information..")
        if self.taintSliceInfo == None:
            self.taintSliceInfo = TaintSliceInfoForm()

        self.taintSliceInfo.Show()
        if len(self.slice_addrs) > 0:
            self.taintSliceInfo.refreshTaintSliceTable(self.slice_addrs, self.slice_comments)



    def ClearFunctionColoring(self, addrs):
        for addr in addrs:
            func = idaapi.get_func(addr)
            for head in idautils.Heads(func.startEA, func.endEA):
                idc.SetColor(head, idc.CIC_ITEM, 0xFFFFFF)


    def openTraceDialog(self):
        trace_path, _ = QtGui.QFileDialog.getOpenFileName(None, "Open Taint Trace", idaflow_path, "*.bapflow")
        self.moflowOutput.append("Opening trace: %s\n" % trace_path)
        start_time = datetime.now()
        self.reader = TraceReader(trace_path)
        stop_time = datetime.now() - start_time
        self.moflowOutput.append("Trace reader indexed %d frames in %s" % (self.reader.frame_count, str(stop_time)))
        self.moflowOutput.append("    %d tainted frames\n    %d modules loaded\n    %d exceptions\n" % \
                                    (len(self.reader.taint_addrs), len(self.reader.module_frames), len(self.reader.exception_frames)))

        # update trace path gui textbox - FIX with context
        self.filePath.setText(trace_path)

        #
        # analyze instructions for containing functions & blocks
        tainted_addrs = self.reader.taint_addrs
        covered_addrs = set(tainted_addrs)

        # get covered functions
        prev_addr = 0
        for addr in covered_addrs:
            func = idaapi.get_func(addr)
            if func and func.startEA != prev_addr:
                function_addrs.append(func.startEA)
                covered_functions.add(func.startEA)
                prev_addr = func.startEA

        # get covered blocks
        for addr in covered_addrs:
            func = idaapi.get_func(addr)
            if func:
                for block in idaapi.FlowChart(func):
                    if addr >= block.startEA and addr <= block.endEA:
                        covered_blocks.add(block)
                        break;


        # clear function coloring
        self.ClearFunctionColoring(covered_functions)

        # color tainted functions
        for addr in covered_functions:
            func = idaapi.get_func(addr)
            for head in idautils.Heads(func.startEA, func.endEA):
                idc.SetColor(head, idc.CIC_ITEM, func_coverage_color)

        # color blocks
        for block in covered_blocks:
            #print "BLOCK: %X" % block.startEA
            for head in idautils.Heads(block.startEA, block.endEA):
                idc.SetColor(head, idc.CIC_ITEM, block_coverage_color)
                # calls
                for xref in idautils.XrefsFrom(head):
                    if xref.type == CALL_NEAR or xref.type == CALL_FAR:
                        idc.SetColor(head, idc.CIC_ITEM, func_call_color)

        # color instructions
        for addr in covered_addrs:
            idc.SetColor(addr, idc.CIC_ITEM, taint_color)


        # update exception list
        e = self.reader.exception_frames[len(self.reader.exception_frames) - 1]
        self.moflowOutput.append("Jumping to last exception event..\nException 0x%08x in thread %d from 0x%08x to 0x%08x\n" % (e.exception_number, e.thread_id, e.from_addr, e.to_addr))
        self.exceptionTable.setRowCount(1)
        table_item = QtGui.QTableWidgetItem("%d" % (e.thread_id))
        table_item.setFont(QtGui.QFont("Consolas", 10))
        self.exceptionTable.setItem(0, 0, table_item)
        table_item = QtGui.QTableWidgetItem("0x%08x" % (e.exception_number))
        table_item.setFont(QtGui.QFont("Consolas", 10))
        self.exceptionTable.setItem(0, 1, table_item)
        table_item = QtGui.QTableWidgetItem("0x%08x" % (e.from_addr))
        table_item.setFont(QtGui.QFont("Consolas", 10))
        self.exceptionTable.setItem(0, 2, table_item)
        table_item = QtGui.QTableWidgetItem("0x%08x" % (e.to_addr))
        table_item.setFont(QtGui.QFont("Consolas", 10))
        self.exceptionTable.setItem(0, 3, table_item)
        idc.Jump(e.from_addr)

        # module table
        self.moduleTable.setRowCount(len(self.reader.module_frames))
        row = 0
        for frame in self.reader.module_frames:
            name = frame.module_name
            low = frame.low_address
            high = frame.high_address
            #print name, low, high
            table_item = QtGui.QTableWidgetItem(name)
            table_item.setFont(QtGui.QFont("Consolas", 10))
            self.moduleTable.setItem(row, 0, table_item)
            table_item = QtGui.QTableWidgetItem("%08x" % (low))
            table_item.setFont(QtGui.QFont("Consolas", 10))
            self.moduleTable.setItem(row, 1, table_item)
            table_item = QtGui.QTableWidgetItem("%08x" % (high))
            table_item.setFont(QtGui.QFont("Consolas", 10))
            self.moduleTable.setItem(row, 2, table_item)
            row = row + 1

        self.moduleTable.sortItems(1, Qt.AscendingOrder)
        self.moduleTable.resizeColumnsToContents()


        # taint table
        """
        self.openTaintInfo()
        if hasattr(self, 'reader'):
            print "refresh"
            self.taintInfo.refreshTaintTable(self.reader.taint_frames)
        """
        """
        # add IDA comments
        last_func_addr = 0
        for tainted in self.reader.taint_frames:
            # get covered functions
            func = idaapi.get_func(tainted.address)
            #if func:
            #    covered_functions.add(func.startEA)
            #if func.startEA != last_func_addr:


            ida_comment = ''
            for el in tainted.operand_list.elem:
                taint_str  = ''
                if el.taint_info.no_taint == False:
                    taint_str = "tainted"
                op_usage = ''
                if el.operand_usage.read:
                    op_usage = " read"
                elif el.operand_usage.written:
                    op_usage += " write"
                elif el.operand_usage.index:
                    op_usage += " index"
                elif el.operand_usage.base:
                    op_usage += " base"
                if op_usage != '':
                    if el.operand_info_specific.mem_operand.IsInitialized():
                        el_value = ''
                        for byte in el.value:
                            el_value += "%02x " % (ord(byte))
                        #print "    memory: 0x%08x[0:%d] %s %s val: %s" % (el.operand_info_specific.mem_operand.address, el.bit_length, op_usage, taint_str, el_value)
                        ida_comment += "mem[0x%08x]:u%d = 0x%s %s %s\n" % (el.operand_info_specific.mem_operand.address, el.bit_length, el_value, op_usage, taint_str)
                    elif el.operand_info_specific.reg_operand.IsInitialized():
                        el_value = ''
                        for byte in el.value:
                            el_value += "%02x" % (ord(byte))
                        #print "  register: %s[0:%d] %s %s val: %s" % (el.operand_info_specific.reg_operand.name, el.bit_length, op_usage, taint_str, el_value[::-1])
                        ida_comment += "%s:u%d = 0x%s %s %s\n" % (el.operand_info_specific.reg_operand.name, el.bit_length, el_value[::-1], op_usage, taint_str)

                #print ida_comment
                idc.MakeRptCmt(tainted.address, str(ida_comment))
        """


    def OnCreate(self, Form):
        self.parent = self.FormToPySideWidget(Form)

        self.filePath = QtGui.QLineEdit()
        self.filePath.setReadOnly(True)

        self.slicePath = QtGui.QLineEdit()
        self.slicePath.setReadOnly(True)

        self.fileOpenButton = QtGui.QPushButton("Open Trace...")
        self.viewGraphButton = QtGui.QPushButton("Taint Graph")
        self.viewTaintInfoButton = QtGui.QPushButton("Taint Information")

        self.viewTaintSliceButton = QtGui.QPushButton("Open Slice...")
        self.viewSliceGraphButton = QtGui.QPushButton("Slice Graph")
        self.viewSliceInfoButton = QtGui.QPushButton("Slice Information")

        self.moflowOutput = QtGui.QTextBrowser()
        self.moflowOutput.append("Moflow IDA Plugin v0.1\nRichard Johnson\nrjohnson@sourcefire.com\n")

        self.exceptionTableLabel = QtGui.QLabel("Exceptions")
        self.exceptionTable = QtGui.QTableWidget()
        self.exceptionTable.setColumnCount(4)
        self.exceptionTable.setHorizontalHeaderLabels(("Thread", "Exception #", "Source", "Destination"))
        self.exceptionTable.setEditTriggers(QtGui.QTableWidget.NoEditTriggers)
        self.exceptionTable.setSelectionBehavior(QtGui.QTableWidget.SelectRows)

        self.moduleTableLabel = QtGui.QLabel("Modules")
        self.moduleTable = QtGui.QTableWidget()
        self.moduleTable.setColumnCount(3)
        self.moduleTable.setHorizontalHeaderLabels(("Path", "Start", "End"))
        self.moduleTable.setSortingEnabled(True)
        self.moduleTable.setEditTriggers(QtGui.QTableWidget.NoEditTriggers)
        self.moduleTable.setSelectionBehavior(QtGui.QTableWidget.SelectRows)

        # Create layout
        layout = QtGui.QVBoxLayout()

        traceslayout = QtGui.QVBoxLayout()
        fileLayout = QtGui.QHBoxLayout()
        fileLayout.addWidget(self.fileOpenButton)
        fileLayout.addWidget(self.filePath)
        fileLayout.addWidget(self.viewGraphButton)
        fileLayout.addWidget(self.viewTaintInfoButton)
        fileLayout.addWidget(QtGui.QLabel("                 Moflow IDA Plugin v0.1    "))

        sliceLayout = QtGui.QHBoxLayout()
        sliceLayout.addWidget(self.viewTaintSliceButton)
        sliceLayout.addWidget(self.slicePath)
        sliceLayout.addWidget(self.viewSliceGraphButton)
        sliceLayout.addWidget(self.viewSliceInfoButton)
        sliceLayout.addWidget(QtGui.QLabel("                 rjohnson@sourcefire.com"))

        layout.addLayout(fileLayout)
        layout.addLayout(sliceLayout)
        #layout.addWidget(self.filePath)
        #layout.addWidget(self.fileOpenButton)

        info_layout = QtGui.QVBoxLayout()
        info_layout_horiz = QtGui.QHBoxLayout()

        info_layout_vert = QtGui.QVBoxLayout()
        info_layout_vert.addWidget(self.exceptionTableLabel)
        info_layout_vert.addWidget(self.exceptionTable)

        info_layout_vert.addWidget(self.moduleTableLabel)
        info_layout_vert.addWidget(self.moduleTable)

        #splitter = QtGui.QSplitter(Qt.Horizontal)
        #splitter.addWidget(self.moflowOutput)
        #splitter.addLayout(info_layout_vert)
        #info_layout_horiz.addWidget(splitter)
        info_layout_horiz.addWidget(self.moflowOutput)
        info_layout_horiz.addLayout(info_layout_vert)
        layout.addLayout(info_layout_horiz)
        #layout.addWidget(self.tree)
        #self.PopulateTree()
        # Populate PluginForm
        self.parent.setLayout(layout)

        self.fileOpenButton.connect(self.fileOpenButton, QtCore.SIGNAL('clicked()'), self.openTraceDialog)
        self.viewGraphButton.connect(self.viewGraphButton, QtCore.SIGNAL('clicked()'), self.openTaintGraph)
        self.viewTaintInfoButton.connect(self.viewTaintInfoButton, QtCore.SIGNAL('clicked()'), self.openTaintInfo)
        self.viewTaintSliceButton.connect(self.viewTaintSliceButton, QtCore.SIGNAL('clicked()'), self.openTaintSlice)
        self.viewSliceGraphButton.connect(self.viewSliceGraphButton, QtCore.SIGNAL('clicked()'), self.openTaintSliceGraph)
        self.viewSliceInfoButton.connect(self.viewSliceInfoButton, QtCore.SIGNAL('clicked()'), self.openTaintSliceInformation)
        #self.tree.connect(self.tree, QtCore.SIGNAL('itemSelectionChanged()'), self.taintTreeSelectionChanged)

    def OnClose(self, form):
        del(sys.modules['TraceReader'])
        del(sys.modules['TraceReader.tracereader'])

        global MoflowMain
        del MoflowMain


    def Show(self):
        return PluginForm.Show(self, "Moflow IDA Plugin", options=PluginForm.FORM_PERSIST)


if __name__ == "__main__":
    global MoflowMain

    try:
        MoflowMain
    except:
        MoflowMain = MoflowMainForm()

    MoflowMain.Show()
