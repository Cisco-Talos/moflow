Follow the directions in install\INSTALL.txt first

Load the included idb file in IDA: demo\QuickTime_20130515105912.idb
NOTE: Do not save the database when you exit unless you want to keep the colors/comments

Launch scripts from the idapython output window using this command:
execfile('c:/idaflow/idaflow.py')

Load the included taint trace using the UI: demo\1f5eed2a2eb4517a0cf1099f0276060a.mov.bapflow
NOTE: This will take ~30sec depending on the machine

Load the included slice using the UI: demo\1f.mov.slice_EDI.bil

Use the taint info / slice info views to navigate the disassembly
NOTE: taint info will take 30-60sec to render at the moment


