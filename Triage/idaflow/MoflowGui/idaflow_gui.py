# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'idaflow_gui.ui'
#
# Created: Sun Jan 06 01:53:11 2013
#      by: pyside-uic 0.2.14 running on PySide 1.1.2
#
# WARNING! All changes made in this file will be lost!

from PySide import QtCore, QtGui

class Ui_Form(object):
    def setupUi(self, Form):
        self.BIL_plainTextEdit = QtGui.QPlainTextEdit(Form)
        self.BIL_plainTextEdit.setGeometry(QtCore.QRect(387, 9, 541, 361))
        self.BIL_plainTextEdit.setObjectName("BIL_plainTextEdit")
        self.taintedBlocks_listWidget = QtGui.QListWidget(Form)
        self.taintedBlocks_listWidget.setGeometry(QtCore.QRect(135, 73, 120, 298))
        self.taintedBlocks_listWidget.setMaximumSize(QtCore.QSize(120, 16777215))
        self.taintedBlocks_listWidget.setObjectName("taintedBlocks_listWidget")
        self.taintedInstructions_listWidget = QtGui.QListWidget(Form)
        self.taintedInstructions_listWidget.setGeometry(QtCore.QRect(261, 73, 120, 298))
        self.taintedInstructions_listWidget.setMaximumSize(QtCore.QSize(120, 16777215))
        self.taintedInstructions_listWidget.setObjectName("taintedInstructions_listWidget")
        self.label_3 = QtGui.QLabel(Form)
        self.label_3.setGeometry(QtCore.QRect(261, 54, 95, 16))
        self.label_3.setObjectName("label_3")
        self.label_2 = QtGui.QLabel(Form)
        self.label_2.setGeometry(QtCore.QRect(9, 54, 84, 16))
        self.label_2.setObjectName("label_2")
        self.label_4 = QtGui.QLabel(Form)
        self.label_4.setGeometry(QtCore.QRect(135, 54, 67, 16))
        self.label_4.setObjectName("label_4")
        self.label = QtGui.QLabel(Form)
        self.label.setGeometry(QtCore.QRect(9, 9, 44, 16))
        self.label.setObjectName("label")
        self.BapTrace_lineEdit = QtGui.QLineEdit(Form)
        self.BapTrace_lineEdit.setGeometry(QtCore.QRect(9, 28, 371, 20))
        self.BapTrace_lineEdit.setObjectName("BapTrace_lineEdit")
        self.taintedFuncs_listWidget = QtGui.QListWidget(Form)
        self.taintedFuncs_listWidget.setGeometry(QtCore.QRect(9, 73, 120, 298))
        self.taintedFuncs_listWidget.setMaximumSize(QtCore.QSize(120, 16777215))
        self.taintedFuncs_listWidget.setObjectName("taintedFuncs_listWidget")

        self.retranslateUi(Form)
        QtCore.QMetaObject.connectSlotsByName(Form)

    def retranslateUi(self, Form):
        Form.setWindowTitle(QtGui.QApplication.translate("Form", "Form", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("Form", "Tainted Instructions", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("Form", "Tainted Functions", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("Form", "Tainted Blocks", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("Form", "BapTrace", None, QtGui.QApplication.UnicodeUTF8))
        self.BapTrace_lineEdit.setText(QtGui.QApplication.translate("Form", "Click to open...", None, QtGui.QApplication.UnicodeUTF8))

