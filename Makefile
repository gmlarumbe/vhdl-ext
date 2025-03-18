######################################################
#
#  Makefile for test-hdl with vhdl-ext
#
#  Copyright (c) 2022-2025 Gonzalo Larumbe
#  All rights reserved.
# 
######################################################

# Variables
TEST_HDL_PATH = test/test-hdl
ERT_TESTS = $(TEST_HDL_PATH)/ert-tests.sh
PACKAGE = vhdl-ext

include $(TEST_HDL_PATH)/Makefile.mk
