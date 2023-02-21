ERT_TESTS=test/scripts/ert-tests.sh

all: test

test: test_setup test_run

test_setup:
	$(ERT_TESTS) recompile

test_run:
	$(ERT_TESTS) run_tests

test_package_el: test_setup_pkg_el test_run_pkg_el

test_setup_pkg_el:
	$(ERT_TESTS) recompile pkg_el

test_run_pkg_el:
	$(ERT_TESTS) run_tests t pkg_el
