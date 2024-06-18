while true; do find . -name *.el | entr -cd eask test ert-runner ./test/*.el; done
