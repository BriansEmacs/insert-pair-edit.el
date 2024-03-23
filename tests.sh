EMACS_ROOT=D:/Apps/Emacs

for version in \
    24.3 \
    25.3 \
    26.3 \
    27.2 \
    28.2 \
    29.2.1
do
    export EMACS_COMMAND=${EMACS_ROOT}/${version}/bin/emacs.exe
    export emacs_dir=

    make test
done
