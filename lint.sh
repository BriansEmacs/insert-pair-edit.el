        EMACS_DIR=d:/Apps/Emacs/29.1/bin
            EMACS=${EMACS_DIR}/emacs.exe
      PACKAGE_DIR=`cygpath -ma ~/.emacs.d/packages/elpa`

         DASH_LIB=`(cd ${PACKAGE_DIR}; ls -d dash-*         | grep -v .txt | tail -1)`
 PACKAGE_LINT_LIB=`(cd ${PACKAGE_DIR}; ls -d package-lint-* | grep -v .txt | tail -1)`
   ELISP_LINT_LIB=`(cd ${PACKAGE_DIR}; ls -d elisp-lint-*   | grep -v .txt | tail -1)`

LOCAL_PACKAGE_DIR=`cygpath -ma ~/.emacs.d/my/packages`
            FILES="${LOCAL_PACKAGE_DIR}/ipe/ipe.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-char.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-custom.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-defn.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-edit.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-help.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-line.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-list.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-menu.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-mouse.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-read.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-updt.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/ipe-word.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/test/ipe-test.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/modes/ipe-c-mode.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/modes/ipe-html-mode.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/modes/ipe-markdown-mode.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/modes/ipe-texinfo-mode.el \
                   ${LOCAL_PACKAGE_DIR}/ipe/insert-pair-edit.el"

${EMACS} \
    --quick \
    --batch \
    --directory ${PACKAGE_DIR}/${DASH_LIB}/ \
    --directory ${PACKAGE_DIR}/${PACKAGE_LINT_LIB}/ \
    --load      ${PACKAGE_DIR}/${ELISP_LINT_LIB}/elisp-lint.el \
    --funcall   elisp-lint-files-batch \
    ${FILES} \
    2>&1 \
    | sed 's/^..[0-9;]*m//g'

rm -f ipe*.elc insert-pair-edit*.elc ipe-autoloads.el
