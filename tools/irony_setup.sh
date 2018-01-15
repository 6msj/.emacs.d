#/bin/bash
install_name_tool -change @rpath/libclang.dylib /usr/local/Cellar/llvm/4.0.0/lib/libclang.dylib ~/.emacs.d/servers/26/irony/bin/irony-server
install_name_tool -change @rpath/libclang.dylib /usr/local/Cellar/llvm/4.0.0/lib/libclang.dylib ~/.emacs.d/servers/25/irony/bin/irony-server
