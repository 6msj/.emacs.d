;;; yarn.el --- Interface to yarn from Emacs.

;; ((name . "MyAwesomeProject")
;;  (version . "0.0.1")
;;  (private . t)
;;  (scripts
;;   (start . "node node_modules/react-native/local-cli/cli.js start")
;;   (test . "jest"))
;;  (dependencies
;;   (react . "16.0.0-alpha.6")
;;   (react-native . "^0.44.0"))
;;  (devDependencies
;;   (@types/enzyme . "^2.8.0")
;;   (@types/jest . "^19.2.3")
;;   (@types/react . "^15.0.23")
;;   (@types/react-native . "^0.43.9")
;;   (@types/react-test-renderer . "^15.5.0")
;;   (babel-jest . "19.0.0")
;;   (babel-preset-react-native . "1.9.1")
;;   (enzyme . "^2.8.2")
;;   (jest . "19.0.2")
;;   (react-addons-test-utils . "^15.5.1")
;;   (react-dom . "^15.5.4")
;;   (react-test-renderer . "^15.5.4")
;;   (ts-jest . "^19.0.14")
;;   (typescript . "^2.3.2"))
;;  (jest
;;   (preset . "react-native")
;;   (moduleFileExtensions . ["ts" "tsx" "js"])
;;   (transform
;;    (^\.+\\\.\
;;     (js\)$ . "<rootDir>/node_modules/babel-jest")
;;     (\\\.\
;;      (ts|tsx\)$ . "<rootDir>/node_modules/ts-jest/preprocessor.js"))
;;     (testRegex . "(/__tests__/.*|\\.(test|spec))\\.(ts|tsx|js)$")
;;     (testPathIgnorePatterns . ["\\.snap$" "<rootDir>/node_modules/" "<rootDir>/lib/"])
;;     (cacheDirectory . ".jest/cache")))))

(require 'json)

;;;###autoload
(defun +yarn-get-package-info-alist ()
  "Return package.json as an alist."
  (interactive)
  (json-read-file
   (concat (or (locate-dominating-file default-directory "package.json")
               (locate-dominating-file
                "/Users/james/Code/React/TypeScript-React-Native-Starter"
                "package.json"))
           "package.json")))

;;;###autoload
(defun +yarn-run-script ()
  "Run a yarn script run package.json."
  (interactive)
  (let* ((pkg-alist (+yarn-get-package-info-alist))
         (cmd-list (alist-get 'scripts pkg-alist))
         (cmd (completing-read
               "Yarn Command > "
               (mapcar (lambda (x)
                         (concat  (symbol-name (car x)) " . " (cdr x)))
                       cmd-list)))
         (key (intern (car (split-string cmd ".")))))
    (compilation-start (alist-get key cmd-list)
                       'compilation-mode (lambda (_name) "*yarn*") nil)))

(provide 'yarn)
;;; yarn.el ends here
