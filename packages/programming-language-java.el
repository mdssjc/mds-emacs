;;
;; Programming-Language-Java.el
;;
;; autor: Marcelo dos Santos
;; url  : https://github.com/mdssjc/mds-emacs
;;
;; Linguagem de Programação (Programming Language)
;; Java
;; FIXME -> o pacote está em testes
(use-package meghanada
  :ensure t
  :init (add-hook 'java-mode-hook (lambda () (meghanada-mode t))))
