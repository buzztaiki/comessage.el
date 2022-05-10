# comessage

`comessage-mode` is a minor mode that provides message coexistence.
It will resolve multiple packages competing for message output (e.g. flymake and eldoc).

## Example
For example, to separate flymake and eldoc messages, you can do as the following configuration:

```lisp
(comessage-define-advice my/comessage-group-flymake 'flymake)
(comessage-define-advice my/comessage-group-eldoc 'eldoc)

(advice-add 'flymake-goto-next-error :around #'my/comessage-group-flymake)
(advice-add 'eldoc-message :around #'my/comessage-group-eldoc)
(advice-add 'eldoc-minibuffer-message :around #'my/comessage-group-eldoc)
(comessage-mode 1)
```
## License
GPLv3
