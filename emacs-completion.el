;;; emacs-completion.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun emacs-choose-pure-completions (completions-list)
  "从 `completion-all-completions` 返回的列表中提取纯粹的补全字符串。
移除可能存在的末尾 `base-size`。"
  (let ((result '())
        (current completions-list))
    ;; 循环遍历列表，直到遇到非 cons cell 或者 cdr 是整数的情况
    (while (and (consp current)        ; 确保当前元素是 cons cell
                (not (integerp (cdr current)))) ; 确保 cdr 不是 base-size (整数)
      (push (car current) result) ; 将当前 cons cell 的 car 添加到结果列表
      (setq current (cdr current))) ; 移动到下一个 cons cell

    ;; 循环结束后，`current` 可能有几种情况：
    ;; 1. nil：如果原始列表是正常列表 (proper list)，例如 ("a" "b")
    ;; 2. 一个 cons cell，其 cdr 是整数：例如 ("b" . 5)，这是 `base-size` 所在的部分
    ;; 3. 一个 cons cell，其 cdr 不是整数但也不是列表 (不常见，但理论上可能)

    ;; 如果 `current` 仍然是一个 cons cell，说明我们停在了最后一个实际的补全字符串处。
    ;; 它的 `car` 是最后一个补全字符串，`cdr` 可能是 `base-size` 或 `nil`。
    (when (consp current)
      (push (car current) result)) ; 添加最后一个补全字符串

    ;; 因为我们使用 `push`，所以结果是反向的，需要 `nreverse`
    (nreverse result)))

(defun emacs-choose-output (str alist)
  "Extracts all text property segments from STR.
Returns a list of alists, where each alist represents a segment:
(:segment \"substring\" :start 0 :end 5 :properties (face bold))"
  (let ((results '())
        (current-pos 0)
        (str-len (length str))
        (table (make-hash-table)))
    (while (< current-pos str-len)
      (let* ((next-change-pos (or (next-property-change current-pos str) str-len))
             (props (text-properties-at current-pos str)))
        (when props
          (push (list
                 (cons :start current-pos)
                 (cons :end next-change-pos))
                results))
        (setq current-pos next-change-pos)))
    (list (cons :normalized (substring-no-properties str))
          (cons :raw (alist-get str alist nil nil #'equal))
          (cons :indexes (nreverse results))))) ; Reverse to maintain original order

(defun emacs-choose-all-completion (string table pred point &optional metadata)
  (let* ((match-items (emacs-choose-pure-completions
                       (completion-all-completions string table pred point metadata)))
         (match-strs (when (integerp (cdr match-items))(map-filter #'stringp match-items)))
         (output-items (mapcar (lambda (item) (emacs-choose-output item table))  match-items)))
    ;; (print (json-encode (list :data output-items)))
    (base64-encode-string (encode-coding-string (json-encode (list :data output-items)) 'utf-8))))

(provide 'emacs-completion)
;;; emacs-completion.el ends here

