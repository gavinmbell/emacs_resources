;/**************************************************************************
;*                                                                         *
;*   Copyright (c) 2012, The 6Th Column Project, LLC.                      *
;*   Written by: The codewrx.org development team .et al                   *
;*                                                                         *
;*   All rights reserved. This file is part of the:                        *
;*   The 6th Column Project, LLC / CodeWRX Software Stack                  *
;*                                                                         *
;*   For details, see http://6thcolum.org/                                 *
;*   Please also read this link                                            *
;*    http://6thcolumn.org/LICENSE                                         *
;*                                                                         *
;*   Redistribution and use in source and binary forms, with or            *
;*   without modification, are permitted provided that the following       *
;*   conditions are met:                                                   *
;*                                                                         *
;*   * Redistributions of source code must retain the above copyright      *
;*   notice, this list of conditions and the disclaimer below.             *
;*                                                                         *
;*   * Redistributions in binary form must reproduce the above copyright   *
;*   notice, this list of conditions and the disclaimer (as noted below)   *
;*   in the documentation and/or other materials provided with the         *
;*   distribution.                                                         *
;*                                                                         *
;*   Neither the name of The 6Th Column [codewrx, techwrx, cogents] nor    *
;*   the names of its contributors may be used to endorse or promote       *
;*   products derived from this software without specific prior written    *
;*   permission.                                                           *
;*                                                                         *
;*   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *
;*   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *
;*   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *
;*   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL LAWRENCE   *
;*   LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR    *
;*   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,          *
;*   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT      *
;*   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF      *
;*   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND   *
;*   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,    *
;*   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT    *
;*   OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF    *
;*   SUCH DAMAGE.                                                          *
;*                                                                         *
;**************************************************************************/
; 
; Description:
; 
; It is often adantageous to scroll the emacs screen underneath the cursor.
; This code allows you to do that (and other stuff
;
;**************************************************************************/

(global-set-key "\eg" 'goto-line)
(global-set-key "\^h" 'delete-backward-char)
(global-set-key "\^x\^l" 'repeat-complex-command)


;; Scrolling Fixes
;; Panning and scrolling commands 

(defun partial-scroll-left (columns)
  "Scroll window left by arg columns"
  (interactive "p")
  (scroll-left (if (= columns 1) 20
		 columns)))

(defun partial-scroll-right (columns)
  "Scroll window right by arg columns"
  (interactive "p")
  (scroll-right (if (= columns 1) 20
		  columns)))

(defun scroll-left-one-column ()
  "Scroll window left by 1 columns"
  (interactive)
  (scroll-left 1))

(defun scroll-right-one-column ()
  "Scroll window right by 1 columns"
  (interactive)
  (scroll-right 1))

(defun scroll-up-one-line ()
  "Scroll by 1 line."
  (interactive)
  (scroll-up 1))

(defun scroll-down-one-line ()
  "Scroll by 1 line."
  (interactive)
  (scroll-down 1))

(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(defun scroll-down-page-place ()
  (interactive)
  (scroll-up )
  (recenter))

(defun scroll-up-page-place ()
  (interactive)
  (scroll-down )
  (recenter))

(defun my-tabify ()
  "Tab things according to mode"
  (interactive)
  (setq cline (count-lines (point-min) (point)))
  (beginning-of-buffer)
  (while ( not (eobp) )
    (next-line 1)
    )
  (goto-line cline)
  )

;My scrolling stuff
(global-set-key "\^v" 'scroll-down-page-place)    
(global-set-key "\ev" 'scroll-up-page-place)
(global-set-key [S-up]       'scroll-up-page-place)
(global-set-key [S-down]     'scroll-down-page-place)
(global-set-key [S-right]    'scroll-left-one-column)
(global-set-key [S-left]     'scroll-right-one-column)
(global-set-key [C-right]  'partial-scroll-left)
(global-set-key [C-left]   'partial-scroll-right)
(global-set-key [C-down]	'scroll-down-in-place)
(global-set-key [C-up]	'scroll-up-in-place)





