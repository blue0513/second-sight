# WIP: Second Sight

Quickly view file-contents without visiting the file, as if you had __second-sight__.

## What's This

You can view file-contents without opening or visiting the file.

This package will reduce the number of times you open a buffer.  
For example, after you call `find-file`, you can see the file-content without jumping to the file.

![thing-at-point.gif]()

![counsel.gif]()

## Setup

`git clone` and edit your init.el as below.

```elisp
(add-to-list 'load-path "/pscp/to/second-sight")
(require 'second-sight)

;; optional but recommended
(global-set-key (kbd "YOUR KEY") 'second-sight)
```

### Requirements

This package requires [posframe.el](https://github.com/tumashu/posframe).  
You need to install `posframe` before.

## Usage

### Basic Usage

After evaluating this expression, then `posframe` will appear with file-content.

```elisp
(second-sight-file "filepath")
```

When your cursor is on the filepath like below, you can execute `M-x second-sight-at-point`.

```
"/pa|th/to/file"
    ↑ your cursor
```

### Advanced Usage

This package partially supports [ivy/counsel](https://github.com/abo-abo/swiper).  
After you execute `counsel-find-file`, `counsel-recentf`, `counsel-git` etc, you can call `second-sight-counsel`.

**Notice:**  
This package use a bit hacks to get one of `ivy` candidates like `(ivy-state-current ivy-last)`
So if you want to use this package with `counsel`, you should assign short-cut key as below to avoid selecting a wrong candidate.

```elisp
(global-set-key (kbd "YOUR KEY") 'second-sight)
```
