# Second Sight

Quickly view file-contents without visiting the file, as if you had _second-sight_.

## What's This

You can view file-contents without visiting the file.

This package will reduce the number of times you open a buffer.  
For example, after you call `find-file`, you can see the file-content without jumping to the file.

[![thing-at-point](https://i.gyazo.com/04cf3320ecec8a007354d72456fb4d26.gif)](https://gyazo.com/04cf3320ecec8a007354d72456fb4d26)

[![counsel](https://i.gyazo.com/7050db79f0e21280cd67f00e46a0ec65.gif)](https://gyazo.com/7050db79f0e21280cd67f00e46a0ec65)

## Setup

### Requirements

This package requires [posframe.el](https://github.com/tumashu/posframe).  
You need to install `posframe` before.

### Settings

`git clone` and edit your init.el as below.

```elisp
(add-to-list 'load-path "/pscp/to/second-sight")
(require 'second-sight)

;; optional but recommended
(global-set-key (kbd "YOUR KEY") 'second-sight)
```

## Usage

### Basic Usage

After evaluating this expression, `posframe` will appear with the file-content.

```elisp
(second-sight-file "filepath")
```

When your cursor is on the filepath like below, you can execute `M-x second-sight`.

```
"/pa|th/to/file"
    ↑ your cursor
```

### Advanced Usage

This package partially supports [ivy/counsel](https://github.com/abo-abo/swiper).  
After you execute `counsel-find-file`, `counsel-recentf`, `counsel-git` etc, you can call `second-sight`.

**Notice:**  
This package use a bit hacks to get one of `ivy` candidates like `(ivy-state-current ivy-last)`
So if you want to use this package with `counsel`, you should assign a short-cut key as below to avoid selecting a wrong candidate.

```elisp
(global-set-key (kbd "YOUR KEY") 'second-sight)
```
